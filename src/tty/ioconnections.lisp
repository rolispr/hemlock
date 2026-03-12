;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; Native backend: sb-sys event loop + sb-bsd-sockets + sb-ext:run-program.
;;; Replaces iolib entirely.

(in-package :hemlock.tty)

#+sbcl (declaim (optimize (speed 2)))

;;;;
;;;; Event loop — delegates to SBCL's built-in sb-sys fd handler system.
;;;;

(defmethod invoke-with-new-event-loop ((backend (eql :sb-sys)) fun)
  (funcall fun))

(defmethod make-event-loop ((backend (eql :sb-sys)))
  nil)

(defmethod invoke-with-existing-event-loop ((backend (eql :sb-sys)) loop fun)
  (declare (ignore loop))
  (funcall fun))

(defmethod dispatch-events-with-backend ((backend (eql :sb-sys)))
  (drain-pending-invocations)
  (sb-sys:serve-event))

(defmethod dispatch-events-no-hang-with-backend ((backend (eql :sb-sys)))
  (drain-pending-invocations)
  (sb-sys:serve-event 0))

(defmethod dispatch-events-with-timeout-backend ((backend (eql :sb-sys)) seconds)
  (drain-pending-invocations)
  (sb-sys:serve-event seconds))

;;; invoke-later: push onto a list and drain at the next dispatch point.
(defvar *pending-invocations* nil)

(defmethod invoke-later ((backend (eql :sb-sys)) fun)
  (push fun *pending-invocations*))

(defun drain-pending-invocations ()
  (loop while *pending-invocations*
        do (funcall (pop *pending-invocations*))))


;;;;
;;;; fd-readable-p — poll(2) with timeout 0
;;;;

;;; struct pollfd layout: fd(int32) events(int16) revents(int16)
(defconstant +pollin+ 1)

(defun fd-readable-p (fd)
  "Return true if FD has data available without blocking."
  (cffi:with-foreign-object (pfd :uint64)
    (setf (cffi:mem-ref pfd :int32 0) fd
          (cffi:mem-ref pfd :int16 4) +pollin+
          (cffi:mem-ref pfd :int16 6) 0)
    (plusp (cffi:foreign-funcall "poll" :pointer pfd :uint32 1 :int 0 :int))))


;;;;
;;;; SB-SYS-CONNECTION — base class
;;;;

(defclass sb-sys-connection (io-connection)
  ((read-fd   :initarg :read-fd  :initarg :fd :initform nil
              :accessor connection-read-fd)
   (write-fd  :initarg :write-fd :initarg :fd :initform nil
              :accessor connection-write-fd)
   (read-handler  :initform nil :accessor connection-read-handler)
   (write-handler :initform nil :accessor connection-write-handler)
   (write-buffers :initform nil :accessor connection-write-buffers)))

(defmethod (setf connection-read-fd) :after
    ((newval t) (connection sb-sys-connection))
  (when (connection-read-fd connection)
    (install-read-handler connection)))

(defun install-read-handler (connection)
  (let* ((fd (connection-read-fd connection))
         (handler
           (sb-sys:add-fd-handler
            fd :input
            (lambda (fd)
              (declare (ignore fd))
              (when (eq (process-incoming-data connection) :eof)
                (let ((h (connection-read-handler connection)))
                  (when h
                    (sb-sys:remove-fd-handler h)
                    (setf (connection-read-handler connection) nil))))))))
    (setf (connection-read-handler connection) handler)))

(defmethod %read ((connection sb-sys-connection))
  (let* ((fd     (connection-read-fd connection))
         (buffer (connection-input-buffer connection))
         (n (cffi:with-pointer-to-vector-data (ptr buffer)
              (cffi:foreign-funcall "read"
                                    :int fd :pointer ptr
                                    :size (length buffer) :long))))
    (cond ((or (zerop n) (minusp n)) :eof)
          (t (subseq buffer 0 n)))))

(defmethod delete-connection :before ((connection sb-sys-connection))
  (with-slots (read-fd write-fd read-handler write-handler) connection
    (when read-handler
      (sb-sys:remove-fd-handler read-handler)
      (setf read-handler nil))
    (when write-handler
      (sb-sys:remove-fd-handler write-handler)
      (setf write-handler nil))
    (when read-fd
      (cffi:foreign-funcall "close" :int read-fd :int)
      (setf read-fd nil))
    (when (and write-fd (not (eql write-fd read-fd)))
      (cffi:foreign-funcall "close" :int write-fd :int)
      (setf write-fd nil))))

(defmethod connection-listen ((connection sb-sys-connection))
  (fd-readable-p (connection-read-fd connection)))

(defmethod connection-write (data (connection sb-sys-connection))
  (let* ((bytes (filter-connection-output connection data))
         (fd    (connection-write-fd connection))
         (need-handler (null (connection-write-buffers connection))))
    (check-type bytes (simple-array (unsigned-byte 8) (*)))
    (setf (connection-write-buffers connection)
          (nconc (connection-write-buffers connection) (list bytes)))
    (when need-handler
      (labels ((write-some ()
                 (let ((buf (first (connection-write-buffers connection))))
                   (when buf
                     (let ((n (cffi:with-pointer-to-vector-data (ptr buf)
                                (cffi:foreign-funcall "write"
                                                      :int fd :pointer ptr
                                                      :size (length buf) :long))))
                       (cond
                         ((minusp n)
                          (error "write error on ~A" connection))
                         ((< n (length buf))
                          (setf (first (connection-write-buffers connection))
                                (subseq buf n)))
                         (t
                          (pop (connection-write-buffers connection)))))
                     (when (null (connection-write-buffers connection))
                       (let ((h (connection-write-handler connection)))
                         (when h
                           (sb-sys:remove-fd-handler h)
                           (setf (connection-write-handler connection) nil))))))))
        (setf (connection-write-handler connection)
              (sb-sys:add-fd-handler fd :output
                                     (lambda (fd)
                                       (declare (ignore fd))
                                       (write-some))))))))


;;;;
;;;; PROCESS-CONNECTION/SB-SYS — spawned child process via sb-ext:run-program
;;;;

(defclass process-connection/sb-sys
    (process-connection-mixin sb-sys-connection)
  ((process :initform nil :accessor connection-process)))

(defmethod connection-pid ((connection process-connection/sb-sys))
  (let ((p (connection-process connection)))
    (when p (sb-ext:process-pid p))))

(defmethod initialize-instance :after
    ((instance process-connection/sb-sys) &key)
  (with-slots (read-fd write-fd process command directory
               (slave-fd hemlock.command::slave-fd)) instance
    (connection-note-event instance :initialized)
    (when (stringp command)
      (setf command (cl-ppcre:split " " command)))
    (assert (every #'stringp command))
    (assert command)
    (let* ((prog (car command))
           (args (cdr command)))
      (if slave-fd
          ;; PTY mode: child's stdin/stdout/stderr go through the slave PTY fd.
          ;; The pipelike-connection reads/writes the master side, so we do NOT
          ;; set read-fd/write-fd or install a read handler here.
          (let* ((slave-stream (sb-sys:make-fd-stream slave-fd :input t :output t
                                                      :external-format :utf-8
                                                      :name "PTY slave"))
                 (custom-env (hemlock.command::connection-environment instance))
                 (env (or custom-env
                          (list* "TERM=dumb"
                                 (remove-if (lambda (s)
                                              (or (uiop:string-prefix-p "TERM=" s)
                                                  (uiop:string-prefix-p "COLORTERM=" s)))
                                            (sb-ext:posix-environ)))))
                 (proc (sb-ext:run-program prog args
                                           :input  slave-stream
                                           :output slave-stream
                                           :error  slave-stream
                                           :wait   nil
                                           :pty    nil
                                           :environment env
                                           :directory (or directory
                                                         (uiop:getcwd)))))
            ;; Don't close slave-stream here — sb-ext:run-program dups the fd
            ;; and the caller closes the slave fd after we return.
            (setf process proc))
          ;; Pipe mode: use sb-ext:run-program's built-in pipes.
          (let ((proc (sb-ext:run-program prog args
                                          :input  :stream
                                          :output :stream
                                          :wait   nil
                                          :directory (or directory "/"))))
            (setf process proc
                  write-fd (sb-sys:fd-stream-fd (sb-ext:process-input proc))
                  read-fd  (sb-sys:fd-stream-fd (sb-ext:process-output proc)))
            (install-read-handler instance))))
    (note-connected instance)))

(defmethod delete-connection :before ((connection process-connection/sb-sys))
  (let ((p (connection-process connection)))
    (when p
      (sb-ext:process-kill p 15)
      (setf (connection-process connection) nil))))


;;;;
;;;; TCP-CONNECTION/SB-SYS — TCP client socket via sb-bsd-sockets
;;;;

(defclass tcp-connection/sb-sys (tcp-connection-mixin sb-sys-connection)
  ((socket :accessor connection-socket)))

(defmethod initialize-instance :after ((instance tcp-connection/sb-sys) &key)
  (with-slots (read-fd write-fd socket host port) instance
    (connection-note-event instance :initialized)
    (unless (or read-fd write-fd)
      (let* ((sock (make-instance 'sb-bsd-sockets:inet-socket
                                  :type :stream :protocol :tcp))
             (addr (sb-bsd-sockets:make-inet-address host)))
        (sb-bsd-sockets:socket-connect sock addr port)
        (let ((fd (sb-bsd-sockets:socket-file-descriptor sock)))
          (setf socket  sock
                read-fd  fd
                write-fd fd))))
    (install-read-handler instance)
    (note-connected instance)))

(defmethod class-for
    ((backend (eql :sb-sys)) (type (eql 'tcp-connection-mixin)))
  'tcp-connection/sb-sys)

(defmethod class-for
    ((backend (eql :sb-sys)) (type (eql 'process-connection-mixin)))
  'process-connection/sb-sys)


;;;;
;;;; PIPELIKE-CONNECTION/SB-SYS
;;;;

(defclass pipelike-connection/sb-sys
    (pipelike-connection-mixin sb-sys-connection)
  ())

(defmethod initialize-instance :after
    ((instance pipelike-connection/sb-sys) &key)
  (connection-note-event instance :initialized)
  (install-read-handler instance))


;;;;
;;;; PROCESS-WITH-PTY-CONNECTION/SB-SYS
;;;;

(defclass process-with-pty-connection/sb-sys
    (process-with-pty-connection-mixin pipelike-connection/sb-sys)
  ())

(defmethod class-for
    ((backend (eql :sb-sys)) (type (eql 'pipelike-connection-mixin)))
  'pipelike-connection/sb-sys)

(defmethod class-for
    ((backend (eql :sb-sys)) (type (eql 'process-with-pty-connection-mixin)))
  'process-with-pty-connection/sb-sys)


;;;;
;;;; LISTENING-CONNECTION/SB-SYS — TCP server socket
;;;;

(defclass listening-connection/sb-sys (listening-connection)
  ((socket  :accessor connection-socket)
   (fd      :initform nil :accessor connection-fd)
   (handler :initform nil :accessor connection-accept-handler)))

(defmethod initialize-instance :after
    ((instance listening-connection/sb-sys) &key)
  (with-slots (fd socket host port) instance
    (unless fd
      (connection-note-event instance :initialized)
      (let ((addr (if host
                      (sb-bsd-sockets:make-inet-address host)
                      (sb-bsd-sockets:make-inet-address "0.0.0.0"))))
        (flet ((try-bind (p)
                 (let ((sock (make-instance 'sb-bsd-sockets:inet-socket
                                            :type :stream :protocol :tcp)))
                   (setf (sb-bsd-sockets:sockopt-reuse-address sock) t)
                   (sb-bsd-sockets:socket-bind sock addr p)
                   (sb-bsd-sockets:socket-listen sock 5)
                   sock)))
          (setf socket
                (if port
                    (try-bind port)
                    (loop for p from 1024 below 65536
                          do (handler-case
                                 (try-bind p)
                               (:no-error (sock)
                                 (setf port p)
                                 (return sock))
                               (error ()))))))
        (setf fd (sb-bsd-sockets:socket-file-descriptor socket)))))
  (install-accept-handler instance))

(defun install-accept-handler (instance)
  (setf (connection-accept-handler instance)
        (sb-sys:add-fd-handler
         (connection-fd instance) :input
         (lambda (fd)
           (declare (ignore fd))
           (process-incoming-connection instance)))))

(defmethod delete-connection :before ((connection listening-connection/sb-sys))
  (let ((h (connection-accept-handler connection)))
    (when h
      (sb-sys:remove-fd-handler h)
      (setf (connection-accept-handler connection) nil)))
  (sb-bsd-sockets:socket-close (connection-socket connection)))

(defmethod (setf connection-fd) :after
    ((newval t) (connection listening-connection/sb-sys))
  (install-accept-handler connection))

(defun %tcp-connection-from-fd (name fd host port initargs)
  (apply #'make-instance
         'tcp-connection/sb-sys
         :name name
         :fd fd
         :host host
         :port port
         initargs))


;;;;
;;;; TCP-LISTENER/SB-SYS
;;;;

(defclass tcp-listener/sb-sys (tcp-listener-mixin listening-connection/sb-sys)
  ())

(defmethod initialize-instance :after ((instance tcp-listener/sb-sys) &key)
  )

(defmethod convert-pending-connection ((connection tcp-listener/sb-sys))
  (multiple-value-bind (client-sock client-addr)
      (sb-bsd-sockets:socket-accept (connection-socket connection))
    (declare (ignore client-addr))
    (multiple-value-bind (host port)
        (sb-bsd-sockets:socket-peername client-sock)
      (let ((fd (sb-bsd-sockets:socket-file-descriptor client-sock)))
        (%tcp-connection-from-fd
         (format nil "Accepted for: ~A" (connection-name connection))
         fd
         (format nil "~{~A~^.~}" (coerce host 'list))
         port
         (connection-initargs connection))))))

(defmethod class-for
    ((backend (eql :sb-sys)) (type (eql 'tcp-listener-mixin)))
  'tcp-listener/sb-sys)
