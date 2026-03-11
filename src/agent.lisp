;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; hemlock-agent — agent-side agent for hemlock.
;;;
;;; This file is loaded in agent Lisp processes (not in the editor).
;;; Entry point: (hemlock-agent:connect :host "127.0.0.1" :port NNN :name "...")
;;;
;;; Responsibilities:
;;;   - Open TCP connection back to master
;;;   - Call set-up-agent-buffers on master to register
;;;   - Set up wire-backed I/O streams (*terminal-io* etc.)
;;;   - Run wire-native REPL with master-side interactive debugger
;;;
;;; The hemlock package is defined here at a minimum level so that
;;; symbols sent over the wire as ("NAME" "HEMLOCK") can be interned
;;; in the agent and dispatched correctly.

;;; ---- Package setup -------------------------------------------------------

;;; Ensure the hemlock package exists.  If the full editor is already loaded
;;; (thread-agent case), this is a no-op.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :hemlock)
    (defpackage :hemlock
      (:use :common-lisp :trivial-gray-streams)
      (:import-from :hemlock.wire
        #:dispatch-events
        #:dispatch-events-no-hang))))

(defpackage :hemlock.agent
  (:use :common-lisp :hemlock.wire)
  (:export #:connect))


;;; ---- Agent state ---------------------------------------------------------

(in-package :hemlock.agent)

(defvar *agent-wire* nil
  "The single wire connecting this agent to the hemlock master.")


;;; ---- Dispatch-events for the agent event loop ---------------------------
;;;
;;; In the full hemlock editor, dispatch-events is defined in connections.lisp
;;; and backed by the async sb-sys event loop.  The agent reuses that same
;;; loop — sb-sys:serve-event fires fd-handlers installed by make-agent-tcp-wire.

(declaim (ftype (function () t)
                hemlock.wire:dispatch-events
                hemlock.wire:dispatch-events-no-hang))

(setf (fdefinition 'hemlock.wire:dispatch-events)
      (lambda () (sb-sys:serve-event)))

(setf (fdefinition 'hemlock.wire:dispatch-events-no-hang)
      (lambda () (sb-sys:serve-event 0)))


;;; ---- Socket device -------------------------------------------------------
;;;
;;; A minimal hemlock.wire device backed by a TCP socket fd.
;;; Mirrors what connection-device does in connections.lisp, but without
;;; the full connection abstraction.

(defstruct (agent-device
             (:include hemlock.wire:device)
             (:conc-name agent-device-)
             (:constructor make-agent-device (fd)))
  "Wire device backed by a TCP socket fd."
  (fd   (error "missing") :type fixnum)
  ;; Bumped by the fd-handler each time new bytes arrive in ibuf.
  (tick 0                 :type fixnum))

(defmethod hemlock.wire:device-listen ((device agent-device))
  "Return T if wire ibuf has unconsumed bytes or the fd has data pending."
  (let ((wire (hemlock.wire:device-wire device)))
    (or (and wire
             (< (hemlock.wire::wire-ibuf-offset wire)
                (hemlock.wire::wire-ibuf-end    wire)))
        (sb-sys:wait-until-fd-usable (agent-device-fd device) :input 0))))

(defmethod hemlock.wire:device-read ((device agent-device) buffer)
  "Block via sb-sys event loop until the fd-handler deposits bytes into ibuf."
  (declare (ignore buffer))
  (let ((prev (agent-device-tick device)))
    (loop while (eql prev (agent-device-tick device))
          do (sb-sys:serve-event)))
  ;; Data was appended to ibuf by the fd-handler; return 0 so
  ;; fill-input-buffer doesn't try to account for our return value.
  0)

(defmethod hemlock.wire:device-write ((device agent-device) buffer
                                      &optional (end (length buffer)))
  (cffi:with-pointer-to-vector-data (ptr buffer)
    (cffi:foreign-funcall "write"
                          :int     (agent-device-fd device)
                          :pointer ptr
                          :size    end
                          :long)))


;;; ---- TCP wire creation --------------------------------------------------

(defun make-agent-tcp-wire (host port)
  "Open a blocking TCP connection to HOST:PORT.
   Install an sb-sys fd-handler that feeds bytes into the wire ibuf.
   Return the wire."
  (let* ((sock (make-instance 'sb-bsd-sockets:inet-socket
                              :type :stream :protocol :tcp))
         (addr (sb-bsd-sockets:make-inet-address host)))
    (sb-bsd-sockets:socket-connect sock addr port)
    (let* ((fd     (sb-bsd-sockets:socket-file-descriptor sock))
           (device (make-agent-device fd))
           (wire   (hemlock.wire:make-wire device))
           (ibuf   (make-array 65536 :element-type '(unsigned-byte 8))))
      (sb-sys:add-fd-handler fd :input
        (lambda (fd)
          (declare (ignore fd))
          (let ((n (cffi:with-pointer-to-vector-data (ptr ibuf)
                     (cffi:foreign-funcall "read"
                                           :int     (agent-device-fd device)
                                           :pointer ptr
                                           :size    65536
                                           :long))))
            (when (plusp n)
              (hemlock.wire:device-append-to-input-buffer
               device (subseq ibuf 0 n))
              (incf (agent-device-tick device))))))
      wire)))


;;; ---- Agent-side hemlock package -----------------------------------------
;;;
;;; Everything below runs in the HEMLOCK package so that function symbols
;;; sent over the wire from the master (e.g. "MADE-REPL-BUFFERS" "HEMLOCK")
;;; resolve correctly in the agent.

(in-package :hemlock)


;;; -- repl-stream: wire-backed gray stream (agent side) --------------------

(defconstant repl-stream-output-buffer-size 512)

(defclass repl-stream (trivial-gray-stream-mixin
                       fundamental-character-output-stream
                       fundamental-character-input-stream)
  ((wire
    :initarg  :wire
    :initform nil
    :accessor repl-stream-wire)
   (buf-data
    :initarg  :buf-data
    :initform nil
    :accessor repl-stream-buf-data)
   (output-buffer
    :initform (make-string repl-stream-output-buffer-size)
    :accessor repl-stream-output-buffer
    :type     simple-string)
   (output-buffer-index
    :initform 0
    :accessor repl-stream-output-buffer-index
    :type     fixnum)
   (char-pos
    :initform 0
    :accessor repl-stream-char-pos
    :type     fixnum)
   (line-length
    :initform 80
    :accessor repl-stream-line-length)
   (current-input
    :initform nil
    :accessor repl-stream-current-input
    :type     list)
   (input-read-index
    :initform 0
    :accessor repl-stream-input-read-index
    :type     fixnum)))

(defun make-repl-stream (wire buf-data)
  (make-instance 'repl-stream :wire wire :buf-data buf-data))


;;; Called by master to deliver input and adjust line-length.

(defun repl-stream-accept-input (remote input)
  (let ((stream (hemlock.wire:remote-object-value remote)))
    (setf (repl-stream-current-input stream)
          (nconc (repl-stream-current-input stream)
                 (list (etypecase input
                         (string
                          (let ((nl (position #\newline input :from-end t)))
                            (setf (repl-stream-char-pos stream)
                                  (if nl (- (length input) nl 1) (length input)))
                            input)))))))
  nil)

(defun repl-stream-set-line-length (remote length)
  (let ((stream (hemlock.wire:remote-object-value remote)))
    (setf (repl-stream-line-length stream) length)))


;;; Gray stream methods.

(defun repl-stream-listen (stream)
  (loop
    (let* ((current (repl-stream-current-input stream))
           (first   (first current)))
      (cond ((null current) (return nil))
            ((>= (repl-stream-input-read-index stream)
                 (length (the simple-string first)))
             (pop (repl-stream-current-input stream))
             (setf (repl-stream-input-read-index stream) 0))
            (t (return t))))))

(defmethod stream-listen ((stream repl-stream))
  (repl-stream-listen stream))

(defun wait-for-repl-input (stream)
  (unless (repl-stream-listen stream)
    (loop until (repl-stream-listen stream)
          do (dispatch-events))))

(defmethod stream-read-char ((stream repl-stream))
  (stream-force-output stream)
  (wait-for-repl-input stream)
  (let ((first (first (repl-stream-current-input stream))))
    (prog1 (schar first (repl-stream-input-read-index stream))
      (incf (repl-stream-input-read-index stream)))))

(defmethod stream-read-char-no-hang ((stream repl-stream))
  (cond ((repl-stream-listen stream)
         (stream-force-output stream)
         (let ((first (first (repl-stream-current-input stream))))
           (prog1 (schar first (repl-stream-input-read-index stream))
             (incf (repl-stream-input-read-index stream)))))
        (t nil)))

(defun repl-stream-flsbuf (stream)
  (when (and (repl-stream-wire stream)
             (repl-stream-output-buffer stream)
             (not (zerop (repl-stream-output-buffer-index stream))))
    (hemlock.wire:remote (repl-stream-wire stream)
      (repl-buffer-output-string
       (repl-stream-buf-data stream)
       (subseq (the simple-string (repl-stream-output-buffer stream))
               0 (repl-stream-output-buffer-index stream))))
    (setf (repl-stream-output-buffer-index stream) 0)))

(defmethod stream-write-char ((stream repl-stream) char)
  (declare (base-char char))
  (when (= (repl-stream-output-buffer-index stream) repl-stream-output-buffer-size)
    (repl-stream-flsbuf stream))
  (setf (schar (repl-stream-output-buffer stream)
               (repl-stream-output-buffer-index stream))
        char)
  (incf (repl-stream-output-buffer-index stream))
  (incf (repl-stream-char-pos stream))
  (when (= (char-code char) (char-code #\newline))
    (repl-stream-flsbuf stream)
    (setf (repl-stream-char-pos stream) 0)
    (hemlock.wire:wire-force-output (repl-stream-wire stream)))
  char)

(defmethod stream-unread-char ((stream repl-stream) char)
  (let ((first (first (repl-stream-current-input stream))))
    (cond ((and (stringp first)
                (> (repl-stream-input-read-index stream) 0))
           (setf (schar first (decf (repl-stream-input-read-index stream)))
                 char))
          (t
           (push (string char) (repl-stream-current-input stream))
           (setf (repl-stream-input-read-index stream) 0)))))

(defmethod close ((stream repl-stream) &key abort)
  (unless abort (force-output stream)))

(defmethod stream-clear-input ((stream repl-stream))
  (when (repl-stream-wire stream)
    (hemlock.wire:remote-value (repl-stream-wire stream)
      (repl-buffer-clear-input (repl-stream-buf-data stream))))
  (setf (repl-stream-current-input stream) nil
        (repl-stream-input-read-index stream) 0))

(defmethod stream-finish-output ((stream repl-stream))
  (when (repl-stream-wire stream)
    (repl-stream-flsbuf stream)
    (hemlock.wire:remote-value (repl-stream-wire stream)
      (repl-buffer-finish-output (repl-stream-buf-data stream))))
  t)

(defmethod stream-force-output ((stream repl-stream))
  (stream-finish-output stream)
  t)

(defmethod stream-line-column  ((stream repl-stream)) (repl-stream-char-pos    stream))
(defmethod stream-line-length  ((stream repl-stream)) (repl-stream-line-length stream))
(defmethod stream-clear-output ((stream repl-stream))
  (setf (repl-stream-output-buffer-index stream) 0))

(defmethod stream-write-sequence ((stream repl-stream) (seq string) start end &key)
  (loop for i from start below end do (write-char (elt seq i) stream)))

(defmethod stream-read-sequence ((stream repl-stream) (seq string) start end &key)
  (loop for i from start below end do (setf (elt seq i) (read-char stream))))


;;; -- Agent-side state variables -------------------------------------------

(defvar *in-hemlock-agent-p* nil
  "T when this Lisp is running as a hemlock agent (set by hemlock.agent:connect).")

(defvar *io*          nil "Stream for the editor's interactive agent buffer.")
(defvar *background-io* nil "Stream for the editor's background buffer.")

(defvar *abort-operations* nil "T iff we should ignore incoming operations.")
(defvar *inside-operation* nil "T iff we are currently processing an operation.")

(defvar *compiler-wire*         nil)
(defvar *compiler-error-stream* nil)
(defvar *compiler-note*         nil)

(defvar *original-terminal-io*    nil)
(defvar *original-standard-input* nil)
(defvar *original-standard-output* nil)
(defvar *original-error-output*   nil)
(defvar *original-debug-io*       nil)
(defvar *original-query-io*       nil)
(defvar *original-trace-output*   nil)

(defvar *eval-form-stream*
  (make-two-way-stream (make-concatenated-stream) (make-broadcast-stream)))


;;; -- Stream setup (called by master on agent) -----------------------------

(defun connect-stream (remote-buffer)
  "Create a repl-stream and register it with the master buffer."
  (let ((stream (make-repl-stream hemlock.wire:*current-wire* remote-buffer)))
    (hemlock.wire:remote hemlock.wire:*current-wire*
      (repl-buffer-set-stream remote-buffer
                              (hemlock.wire:make-remote-object stream)))
    stream))

(defun made-repl-buffers (agent-info background-info)
  "Called by master after set-up-agent-buffers.
   Redirects all standard streams to wire-backed repl-streams."
  (setf *original-terminal-io* *terminal-io*)
  (macrolet ((frob (symbol new-value)
               `(setf ,(intern (concatenate 'simple-string
                                            (symbol-name '#:*original-)
                                            (subseq (string symbol) 1)))
                      ,symbol
                      ,symbol ,new-value)))
    (frob *terminal-io*       (connect-stream agent-info))
    (frob *standard-input*    (make-synonym-stream '*terminal-io*))
    (frob *standard-output*   *standard-input*)
    (frob *error-output*      *standard-input*)
    (frob *debug-io*          *standard-input*)
    (frob *query-io*          *standard-input*)
    (frob *trace-output*      *standard-input*))
  (setf *background-io* (connect-stream background-info))
  (setf *io* *terminal-io*)
  nil)

(defun editor-died ()
  "Called by master when it shuts down."
  (macrolet ((frob (symbol)
               (let ((orig (intern (concatenate 'simple-string
                                                (symbol-name '#:*original-)
                                                (subseq (string symbol) 1)))))
                 `(when ,orig (setf ,symbol ,orig)))))
    (frob *terminal-io*)
    (frob *standard-input*)
    (frob *standard-output*)
    (frob *error-output*)
    (frob *debug-io*)
    (frob *query-io*)
    (frob *trace-output*))
  (setf *background-io* nil)
  (format t "~2&Connection to editor died.~%"))


;;; -- Agent eval/compile operations ----------------------------------------
;;;
;;; These functions are called by the master (via wire) to evaluate or
;;; compile forms in the agent.

(defun stringify-list (list)
  (mapcar #'prin1-to-string list))

(defmacro do-operation ((note package terminal-io) &body body)
  `(let ((aborted t)
         (*terminal-io* (if ,terminal-io
                          (hemlock.wire:remote-object-value ,terminal-io)
                          *terminal-io*))
         (*package* (maybe-make-package ,package)))
     (unwind-protect
         (unless *abort-operations*
           (when (eq :was-in-debugger
                     (catch 'abort-operation
                       (let ((*inside-operation* t))
                         (hemlock.wire:remote hemlock.wire:*current-wire*
                           (operation-started ,note))
                         (hemlock.wire:wire-force-output hemlock.wire:*current-wire*)
                         ,@body
                         (setf aborted nil))))
             (format t "~&[Operation aborted.]~%")))
       (hemlock.wire:remote hemlock.wire:*current-wire*
         (operation-completed ,note aborted))
       (hemlock.wire:wire-force-output hemlock.wire:*current-wire*))))

(defun maybe-make-package (name)
  (cond ((null name) *package*)
        ((find-package name))
        (t
         (hemlock.wire:remote-value (repl-stream-wire *terminal-io*)
           (repl-buffer-output-string
            (repl-stream-buf-data *terminal-io*)
            (format nil "~&Creating package ~A.~%" name)
            t))
         (make-package name))))

(defparameter unique-thingie (gensym)
  "EOF sentinel for READ in server-eval-text.")

(defun server-eval-form (package form)
  (declare (type (or string null) package) (simple-string form))
  (handler-bind
      ((error (lambda (condition)
                (hemlock.wire:remote hemlock.wire:*current-wire*
                  (eval-form-error (format nil "~A~&" condition)))
                (return-from server-eval-form nil))))
    (let ((*package* (if package
                         (or (find-package package)
                             (error "no such package: ~A" package))
                         *package*))
          (*terminal-io* *eval-form-stream*))
      (stringify-list (multiple-value-list (eval (read-from-string form)))))))

(defun server-eval-text (note package text terminal-io)
  (do-operation (note package terminal-io)
    (with-input-from-string (stream text)
      (let ((last-pos 0))
        (handler-bind
            ((error (lambda (condition)
                      (hemlock.wire:remote hemlock.wire:*current-wire*
                        (lisp-error note last-pos
                                    (file-position stream)
                                    (format nil "~A~&" condition))))))
          (loop
            (let ((form (read stream nil unique-thingie)))
              (when (eq form unique-thingie) (return nil))
              (let* ((values (stringify-list (multiple-value-list (eval form))))
                     (pos    (file-position stream)))
                (hemlock.wire:remote hemlock.wire:*current-wire*
                  (eval-text-result note last-pos pos values))
                (setf last-pos pos)))))))))

(defmacro do-compiler-operation ((note package terminal-io error) &body body)
  `(let ((*compiler-note*         ,note)
         (*compiler-error-stream* ,error)
         (*compiler-wire*         hemlock.wire:*current-wire*))
     (do-operation (*compiler-note* ,package ,terminal-io)
       (unwind-protect
           (handler-bind ((error #'compiler-error-handler))
             ,@body)
         (when *compiler-error-stream*
           (force-output *compiler-error-stream*))))))

(defun compiler-error-handler (condition)
  (when *compiler-wire*
    (hemlock.wire:remote *compiler-wire*
      (lisp-error *compiler-note* nil nil
                  (format nil "~A~&" condition)))))

(defmacro with-temporary-file-name ((var) &body body)
  `(invoke-with-temporary-file-name (lambda (,var) ,@body)))

(defun invoke-with-temporary-file-name (fun)
  (multiple-value-bind (fd pathname)
      (sb-posix:mkstemp "/tmp/hemlockXXXXXX")
    (sb-posix:close fd)
    (funcall fun pathname)
    (when (probe-file pathname)
      (delete-file pathname))))

(defun server-compile-text (note package text defined-from terminal-io error-output)
  (declare (ignore defined-from))
  (let ((error-output (if error-output (hemlock.wire:remote-object-value error-output))))
    (do-compiler-operation (note package terminal-io error-output)
      (with-temporary-file-name (tmp)
        (with-open-file (s tmp :direction :output :if-exists :supersede)
          (write-string text s))
        (terpri error-output)
        (load (compile-file tmp))))))

(defun server-compile-file (note package input output error trace load terminal background)
  (declare (ignore error load output trace))
  (macrolet ((frob (x)
               `(if (hemlock.wire:remote-object-p ,x)
                    (hemlock.wire:remote-object-value ,x)
                    ,x)))
    (let ((error-stream (frob background)))
      (do-compiler-operation (note package terminal error-stream)
        (multiple-value-bind (fasl warning-free-p)
            (compile-file (frob input))
          (when fasl (load fasl))
          (format nil "~A ~A" fasl warning-free-p))))))

(defun server-set-package (package)
  (setf *package* (maybe-make-package package)))

(defun server-accept-operations ()
  (setf *abort-operations* nil))


;;; ---- Wire-native REPL ----------------------------------------------------
;;;
;;; Replaces prepl:repl with a REPL that is native to the hemlock wire.
;;; All I/O goes through *terminal-io* (a repl-stream backed by the wire).
;;; Conditions are sent to the master over the wire for interactive debugging.

(defvar *repl-eof-marker* (make-symbol "#<EOF>")
  "Sentinel returned when the wire input stream is closed.")

(defvar *agent-read-fn* nil
  "If non-nil, a function (prompt-string) => form used to read REPL input.
Overridden by linedit:repl to provide TTY line editing for the master REPL.")

(defvar *agent-prompt-fn* nil
  "If non-nil, a function () => string used to generate the REPL prompt.
Overridden by linedit:repl to capture the prompt for line editing.")

(defun agent-repl-prompt ()
  (if *agent-prompt-fn*
      (funcall *agent-prompt-fn*)
      (format nil "~A> " (package-name *package*))))

(defun agent-read-form (prompt)
  (if *agent-read-fn*
      (funcall *agent-read-fn* prompt)
      (progn
        (write-string prompt *terminal-io*)
        (force-output *terminal-io*)
        (read *terminal-io* nil *repl-eof-marker*))))

(defun agent-print-values (values)
  (dolist (v values)
    (fresh-line *terminal-io*)
    (prin1 v *terminal-io*)
    (terpri *terminal-io*)
    (force-output *terminal-io*)))

(defun agent-repl ()
  "Wire-native interactive REPL.
Reads/writes via *terminal-io* (which is the wire-backed repl-stream).
Conditions invoke agent-debugger (via *debugger-hook*), which calls
master-agent-debugger on the master to show an interactive debugger buffer."
  (let ((- nil) (+ nil) (++ nil) (+++ nil)
        (* nil) (** nil) (*** nil)
        (/ nil) (// nil) (/// nil))
    (loop
      (with-simple-restart (abort "Return to top level.")
        (let ((form (agent-read-form (agent-repl-prompt))))
          (when (eq form *repl-eof-marker*)
            (return))
          (setf +++ ++ ++ + + - - form)
          (let ((values (multiple-value-list (eval form))))
            (setf *** ** ** * * (first values)
                  /// // // / / values)
            (agent-print-values values)))))))

(defun agent-debugger (condition hook)
  "Installed as *debugger-hook* on the agent side.
Sends condition + restarts + backtrace to master and blocks for restart choice.
The master shows an interactive debugger buffer and returns the chosen index."
  (declare (ignore hook))
  (let* ((restarts (compute-restarts condition))
         (frames   (ignore-errors
                     (hemlock.introspect:compute-backtrace 0 20)))
         (chosen
           (hemlock.wire:remote-value hemlock.wire:*current-wire*
             (master-agent-debugger
              (format nil "~A" (type-of condition))
              (format nil "~A" condition)
              (mapcar (lambda (r) (format nil "~A" r)) restarts)
              (mapcar (lambda (f)
                        (with-output-to-string (s)
                          (hemlock.introspect:print-frame f s)))
                      frames)))))
    (cond
      ((and (integerp chosen) (<= 0 chosen) (< chosen (length restarts)))
       (invoke-restart-interactively (nth chosen restarts)))
      (t
       (let ((abort-restart (find-restart 'abort condition)))
         (if abort-restart
             (invoke-restart abort-restart)
             (error "No abort restart available; agent debugger returned ~S"
                    chosen)))))))


;;; -- Entry point ----------------------------------------------------------

(in-package :hemlock.agent)

(defun connect (&key host port name)
  "Connect this Lisp process to a hemlock master at HOST:PORT.
   NAME is the agent's identifier (shown in the agent buffer title).

   Flow:
     1. Open TCP wire to master.
     2. Call set-up-agent-buffers on master — master creates editor
        buffers and returns opaque handles (remote-objects).
     3. Call made-repl-buffers locally — redirects *terminal-io* etc.
        to wire-backed streams that route I/O to the master's buffers.
     4. Run wire-native REPL; conditions call master-agent-debugger.

   This function does not return until the REPL exits."
  (declare (type string host name)
           (type (unsigned-byte 16) port))
  (let* ((wire (make-agent-tcp-wire host port))
         (*agent-wire* wire)
         (hemlock.wire:*current-wire* wire)
         (hemlock::*in-hemlock-agent-p* t))
    (format t "Connecting to ~A:~D as ~S~%" host port name)
    ;; Perform version handshake: agent sends first, then receives the echo.
    (hemlock.wire:wire-send-handshake wire)
    (hemlock.wire:wire-receive-handshake wire)
    ;; Call set-up-agent-buffers on master; receive remote-object handles
    ;; for the agent and background repl-buf-data structs.  Then call
    ;; made-repl-buffers locally to wire up our streams.
    (hemlock.wire:remote-value-bind wire
        (agent-info background-info)
        (hemlock::set-up-agent-buffers
         (lisp-implementation-type)
         (lisp-implementation-version))
      (hemlock::made-repl-buffers agent-info background-info))
    ;; Wait until made-repl-buffers has run and *io* is live.
    (loop until hemlock::*io*
          do (dispatch-events))
    ;; Wire-native REPL with agent-side debugger hook.
    (setf *debugger-hook* #'hemlock::agent-debugger)
    (hemlock::agent-repl)))
