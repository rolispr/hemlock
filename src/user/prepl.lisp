;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; Filesystem utility wrappers and repl background utilities.
;;;

(in-package :hemlock)


;;;; Background thread typeout utilities

(defun call-with-typeout-pop-up-in-master (fun buffer-name)
  (let* ((buffer-name (or buffer-name "Unnamed REPL"))
         (repl-data
          (hemlock.wire:remote-value
           hemlock.wire::*current-wire*
           (make-extra-repl-buffer-impl buffer-name)))
         (stream
          (connect-stream repl-data)))
    (funcall fun stream)))

(defmacro with-typeout-pop-up-in-master ((stream buffer-name) &body body)
  `(call-with-typeout-pop-up-in-master
    (lambda (,stream) ,@body)
    ,buffer-name))

(defun call-with-standard-synonym-streams (fun)
  (let ((*standard-input*  (make-synonym-stream '*terminal-io*))
        (*standard-output* (make-synonym-stream '*terminal-io*))
        (*error-output*    (make-synonym-stream '*terminal-io*))
        (*debug-io*        (make-synonym-stream '*terminal-io*))
        (*query-io*        (make-synonym-stream '*terminal-io*)))
    (funcall fun)))

(defun call-with-typeout-for-thread-debugger (cont)
  (with-new-event-loop ()
    (let ((*in-hemlock-agent-p* t)
          (hemlock.wire:*current-wire* :not-yet))
      (connect-to-editor-for-background-thread
       (car *master-machine-and-port*)
       (cadr *master-machine-and-port*))
      (dispatch-events-no-hang)
      (do ()
          ((not (eq hemlock.wire:*current-wire* :not-yet)))
        (dispatch-events)
        (write-line "Thread waiting for connection to master..."
                    *original-terminal-io*)
        (force-output *original-terminal-io*))
      (with-typeout-pop-up-in-master
          (*terminal-io* (format nil "Agent thread ~A"
                                 (bt:thread-name (bt:current-thread))))
        (call-with-standard-synonym-streams cont)))))


;;;; Filesystem utilities

(defun file-kind (pathname)
  "Return :directory, :symbolic-link, :regular-file, or NIL."
  (if (uiop:directory-exists-p pathname)
      :directory
      (when (uiop:file-exists-p pathname)
        :regular-file)))

(defun read-link (pathname)
  "Return the target of the symbolic link at PATHNAME."
  (sb-posix:readlink (namestring pathname)))

(defun user-info (username)
  "Return an alist with :name and :home for USERNAME, or NIL."
  (when (and username (not (string= username "")))
    (let ((pw (ignore-errors (sb-posix:getpwnam username))))
      (when pw
        (list (cons :name (sb-posix:passwd-name pw))
              (cons :home (sb-posix:passwd-dir pw)))))))

(defun relative-pathname-p (string)
  "Return true if STRING names a relative pathname."
  (let ((p (pathname string)))
    (eq :relative (car (pathname-directory p)))))

(defmacro with-directory-iterator ((iterator pathspec) &body body)
  "Iterate over directory entries of PATHSPEC."
  `(let ((entries (append (uiop:directory-files ,pathspec)
                          (uiop:subdirectories ,pathspec)))
         (pos 0))
     (flet ((,iterator () (when (< pos (length entries))
                            (prog1 (nth pos entries) (incf pos)))))
       ,@body)))

(defun get-init-file-string (filename)
  "Read FILENAME and return its contents as a string, or NIL."
  (with-open-file (s filename :if-does-not-exist nil)
    (when s
      (let* ((len (file-length s))
             (buf (make-string len)))
        (read-sequence buf s)
        buf))))
