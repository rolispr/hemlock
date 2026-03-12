;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :hemlock.term)

(defconstant +tiocswinsz+
  #+darwin #x80087467
  #+linux  #x00005414)

(defun set-pty-size (fd rows cols)
  (cffi:with-foreign-object (ws '(:unsigned-char) 8)
    (setf (cffi:mem-ref ws :uint16 0) rows
          (cffi:mem-ref ws :uint16 2) cols
          (cffi:mem-ref ws :uint16 4) 0
          (cffi:mem-ref ws :uint16 6) 0)
    (cffi:foreign-funcall "ioctl" :int fd :unsigned-long +tiocswinsz+
                                  :pointer ws :int)))

(defun make-terminal-env (&optional (term-type "xterm-256color"))
  (let ((env (sb-ext:posix-environ)))
    (flet ((set-var (name value)
             (setf env (cons (format nil "~A=~A" name value)
                             (remove-if (lambda (e)
                                          (and (> (length e) (1+ (length name)))
                                               (string= e name
                                                        :end1 (length name))
                                               (char= (char e (length name))
                                                      #\=)))
                                        env)))))
      (set-var "TERM" term-type)
      (set-var "TERMCAP" "")
      (set-var "INSIDE_HEMLOCK" "")
      env)))

(defun spawn-pty-process (command &key (rows 24) (cols 80))
  (multiple-value-bind (master slave slave-name)
      (hemlock::find-a-pty)
    (set-pty-size master rows cols)
    (let ((pc (hemlock::make-process-connection
               (list "/bin/sh" "-c" command)
               :slave-pty-name slave-name
               :slave-fd slave
               :environment (make-terminal-env))))
      (sb-posix:close slave)
      (values master pc))))
