;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;; Lisp implementation dependent stuff.

(in-package :hemlock-ext)

(defun getenv (name)
  (sb-ext:posix-getenv name))

(defmacro without-interrupts (&body body)
  `(sb-sys:without-interrupts ,@body))

(defmacro fixnump (object)
  `(typep ,object 'fixnum))

(defun file-writable (pathname)
  "File-writable accepts a pathname and returns T if the current
  process can write it, and NIL otherwise. Also if the file does
  not exist return T."
  (handler-case (let ((io (open pathname
                                :direction :output
                                :if-exists :append
                                :if-does-not-exist nil)))
                  (if io
                      (close io :abort t)
                      (let ((io (open pathname
                                      :direction :output
                                      :if-exists nil
                                      :if-does-not-exist :create)))
                        (if io
                            (progn
                              (close io)
                              (delete-file io))
                            t))))
    (file-error (err)
                (declare (ignore err))
                nil)))
