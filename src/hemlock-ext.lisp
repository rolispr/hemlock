;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :hemlock-ext)

(defconstant hi::char-code-limit 256)
(defconstant char-code-limit 256)

(defun skip-whitespace (&optional (stream *standard-input*))
  (peek-char t stream))

(defun quit ()
  (uiop:quit))

(defvar hi::*command-line-switches* nil)

(defun default-directory ()
  (let* ((p (hemlock::buffer-default-directory (current-buffer)))
         (p (and p (namestring p))))
    (if (and p (uiop:directory-exists-p p))
        p
        (sb-posix:getcwd))))

(defun find-buffer (name)
  (getstring name hi::*buffer-names*))

(defun maybe-rename-buffer (buffer new-name)
  (unless (find-buffer new-name)
    (setf (buffer-name buffer) new-name)))

(defun rename-buffer-uniquely (buffer new-name)
  (or (maybe-rename-buffer buffer new-name)
      (loop for i from 2
            until (maybe-rename-buffer buffer (format nil "~A<~D>" new-name i)))))

;; CLX integration: define this macro unconditionally.  The function
;; called by its expansion is defined by the hemlock.clx system only.
(defmacro with-clx-event-handling ((display handler) &rest body)
  "Evaluates body in a context where events are handled for the display
   by calling handler on the display.  This destroys any previously established
   handler for display."
  `(call-with-clx-event-handling (lambda () ,@body) ,display ,handler))


(defun delq (item list)
  (delete item list))

(defun memq (item list)
  (member item list))

(defun assq (item alist)
  (assoc item alist))

(defun concat (&rest args)
  (apply #'concatenate 'string args))


;;;; complete-file

(defun complete-file (pathname &key (defaults *default-pathname-defaults*)
                      ignore-types)
  (let ((files (complete-file-directory pathname defaults)))
    (cond ((null files)
           (values nil nil))
          ((null (cdr files))
           (values (car files)
                   t))
          (t
           (let ((good-files
                  (delete-if #'(lambda (pathname)
                                 (and (simple-string-p
                                       (pathname-type pathname))
                                      (member (pathname-type pathname)
                                              ignore-types
                                              :test #'string=)))
                             files)))
             (cond ((null good-files))
                   ((null (cdr good-files))
                    (return-from complete-file
                      (values (car good-files)
                              t)))
                   (t
                    (setf files good-files)))
             (let ((common (file-namestring (car files))))
               (dolist (file (cdr files))
                 (let ((name (file-namestring file)))
                   (dotimes (i (min (length common) (length name))
                             (when (< (length name) (length common))
                               (setf common name)))
                     (unless (char= (schar common i) (schar name i))
                       (setf common (subseq common 0 i))
                       (return)))))
               (values (merge-pathnames common pathname)
                       nil)))))))

(defun complete-file-directory (pathname defaults)
  (let* ((namestring
          (namestring
           (merge-pathnames pathname (directory-namestring defaults))))
         (directory
          (if (uiop:directory-exists-p namestring)
              namestring
              (directory-namestring namestring))))
    (delete-if-not (lambda (candidate)
                     (search namestring candidate))
                   (append
                    (when (probe-file namestring)
                      (list namestring))
                    (mapcar #'namestring
                            (uiop:directory-files directory))))))

(defun ambiguous-files (pathname
                        &optional (defaults *default-pathname-defaults*))
  "Return a list of all files which are possible completions of Pathname."
  (complete-file-directory pathname defaults))

(defun set-file-permissions (pathname access)
  (declare (ignorable pathname access))
  nil)
