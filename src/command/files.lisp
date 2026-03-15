;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;;
;;; Hemlock File manipulation functions.
;;; Unhacked by Gilbert Baumann.
;;;

(in-package :hemlock.command)


;;;; Directory and file completion

(defun default-directory ()
  (let* ((p (hemlock::buffer-default-directory (current-buffer)))
         (p (and p (namestring p))))
    (if (and p (uiop:directory-exists-p p))
        p
        (sb-posix:getcwd))))

(defun set-file-permissions (pathname access)
  (declare (ignorable pathname access))
  nil)

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
                            (uiop:subdirectories directory))
                    (mapcar #'namestring
                            (uiop:directory-files directory))))))

(defun ambiguous-files (pathname
                        &optional (defaults *default-pathname-defaults*))
  "Return a list of all files which are possible completions of Pathname."
  (complete-file-directory pathname defaults))


;;; Read-File:

(defun read-file (pathname mark)
  "Inserts the contents of the file named by Pathname at the Mark."
  (with-mark ((mark mark :left-inserting))
    (let* ((first-line (mark-line mark))
           (buffer (line-%buffer first-line)))
      (modifying-buffer buffer)
      (with-open-file (input pathname :direction :input :element-type 'character)
        (do ((line (read-line input nil :eof) (read-line input nil :eof)))
            ((eql line :eof))
          (insert-string mark line)
          (insert-character mark #\newline))))))


;;; Write-File:

(defun write-file (region pathname &key append
                          (keep-backup (value hemlock::keep-backup-files))
                          access)
  "Writes the characters in region to the file named by pathname.  This writes
   region using a stream opened with :if-exists :rename-and-delete, unless
   either append or keep-backup is supplied.  If append is supplied, this
   writes the file opened with :if-exists :append.  If keep-backup is supplied,
   this writes the file opened with :if-exists :rename.  This signals an error
   if both append and keep-backup are supplied.  Access is an implementation
   dependent value that is suitable for setting pathname's access or protection
   bits."
  (let ((if-exists-action (cond ((and keep-backup append)
                                 (error "Cannot supply non-nil values for ~
                                         both keep-backup and append."))
                                (keep-backup :rename)
                                (append :append)
                                (t :rename-and-delete))))
    (with-open-file (file pathname :direction :output
                          :element-type 'character
                          :external-format :utf-8 ;fixme?
                          :if-exists if-exists-action)
      (close-line)
      (fast-write-file region file))
    (set-file-permissions pathname access)))

(defun fast-write-file (region file)
  (let* ((start (region-start region))
         (start-line (mark-line start))
         (start-charpos (mark-charpos start))
         (end (region-end region))
         (end-line (mark-line end))
         (end-charpos (mark-charpos end)))
    (if (eq start-line end-line)
        ;; just one line (fragment)
        (write-string (line-chars start-line) file
                      :start start-charpos :end end-charpos)
        ;; multiple lines
        (let* ((first-length (- (line-length start-line) start-charpos))
               (length (+ first-length end-charpos 1)))
          ;; count number of octets to be written
          (do ((line (line-next start-line) (line-next line)))
              ((eq line end-line))
            (incf length (1+ (line-length line))))
          ;;
          (write-sequence (line-chars start-line) file :start start-charpos :end (+ start-charpos first-length))
          (write-char #\newline file)
          (let ((offset (1+ first-length)))
            (do ((line (line-next start-line)
                       (line-next line)))
                ((eq line end-line))
              (let ((end (+ offset (line-length line))))
                (write-sequence (line-chars line) file :start 0 :end (- end offset))
                (write-char #\newline file)
                (setf offset (1+ end))))
            (unless (zerop end-charpos)
              (write-sequence (line-chars end-line) file :start 0 :end end-charpos)))))))
