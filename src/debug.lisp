;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; Slave debugging

(in-package :hemlock)


(defvar *slave-stack-frames* nil)
(defvar *slave-stack-frames-end* nil)
;;;

(defstruct (slave-stack-frame
             (:constructor make-slave-stack-frame (label remote-frame)))
  label
  remote-frame)


;;; This is the debug buffer if it exists.
;;;
(defvar *debug-buffer* nil)

;;; This is the cleanup method for deleting *debug-buffer*.
;;;
(defun delete-debug-buffers (buffer)
  (when (eq buffer *debug-buffer*)
    (setf *debug-buffer* nil)
    (setf *slave-stack-frames* nil)))


;;;; Commands.

(defmode "Debug" :major-p t
  :documentation "Debug mode presents a list of slave symbols.")

(defcommand "Debug Quit" (p)
  "Kill the debug buffer."
  ""
  (declare (ignore p))
  (when *debug-buffer* (delete-buffer-if-possible *debug-buffer*)))

(defun slave-stack-frame-from-mark (mark)
  )

(defun refresh-debug (buf entries)
  (with-writable-buffer (buf)
    (delete-region (buffer-region buf))
    (setf *slave-stack-frames-end* (length entries))
    (setf *slave-stack-frames* (coerce entries 'vector))
    (with-output-to-mark (s (buffer-point buf))
      (loop for entry in entries
            for i from 0
            do (debug-write-line i entry s)))))

(defvar *debug-context* nil)

(defun make-debug-buffer (context entries impl thread)
  (let ((buf (or *debug-buffer*
                 (make-buffer (format nil "Slave Debugger ~A" impl thread)
                              :modes '("Debug")))))
    (setf *debug-buffer* buf)
    (setf *debug-context* context)
    (refresh-debug buf
                   (mapcar (lambda (entry)
                             (make-slave-stack-frame (car entry)
                                                     (cdr entry)))
                           entries))
    (let ((fields (buffer-modeline-fields *debug-buffer*)))
      (setf (cdr (last fields))
            (list (or (modeline-field :debug-cmds)
                      (make-modeline-field
                       :name :debug-cmds :width 18
                       :function
                       #'(lambda (buffer window)
                           (declare (ignore buffer window))
                           "  Type ? for help.")))))
      (setf (buffer-modeline-fields *debug-buffer*) fields))
    (buffer-start (buffer-point buf))
    (change-to-buffer buf)))

(defun debug-write-line (i entry s)
  (format s "~D: ~A~%" i (slave-stack-frame-label entry)))

(defun debug-using-master (&optional (start 0) (end 10))
  (let ((frames
         (let ((result '()) (i -1))
           (sb-debug:map-backtrace
            (lambda (frame)
              (incf i)
              (when (and (>= i start) (< i end))
                (push (cons (with-output-to-string (s)
                              (sb-debug::print-frame-call frame s))
                            (hemlock.wire:make-remote-object frame))
                      result)))
            :count end)
           (nreverse result)))
        (context nil)
        (impl (lisp-implementation-type))
        (thread (bordeaux-threads:thread-name
                 (bordeaux-threads:current-thread))))
    (hemlock::eval-in-master
     `(make-debug-buffer ',context ',frames ',impl ',thread))))
