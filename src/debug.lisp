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


;;;; Debugger mode — interactive restart selection
;;;
;;; Like Dired: mode commands operate on shared state, normal command loop
;;; handles dispatch.  invoke-restart-interactively IS the non-local exit
;;; out of the recursive %command-loop — no catch/throw needed.

(defmode "Debugger" :major-p t
  :documentation "Presents condition info and restart choices for selection.")

(defvar *debugger-restarts* nil
  "Live restart objects for a master-process error being debugged.
Non-nil means we are inside enter-master-debugger.")

(defvar *debugger-restart-count* nil
  "Number of restarts in an agent debugger session.
Non-nil means we are inside master-agent-debugger.")


(defcommand "Debugger Invoke Restart" (p)
  "Invoke the numbered restart (from digit key or prefix arg)."
  "Invoke the numbered restart."
  (let ((n (or p
               (digit-char-p
                (hemlock-ext:key-event-char *last-key-event-typed*)))))
    (when n
      (cond
        ;; Master-process error: invoke restart directly (non-local exit).
        ((and *debugger-restarts* (< n (length *debugger-restarts*)))
         (invoke-restart-interactively (nth n *debugger-restarts*)))
        ;; Agent error: return index to caller via restart.
        ((and *debugger-restart-count* (< n *debugger-restart-count*))
         (invoke-restart 'debugger-chose n))))))

(defcommand "Debugger Abort" (p)
  "Abort — invoke the ABORT restart or return NIL to agent."
  "Abort from the debugger."
  (declare (ignore p))
  (cond
    (*debugger-restarts*
     (let ((abort (find-restart 'abort)))
       (if abort
           (invoke-restart abort)
           (throw 'command-loop-catcher nil))))
    (*debugger-restart-count*
     (invoke-restart 'debugger-chose nil))))


(defun render-debugger-buffer (buf condition-type condition-msg restart-strings frame-strings)
  "Fill BUF with a human-readable debugger display."
  (with-writable-buffer (buf)
    (delete-region (buffer-region buf))
    (with-output-to-mark (s (buffer-point buf))
      (format s "--- Debugger [~A] ---~%~%  ~A~%~%Restarts:~%"
              condition-type condition-msg)
      (loop for r in restart-strings for i from 0
            do (format s "  ~D: ~A~%" i r))
      (when frame-strings
        (format s "~%Backtrace:~%")
        (loop for f in frame-strings for i from 0
              do (format s "  ~D: ~A~%" i f)))
      (format s "~%Press 0-9 to invoke a restart, q to abort.~%")))
  (buffer-start (buffer-point buf)))


(defun master-agent-debugger (condition-type condition-msg restart-strings frame-strings)
  "Called via wire from agent-debugger on the slave side.
Returns the chosen restart index (integer) or NIL for abort."
  (let ((buf (or (getstring "*Agent Debugger*" *buffer-names*)
                 (make-buffer "*Agent Debugger*" :modes '("Debugger"))))
        (*debugger-restart-count* (length restart-strings)))
    (render-debugger-buffer buf condition-type condition-msg restart-strings frame-strings)
    (change-to-buffer buf)
    (restart-case
        (hemlock.command::%command-loop)
      (debugger-chose (n)
        :report "Debugger restart chosen"
        n))))


(defun enter-master-debugger (condition)
  "Show an interactive restart menu for a condition signalled in the master process.
Must be called from within a handler-bind that caught CONDITION so restarts remain live."
  (let* ((restarts (compute-restarts condition))
         (frames   (ignore-errors
                     (hemlock.introspect:compute-backtrace 0 20)))
         (buf (or (getstring "*Debugger*" *buffer-names*)
                  (make-buffer "*Debugger*" :modes '("Debugger"))))
         (*debugger-restarts* restarts))
    (render-debugger-buffer
     buf
     (type-of condition)
     (format nil "~A" condition)
     (mapcar (lambda (r) (format nil "~A" r)) restarts)
     (when frames
       (mapcar (lambda (f)
                 (with-output-to-string (s)
                   (hemlock.introspect:print-frame f s)))
               frames)))
    (change-to-buffer buf)
    (hemlock.command::%command-loop)))
