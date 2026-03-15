;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; Agent debugging

(in-package :hemlock)


(defvar *agent-stack-frames* nil)
(defvar *agent-stack-frames-end* nil)
;;;

(defstruct (agent-stack-frame
             (:constructor make-agent-stack-frame (label remote-frame)))
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
    (setf *agent-stack-frames* nil)))


;;;; Commands.

(defmode "Debug" :major-p t
  :documentation "Debug mode presents a list of agent symbols.")

(defcommand "Debug Quit" (p)
  "Kill the debug buffer."
  ""
  (declare (ignore p))
  (when *debug-buffer* (delete-buffer-if-possible *debug-buffer*)))

(defun agent-stack-frame-from-mark (mark)
  )

(defun refresh-debug (buf entries)
  (with-writable-buffer (buf)
    (delete-region (buffer-region buf))
    (setf *agent-stack-frames-end* (length entries))
    (setf *agent-stack-frames* (coerce entries 'vector))
    (with-output-to-mark (s (buffer-point buf))
      (loop for entry in entries
            for i from 0
            do (debug-write-line i entry s)))))

(defvar *debug-context* nil)

(defun make-debug-buffer (context entries impl thread)
  (let ((buf (or *debug-buffer*
                 (make-buffer (format nil "Agent Debugger ~A" impl thread)
                              :modes '("Debug")))))
    (setf *debug-buffer* buf)
    (setf *debug-context* context)
    (refresh-debug buf
                   (mapcar (lambda (entry)
                             (make-agent-stack-frame (car entry)
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
  (format s "~D: ~A~%" i (agent-stack-frame-label entry)))

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

(defvar *showing-error* nil
  "T while show-error-in-buffer is active.  Prevents nested error display.")

(defvar *debugger-restarts* nil
  "Live restart objects for a master-process error being debugged.")

(defvar *debugger-restart-count* nil
  "Number of restarts in an agent debugger session.
Non-nil means we are inside master-agent-debugger.")


(defcommand "Debugger Invoke Restart" (p)
  "Invoke the numbered restart (from digit key or prefix arg)."
  "Invoke the numbered restart."
  (let ((n (or p
               (digit-char-p
                (key-event-char *last-key-event-typed*)))))
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
     (let ((abort (find 'abort *debugger-restarts* :key #'restart-name)))
       (if abort
           (invoke-restart abort)
           (throw 'leave-recursive-edit (values :abort nil)))))
    (*debugger-restart-count*
     (invoke-restart 'debugger-chose nil))
    (t
     (throw 'leave-recursive-edit (values :abort nil)))))

;;; Bind keys here (not in bindings.lisp) so they're always set up
;;; together with the mode and commands when debug.lisp is loaded.
(bind-key "Debugger Invoke Restart" #k"0" :mode "Debugger")
(bind-key "Debugger Invoke Restart" #k"1" :mode "Debugger")
(bind-key "Debugger Invoke Restart" #k"2" :mode "Debugger")
(bind-key "Debugger Invoke Restart" #k"3" :mode "Debugger")
(bind-key "Debugger Invoke Restart" #k"4" :mode "Debugger")
(bind-key "Debugger Invoke Restart" #k"5" :mode "Debugger")
(bind-key "Debugger Invoke Restart" #k"6" :mode "Debugger")
(bind-key "Debugger Invoke Restart" #k"7" :mode "Debugger")
(bind-key "Debugger Invoke Restart" #k"8" :mode "Debugger")
(bind-key "Debugger Invoke Restart" #k"9" :mode "Debugger")
(bind-key "Debugger Abort"          #k"q" :mode "Debugger")


(defun render-debugger-buffer (buf condition-type condition-msg restart-strings frame-strings)
  "Fill BUF with a human-readable debugger display."
  (with-writable-buffer (buf)
    (delete-region (buffer-region buf))
    (with-output-to-mark (s (buffer-point buf))
      (format s "--- Debugger [~A] ---~%~%  ~A~%~%Restarts:~%"
              condition-type condition-msg)
      (if restart-strings
          (loop for r in restart-strings for i from 0
                do (format s "  ~D: ~A~%" i r))
          (format s "  (none)~%"))
      (when frame-strings
        (format s "~%Backtrace:~%")
        (loop for f in frame-strings for i from 0
              do (format s "  ~D: ~A~%" i f)))
      (format s "~%~A~%"
              (if restart-strings
                  "Press 0-9 to invoke a restart, q to abort."
                  "Press q to dismiss."))))
  (buffer-start (buffer-point buf)))


(defun master-agent-debugger (condition-type condition-msg restart-strings frame-strings)
  "Called via wire from agent-debugger on the agent side.
Returns the chosen restart index (integer) or NIL for abort."
  (let* ((buf (or (getstring "*Agent Debugger*" *buffer-names*)
                  (make-buffer "*Agent Debugger*" :modes '("Debugger"))))
         (*debugger-restart-count* (length restart-strings))
         (prev-buffer (current-buffer)))
    (render-debugger-buffer buf condition-type condition-msg restart-strings frame-strings)
    (unwind-protect
        (progn
          (change-to-buffer buf)
          (restart-case
              (%command-loop)
            (debugger-chose (n)
              :report "Debugger restart chosen"
              n)))
      (when (member prev-buffer *buffer-list*)
        (change-to-buffer prev-buffer)))))


(defun show-error-in-buffer (condition)
  "Display error info in a buffer and enter a command loop so the user
can inspect and press q to dismiss.  Hooks are inhibited during rendering.
Does not nest — if already showing an error, returns immediately."
  (when *showing-error*
    (return-from show-error-in-buffer nil))
  (let* ((*showing-error* t)
         (*inhibit-hooks* t)
         (buf (or (getstring "*Editor Errors*" *buffer-names*)
                  (make-buffer "*Editor Errors*" :modes '("Debugger"))))
         (frames (ignore-errors
                   (hemlock.introspect:compute-backtrace 0 20)))
         (prev-buffer (current-buffer)))
    (render-debugger-buffer
     buf
     (type-of condition)
     (format nil "~A" condition)
     nil
     (when frames
       (mapcar (lambda (f)
                 (with-output-to-string (s)
                   (hemlock.introspect:print-frame f s)))
               frames)))
    (unwind-protect
        (progn
          (setf *inhibit-hooks* nil)
          ;; If we're in the echo area, switch to a real window first
          (when (eq (current-window) *echo-area-window*)
            (let ((win (car (remove *echo-area-window* *window-list*))))
              (when win (setf (current-window) win))))
          (change-to-buffer buf)
          (catch 'leave-recursive-edit
            (%command-loop)))
      (when (member prev-buffer *buffer-list*)
        (change-to-buffer prev-buffer)))))
