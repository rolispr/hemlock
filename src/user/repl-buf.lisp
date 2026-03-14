;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;;
;;; This file contains code for processing input to and output from agents
;;; using session streams.  It maintains the stuff that hacks on the
;;; session buffer and maintains its state.
;;;
;;;

(in-package :hemlock)


(defhvar "Input Wait Alarm"
  "When non-nil, the user is informed when a session buffer goes into
   an input wait, and it is not visible.  Legal values are :message,
   :loud-message (the default), and nil."
  :value :loud-message)



;;;; Structures.

(defstruct (session-data
            (:print-function
             (lambda (ts s d)
               (declare (ignore ts d))
               (write-string "#<TS Data>" s)))
            (:constructor
             make-session-data (buffer
                           &aux
                           (fill-mark (copy-mark (buffer-end-mark buffer)
                                                 :right-inserting)))))
  buffer                      ; The buffer we are in
  stream                      ; Stream in the agent.
  wire                        ; Wire to agent
  server                      ; Server info struct.
  fill-mark                   ; Mark where output goes.  This is actually the
                              ;   "Buffer Input Mark" which is :right-inserting,
                              ;   and we make sure it is :left-inserting for
                              ;   inserting output.
  )


;;;; Output routines.

;;; TS-BUFFER-OUTPUT-STRING --- internal interface.
;;;
;;; Called by the agent to output stuff in the session.  Can also be called
;;; by other random parts of hemlock when they want to output stuff to the
;;; buffer.  Since this is called for value from the agent, we have to be
;;; careful about what values we return, so the result can be sent back.  It is
;;; called for value only as a synchronization thing.
;;;
;;; Whenever the output is gratuitous, we want it to go behind the prompt.
;;; When it's gratuitous, and we're not at the line-start, then we can output
;;; it normally, but we also make sure we end the output in a newline for
;;; visibility's sake.
;;;
(defun session-buffer-output-string (ts string &optional gratuitous-p)
  "Outputs STRING to the session described with TS. The output is inserted
   before the fill-mark and the current input."
  (when (hemlock.wire:remote-object-p ts)
    (setf ts (hemlock.wire:remote-object-value ts)))
  (without-interrupts
    (let ((mark (session-data-fill-mark ts)))
      (cond ((and gratuitous-p (not (start-line-p mark)))
             (with-mark ((m mark :left-inserting))
               (line-start m)
               (insert-string m string)
               (unless (start-line-p m)
                 (insert-character m #\newline))))
            (t
             (setf (mark-kind mark) :left-inserting)
             (insert-string mark string)
             (when (and gratuitous-p (not (start-line-p mark)))
               (insert-character mark #\newline))
             (setf (mark-kind mark) :right-inserting)))))
  (values))

;;; TS-BUFFER-FINISH-OUTPUT --- internal interface.
;;;
;;; Redisplays the windows. Used by session-stream in order to finish-output.
;;;
(defun session-buffer-finish-output (ts)
  (declare (ignore ts))
  (redisplay)
  nil)

;;; TS-BUFFER-CHARPOS --- internal interface.
;;;
;;; Used by session-stream in order to find the charpos.
;;;
(defun session-buffer-charpos (ts)
  (mark-charpos (session-data-fill-mark (if (hemlock.wire:remote-object-p ts)
                                       (hemlock.wire:remote-object-value ts)
                                       ts))))

;;; TS-BUFFER-LINE-LENGTH --- internal interface.
;;;
;;; Used by session-stream to find out the line length.  Returns the width of the
;;; first window, or 80 if there are no windows.
;;;
(defun session-buffer-line-length (ts)
  (let* ((ts (if (hemlock.wire:remote-object-p ts)
                 (hemlock.wire:remote-object-value ts)
                ts))
         (window (car (buffer-windows (session-data-buffer ts)))))
    (if window
        (window-width window)
        80))) ; Seems like a good number to me.


;;;; Input routines

(defun session-buffer-ask-for-input (remote)
  (let* ((ts (hemlock.wire:remote-object-value remote))
         (buffer (session-data-buffer ts)))
    (unless (buffer-windows buffer)
      (let ((input-wait-alarm
             (if (hemlock-bound-p 'input-wait-alarm
                                  :buffer buffer)
               (variable-value 'input-wait-alarm
                               :buffer buffer)
               (variable-value 'input-wait-alarm
                               :global))))
        (when input-wait-alarm
          (when (eq input-wait-alarm :loud-message)
            (beep))
          (message "Waiting for input in buffer ~A."
                   (buffer-name buffer))))))
  nil)

(defun session-buffer-clear-input (ts)
  (let* ((ts (if (hemlock.wire:remote-object-p ts)
                 (hemlock.wire:remote-object-value ts)
                 ts))
         (buffer (session-data-buffer ts))
         (mark (session-data-fill-mark ts)))
    (unless (mark= mark (buffer-end-mark buffer))
      (with-mark ((start mark))
        (line-start start)
        (let ((prompt (region-to-string (region start mark)))
              (end (buffer-end-mark buffer)))
          (unless (zerop (mark-charpos end))
            (insert-character end #\Newline))
          (insert-string end "[Input Cleared]")
          (insert-character end #\Newline)
          (insert-string end prompt)
          (move-mark mark end)))))
  nil)

(defun session-buffer-set-stream (ts stream)
  (let ((ts (if (hemlock.wire:remote-object-p ts)
                (hemlock.wire:remote-object-value ts)
                ts)))
    (setf (session-data-stream ts) stream)
    (hemlock.wire:remote (session-data-wire ts)
      (session-stream-set-line-length stream (session-buffer-line-length ts))))
  nil)


;;;; Session mode.

(defun setup-session (buffer)
  (let ((ts (make-session-data buffer)))
    (defhvar "Current Package"
      "The package used for evaluation of Lisp in this buffer."
      :buffer buffer
      :value nil)

    (defhvar "Session Data"
      "The session-data structure for this buffer"
      :buffer buffer
      :value ts)

    (defhvar "Buffer Input Mark"
      "Beginning of session input in this buffer."
      :value (session-data-fill-mark ts)
      :buffer buffer)

    (defhvar "Interactive History"
      "A ring of the regions input to the Hemlock session."
      :buffer buffer
      :value (make-ring (value interactive-history-length)))

    (defhvar "Interactive Pointer"
      "Pointer into the Hemlock session input history."
      :buffer buffer
      :value 0)

    (defhvar "Searching Interactive Pointer"
      "Pointer into \"Interactive History\"."
      :buffer buffer
      :value 0)))

(defmode "Session"
  :setup-function #'setup-session
  :documentation "The Session mode is used to interact with agent lisps.")


;;; TYPESCRIPTIFY-BUFFER -- Internal interface.
;;;
;;; Buffer creation code for eval server connections calls this to setup a
;;; session buffer, tie things together, and make some local Hemlock
;;; variables.
;;;
(defun sessionify-buffer (buffer server wire)
  (setf (buffer-minor-mode buffer "Session") t)
  (let ((info (variable-value 'session-data :buffer buffer)))
    (setf (session-data-server info) server)
    (setf (session-data-wire info) wire)
    (defhvar "Server Info"
      "Server-info structure for this buffer."
      :buffer buffer :value server)
    (defhvar "Current Eval Server"
      "The Server-Info object for the server currently used for evaluation and
       compilation."
      :buffer buffer :value server)
    info))

(defun session-buffer-wire-connected (ts wire)
  (setf (session-data-wire ts) wire))

(defun session-buffer-wire-died (ts)
  (setf (session-data-stream ts) nil)
  (setf (session-data-wire ts) nil)
  (buffer-end (session-data-fill-mark ts) (session-data-buffer ts))
  (session-buffer-output-string ts (format nil "~%~%Agent died!~%")))

(defun unwedge-session-buffer ()
  (session-agent-to-top-level-command nil)
  (buffer-end (current-point) (current-buffer)))

(defhvar "Unwedge Interactive Input Fun"
  "Function to call when input is confirmed, but the point is not past the
   input mark."
  :value #'unwedge-session-buffer
  :mode "Session")

(defhvar "Unwedge Interactive Input String"
  "String to add to \"Point not past input mark.  \" explaining what will
   happen if the the user chooses to be unwedged."
  :value "Cause the agent to throw to the top level? "
  :mode "Session")

;;; TYPESCRIPT-DATA-OR-LOSE -- internal
;;;
;;; Return the session-data for the current buffer, or die trying.
;;;
(defun session-data-or-lose ()
  (if (hemlock-bound-p 'session-data)
      (let ((ts (value session-data)))
        (if ts
            ts
            (editor-error "Can't find the session data?")))
      (editor-error "Not in a session buffer.")))

(defcommand "Confirm Session Input" (p)
  "Send the current input to the agent session."
  "Send the current input to the agent session."
  (declare (ignore p))
  (let ((ts (session-data-or-lose)))
    (let ((input (get-interactive-input)))
      (when input
        (let ((string (region-to-string input)))
          (declare (simple-string string))
          (insert-character (current-point) #\NewLine)
          (let ((wire (session-data-wire ts)))
            (if (eq wire :local)
                ;; Local master eval — spawn thread, post results via later.
                (let ((captured-ts ts))
                  (sb-thread:make-thread
                   (lambda ()
                     (let ((output (make-string-output-stream)))
                       (handler-case
                           (let* ((*standard-output* output)
                                  (*error-output* output)
                                  (*trace-output* output)
                                  (values (multiple-value-list
                                           (eval (read-from-string string))))
                                  (out-str (get-output-stream-string output))
                                  (result-str
                                   (format nil "=> ~{~#[~;~A~:;~A, ~]~}~%"
                                           (mapcar #'prin1-to-string values))))
                             (later
                               (unless (zerop (length out-str))
                                 (session-buffer-output-string captured-ts out-str))
                               (session-buffer-output-string captured-ts result-str)))
                         (error (c)
                           (later
                             (session-buffer-output-string
                              captured-ts (format nil "Error: ~A~%" c)))))))
                   :name "hemlock-repl-eval"))
                ;; Wire-backed agent — send over wire.
                (progn
                  (hemlock.wire:remote wire
                    (session-stream-accept-input (session-data-stream ts)
                                            (concatenate 'simple-string
                                                         string
                                                         (string #\newline))))
                  (hemlock.wire:wire-force-output wire))))
          (buffer-end (session-data-fill-mark ts)
                      (session-data-buffer ts)))))))

(defcommand "Session Agent Break" (p)
  "Interrupt the agent Lisp process associated with this interactive buffer,
   causing it to invoke BREAK."
  "Interrupt the agent Lisp process associated with this interactive buffer,
   causing it to invoke BREAK."
  (declare (ignore p))
  (send-oob-to-agent "B"))

(defcommand "Session Agent to Top Level" (p)
  "Interrupt the agent Lisp process associated with this interactive buffer,
   causing it to throw to the top level REP loop."
  "Interrupt the agent Lisp process associated with this interactive buffer,
   causing it to throw to the top level REP loop."
  (declare (ignore p))
  (send-oob-to-agent "T"))

(defcommand "Session Agent Status" (p)
  "Interrupt the agent and cause it to print status information."
  "Interrupt the agent and cause it to print status information."
  (declare (ignore p))
  (send-oob-to-agent "S"))

(defun send-oob-to-agent (string)
  (let* ((ts (session-data-or-lose))
         (wire (session-data-wire ts))
         (socket (hemlock.wire:wire-fd wire)))
    (unless socket
      (editor-error "The agent is no longer alive."))
    (error "SEND-OOB-TO-AGENT seeks an implementation.")
    #+NIL
    (send-character-out-of-band socket (schar string 0))))

(defcommand "Clear Session Buffer" (p)
  "" ""
  (declare (ignore p))
  (let* ((input-region (get-interactive-input))
         (input (if input-region
                    (region-to-string input-region)
                    "")))
    (delete-region (buffer-region (current-buffer)))
    (fresh-prompt)
    (insert-string (buffer-point (current-buffer)) input)))
