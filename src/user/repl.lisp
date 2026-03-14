;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :hemlock)


;;;; PREPL/background buffer integration

(declaim (special *in-hemlock-agent-p*
                  hemlock::*master-machine-and-port*
                  hemlock::*original-terminal-io*))

(defun need-to-redirect-debugger-io (stream)
  (eq stream hemlock::*original-terminal-io*))

;;; Setup an a connection to the editor for the current thread, and
;;; create an editor buffer for I/O and return the client stream.
(defun typeout-for-thread ()
  (assert (or (not (boundp '*event-base*)) (not *event-base*)))
  (setf *event-base* (make-event-loop *connection-backend*))
  (setf *in-hemlock-agent-p* t)
  (let ((hemlock.wire:*current-wire* :not-yet))
    (hemlock::connect-to-editor-for-background-thread
     (car hemlock::*master-machine-and-port*)
     (cadr hemlock::*master-machine-and-port*))
    (dispatch-events-no-hang)
    (do ()
        ((not (eq hemlock.wire:*current-wire* :not-yet)))
      (dispatch-events)
      (write-line "Thread waiting for connection to master..."
                  hemlock::*original-terminal-io*)
      (force-output hemlock::*original-terminal-io*))
    (let* ((name (format nil "Agent thread ~A"
                         (bt:thread-name (bt:current-thread))))
           (session-data (hemlock.wire:remote-value hemlock.wire:*current-wire*
                     (hemlock::%make-extra-session-buffer name))))
      (hemlock::connect-stream session-data hemlock.wire:*current-wire*))))

