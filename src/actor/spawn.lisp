;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; Spawn a process agent.  Starts an SBCL subprocess that loads
;;; hemlock-agent and connects back via sento remoting.

(in-package :hemlock.actor)

(defvar *agent-process-table* (make-hash-table :test 'equal)
  "Map from agent name to the OS process object.")

(defun spawn-process-agent (name &key (master-port *remoting-port*))
  "Spawn a new SBCL subprocess that loads hemlock-agent and connects
to this master's actor-system via sento remoting.

NAME: identifier for the agent (must be unique).
MASTER-PORT: the master's remoting port (default: current).

Returns the agent-info struct once the agent registers, or signals
an error on timeout."
  (unless master-port
    (error "Remoting not enabled; call start-actor-system first."))
  (when (find-agent name)
    (error "Agent ~S already registered." name))
  ;; Build the Lisp form the subprocess will eval.
  (let* ((script (format nil
                  "(require :asdf)~%~
                   (push ~S asdf:*central-registry*)~%~
                   (push ~S asdf:*central-registry*)~%~
                   (handler-bind ((warning #'muffle-warning))~%~
                     (asdf:load-system :hemlock-agent))~%~
                   (funcall (find-symbol \"CONNECT\" \"HEMLOCK.AGENT\")~%~
                     :name ~S :master-port ~D)~%"
                  (namestring (asdf:system-source-directory :hemlock))
                  (namestring (truename #P"/Users/bret.horne/common-lisp/cl-gserver/"))
                  name
                  master-port))
         (script-path (format nil "/tmp/hemlock-agent-~A.lisp" name))
         (_  (with-open-file (s script-path :direction :output
                                            :if-exists :supersede)
               (write-string script s)))
         (process (sb-ext:run-program
                   (first sb-ext:*posix-argv*)
                   (list "--noinform" "--non-interactive"
                         "--load" script-path)
                   :wait nil
                   :output :stream
                   :error :output)))
    (setf (gethash name *agent-process-table*) process)
    ;; Wait for the agent to register with the registry.
    (loop repeat 150  ; 15 seconds max
          for info = (find-agent name)
          when info return info
          do (sleep 0.1)
          finally (error "Agent ~S did not register within 15 seconds." name))))

(defun kill-process-agent (name)
  "Kill a process agent by name."
  (let ((process (gethash name *agent-process-table*)))
    (when process
      (handler-case (sb-ext:process-kill process sb-posix:sigterm)
        (error () nil))  ; process may already be dead
      (remhash name *agent-process-table*)))
  (handler-case (unregister-agent name)
    (error () nil)))
