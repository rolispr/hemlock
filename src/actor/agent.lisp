;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; hemlock sento-agent -- agent-side actor for hemlock.
;;;
;;; This file is loaded in agent Lisp processes (not in the editor).
;;; Entry point: (hemlock.agent:connect :host "127.0.0.1" :port NNN :name "...")
;;;
;;; The agent creates its own sento actor-system with remoting enabled,
;;; registers an "agent" actor, and tells the master's agent-registry
;;; about itself via a remote-ref.

(in-package :hemlock.agent)

;;;; Agent state.

(defvar *agent-system* nil
  "The sento actor-system running in this agent process.")

(defvar *agent-actor* nil
  "The agent actor that receives eval/compile/etc. messages.")

(defvar *master-registry* nil
  "Remote-ref to the master's agent-registry actor.")


;;;; Agent actor.

(defun make-agent-actor (system name)
  "Create the agent actor that handles eval/compile/set-package/ping/shutdown."
  (actor-of system
    :name "agent"
    :receive
    (lambda (msg)
      (case (car msg)
        (:eval
         (let ((form-string (cadr msg)))
           (handler-case
               (let* ((form (read-from-string form-string))
                      (values (multiple-value-list (eval form)))
                      (result (format nil "~{~S~^~%~}" values)))
                 (reply (list :ok result)))
             (error (c)
               (reply (list :error (princ-to-string c)))))))

        (:compile
         (let ((text (cadr msg))
               (package-name (caddr msg)))
           (handler-case
               (let ((*package* (or (and package-name
                                         (find-package package-name))
                                    *package*)))
                 (eval (read-from-string text))
                 (reply (list :ok "compiled")))
             (error (c)
               (reply (list :error (princ-to-string c)))))))

        (:set-package
         (let* ((pkg-name (cadr msg))
                (pkg (find-package pkg-name)))
           (if pkg
               (progn (setf *package* pkg)
                      (reply (list :ok (package-name pkg))))
               (reply (list :error
                            (format nil "No package ~S" pkg-name))))))

        (:ping (reply :pong))

        (:shutdown
         (reply :ok)
         (bordeaux-threads:make-thread
          (lambda ()
            (sleep 0.1)
            (when *agent-system*
              (ignore-errors
                (when (remoting-enabled-p *agent-system*)
                  (disable-remoting *agent-system*))
                (shutdown *agent-system* :wait t))
              (setf *agent-system* nil))
            (uiop:quit 0))
          :name "agent-shutdown"))

        (t (reply (list :error :unknown-message (car msg))))))))


;;;; Entry point.

(defun connect (&key (name "agent")
                     (master-host "127.0.0.1")
                     (master-port (error ":master-port required"))
                     (workers 2) (remoting-port 0))
  "Connect to a hemlock master at MASTER-HOST:MASTER-PORT.
   Creates a local actor-system, registers with the master's agent-registry,
   and blocks until shutdown."
  (format t "~&Starting agent ~S, connecting to master at ~A:~D~%"
          name master-host master-port)
  ;; 1. Create actor-system with remoting
  (setf *agent-system*
        (make-actor-system
         `(:dispatchers
           (:shared (:workers ,workers :strategy :random))
           :timeout-timer (:resolution 50 :max-size 500))))
  (enable-remoting *agent-system*
                    :host "127.0.0.1"
                    :port remoting-port)
  (let ((local-port (remoting-port *agent-system*)))
    (format t "~&Agent remoting listening on port ~D~%" local-port)
    ;; 2. Create agent actor
    (setf *agent-actor* (make-agent-actor *agent-system* name))
    ;; 3. Remote-ref to master's agent-registry
    (let* ((registry-uri (format nil "sento://~A:~D/user/agent-registry"
                                 master-host master-port))
           (registry-ref (make-remote-ref *agent-system* registry-uri)))
      (setf *master-registry* registry-ref)
      ;; 4. Register with master
      (let ((result (ask-s registry-ref
                           (list :register-remote name
                                 master-host local-port)
                           :time-out 10)))
        (format t "~&Registration result: ~S~%" result))
      ;; 5. Block to keep process alive
      (format t "~&Agent ~S ready.~%" name)
      (loop (sleep 1)))))
