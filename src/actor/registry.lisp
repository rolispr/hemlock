;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; Agent registry.  Tracks all agents (local, process, remote).

(in-package :hemlock.actor)

(defvar *agent-registry* nil
  "The agent-registry actor. Tracks all connected agents.")

(defstruct agent-info
  "Metadata for a connected agent."
  name
  type        ; :local, :process, :remote
  actor       ; sento actor (local) or remote-ref (process/remote)
  buffers     ; associated hemlock buffers
  port)       ; remoting port (for process/remote agents)

(defun start-agent-registry ()
  "Create the agent-registry actor in the hemlock actor system."
  (setf *agent-registry*
        (ac:actor-of *actor-system*
          :name "agent-registry"
          :state (make-hash-table :test 'equal)
          :receive
          (lambda (msg)
            (case (car msg)
              (:register
               (let* ((info (cadr msg))
                      (name (agent-info-name info)))
                 (setf (gethash name act:*state*) info)
                 (act:reply info)))

              (:register-remote
               (let* ((name (cadr msg))
                      (host (caddr msg))
                      (port (cadddr msg))
                      (uri (format nil "sento://~A:~D/user/agent" host port))
                      (ref (rem:make-remote-ref *actor-system* uri))
                      (info (make-agent-info :name name
                                             :type :process
                                             :actor ref
                                             :port port)))
                 (setf (gethash name act:*state*) info)
                 (act:reply info)))

              (:unregister
               (let ((name (cadr msg)))
                 (remhash name act:*state*)
                 (act:reply t)))

              (:lookup
               (let ((name (cadr msg)))
                 (act:reply (gethash name act:*state*))))

              (:list
               (let ((agents nil))
                 (maphash (lambda (k v)
                            (declare (ignore k))
                            (push v agents))
                          act:*state*)
                 (act:reply (nreverse agents))))

              (t (act:reply (list :error :unknown msg))))))))

(defun register-agent (name type actor &key buffers port)
  "Register an agent with the registry."
  (act:ask-s *agent-registry*
             (list :register (make-agent-info :name name
                                              :type type
                                              :actor actor
                                              :buffers buffers
                                              :port port))
             :time-out 5))

(defun unregister-agent (name)
  "Remove an agent from the registry."
  (act:ask-s *agent-registry* (list :unregister name) :time-out 5))

(defun find-agent (name)
  "Look up an agent by name. Returns agent-info or NIL."
  (act:ask-s *agent-registry* (list :lookup name) :time-out 5))

(defun list-agents ()
  "Return a list of all registered agent-info structs."
  (act:ask-s *agent-registry* '(:list) :time-out 5))

(defun agent-eval (agent-or-name form-string &key (time-out 30))
  "Eval FORM-STRING in an agent. AGENT-OR-NAME is an agent-info, actor, or name string."
  (let ((actor (etypecase agent-or-name
                 (agent-info (agent-info-actor agent-or-name))
                 (string (let ((info (find-agent agent-or-name)))
                           (unless info (error "No agent named ~S" agent-or-name))
                           (agent-info-actor info)))
                 (t agent-or-name))))
    (act:ask-s actor (list :eval form-string) :time-out time-out)))

(defun agent-compile (agent-or-name text &key package (time-out 60))
  "Compile TEXT in an agent."
  (let ((actor (etypecase agent-or-name
                 (agent-info (agent-info-actor agent-or-name))
                 (string (agent-info-actor (find-agent agent-or-name)))
                 (t agent-or-name))))
    (act:ask-s actor (list :compile text package) :time-out time-out)))
