;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; Hemlock actor system lifecycle.  One actor-system per hemlock process.
;;; Remoting enables agent connections over TCP/TLS.

(in-package :hemlock.actor)

(defvar *actor-system* nil
  "The hemlock actor-system. Created once at startup.")

(defvar *remoting-port* nil
  "TCP port for sento remoting, or NIL if not enabled.")

(defun start-actor-system (&key (workers 4) (remoting-port 0))
  "Create and start the hemlock actor system.
   WORKERS: number of shared dispatcher threads.
   REMOTING-PORT: TCP port for agent connections (0 = auto-assign)."
  (when *actor-system*
    (error "Actor system already running."))
  (setf *actor-system*
        (asys:make-actor-system
         `(:dispatchers
           (:shared (:workers ,workers :strategy :random))
           :timeout-timer (:resolution 50 :max-size 500)
           :scheduler (:enabled :true :resolution 100 :max-size 500))))
  (when remoting-port
    (rem:enable-remoting *actor-system*
                         :host "127.0.0.1"
                         :port remoting-port)
    (setf *remoting-port* (rem:remoting-port *actor-system*)))
  *actor-system*)

(defun stop-actor-system ()
  "Shutdown the hemlock actor system."
  (when *actor-system*
    (when (rem:remoting-enabled-p *actor-system*)
      (rem:disable-remoting *actor-system*))
    (ac:shutdown *actor-system* :wait t)
    (setf *actor-system* nil
          *remoting-port* nil)))
