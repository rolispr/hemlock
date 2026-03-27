;;;; Smoke test: two actor-systems communicating via sento remoting.
;;;; Run: sbcl --load t/remoting-smoke.lisp

(require :asdf)
(push #P"./" asdf:*central-registry*)
(push #P"/Users/bret.horne/common-lisp/cl-gserver/" asdf:*central-registry*)

(handler-bind ((warning #'muffle-warning))
  (asdf:load-system :sento-remoting))

(defpackage :hemlock.remoting-test
  (:use :cl))
(in-package :hemlock.remoting-test)

(format t "~%=== Sento Remoting Smoke Test ===~%")

;; Create two actor-systems (simulating master and agent)
(defvar *master* (asys:make-actor-system))
(defvar *agent* (asys:make-actor-system))

;; Enable remoting on both
(rem:enable-remoting *master* :host "127.0.0.1" :port 19900)
(rem:enable-remoting *agent* :host "127.0.0.1" :port 19901)

(format t "Remoting enabled: master=19900 agent=19901~%")

;; Create an "agent" actor on the agent system
(ac:actor-of *agent* :name "agent"
  :receive (lambda (msg)
             (cond
               ((and (consp msg) (eq (car msg) :eval))
                (let* ((form-string (cadr msg))
                       (result (eval (read-from-string form-string))))
                  (act:reply (prin1-to-string result))))
               ((and (consp msg) (eq (car msg) :ping))
                (act:reply :pong))
               (t
                (act:reply (format nil "unknown: ~S" msg))))))

(format t "Agent actor created~%")

;; From master, create a remote reference to the agent
(defvar *agent-ref*
  (rem:make-remote-ref *master* "sento://127.0.0.1:19901/user/agent"))

(format t "Remote ref created: ~A~%" *agent-ref*)

;; Test 1: ping
(format t "~%Test 1: ping... ")
(let ((result (act:ask-s *agent-ref* '(:ping) :time-out 5)))
  (if (eq result :pong)
      (format t "PASS (got :pong)~%")
      (format t "FAIL (got ~S)~%" result)))

;; Test 2: eval
(format t "Test 2: eval (+ 1 2)... ")
(let ((result (act:ask-s *agent-ref* '(:eval "(+ 1 2)") :time-out 5)))
  (if (string= result "3")
      (format t "PASS (got ~S)~%" result)
      (format t "FAIL (got ~S)~%" result)))

;; Test 3: eval with state
(format t "Test 3: eval (list 1 2 3)... ")
(let ((result (act:ask-s *agent-ref* '(:eval "(list 1 2 3)") :time-out 5)))
  (if (string= result "(1 2 3)")
      (format t "PASS (got ~S)~%" result)
      (format t "FAIL (got ~S)~%" result)))

;; Test 4: tell (fire-and-forget)
(format t "Test 4: tell (fire-and-forget)... ")
(act:tell *agent-ref* '(:ping))
(sleep 0.5)
(format t "PASS (no error)~%")

;; Cleanup
(format t "~%Cleaning up...~%")
(rem:disable-remoting *master*)
(rem:disable-remoting *agent*)
(ac:shutdown *master*)
(ac:shutdown *agent*)

(format t "~%=== All tests complete ===~%")
(uiop:quit 0)
