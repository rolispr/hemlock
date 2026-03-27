;;;; Actor integration test — run standalone:
;;;; sbcl --load t/actor-integration.lisp

(require :asdf)
(push #P"./" asdf:*central-registry*)
(push #P"/Users/bret.horne/common-lisp/cl-gserver/" asdf:*central-registry*)

(handler-bind ((warning #'muffle-warning))
  (asdf:load-system :hemlock))

(format t "~%=== ACTOR INTEGRATION TEST ===~%")

;; 1. Start actor system
(hemlock.actor:start-actor-system)
(hemlock.actor:start-agent-registry)
(hemlock.actor:start-local-agent)
(format t "Actor system started, remoting port: ~A~%" hemlock.actor:*remoting-port*)

;; 2. Test local agent via registry
(format t "~%--- Local agent eval ---~%")
(let ((r (hemlock.actor:agent-eval "local" "(+ 40 2)")))
  (format t "  (+ 40 2) => ~S~%" r)
  (assert (equal r '(:ok "42")) () "Expected (:ok \"42\"), got ~S" r))

(let ((r (hemlock.actor:agent-eval "local" "(values 1 2 3)")))
  (format t "  (values 1 2 3) => ~S~%" r)
  (assert (equal r '(:ok "1
2
3")) () "Expected multi-value, got ~S" r))

;; 3. Test compile
(let ((r (hemlock.actor:agent-compile "local" "(defun test-fn-actor (x) (* x x))"
                                       :package "CL-USER")))
  (format t "  compile defun => ~S~%" r)
  (assert (equal (first r) :ok) () "Compile failed: ~S" r))

(let ((r (hemlock.actor:agent-eval "local" "(cl-user::test-fn-actor 7)")))
  (format t "  (test-fn-actor 7) => ~S~%" r)
  (assert (equal r '(:ok "49")) () "Expected (:ok \"49\"), got ~S" r))

;; 4. Test ping
(let ((r (act:ask-s hemlock.actor:*local-agent* '(:ping) :time-out 5)))
  (format t "  ping => ~S~%" r)
  (assert (eq r :pong) () "Expected :pong, got ~S" r))

;; 5. List agents
(let ((agents (mapcar #'hemlock.actor:agent-info-name (hemlock.actor:list-agents))))
  (format t "~%Registered agents: ~S~%" agents)
  (assert (member "local" agents :test #'string=) () "local agent not registered"))

;; 6. Shutdown
(hemlock.actor:stop-actor-system)
(format t "~%=== ALL TESTS PASSED ===~%")
(uiop:quit 0)
