;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

;;; hemlock-tests — full test suite for hemlock-port.
;;;
;;; Tests each bootstrap layer independently:
;;;   Layer 0: Wire protocol (hemlock.wire)     — wire-codec suite
;;;   Layer 1: Text data model (hemlock.text)    — text-layer suite
;;;   Layer 2: Editor framework (hemlock.command) — command-layer suite
;;;   Layer 3: I/O plumbing (hemlock.io)         — io-layer suite
;;;   Layer 4: Eval guard (hemlock)              — eval-guard suite
;;;
;;; Usage:
;;;   cd /path/to/cl
;;;   sbcl --eval '(push #P"hemlock-port/" asdf:*central-registry*)' \
;;;        --eval '(asdf:test-system :hemlock-tests)'

(asdf:defsystem :hemlock-tests
  :description "Full hemlock test suite"
  :depends-on (:hemlock :hemlock-wire-tests :fiveam)
  :components
  ((:module tests
    :pathname "t"
    :components
    ((:file "text-layer")
     (:file "command-layer")
     (:file "io-layer")
     (:file "eval-guard"))))
  :perform (asdf:test-op (op system)
             (let ((suites '(:wire-codec :text-layer :command-layer :io-layer :eval-guard))
                   (packages '(:hemlock.wire-tests :hemlock.text-tests :hemlock.command-tests
                               :hemlock.io-tests :hemlock.eval-guard-tests)))
               (let ((results
                       (loop for suite in suites
                             for pkg in packages
                             collect (uiop:symbol-call :fiveam :run!
                                       (uiop:find-symbol* suite pkg)))))
                 (unless (every #'identity results)
                   (uiop:quit 1))))))
