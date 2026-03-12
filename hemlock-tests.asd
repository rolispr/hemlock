;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

;;; hemlock-tests — full test suite for hemlock-port.
;;;
;;; Depends on hemlock.base.  Run after the editor loads cleanly.
;;;
;;; Usage:
;;;   cd /path/to/cl
;;;   sbcl --eval '(push #P"hemlock-port/" asdf:*central-registry*)' \
;;;        --eval '(asdf:test-system :hemlock-tests)'

(asdf:defsystem :hemlock-tests
  :description "Full hemlock test suite"
  :depends-on (:hemlock.base :hemlock-wire-tests :fiveam)
  :components
  ((:module tests
    :pathname "t"
    :components
    ((:file "eval-guard"))))
  :perform (asdf:test-op (op system)
             (let ((results
                     (list
                      (uiop:symbol-call :fiveam :run!
                        (uiop:find-symbol* :wire-codec :hemlock.wire-tests))
                      (uiop:symbol-call :fiveam :run!
                        (uiop:find-symbol* :eval-guard :hemlock.eval-guard-tests)))))
               (unless (every #'identity results)
                 (uiop:quit 1)))))
