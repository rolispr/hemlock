;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

;;; hemlock-wire-tests — wire protocol codec tests.
;;;
;;; Usage:
;;;   sbcl --eval '(asdf:test-system :hemlock-wire-tests)'

(asdf:defsystem :hemlock-wire-tests
  :description "Wire protocol codec tests"
  :depends-on (:hemlock :fiveam)
  :components
  ((:module tests
    :pathname "t"
    :components
    ((:file "wire-codec"))))
  :perform (asdf:test-op (op system)
             (let ((suite (uiop:find-symbol* :wire-codec :hemlock.wire-tests)))
               (unless (uiop:symbol-call :fiveam :run! suite)
                 (uiop:quit 1)))))
