;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

;;; hemlock-wire-tests — wire protocol codec tests.
;;;
;;; No editor dependencies — loads only the wire module files.
;;; Fast, suitable for CI.
;;;
;;; Usage:
;;;   cd /path/to/cl
;;;   sbcl --eval '(push #P"hemlock-port/" asdf:*central-registry*)' \
;;;        --eval '(asdf:test-system :hemlock-wire-tests)'

(asdf:defsystem :hemlock-wire-tests
  :description "Wire protocol codec tests — no editor dependencies"
  :depends-on (:fiveam
               :bordeaux-threads
               :trivial-gray-streams
               :cffi
               :sb-bsd-sockets)
  :components
  ((:module wire
    :pathname "src"
    :serial t
    :components
    ((:file "wire-package")
     (:file "port")
     (:file "introspect")
     (:file "wire")
     (:file "remote")))
   (:module tests
    :pathname "t"
    :depends-on (wire)
    :components
    ((:file "wire-codec"))))
  :perform (asdf:test-op (op system)
             (let ((suite (uiop:find-symbol* :wire-codec :hemlock.wire-tests)))
               (unless (uiop:symbol-call :fiveam :run! suite)
                 (uiop:quit 1)))))
