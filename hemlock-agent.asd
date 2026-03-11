;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

;;; hemlock-agent: loaded by agent Lisp processes.
;;;
;;; Minimal standalone system — only wire protocol + introspection.
;;; Zero editor dependencies.
;;;
;;; Agent processes are spawned by the master like:
;;;
;;;   sbcl --eval "(asdf:load-system :my-project)"
;;;        --eval "(asdf:load-system :hemlock-agent)"
;;;        --eval "(hemlock-agent:connect :host \"127.0.0.1\" :port NNN :name \"my-project\")"
;;;
;;; The agent connects back to the master's TCP port, registers itself,
;;; sets up wire-backed I/O streams, and runs a REPL.

(asdf:defsystem :hemlock-agent
  :description "Hemlock agent — standalone system loaded by agent Lisp processes"
  :licence "Public Domain"
  :depends-on (:bordeaux-threads
               :trivial-gray-streams
               :cffi
               :sb-bsd-sockets)
  :components
  ((:module agent
    :pathname "src"
    :serial t
    :components
    ((:file "wire-package")
     (:file "port")
     (:file "wire")
     (:file "remote")
     (:file "introspect")
     (:file "agent")))))
