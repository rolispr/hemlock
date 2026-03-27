;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(asdf:defsystem :hemlock-agent
  :description "Hemlock agent — loaded by agent processes to connect back to the editor"
  :depends-on (:bordeaux-threads :sento :sento-remoting)
  :pathname "src/actor/"
  :serial t
  :components
  ((:file "agent-package")
   (:file "agent")))
