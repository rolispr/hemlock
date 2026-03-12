;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(asdf:defsystem :hemlock.term
  :description "Terminal emulator for Hemlock (ported from Eat)"
  :licence "GPLv3"
  :depends-on (:hemlock.base :cffi)
  :pathname "src/term/"
  :serial t
  :components
  ((:file "package")
   (:file "types")
   (:file "color")
   (:file "sgr")
   (:file "ops")
   (:file "write")
   (:file "parser")
   (:file "pty")
   (:file "input")
   (:file "render")
   (:file "mode")))
