;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(asdf:defsystem :hemlock
  :description "Hemlock text editor"
  :licence "Public Domain"
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "hemlock"
  :entry-point "hemlock::main"
  :depends-on (:hemlock.tty :hemlock.term))

(deploy:define-hook (:deploy asdf) (directory)
  (declare (ignorable directory))
  #+asdf (asdf:clear-source-registry)
  #+asdf (defun asdf:upgrade-asdf () nil))
