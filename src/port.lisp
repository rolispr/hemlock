;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'sb-posix))

(in-package :hemlock.wire)

;;; unix-gethostid returns a hash of the hostname for use as the host-id
;;; component of remote-object identity triples. Uses machine-instance (standard
;;; CL) rather than sb-posix:gethostname which is not available in all SBCL builds.
(defun unix-gethostid ()
  "Return a hash of the hostname suitable for remote-object identity."
  (sxhash (machine-instance)))

(defun unix-getpid ()
  (sb-unix:unix-getpid))

;; fixme: remove this?
(push (cons '*print-readably* nil)
      bt:*default-special-bindings*)
