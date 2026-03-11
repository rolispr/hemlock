;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;; Forward declarations for functions defined in later files.

(in-package :hemlock.text)

(declaim (ftype (function (t) t) window-buffer))
(declaim (ftype (function (t) t) change-to-buffer))   ; filecoms.lisp
(declaim (ftype (function (t t) t) hemlock::to-line-comment)) ; comments.lisp, used in lispbuf.lisp
