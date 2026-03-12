;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; eval-guard.lisp — Tests for eval-server-ready-p.
;;;
;;; Tests the guard that prevents eval commands from trying to use a
;;; server-info whose wire is still just the :wire-not-yet-established
;;; placeholder (the bug this fix addresses).
;;;
;;; Requires: hemlock.base loaded (for server-info struct and
;;;           eval-server-ready-p).
;;;

(defpackage :hemlock.eval-guard-tests
  (:use :common-lisp :fiveam))

(in-package :hemlock.eval-guard-tests)

(def-suite eval-guard :description "eval-server-ready-p guard logic")
(in-suite eval-guard)

;;; ---- eval-server-ready-p ---------------------------------------------------

(test ready-p-nil-info
  "nil info (no server started) → not ready."
  (is-false (hemlock.command:eval-server-ready-p nil)))

(test ready-p-placeholder-wire
  "Server started but slave not yet connected: wire is keyword placeholder → not ready.
   This is the exact scenario that caused the type error crash."
  (let ((info (hemlock::make-server-info
               :name "test-pending"
               :wire :wire-not-yet-established)))
    (is-false (hemlock.command:eval-server-ready-p info))))

(test ready-p-nil-wire
  "Server-info exists but wire is nil → not ready."
  (let ((info (hemlock::make-server-info :name "test-nil-wire" :wire nil)))
    (is-false (hemlock.command:eval-server-ready-p info))))

(test ready-p-real-wire
  "Server-info with a real wire struct → ready."
  (let* ((wire (hemlock.wire:make-wire (hemlock.wire::make-vector-device)))
         (info (hemlock::make-server-info :name "test-ready" :wire wire)))
    (is-true (hemlock.command:eval-server-ready-p info))))

;;; ---- make-extra-repl-buffer-impl alias ------------------------------------

(test alias-bound
  "make-extra-repl-buffer-impl fdefinition was installed."
  (is-true (fboundp 'hemlock::make-extra-repl-buffer-impl)))

(test alias-same-function
  "make-extra-repl-buffer-impl is the same function as %make-extra-typescript-buffer."
  (is (eq (fdefinition 'hemlock::make-extra-repl-buffer-impl)
          (fdefinition 'hemlock::%make-extra-typescript-buffer))))
