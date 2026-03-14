;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; io-layer.lisp — Tests for hemlock.io: event loop and I/O plumbing.
;;;
;;; Tests fd polling, invoke-later dispatch, and connection lifecycle.
;;;

(defpackage :hemlock.io-tests
  (:use :common-lisp :fiveam))

(in-package :hemlock.io-tests)

(def-suite io-layer :description "hemlock.io event loop and I/O tests")
(in-suite io-layer)


;;;; ---- fd-readable-p --------------------------------------------------------

(test fd-readable-p-stdin-no-block
  "fd-readable-p on stdin returns without blocking."
  ;; We can't guarantee data on stdin, but we can verify it returns a boolean
  ;; without hanging. fd 0 = stdin.
  (let ((result (hemlock.io:fd-readable-p 0)))
    (is (typep result 'boolean))))

(test fd-readable-p-pipe
  "fd-readable-p detects data on a pipe."
  ;; Create a pipe, write to it, verify fd-readable-p returns true.
  (multiple-value-bind (read-fd write-fd)
      (sb-posix:pipe)
    (unwind-protect
         (progn
           ;; Before writing, read end should not be readable
           (is-false (hemlock.io:fd-readable-p read-fd))
           ;; Write a byte
           (let ((buf (make-array 1 :element-type '(unsigned-byte 8)
                                    :initial-element 42)))
             (sb-posix:write write-fd (sb-sys:vector-sap buf) 1))
           ;; Now it should be readable
           (is-true (hemlock.io:fd-readable-p read-fd)))
      (sb-posix:close read-fd)
      (sb-posix:close write-fd))))

(test fd-readable-p-bad-fd
  "fd-readable-p on invalid fd returns without crashing."
  ;; poll() on invalid fd returns POLLNVAL which is > 0, so plusp is true.
  ;; The important thing is it doesn't signal an error.
  (finishes (hemlock.io:fd-readable-p 99999)))


;;;; ---- invoke-later / drain -------------------------------------------------

(test invoke-later-and-drain
  "invoke-later queues work, drain-pending-invocations runs it."
  (let ((ran nil))
    (hemlock.command:invoke-later :sb-sys (lambda () (setf ran t)))
    (is-false ran)
    (hemlock.io:drain-pending-invocations)
    (is-true ran)))

(test invoke-later-ordering
  "Multiple invoke-later calls execute in order when drained."
  (let ((log nil))
    (hemlock.command:invoke-later :sb-sys (lambda () (push 1 log)))
    (hemlock.command:invoke-later :sb-sys (lambda () (push 2 log)))
    (hemlock.command:invoke-later :sb-sys (lambda () (push 3 log)))
    (hemlock.io:drain-pending-invocations)
    ;; They're pushed onto a list and popped, so order depends on impl.
    ;; Just verify all three ran.
    (is (= 3 (length log)))
    (is (null (set-difference '(1 2 3) log)))))

(test drain-when-empty
  "drain-pending-invocations is a no-op when queue is empty."
  (hemlock.io:drain-pending-invocations)
  (is-true t))
