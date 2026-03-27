;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; FiveAM tests for FSet-based buffer state.

(defpackage :hemlock.fset-tests
  (:use :common-lisp :fiveam))

(in-package :hemlock.fset-tests)

(def-suite fset-state :description "FSet buffer-state tests")
(in-suite fset-state)

(test empty-buffer
  (let ((s (hemlock.text::make-empty-buffer-state "test")))
    (is (= 1 (hemlock.text::bs-line-count s)))
    (is (string= "" (hemlock.text::bs-line-string s 0)))
    (is (= 0 (hemlock.text::bs-tick s)))))

(test insert-single-char
  (let* ((s (hemlock.text::make-empty-buffer-state "test"))
         (s2 (hemlock.text::bs-insert-char s 0 0 #\H)))
    (is (string= "H" (hemlock.text::bs-line-string s2 0)))
    (is (= 1 (hemlock.text::bs-tick s2)))
    ;; Original unchanged
    (is (string= "" (hemlock.text::bs-line-string s 0)))))

(test insert-string-single-line
  (let* ((s (hemlock.text::make-empty-buffer-state "test"))
         (s2 (hemlock.text::bs-insert-string s 0 0 "Hello World")))
    (is (string= "Hello World" (hemlock.text::bs-line-string s2 0)))
    (is (= 1 (hemlock.text::bs-line-count s2)))))

(test insert-string-middle
  (let* ((s (hemlock.text::make-empty-buffer-state "test"))
         (s2 (hemlock.text::bs-insert-string s 0 0 "HelloWorld"))
         (s3 (hemlock.text::bs-insert-string s2 0 5 " ")))
    (is (string= "Hello World" (hemlock.text::bs-line-string s3 0)))))

(test insert-newline
  (let* ((s (hemlock.text::make-empty-buffer-state "test"))
         (s2 (hemlock.text::bs-insert-string s 0 0 "HelloWorld"))
         (s3 (hemlock.text::bs-insert-char s2 0 5 #\Newline)))
    (is (= 2 (hemlock.text::bs-line-count s3)))
    (is (string= "Hello" (hemlock.text::bs-line-string s3 0)))
    (is (string= "World" (hemlock.text::bs-line-string s3 1)))))

(test insert-multiline-string
  (let* ((s (hemlock.text::make-empty-buffer-state "test"))
         (s2 (hemlock.text::bs-insert-string s 0 0 (format nil "line1~%line2~%line3"))))
    (is (= 3 (hemlock.text::bs-line-count s2)))
    (is (string= "line1" (hemlock.text::bs-line-string s2 0)))
    (is (string= "line2" (hemlock.text::bs-line-string s2 1)))
    (is (string= "line3" (hemlock.text::bs-line-string s2 2)))))

(test delete-single-char
  (let* ((s (hemlock.text::make-empty-buffer-state "test"))
         (s2 (hemlock.text::bs-insert-string s 0 0 "Hello"))
         (s3 (hemlock.text::bs-delete-char s2 0 4)))
    (is (string= "Hell" (hemlock.text::bs-line-string s3 0)))))

(test delete-joins-lines
  (let* ((s (hemlock.text::make-empty-buffer-state "test"))
         (s2 (hemlock.text::bs-insert-string s 0 0 (format nil "Hello~%World")))
         ;; Delete at end of line 0 joins with line 1
         (s3 (hemlock.text::bs-delete-char s2 0 5)))
    (is (= 1 (hemlock.text::bs-line-count s3)))
    (is (string= "HelloWorld" (hemlock.text::bs-line-string s3 0)))))

(test delete-region-single-line
  (let* ((s (hemlock.text::make-empty-buffer-state "test"))
         (s2 (hemlock.text::bs-insert-string s 0 0 "Hello World"))
         (s3 (hemlock.text::bs-delete-region s2 0 5 0 11)))
    (is (string= "Hello" (hemlock.text::bs-line-string s3 0)))))

(test delete-region-multiline
  (let* ((s (hemlock.text::make-empty-buffer-state "test"))
         (s2 (hemlock.text::bs-insert-string s 0 0 (format nil "aaa~%bbb~%ccc~%ddd")))
         ;; Delete from middle of line 1 to middle of line 2
         (s3 (hemlock.text::bs-delete-region s2 1 1 2 2)))
    (is (= 3 (hemlock.text::bs-line-count s3)))
    (is (string= "aaa" (hemlock.text::bs-line-string s3 0)))
    (is (string= "bc" (hemlock.text::bs-line-string s3 1)))
    (is (string= "ddd" (hemlock.text::bs-line-string s3 2)))))

(test mark-adjustment-insert
  (let* ((s (hemlock.text::make-empty-buffer-state "test"))
         (s2 (hemlock.text::bs-insert-string s 0 0 "Hello"))
         ;; Point should have moved to end
         (point (fset:@ (hemlock.text::bs-marks s2) :point)))
    (is (= 0 (first point)))
    (is (= 5 (second point)))))

(test mark-adjustment-newline
  (let* ((s (hemlock.text::make-empty-buffer-state "test"))
         (s2 (hemlock.text::bs-insert-string s 0 0 "HelloWorld"))
         ;; Move point to position 5
         (s3 (hemlock.text::bs-move-mark s2 :point 0 5))
         ;; Insert newline at position 5
         (s4 (hemlock.text::bs-insert-char s3 0 5 #\Newline))
         (point (fset:@ (hemlock.text::bs-marks s4) :point)))
    ;; Point was at (0,5) right-inserting — should move to (1,0)
    (is (= 1 (first point)))
    (is (= 0 (second point)))))

(test region-to-string
  (let* ((s (hemlock.text::make-empty-buffer-state "test"))
         (s2 (hemlock.text::bs-insert-string s 0 0 (format nil "aaa~%bbb~%ccc"))))
    (is (string= "bbb" (hemlock.text::bs-region-to-string s2 1 0 1 3)))
    (is (string= (format nil "aaa~%bbb~%ccc")
                 (hemlock.text::bs-region-to-string s2 0 0 2 3)))))

(test snapshot
  (let* ((s (hemlock.text::make-empty-buffer-state "test"))
         (s2 (hemlock.text::bs-insert-string s 0 0 "Hello"))
         (snap (hemlock.text::state->snapshot s2)))
    (is (string= "test" (hemlock.text::snap-name snap)))
    (is (= 1 (hemlock.text::snap-tick snap)))
    (is (= 1 (hemlock.text::snap-line-count snap)))))

(test structural-sharing
  "Verify old and new states share structure."
  (let* ((s (hemlock.text::make-empty-buffer-state "test"))
         (s2 (hemlock.text::bs-insert-string s 0 0 "line1"))
         (s3 (hemlock.text::bs-insert-char s2 0 5 #\Newline))
         (s4 (hemlock.text::bs-insert-string s3 1 0 "line2")))
    ;; s3 and s4 should share line 0 ("line1")
    (is (string= "line1" (hemlock.text::bs-line-string s3 0)))
    (is (string= "line1" (hemlock.text::bs-line-string s4 0)))
    ;; But s4 has different line 1
    (is (string= "" (hemlock.text::bs-line-string s3 1)))
    (is (string= "line2" (hemlock.text::bs-line-string s4 1)))))

(test split-on-newlines
  (is (equal '("a" "b" "c") (hemlock.text::split-on-newlines (format nil "a~%b~%c"))))
  (is (equal '("hello") (hemlock.text::split-on-newlines "hello")))
  (is (equal '("" "") (hemlock.text::split-on-newlines (string #\Newline)))))
