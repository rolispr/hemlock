;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; wire-codec.lisp — Unit tests for hemlock.wire encode/decode round-trips.
;;;
;;; Strategy: write objects into a staging wire (vector-device in memory),
;;; grab the raw bytes, pre-load them into a fresh read wire's ibuf, then
;;; read them back with wire-get-object.  No sockets, no threads.
;;;

(defpackage :hemlock.wire-tests
  (:use :common-lisp :fiveam)
  (:import-from :hemlock.wire
    #:make-staging-wire #:make-wire
    #:wire-output-object #:wire-output-funcall #:wire-force-output
    #:wire-get-object #:wire-read-frame
    #:wire-send-handshake #:wire-receive-handshake
    #:vector-device-bytes #:wire-device #:wire-p
    #:make-vector-device #:device-append-to-input-buffer
    #:wire-ibuf #:wire-ibuf-offset #:wire-ibuf-end))

(in-package :hemlock.wire-tests)

(def-suite wire-codec :description "Wire protocol encode/decode round-trips")
(in-suite wire-codec)

;;; ---- Helpers ---------------------------------------------------------------

(defun encode-object (object)
  "Encode OBJECT to a fresh byte vector using a staging wire."
  (let ((wire (make-staging-wire)))
    (wire-output-object wire object)
    (wire-force-output wire)
    (copy-seq (vector-device-bytes (wire-device wire)))))

(defun make-read-wire-from-bytes (bytes)
  "Create a wire with BYTES pre-loaded in its input buffer, ready to read."
  (let* ((device (make-vector-device))
         (wire   (make-wire device)))
    (device-append-to-input-buffer device bytes)
    wire))

(defun roundtrip (object)
  "Encode then decode OBJECT and return the decoded value."
  (decode-bytes (encode-object object)))

(defun decode-bytes (bytes)
  "Decode one object from BYTES."
  (wire-get-object (make-read-wire-from-bytes bytes)))

;;; ---- Numbers ---------------------------------------------------------------

(test number-zero
  (is (= 0 (roundtrip 0))))

(test number-positive
  (is (= 42 (roundtrip 42))))

(test number-negative
  (is (= -1 (roundtrip -1))))

(test number-max-signed-32
  (is (= #x7FFFFFFF (roundtrip #x7FFFFFFF))))

(test number-min-signed-32
  (is (= -2147483648 (roundtrip -2147483648))))

(test number-bignum
  ;; Values outside signed-32 are encoded as bignums
  (is (= #x100000000 (roundtrip #x100000000)))
  (is (= (- #x100000000) (roundtrip (- #x100000000)))))

;;; ---- Strings ---------------------------------------------------------------

(test string-empty
  (is (equal "" (roundtrip ""))))

(test string-ascii
  (is (equal "hello, world" (roundtrip "hello, world"))))

(test string-with-spaces
  (is (equal "foo bar baz" (roundtrip "foo bar baz"))))

(test string-unicode
  (is (equal "αβγ" (roundtrip "αβγ"))))

(test string-long
  (let ((s (make-string 1000 :initial-element #\x)))
    (is (equal s (roundtrip s)))))

;;; ---- Symbols ---------------------------------------------------------------

(test symbol-cl-nil
  ;; NIL encodes as symbol NIL in package COMMON-LISP
  (is (eq nil (roundtrip nil))))

(test symbol-cl-t
  (is (eq t (roundtrip t))))

(test symbol-keyword
  (is (eq :foo (roundtrip :foo))))

(test symbol-in-hemlock-wire
  (is (eq 'hemlock.wire:wire-p (roundtrip 'hemlock.wire:wire-p))))

;;; ---- Cons / list -----------------------------------------------------------

(test cons-simple
  (is (equal '(1 . 2) (roundtrip '(1 . 2)))))

(test list-of-numbers
  (is (equal '(1 2 3) (roundtrip '(1 2 3)))))

(test nested-list
  (is (equal '((a b) (c d)) (roundtrip '((a b) (c d))))))

;;; ---- Symbol caching --------------------------------------------------------
;;;
;;; When the same symbol is encoded twice in the same wire, the second
;;; occurrence uses a lookup-op reference instead of re-encoding the name.
;;; Round-tripping two identical symbols should still produce equal results.

(test symbol-caching-two-occurrences
  (let* ((wire (make-staging-wire)))
    (wire-output-object wire 'hemlock.wire:wire-p)
    (wire-output-object wire 'hemlock.wire:wire-p)
    (wire-force-output wire)
    (let* ((bytes (copy-seq (vector-device-bytes (wire-device wire))))
           (rw    (make-read-wire-from-bytes bytes)))
      (let ((s1 (wire-get-object rw))
            (s2 (wire-get-object rw)))
        (is (eq 'hemlock.wire:wire-p s1))
        (is (eq s1 s2))))))

;;; ---- Handshake -------------------------------------------------------------

(test handshake-roundtrip
  "wire-send-handshake bytes are accepted by wire-receive-handshake."
  (let* ((send-wire (make-staging-wire)))
    (wire-send-handshake send-wire)
    ;; send-wire now holds [magic-4bytes][version-4bytes]
    (let* ((bytes  (copy-seq (vector-device-bytes (wire-device send-wire))))
           (recv-w (make-read-wire-from-bytes bytes)))
      (is-true (wire-receive-handshake recv-w)))))

(test handshake-bad-magic-errors
  "wire-receive-handshake signals an error on bad magic."
  (let* ((wire (make-staging-wire)))
    ;; Write a wrong magic value, then the correct version
    (hemlock.wire:wire-output-number wire 0)
    (hemlock.wire:wire-output-number wire hemlock.wire:+protocol-version+)
    (wire-force-output wire)
    (let* ((bytes (copy-seq (vector-device-bytes (wire-device wire))))
           (rw    (make-read-wire-from-bytes bytes)))
      (signals error (wire-receive-handshake rw)))))

;;; ---- Framed funcall dispatch -----------------------------------------------
;;;
;;; wire-output-funcall writes [4-byte-length][funcall-body] to the wire.
;;; wire-read-frame reads the length prefix then dispatches wire-get-object,
;;; which actually CALLS the encoded function.  This tests the full dispatch path.

(defvar *funcall-result* nil)

(defun %set-funcall-result (x y z)
  (setf *funcall-result* (list x y z)))

(test framed-funcall-dispatch
  "wire-output-funcall + wire-read-frame dispatches the call correctly."
  ;; wire-output-funcall writes the body bytes directly to the device and
  ;; the 4-byte length to the wire obuf.  wire-force-output flushes the
  ;; length, so both end up in the vector-device bytes in the right order.
  (setf *funcall-result* nil)
  (let* ((wire (make-staging-wire)))
    (wire-output-funcall wire
      'hemlock.wire-tests::%set-funcall-result
      99
      "dispatch-test"
      :keyword-arg)
    ;; Flush the 4-byte length from obuf to device.
    (wire-force-output wire)
    (let* ((bytes (copy-seq (vector-device-bytes (wire-device wire))))
           (rw    (make-read-wire-from-bytes bytes)))
      (wire-read-frame rw)))
  (is (equal *funcall-result* (list 99 "dispatch-test" :keyword-arg))))

(test framed-funcall-three-args
  "wire-output-funcall works with three arguments of mixed types."
  (setf *funcall-result* nil)
  (let* ((wire (make-staging-wire)))
    (wire-output-funcall wire 'hemlock.wire-tests::%set-funcall-result 0 "" nil)
    (wire-force-output wire)
    (let* ((bytes (copy-seq (vector-device-bytes (wire-device wire))))
           (rw    (make-read-wire-from-bytes bytes)))
      (wire-read-frame rw)))
  (is (equal *funcall-result* (list 0 "" nil))))

;;; ---- wire-p predicate ------------------------------------------------------

(test wire-p-on-real-wire
  (let ((w (make-staging-wire)))
    (is-true (wire-p w))))

(test wire-p-on-nil
  (is-false (wire-p nil)))

(test wire-p-on-keyword-placeholder
  "The :wire-not-yet-established placeholder that caused the original crash."
  (is-false (wire-p :wire-not-yet-established)))

(test wire-p-on-integer
  (is-false (wire-p 42)))
