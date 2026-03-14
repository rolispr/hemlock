;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; text-layer.lisp — Tests for hemlock.text: the data model layer.
;;;
;;; Pure data structures, no editor state needed. Tests lines, marks,
;;; regions, text mutation, search, rings, tables, and key events.
;;;

(defpackage :hemlock.text-tests
  (:use :common-lisp :fiveam :hemlock.text)
  (:shadowing-import-from :hemlock.text #:char-code-limit))

(in-package :hemlock.text-tests)

(def-suite text-layer :description "hemlock.text data model tests")
(in-suite text-layer)


;;;; ---- Ring -----------------------------------------------------------------

(test ring-basic
  "Create ring, push, pop, verify LIFO order."
  (let ((r (make-ring 3)))
    (is (= 0 (ring-length r)))
    (ring-push "a" r)
    (ring-push "b" r)
    (is (= 2 (ring-length r)))
    (is (equal "b" (ring-ref r 0)))
    (is (equal "a" (ring-ref r 1)))))

(test ring-overflow
  "Ring wraps when pushed past capacity."
  (let ((r (make-ring 2)))
    (ring-push "a" r)
    (ring-push "b" r)
    (ring-push "c" r)
    (is (= 2 (ring-length r)))
    (is (equal "c" (ring-ref r 0)))
    (is (equal "b" (ring-ref r 1)))))

(test ring-pop
  "ring-pop returns and removes top element."
  (let ((r (make-ring 3)))
    (ring-push "x" r)
    (ring-push "y" r)
    (is (equal "y" (ring-pop r)))
    (is (= 1 (ring-length r)))
    (is (equal "x" (ring-ref r 0)))))

(test ring-rotate
  "rotate-ring shifts elements."
  (let ((r (make-ring 4)))
    (ring-push "a" r)
    (ring-push "b" r)
    (ring-push "c" r)
    (rotate-ring r 1)
    (is (equal "b" (ring-ref r 0)))))


;;;; ---- String Table ---------------------------------------------------------

(test table-basic
  "Store and retrieve string table entries."
  (let ((tbl (make-string-table)))
    (setf (getstring "alpha" tbl) 1)
    (setf (getstring "beta" tbl) 2)
    (is (= 1 (getstring "alpha" tbl)))
    (is (= 2 (getstring "beta" tbl)))))

(test table-not-found
  "getstring returns nil for missing keys."
  (let ((tbl (make-string-table)))
    (multiple-value-bind (val found) (getstring "nope" tbl)
      (is-false found)
      (is (null val)))))

(test table-complete-string
  "complete-string finds unique prefix completion."
  (let ((tbl (make-string-table)))
    (setf (getstring "buffer-name" tbl) 1)
    (setf (getstring "buffer-point" tbl) 2)
    (setf (getstring "command" tbl) 3)
    ;; complete-string takes a LIST of tables
    ;; "com" uniquely completes to "command"
    (multiple-value-bind (prefix val unique) (complete-string "com" (list tbl))
      (declare (ignore val))
      (is (string= "command" prefix))
      (is-true unique))
    ;; "buf" is ambiguous
    (multiple-value-bind (prefix val unique) (complete-string "buf" (list tbl))
      (declare (ignore val))
      (is (string= "buffer-" prefix))
      (is-false unique))))

(test table-find-ambiguous
  "find-ambiguous returns all matching keys."
  (let ((tbl (make-string-table)))
    (setf (getstring "foo" tbl) 1)
    (setf (getstring "foobar" tbl) 2)
    (setf (getstring "baz" tbl) 3)
    (let ((matches (find-ambiguous "foo" tbl)))
      (is (= 2 (length matches))))))

(test table-delete
  "delete-string removes entry."
  (let ((tbl (make-string-table)))
    (setf (getstring "hello" tbl) 42)
    (delete-string "hello" tbl)
    (multiple-value-bind (val found) (getstring "hello" tbl)
      (declare (ignore val))
      (is-false found))))

(test table-clrstring
  "clrstring removes all entries."
  (let ((tbl (make-string-table)))
    (setf (getstring "a" tbl) 1)
    (setf (getstring "b" tbl) 2)
    (clrstring tbl)
    (is-false (getstring "a" tbl))))


;;;; ---- Line / Mark / Region -------------------------------------------------

(test region-from-string
  "string-to-region creates a region with correct content."
  (let* ((r (string-to-region "hello world")))
    (is (string= "hello world" (region-to-string r)))))

(test region-multiline
  "Multi-line string round-trips through region."
  (let* ((text (format nil "line one~%line two~%line three"))
         (r (string-to-region text)))
    (is (string= text (region-to-string r)))
    (is (= 3 (count-lines r)))))

(test region-empty
  "Empty string creates valid region."
  (let ((r (string-to-region "")))
    (is (string= "" (region-to-string r)))
    (is (= 1 (count-lines r)))))

(test mark-comparison
  "Mark ordering works within a region."
  (let* ((r (string-to-region "abcdef"))
         (m1 (copy-mark (region-start r)))
         (m2 (copy-mark (region-start r))))
    (character-offset m2 3)
    (is-true (mark< m1 m2))
    (is-true (mark> m2 m1))
    (is-true (mark= m1 (region-start r)))
    (is-true (mark/= m1 m2))))

(test mark-character-offset
  "character-offset moves mark by N characters."
  (let* ((r (string-to-region "abcdef"))
         (m (copy-mark (region-start r))))
    (character-offset m 3)
    (is (char= #\d (next-character m)))
    (character-offset m -2)
    (is (char= #\b (next-character m)))))

(test mark-line-predicates
  "start-line-p and end-line-p work."
  (let* ((r (string-to-region "hello"))
         (m (copy-mark (region-start r))))
    (is-true (start-line-p m))
    (is-false (end-line-p m))
    (line-end m)
    (is-true (end-line-p m))
    (is-false (start-line-p m))))


;;;; ---- Text Mutation --------------------------------------------------------

(test insert-string-at-mark
  "insert-string adds text at mark position."
  (let* ((r (string-to-region "hello world"))
         (m (copy-mark (region-start r))))
    (character-offset m 5)
    (insert-string m " beautiful")
    (is (string= "hello beautiful world" (region-to-string r)))))

(test insert-character-at-mark
  "insert-character adds single char."
  (let* ((r (string-to-region "hllo"))
         (m (copy-mark (region-start r))))
    (character-offset m 1)
    (insert-character m #\e)
    (is (string= "hello" (region-to-string r)))))

(test delete-characters-forward
  "delete-characters removes N chars forward."
  (let* ((r (string-to-region "hello world"))
         (m (copy-mark (region-start r))))
    (delete-characters m 5)
    (is (string= " world" (region-to-string r)))))

(test delete-characters-backward
  "delete-characters with negative N removes backward."
  (let* ((r (string-to-region "hello world"))
         (m (copy-mark (region-start r))))
    (character-offset m 5)
    (delete-characters m -5)
    (is (string= " world" (region-to-string r)))))

(test copy-region-preserves-original
  "copy-region doesn't modify the source."
  (let* ((r (string-to-region "original text"))
         (c (copy-region r)))
    (is (string= "original text" (region-to-string c)))
    (is (string= "original text" (region-to-string r)))))

(test delete-region-clears-content
  "delete-region removes text between marks."
  (let* ((r (string-to-region "hello world"))
         (start (copy-mark (region-start r)))
         (end (copy-mark (region-start r))))
    (character-offset end 5)
    (delete-region (region start end))
    (is (string= " world" (region-to-string r)))))


;;;; ---- Search ---------------------------------------------------------------

(test search-string-forward
  "find-pattern locates a string forward."
  (let* ((r (string-to-region "the quick brown fox"))
         (p (new-search-pattern :string-insensitive :forward "brown"))
         (m (copy-mark (region-start r))))
    (is (not (null (find-pattern m p))))
    (is (= 10 (mark-charpos m)))))

(test search-string-backward
  "find-pattern searches backward."
  (let* ((r (string-to-region "aaa bbb aaa"))
         (p (new-search-pattern :string-insensitive :backward "aaa"))
         (m (copy-mark (region-end r))))
    (is (not (null (find-pattern m p))))
    (is (= 8 (mark-charpos m)))))

(test search-not-found
  "find-pattern returns nil when no match."
  (let* ((r (string-to-region "hello world"))
         (p (new-search-pattern :string-insensitive :forward "xyz"))
         (m (copy-mark (region-start r))))
    (is (null (find-pattern m p)))))

(test replace-pattern-basic
  "replace-pattern substitutes matches."
  (let* ((r (string-to-region "foo bar foo baz foo"))
         (p (new-search-pattern :string-insensitive :forward "foo"))
         (m (copy-mark (region-start r))))
    ;; replace-pattern returns the final mark position, not a count
    (replace-pattern m p "qux")
    (is (string= "qux bar qux baz qux" (region-to-string r)))))


;;;; ---- Key Events -----------------------------------------------------------

(test key-event-roundtrip
  "char-key-event → key-event-char round-trips for printable chars."
  (let ((ke (char-key-event #\a)))
    (is-true (key-event-p ke))
    (is (char= #\a (key-event-char ke)))))

(test key-event-with-modifier
  "Key event with control modifier."
  (let* ((bits (make-key-event-bits "Control"))
         (ke (make-key-event "a" bits)))
    (is-true (key-event-bit-p ke "Control"))
    (is-false (key-event-bit-p ke "Meta"))))

(test char-key-event-mapping
  "char-key-event returns the key event for a character."
  (let ((ke (char-key-event #\a)))
    (is-true (key-event-p ke))
    (is (char= #\a (key-event-char ke)))))


;;;; ---- Portability (lispdep.lisp) -------------------------------------------

(test fixnump-works
  "fixnump returns true for fixnums."
  (is-true (fixnump 42))
  (is-true (fixnump 0))
  (is-true (fixnump -1))
  (is-false (fixnump "hello"))
  (is-false (fixnump 3.14)))

(test getenv-path
  "getenv retrieves PATH (should always exist)."
  (is (stringp (getenv "PATH"))))

(test file-writable-nonexistent
  "file-writable on non-existent path in writable dir returns truthy."
  (let ((path (merge-pathnames (format nil "hemlock-test-~A.tmp" (get-universal-time))
                               (user-homedir-pathname))))
    (is-true (file-writable path))))
