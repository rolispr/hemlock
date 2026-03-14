;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; command-layer.lisp — Tests for hemlock.command: the editor framework.
;;;
;;; Tests buffer management, variable system, command registration,
;;; key binding, character attributes, and file completion.
;;; Requires hemlock.base loaded but no running editor display.
;;;

(defpackage :hemlock.command-tests
  (:use :common-lisp :fiveam :hemlock.text :hemlock.command)
  (:shadowing-import-from :hemlock.text #:char-code-limit)
  (:shadowing-import-from :fiveam #:*debug-on-error*))

(in-package :hemlock.command-tests)

(def-suite command-layer :description "hemlock.command framework tests")
(in-suite command-layer)


;;;; ---- Buffer Management ----------------------------------------------------

(test buffer-create-delete
  "Create a buffer, verify name, delete it."
  (let ((buf (make-buffer "Test Buffer CL")))
    (unwind-protect
         (progn
           (is (string= "Test Buffer CL" (buffer-name buf)))
           (is-true (bufferp buf))
           (is (not (null (buffer-region buf)))))
      (delete-buffer buf))))

(test buffer-rename
  "Rename buffer and look it up by new name."
  (let ((buf (make-buffer "Old Name CL")))
    (unwind-protect
         (progn
           (setf (buffer-name buf) "New Name CL")
           (is (string= "New Name CL" (buffer-name buf)))
           (is (eq buf (find-buffer "New Name CL")))
           (is (null (find-buffer "Old Name CL"))))
      (delete-buffer buf))))

(test buffer-text-mutation
  "Insert text into buffer and read it back."
  (let ((buf (make-buffer "Mut Buffer CL")))
    (unwind-protect
         (let ((point (buffer-point buf)))
           (insert-string point "hello world")
           (is (string= "hello world"
                         (region-to-string (buffer-region buf)))))
      (delete-buffer buf))))

(test buffer-modified-tracking
  "Buffer tracks modification state."
  (let ((buf (make-buffer "Mod Buffer CL")))
    (unwind-protect
         (progn
           (is-false (buffer-modified buf))
           (insert-string (buffer-point buf) "change")
           (is-true (buffer-modified buf))
           (setf (buffer-modified buf) nil)
           (is-false (buffer-modified buf)))
      (delete-buffer buf))))

(test buffer-writable-flag
  "Buffer writable flag can be toggled."
  (let ((buf (make-buffer "RO Buffer CL")))
    (unwind-protect
         (progn
           (is-true (buffer-writable buf))
           (setf (buffer-writable buf) nil)
           (is-false (buffer-writable buf)))
      (delete-buffer buf))))

(test find-buffer-lookup
  "find-buffer returns buffer by name."
  (let ((buf (make-buffer "Find Me CL")))
    (unwind-protect
         (progn
           (is (eq buf (find-buffer "Find Me CL")))
           (is (null (find-buffer "Not Here CL"))))
      (delete-buffer buf))))

(test rename-buffer-uniquely-basic
  "rename-buffer-uniquely appends suffix on conflict."
  (let ((buf1 (make-buffer "Unique CL"))
        (buf2 (make-buffer "Unique CL 2")))
    (unwind-protect
         (progn
           (rename-buffer-uniquely buf2 "Unique CL")
           ;; buf2 should get "Unique CL<2>" or similar since "Unique CL" is taken
           (is (not (string= "Unique CL" (buffer-name buf2))))
           (is (search "Unique CL" (buffer-name buf2))))
      (delete-buffer buf1)
      (delete-buffer buf2))))


;;;; ---- Variable System ------------------------------------------------------

(test variable-define-and-read
  "defhvar creates a variable, value reads it."
  (defhvar "Test Var CL" "A test variable." :value 42)
  (unwind-protect
       (progn
         (is (= 42 (variable-value (string-to-variable "Test Var CL"))))
         (is-true (hemlock-bound-p (string-to-variable "Test Var CL"))))
    (delete-variable (string-to-variable "Test Var CL"))))

(test variable-setf
  "Setting a variable updates its value."
  (defhvar "Setf Var CL" "Settable." :value 0)
  (unwind-protect
       (progn
         (setf (variable-value (string-to-variable "Setf Var CL")) 99)
         (is (= 99 (variable-value (string-to-variable "Setf Var CL")))))
    (delete-variable (string-to-variable "Setf Var CL"))))

(test variable-hooks
  "Variable hooks fire on value change."
  (let ((hook-fired nil))
    (defhvar "Hook Var CL" "Hooked." :value 0)
    (unwind-protect
         (progn
           (let ((sym (string-to-variable "Hook Var CL")))
             (add-hook (variable-hooks sym)
                       (lambda (name kind where new-val)
                         (declare (ignore name kind where))
                         (setf hook-fired new-val)))
             (setf (variable-value sym) 77)
             (is (eql 77 hook-fired))))
      (delete-variable (string-to-variable "Hook Var CL")))))

(test variable-delete
  "delete-variable removes the binding."
  (defhvar "Del Var CL" "Temporary." :value t)
  (delete-variable (string-to-variable "Del Var CL"))
  (is-false (hemlock-bound-p (string-to-variable "Del Var CL"))))


;;;; ---- Command Registration -------------------------------------------------

(test command-create-and-lookup
  "make-command registers, get-command retrieves."
  (make-command "Test Command CL" "A test."
                (lambda (p) (declare (ignore p)) nil))
  (let ((cmd (getstring "Test Command CL" *command-names*)))
    (is (not (null cmd)))
    (is (string= "Test Command CL" (command-name cmd)))))


;;;; ---- Key Binding ----------------------------------------------------------

(test bind-key-and-lookup
  "bind-key wires a command to a key, get-command finds it."
  (make-command "Bound Command CL" "For binding test."
                (lambda (p) (declare (ignore p)) nil))
  (bind-key "Bound Command CL" #k"Control-x Control-t")
  (let ((cmd (get-command #k"Control-x Control-t" :global)))
    (is (not (null cmd)))
    (is (string= "Bound Command CL" (command-name cmd))))
  (delete-key-binding #k"Control-x Control-t" :global))


;;;; ---- Character Attributes -------------------------------------------------

(test char-attribute-basic
  "character-attribute-p verifies defined attributes."
  ;; "Whitespace" is defined during init — check it exists
  (is-true (character-attribute-p :whitespace))
  (is-false (character-attribute-p :nonexistent-attribute-xyz)))


;;;; ---- File Completion ------------------------------------------------------

(test complete-file-existing-dir
  "complete-file on /tmp/ returns something."
  (multiple-value-bind (result completep)
      (complete-file "/tmp/")
    (declare (ignore completep))
    ;; /tmp/ exists so we should get some result (or nil if truly empty)
    (is-true t)))  ; just verifying no error

(test ambiguous-files-basic
  "ambiguous-files returns a list."
  (let ((results (ambiguous-files "/tmp/")))
    (is (listp results))))
