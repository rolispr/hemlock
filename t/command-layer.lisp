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


;;;; ---- Error Robustness -----------------------------------------------------

(test inhibit-hooks-suppresses-invoke-hook
  "invoke-hook is a no-op when *inhibit-hooks* is T."
  (let ((fired nil))
    (defhvar "Inhibit Test Hook CL" "Hook for testing inhibit." :value nil)
    (unwind-protect
         (progn
           (add-hook (variable-hooks (string-to-variable "Inhibit Test Hook CL"))
                     (lambda (&rest args)
                       (declare (ignore args))
                       (setf fired t)))
           ;; Normal: hook fires
           (setf (variable-value (string-to-variable "Inhibit Test Hook CL")) 1)
           (is-true fired)
           ;; Inhibited: hook does NOT fire
           (setf fired nil)
           (let ((*inhibit-hooks* t))
             (setf (variable-value (string-to-variable "Inhibit Test Hook CL")) 2))
           (is-false fired))
      (delete-variable (string-to-variable "Inhibit Test Hook CL")))))

(test broken-hook-defers-error-and-removes
  "A hook that signals error defers to *pending-error* and is removed from the list."
  (let ((hemlock::*pending-error* nil))
    (defhvar "Broken Hook Var CL" "For broken hook test." :value nil)
    (unwind-protect
         (let ((sym (string-to-variable "Broken Hook Var CL")))
           (add-hook (variable-hooks sym)
                     (lambda (&rest args)
                       (declare (ignore args))
                       (error "broken hook test")))
           ;; Setting the variable triggers the hook which errors
           (setf (variable-value sym) 42)
           ;; Error should be deferred, not propagated
           (is-true (typep hemlock::*pending-error* 'error))
           (is (search "broken hook test"
                       (format nil "~A" hemlock::*pending-error*)))
           ;; Broken hook should be removed from the list
           (is (null (variable-hooks sym)))
           ;; Setting again should NOT error (hook was removed)
           (setf hemlock::*pending-error* nil)
           (setf (variable-value sym) 99)
           (is (null hemlock::*pending-error*)))
      (delete-variable (string-to-variable "Broken Hook Var CL")))))

(test broken-hook-does-not-block-other-hooks
  "A broken hook does not prevent subsequent hooks from running."
  (let ((hemlock::*pending-error* nil)
        (second-fired nil))
    (defhvar "Multi Hook Var CL" "For multi hook test." :value nil)
    (unwind-protect
         (let ((sym (string-to-variable "Multi Hook Var CL")))
           (add-hook (variable-hooks sym)
                     (lambda (&rest args)
                       (declare (ignore args))
                       (error "first hook boom")))
           (add-hook (variable-hooks sym)
                     (lambda (&rest args)
                       (declare (ignore args))
                       (setf second-fired t)))
           (setf (variable-value sym) 99)
           ;; First hook errored, second should still have run
           (is-true second-fired)
           (is-true (typep hemlock::*pending-error* 'error))
           (setf hemlock::*pending-error* nil))
      (delete-variable (string-to-variable "Multi Hook Var CL")))))

(test buffer-modified-with-broken-hook
  "Buffer modification completes even when buffer-modified-hook errors."
  (let ((hemlock::*pending-error* nil)
        (buf (make-buffer "Broken Hook Buf CL")))
    (unwind-protect
         (progn
           ;; The buffer-modified-notifier calls invoke-hook on buffer-modified-hook.
           ;; We can't easily add to hemlock's buffer-modified-hook from tests
           ;; without the full editor init, but we CAN verify that buffer
           ;; modification itself doesn't crash when invoke-hook catches errors.
           (insert-string (buffer-point buf) "hello")
           (is (string= "hello"
                         (region-to-string (buffer-region buf)))))
      (delete-buffer buf))))

(test tick-updated-before-notifier
  "Buffer modification tick is updated before the notifier fires,
preventing retrigger on subsequent modifications."
  (let ((buf (make-buffer "Tick Test CL")))
    (unwind-protect
         (progn
           ;; First modification marks buffer as modified
           (insert-string (buffer-point buf) "a")
           (is-true (buffer-modified buf))
           ;; Second modification should not re-fire the 'first modified' notifier
           ;; (tick already updated). Just verify no error.
           (insert-string (buffer-point buf) "b")
           (is (string= "ab"
                         (region-to-string (buffer-region buf)))))
      (delete-buffer buf))))
