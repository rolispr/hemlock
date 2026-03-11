;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; Hemlock introspection layer — SBCL backend.
;;;
;;; Replaces the conium dependency with direct SBCL internal calls.
;;; Ported from the SLY slynk SBCL backend (public domain).
;;; SBCL-only; hemlock is SBCL-only in practice.
;;;

(defpackage :hemlock.introspect
  (:use :common-lisp)
  (:export
   #:getpid
   #:call-with-debugging-environment
   #:compute-backtrace
   #:print-frame
   #:find-definitions
   #:find-source-location
   #:who-calls
   #:who-binds
   #:who-sets
   #:who-references
   #:who-macroexpands
   #:who-specializes
   #:describe-symbol-for-emacs
   #:type-specifier-arglist))

(in-package :hemlock.introspect)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-introspect)
  (require :sb-posix))


;;;; Process

(defun getpid ()
  "Return the Unix process ID of this image."
  (sb-posix:getpid))


;;;; Debugger

(defvar *hemlock-db-stack-top* nil
  "Top of the stack when the debugger was entered. Bound by
   call-with-debugging-environment.")

(defun call-with-debugging-environment (debugger-loop-fn)
  "Call DEBUGGER-LOOP-FN with the stack captured at the current point.
   Establishes the dynamic context needed for compute-backtrace and
   print-frame to work correctly."
  (declare (type function debugger-loop-fn))
  (let ((*hemlock-db-stack-top*
          (or sb-debug:*stack-top-hint*
              (sb-di:top-frame)))
        (sb-debug:*stack-top-hint* nil))
    (handler-bind ((sb-di:debug-condition
                     (lambda (condition)
                       (signal condition))))
      (funcall debugger-loop-fn))))

(defun nth-frame (index)
  (loop for frame = *hemlock-db-stack-top* then (sb-di:frame-down frame)
        for i from index downto 1
        finally (return frame)))

(defun compute-backtrace (start end)
  "Return a list of frames from START to END (or end of stack if END is nil).
   Must be called within call-with-debugging-environment."
  (let ((end (or end most-positive-fixnum)))
    (loop for f = (nth-frame start) then (sb-di:frame-down f)
          for i from start below end
          while f collect f)))

(defun print-frame (frame stream)
  "Print a human-readable description of FRAME to STREAM."
  (sb-debug::print-frame-call frame stream
                               :allow-other-keys t
                               :emergency-best-effort t))


;;;; Definition finding

;;; Maps sb-introspect definition type keywords to human-readable def forms.
(defparameter *definition-types*
  '(:variable             defvar
    :constant             defconstant
    :type                 deftype
    :symbol-macro         define-symbol-macro
    :macro                defmacro
    :compiler-macro       define-compiler-macro
    :function             defun
    :generic-function     defgeneric
    :method               defmethod
    :setf-expander        define-setf-expander
    :structure            defstruct
    :condition            define-condition
    :class                defclass
    :method-combination   define-method-combination
    :package              defpackage))

(defun definition-source->location (defsrc type name)
  "Convert an sb-introspect:definition-source to a hemlock location list.
   Returns (:location (:file path) (:position charpos)) or (:error msg)."
  (let ((pathname  (sb-introspect:definition-source-pathname defsrc))
        (char-offset (sb-introspect:definition-source-character-offset defsrc))
        (form-path (sb-introspect:definition-source-form-path defsrc)))
    (declare (ignore form-path))
    (cond
      ((not pathname)
       `(:error ,(format nil "No source location for ~A ~A" type name)))
      ((not (probe-file pathname))
       `(:error ,(format nil "Source file not found: ~A" pathname)))
      (t
       `(:location
         (:file ,(namestring (truename pathname)))
         (:position ,(or char-offset 0)))))))

(defun find-definitions (name)
  "Return a list of ((dspec location) ...) for all definitions of NAME.
   NAME is a symbol. dspec is e.g. (defun foo), location is a hemlock
   location list."
  (loop for type in *definition-types* by #'cddr
        for def-form = (getf *definition-types* type)
        for defsrcs = (ignore-errors
                        (sb-introspect:find-definition-sources-by-name name type))
        append (loop for defsrc in defsrcs
                     collect (list (list def-form name)
                                   (definition-source->location defsrc type name)))))

(defun find-source-location (object)
  "Return a hemlock location list for the source of OBJECT (a function,
   class, method, etc.)."
  (handler-case
      (let ((defsrc (sb-introspect:find-definition-source object)))
        (definition-source->location defsrc (type-of object) object))
    (error (e)
      `(:error ,(princ-to-string e)))))


;;;; Cross-references

;;; xref results from sb-introspect are (name . definition-source) pairs.
;;; We convert them to ((name location) ...) for hemlock.

(defun xref-results (xref-data)
  "Convert sb-introspect xref results to hemlock location lists."
  (loop for (name . defsrc) in xref-data
        collect (list name (definition-source->location defsrc :function name))))

(defun sanitize-xrefs (xrefs)
  "Remove duplicates and internal compiler functions from xref results."
  (remove-duplicates
   (remove-if (lambda (entry)
                (let ((name (car entry)))
                  (or (null name)
                      (and (consp name)
                           (member (car name)
                                   '(sb-pcl::fast-method sb-pcl::slow-method
                                     sb-pcl::method))))))
              xrefs)
   :test (lambda (a b) (equal (car a) (car b)))))

(defmacro defxref (name &optional sb-name)
  (let ((fn (intern (symbol-name (or sb-name name)) :sb-introspect)))
    `(defun ,name (what)
       (sanitize-xrefs
        (xref-results
         (,fn what))))))

(defxref who-calls)
(defxref who-binds)
(defxref who-sets)
(defxref who-references)
(defxref who-macroexpands)
#+#.(cl:if (cl:find-symbol "WHO-SPECIALIZES-DIRECTLY" "SB-INTROSPECT") '(:and) '(:or))
(defxref who-specializes who-specializes-directly)
#-#.(cl:if (cl:find-symbol "WHO-SPECIALIZES-DIRECTLY" "SB-INTROSPECT") '(:and) '(:or))
(defun who-specializes (what)
  (declare (ignore what))
  nil)


;;;; Symbol documentation

(defun describe-symbol-for-emacs (symbol)
  "Return a plist describing SYMBOL for display in apropos/documentation buffers.
   Keys: :variable :function :macro :generic-function :special-operator
         :setf :type :class"
  (let ((result '()))
    (flet ((doc (kind)
             (or (documentation symbol kind) :not-documented))
           (push-prop (key value)
             (when value
               (setf result (list* key value result)))))
      (push-prop :variable
                 (multiple-value-bind (kind recorded-p)
                     (sb-int:info :variable :kind symbol)
                   (declare (ignore kind))
                   (when (or (boundp symbol) recorded-p)
                     (doc 'variable))))
      (when (fboundp symbol)
        (push-prop
         (cond ((macro-function symbol)     :macro)
               ((special-operator-p symbol) :special-operator)
               ((typep (fdefinition symbol) 'generic-function) :generic-function)
               (t :function))
         (doc 'function)))
      (push-prop :setf
                 (when (sb-int:info :setf :expander symbol)
                   (doc 'setf)))
      (push-prop :type
                 (when (sb-int:info :type :kind symbol)
                   (doc 'type)))
      result)))


;;;; Type specifier arglists
;;;
;;; Static table from the CLHS. Conium included this same table.

(defparameter *type-specifier-arglists*
  '((and              . (&rest type-specifiers))
    (array            . (&optional element-type dimension-spec))
    (base-string      . (&optional size))
    (bit-vector       . (&optional size))
    (complex          . (&optional type-specifier))
    (cons             . (&optional car-typespec cdr-typespec))
    (double-float     . (&optional lower-limit upper-limit))
    (eql              . (object))
    (float            . (&optional lower-limit upper-limit))
    (function         . (&optional arg-typespec value-typespec))
    (integer          . (&optional lower-limit upper-limit))
    (long-float       . (&optional lower-limit upper-limit))
    (member           . (&rest eql-objects))
    (mod              . (n))
    (not              . (type-specifier))
    (or               . (&rest type-specifiers))
    (rational         . (&optional lower-limit upper-limit))
    (real             . (&optional lower-limit upper-limit))
    (satisfies        . (predicate-symbol))
    (short-float      . (&optional lower-limit upper-limit))
    (signed-byte      . (&optional size))
    (simple-array     . (&optional element-type dimension-spec))
    (simple-base-string . (&optional size))
    (simple-bit-vector . (&optional size))
    (simple-string    . (&optional size))
    (single-float     . (&optional lower-limit upper-limit))
    (simple-vector    . (&optional size))
    (string           . (&optional size))
    (unsigned-byte    . (&optional size))
    (values           . (&rest typespecs))
    (vector           . (&optional element-type size))))

(defun type-specifier-arglist (typespec-operator)
  "Return the argument list for a type specifier operator, or :not-available."
  (typecase typespec-operator
    (symbol (or (cdr (assoc typespec-operator *type-specifier-arglists*))
                :not-available))
    (t :not-available)))
