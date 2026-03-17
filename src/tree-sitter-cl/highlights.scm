;; Common Lisp highlights query for Lem editor
;; Uses tree-sitter-commonlisp grammar by cxxxr
;; Host-side reclassification handles special form / keyword differentiation

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Comments ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment) @comment
(block_comment) @comment

;; #_ discard - effectively comments out the next form
(dis_expr) @comment

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Literals ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Strings
(str_lit) @string
(path_lit) @string

;; Characters (#\a, #\Space, etc.)
(char_lit) @character

;; Pipe-delimited symbols (|foo bar|)
(fancy_literal) @string

;; Numbers
(num_lit) @number
(complex_num_lit) @number

;; nil
(nil_lit) @constant.builtin

;; Keyword symbols (:foo, ::bar)
(kwd_lit) @constant

;; Self-referential reader macros (#1= #1#)
(self_referential_reader_macro) @constant

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function Definitions ;;;;;;;;;;;;;;;;;;;;;;;;;;

;; defun, defmacro, defgeneric, defmethod, lambda
(defun_keyword) @keyword

;; Function name
(defun_header
  function_name: (sym_lit) @function)
(defun_header
  function_name: (package_lit) @function)

;; Method qualifier (:before, :after, :around)
(defun_header
  specifier: (kwd_lit) @keyword)
(defun_header
  specifier: (sym_lit) @keyword)

;; Lambda list - top-level parameters
(defun_header
  lambda_list: (list_lit (sym_lit) @variable.parameter))

;; Lambda list - nested parameters (defmethod specializers, &key defaults)
(defun_header
  lambda_list: (list_lit (list_lit . (sym_lit) @variable.parameter)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function References ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; #'function-name
(var_quoting_lit
  value: (sym_lit) @function)
(var_quoting_lit
  value: (package_lit) @function)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Loop Macro ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Loop keywords (loop, for, do, while, when, with, into, finally, etc.)
(loop_keyword) @keyword

;; Loop clause words (in, across, from, to, by, then, etc.)
(for_clause_word) @keyword

;; Loop accumulation verbs (collect, sum, maximize, etc.)
(accumulation_verb) @keyword

;; Loop iteration variables
(for_clause
  variable: (sym_lit) @variable.parameter)

;; Loop iteration - destructuring variables
(for_clause
  variable: (list_lit (sym_lit) @variable.parameter))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Quoted Symbols ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 'symbol - quoted symbol reference
(quoting_lit
  value: (sym_lit) @constant)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function Calls ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Generic function/macro calls - first element of any list
;; The host-side reclassifier overrides this for special forms
(list_lit . (sym_lit) @function.call)
(list_lit . (package_lit) @function.call)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Non-head Symbols ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Symbols inside lists that aren't the head (for t, variable refs, etc.)
;; Lowest priority for list children - earlier patterns win
;; Reclassifier promotes t/T to constant.builtin
(list_lit (sym_lit) @variable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Format Specifiers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ~A, ~D, ~{~}, etc. inside strings
(format_specifier) @string.escape

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Reader Macros ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; #+feature / #-feature conditional compilation
(include_reader_macro) @keyword

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Package-qualified Symbols ;;;;;;;;;;;;;;;;;;;;;

;; Highlight the package part distinctly
(package_lit
  package: (sym_lit) @type)
