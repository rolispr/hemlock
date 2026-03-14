;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;;
;;; This file contains Xref code, for M-. and other commands.
;;;

(in-package :hemlock)



(defvar *xref-entries* nil)
(defvar *xref-entries-end* nil)
;;;

(defstruct (xref-entry
             (:constructor internal-make-xref-entry (name file position)))
  name
  file
  position)

(defun make-xref-entry (alist)
  (let* ((location (cdr (assoc :location (cdr alist))))
         (file (second (assoc :file location)))
         (position (second (assoc :position location))))
    (internal-make-xref-entry (car alist)
                              file
                              position)))

;;; This is the xref buffer if it exists.
;;;
(defvar *xref-buffer* nil)

;;; This is the cleanup method for deleting *xref-buffer*.
;;;
(defun delete-xref-buffers (buffer)
  (when (eq buffer *xref-buffer*)
    (setf *xref-buffer* nil)
    (setf *xref-entries* nil)))


;;;; Commands.

(defmode "Xref" :major-p t
  :documentation
  "Xref lists Lisp definitions.")

(defcommand "Xref Quit" (p)
  "Kill the xref buffer."
  ""
  (declare (ignore p))
  (when *xref-buffer* (delete-buffer-if-possible *xref-buffer*)))

(defcommand "Xref Goto" (p)
  "Change to the entry's buffer."
  "Change to the entry's buffer."
  (declare (ignore p))
  (let ((entry (array-element-from-mark (current-point) *xref-entries*)))
    (when entry
      (change-to-definition entry))))

(defun refresh-xref (buf entries)
  (with-writable-buffer (buf)
    (delete-region (buffer-region buf))
    (setf *xref-entries-end* (length entries))
    (setf *xref-entries* (coerce entries 'vector))
    (with-output-to-mark (s (buffer-point buf))
      (dolist (entry entries)
        (xref-write-line entry s)))))

(defun make-xref-buffer (entries)
  (let ((buf (or *xref-buffer* (make-buffer "*Xref*" :modes '("Xref")))))
    (setf *xref-buffer* buf)
    (refresh-xref buf entries)
    (let ((fields (buffer-modeline-fields *xref-buffer*)))
      (setf (cdr (last fields))
            (list (or (modeline-field :xref-cmds)
                      (make-modeline-field
                       :name :xref-cmds :width 18
                       :function
                       #'(lambda (buffer window)
                           (declare (ignore buffer window))
                           "  Type ? for help.")))))
      (setf (buffer-modeline-fields *xref-buffer*) fields))
    (buffer-start (buffer-point buf))
    (change-to-buffer buf)))

(defun xref-write-line (entry s)
  (format s "  ~A ~40T~A~%"
          (shorten-string 36 (xref-entry-name entry))
          (shorten-string 39 (xref-entry-file entry))))

(defun shorten-string (len str)
  (if (<= (length str) len)
      str
      (concat (subseq str 0 (floor (- len 3) 2))
              "..."
              (subseq str (- (length str) (ceiling (- len 3) 2))))))

(defcommand "Xref Help" (p)
  "Show this help."
  "Show this help."
  (declare (ignore p))
  (describe-mode-command nil "Xref"))


;;; Find Definition

(defun change-to-definition (entry)
  (let ((file (xref-entry-file entry))
        (position (xref-entry-position entry)))
    (when file
      (change-to-buffer (find-file-buffer file))
      (when position
        (buffer-start (current-point))
        (character-offset (current-point) (1- position)))
      t)))

;;; sb-introspect-based definition finding.

(defun %ds-to-xref-alist (name ds)
  "Convert a definition-source to make-xref-entry alist format."
  (let ((path (ignore-errors (sb-introspect:definition-source-pathname ds))))
    (when path
      (let ((truepath (ignore-errors (truename path)))
            (offset (sb-introspect:definition-source-character-offset ds)))
        (when truepath
          (list (princ-to-string name)
                (list :location
                      (list :file (namestring truepath))
                      (list :position (or offset 0)))))))))

(defun %sbcl-find-definitions (sym)
  "Find definitions of SYM using sb-introspect."
  (loop for type in '(:function :generic-function :method :macro :compiler-macro
                      :type :variable :constant :package :method-combination
                      :setf-expander :special-operator)
        nconc (loop for ds in (ignore-errors
                                (sb-introspect:find-definition-sources-by-name sym type))
                    for entry = (%ds-to-xref-alist
                                 (format nil "~(~A~) ~S" type sym) ds)
                    when entry collect entry)))

(defun %sbcl-who-results (pairs)
  "Convert sb-introspect who-* results ((fn . ds) ...) to xref alist format."
  (loop for (fn . ds) in pairs
        for entry = (%ds-to-xref-alist (princ-to-string fn) ds)
        when entry collect entry))

(defun %sbcl-xref-callers (sym)
  (%sbcl-who-results (ignore-errors (sb-introspect:who-calls sym))))
(defun %sbcl-xref-references (sym)
  (%sbcl-who-results (ignore-errors (sb-introspect:who-references sym))))
(defun %sbcl-xref-bindings (sym)
  (%sbcl-who-results (ignore-errors (sb-introspect:who-binds sym))))
(defun %sbcl-xref-sets (sym)
  (%sbcl-who-results (ignore-errors (sb-introspect:who-sets sym))))
(defun %sbcl-xref-macroexpands (sym)
  (%sbcl-who-results (ignore-errors (sb-introspect:who-macroexpands sym))))
(defun %sbcl-xref-specializes (sym)
  (%sbcl-who-results (ignore-errors
                       (append (sb-introspect:who-specializes-directly sym)
                               (sb-introspect:who-specializes-generally sym)))))

(defun %find-definitions (label xref-fun name)
  (let* ((sym (hemlock::resolve-agent-symbol name nil))
         (data (and sym (funcall xref-fun sym))))
    (hemlock::eval-in-master `(%definitions-found ',label ',name ',data))))

(defun %definitions-found (label name data)
  (let ((entries (mapcar #'make-xref-entry data)))
    (cond
     ((null entries)
      (message "No ~A results for: ~A" label name))
     ((null (cdr entries))
      (change-to-definition (car entries)))
     (t
      (make-xref-buffer entries)))))

(defun find-definitions (name)
  (hemlock::eval-in-agent
   `(%find-definitions "definition" '%sbcl-find-definitions ',name)))

(defcommand "Find Definitions" (p)
  "" ""
  (let ((default (hemlock::symbol-string-at-point)))
    ;; Fixme: MARK-SYMBOL isn't very good, meaning that often we
    ;; will get random forms rather than a symbol.  Let's at least
    ;; catch the case where the result is more than a line long,
    ;; and give up.
    (when (find #\newline default)
      (setf default nil))
    (find-definitions
     (hemlock::parse-agent-symbol
      (if (or p (not default))
          (prompt-for-string
           :prompt "Name: "
           :default default)
          default)))))

(macrolet
    ((% (name fun sbcl-fun)
       `(progn
          (defcommand ,name (p)
            "" ""
            (let ((default (hemlock::symbol-string-at-point)))
              (when (find #\newline default)
                (setf default nil))
              (,fun
               (hemlock::parse-agent-symbol
                (if (or p (not default))
                    (prompt-for-string
                     :prompt "Name: "
                     :default default)
                    default)))))
          (defun ,fun (name)
            (hemlock::eval-in-agent
             (list '%find-definitions
                   (list 'quote ',name)
                   (list 'quote ',sbcl-fun)
                   (list 'quote name)))))))
  (% "Who Calls"        who-calls        %sbcl-xref-callers)
  (% "Who References"   who-references   %sbcl-xref-references)
  (% "Who Binds"        who-binds        %sbcl-xref-bindings)
  (% "Who Sets"         who-sets         %sbcl-xref-sets)
  (% "Who Macroexpands" who-macroexpands %sbcl-xref-macroexpands)
  (% "Who Specializes"  who-specializes  %sbcl-xref-specializes))
