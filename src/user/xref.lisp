;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;;    Cross-reference browser: M-., Who Calls, Who References, etc.
;;;

(in-package :hemlock)


;;;; Data model

(defstruct (xref-entry
             (:constructor internal-make-xref-entry (name file position)))
  name
  file
  position)

(defun make-xref-entry (alist)
  (let* ((location (cdr (assoc :location (cdr alist))))
         (file (second (assoc :file location)))
         (position (second (assoc :position location))))
    (internal-make-xref-entry (car alist) file position)))

(defvar *xref-buffer* nil)

(defun delete-xref-buffers (buffer)
  (when (eq buffer *xref-buffer*)
    (setf *xref-buffer* nil)))

(defun shorten-string (len str)
  (if (<= (length str) len)
      str
      (concat (subseq str 0 (floor (- len 3) 2))
              "..."
              (subseq str (- (length str) (ceiling (- len 3) 2))))))


;;;; Display

(defun xref-window ()
  (find-if (lambda (w) (eq (window-buffer w) *xref-buffer*)) *window-list*))

(defun xref-row (entry width)
  "Build a selectable row for ENTRY within WIDTH columns."
  (let* ((name-w (min 38 (floor width 2)))
         (file-w (max 0 (- width name-w 2))))
    (make-ui-selectable
     :data entry
     :child (make-ui-hstack
             :spacing 0
             :children (list
                        (make-ui-box :width name-w :align :left
                                     :child (make-ui-text
                                             :content (shorten-string name-w (xref-entry-name entry))
                                             :face 10))
                        (make-ui-text :content (shorten-string file-w (xref-entry-file entry))
                                      :face 11))))))

(defun xref-render (tree)
  "Render the entry list into the tree's buffer."
  (let* ((state   (ui-tree-state tree))
         (entries (getf state :entries))
         (sel     (getf state :selection 0))
         (n       (length entries))
         (width   (ui-tree-width tree))
         (win     (xref-window))
         (height  (if win (window-height win) 20))
         (offset  (scroll-to-selection sel (getf state :scroll-offset 0) height)))
    (setf (getf (ui-tree-state tree) :scroll-offset) offset)
    (let ((rows nil))
      (loop for i from offset below (min (+ offset height) n)
            for entry = (nth i entries)
            do (let ((row (xref-row entry width)))
                 (setf (ui-selectable-selectedp row) (= i sel))
                 (push row rows)))
      (setf (ui-tree-root tree) (make-ui-vstack :children (nreverse rows))))
    (with-writable-buffer ((ui-tree-buffer tree))
      (render-tree tree))
    (when win
      (move-mark (window-display-start win)
                 (region-start (buffer-region (ui-tree-buffer tree)))))))

(defun make-xref-buffer (entries)
  "Display ENTRIES in *Xref*, creating the buffer if necessary."
  (let ((buf (or *xref-buffer*
                 (make-buffer "*Xref*" :modes '("Xref")
                              :delete-hook '(delete-xref-buffers)))))
    (setf *xref-buffer* buf)
    (when (buffer-ui-tree buf)
      (uninstall-tree (buffer-ui-tree buf)))
    (let* ((win   (or (xref-window) (car *window-list*)))
           (width (if win (window-width win) 80))
           (tree  (make-ui-tree
                   :buffer buf
                   :width width
                   :state (list :entries entries
                                :selection 0
                                :scroll-offset 0))))
      (install-tree tree)
      (setf (buffer-writable buf) nil)
      (xref-render tree))
    (change-to-buffer buf)))


;;;; Commands

(defmode "Xref" :major-p t
  :documentation
  "Xref lists cross-reference results.")

(defcommand "Xref Quit" (p)
  "Kill the xref buffer."
  ""
  (declare (ignore p))
  (when *xref-buffer* (delete-buffer-if-possible *xref-buffer*)))

(defcommand "Xref Goto" (p)
  "Visit the definition under the cursor."
  "Visit the definition under the cursor."
  (declare (ignore p))
  (let ((tree (and *xref-buffer* (buffer-ui-tree *xref-buffer*))))
    (when tree
      (let* ((state   (ui-tree-state tree))
             (entries (getf state :entries))
             (sel     (getf state :selection 0)))
        (when (< -1 sel (length entries))
          (change-to-definition (nth sel entries)))))))

(defcommand "Xref Next" (p)
  "Move selection to the next entry."
  "Move selection to the next entry."
  (declare (ignore p))
  (let ((tree (and *xref-buffer* (buffer-ui-tree *xref-buffer*))))
    (when tree
      (let* ((state (ui-tree-state tree))
             (n     (length (getf state :entries)))
             (sel   (getf state :selection 0)))
        (setf (getf (ui-tree-state tree) :selection) (min (1- n) (1+ sel)))
        (xref-render tree)))))

(defcommand "Xref Previous" (p)
  "Move selection to the previous entry."
  "Move selection to the previous entry."
  (declare (ignore p))
  (let ((tree (and *xref-buffer* (buffer-ui-tree *xref-buffer*))))
    (when tree
      (let* ((state (ui-tree-state tree))
             (sel   (getf state :selection 0)))
        (setf (getf (ui-tree-state tree) :selection) (max 0 (1- sel)))
        (xref-render tree)))))

(defcommand "Xref Help" (p)
  "Show this help."
  "Show this help."
  (declare (ignore p))
  (describe-mode-command nil "Xref"))


;;;; Find Definition

(defun change-to-definition (entry)
  (let ((file (xref-entry-file entry))
        (position (xref-entry-position entry)))
    (when file
      (change-to-buffer (find-file-buffer file))
      (when position
        (buffer-start (current-point))
        (character-offset (current-point) (1- position)))
      t)))


;;;; sb-introspect backend

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
    (when (find #\newline default)
      (setf default nil))
    (find-definitions
     (hemlock::parse-agent-symbol
      (if (or p (not default))
          (prompt :string
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
                    (prompt :string
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
