;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; Echo area completion display via vnode ui-grid.
;;;
;;; Input is state in the tree.  Buffer is output only (buffer-writable=nil).
;;; Uses input operations from src/ui/input.lisp.
;;;

(in-package :hemlock.command)


;;;; Candidate generation

(defun file-list-directory (dir)
  "Return namestrings for all files and subdirectories in DIR."
  (let ((entries nil))
    (dolist (d (uiop:subdirectories dir))
      (push (namestring d) entries))
    (dolist (f (uiop:directory-files dir))
      (push (namestring f) entries))
    (nreverse entries)))

(defun glob-match-p (pattern name)
  "Return T if NAME matches shell-style glob PATTERN (case-insensitive).
* matches any sequence of characters including empty."
  (let ((p (string-downcase pattern))
        (n (string-downcase name)))
    (labels ((match (ip in)
               (cond
                 ((= ip (length p)) (= in (length n)))
                 ((char= (char p ip) #\*)
                  (loop for k from in to (length n)
                        thereis (match (1+ ip) k)))
                 ((= in (length n)) nil)
                 ((char= (char p ip) (char n in)) (match (1+ ip) (1+ in)))
                 (t nil))))
      (match 0 0))))

(defun file-completion-filter (input)
  "Return file candidates matching INPUT dynamically from the filesystem.
When INPUT ends with /, list that directory's contents.
Otherwise, list the parent directory filtered by the name prefix.
The prefix may contain * wildcards for glob-style matching."
  (when (plusp (length input))
    (let* ((ends-with-slash (char= (char input (1- (length input))) #\/))
           (merged (merge-pathnames input
                     (or (and (prompt-default)
                              (directory-namestring (prompt-default)))
                         (default-directory))))
           (namestring (namestring merged)))
      (cond
        ;; Explicit trailing slash — user wants to descend into this directory.
        ;; Check the filesystem only in this case (not for bare names).
        (ends-with-slash
         (when (uiop:directory-exists-p namestring)
           (file-list-directory namestring)))
        ;; Partial name or glob — list parent dir, filter by prefix.
        ;; Uses glob-match-p when the prefix contains *.
        (t
         (let* ((dir (directory-namestring namestring))
                (prefix (file-namestring namestring))
                (all (file-list-directory dir)))
           (when (plusp (length prefix))
             (let ((p (string-downcase prefix)))
               (setf all (remove-if-not
                          (lambda (f)
                            (let ((name (file-display-name f)))
                              (if (find #\* p)
                                  (glob-match-p p name)
                                  (and (>= (length name) (length p))
                                       (string-equal name p
                                                     :end1 (length p))))))
                          all))))
           all))))))

(defun completion-candidates ()
  "Return the initial candidate list for the current parse type."
  (cond
    ((eq (prompt-type) :file)
     (file-completion-filter (or (prompt-default-string) (prompt-default) "")))
    ((eq (prompt-type) :symbol)
     (let ((matches (hemlock::fuzzy-completions
                     (or (prompt-default-string) "")
                     (or (prompt-symbol-package) "cl"))))
       (mapcar #'first (first matches))))
    (t
     (hemlock::string-table-candidates (prompt-tables)))))

(defun file-display-name (namestring)
  "Return a short display name for a file path.
For directories, returns the last component with a trailing /.
For files, returns the filename."
  (let ((fn (file-namestring namestring)))
    (if (plusp (length fn))
        fn
        ;; directory — extract last component
        (let* ((s (string-right-trim "/" namestring))
               (slash (position #\/ s :from-end t)))
          (if slash
              (concatenate 'string (subseq s (1+ slash)) "/")
              (concatenate 'string s "/"))))))

(defun completion-display-name (candidate)
  "Return the display name for a completion candidate."
  (if (eq (prompt-type) :file)
      (file-display-name candidate)
      candidate))


;;;; Lifecycle

(defun make-echo-completions ()
  "Create a completion display for the echo area."
  (let* ((width (window-width *echo-area-window*))
         (height (window-height *echo-area-window*))
         (all (completion-candidates))
         (w0 (floor width 2))
         (w1 (floor width 4))
         (w2 (- width w0 w1 1))
         (default (or (prompt-default-string) (prompt-default) "")))
    (let ((tree (make-ui-tree
                 :buffer *echo-area-buffer*
                 :width width
                 :state (list :candidates all
                              :filtered all
                              :selection -1
                              :scroll-offset 0
                              :input default
                              :cursor-offset (length default)
                              :message ""
                              :prompt (or (prompt-text) "")
                              :height height
                              :col-widths (list w0 w1 w2)))))
      (install-tree tree)
      (setf (buffer-writable *echo-area-buffer*) nil)
      (echo-render tree)
      tree)))

(defun cleanup-echo-completions ()
  "Remove completion display, restore buffer."
  (when (buffer-ui-tree *echo-area-buffer*)
    (setf (buffer-writable *echo-area-buffer*) t)
    (uninstall-tree (buffer-ui-tree *echo-area-buffer*))
    (let ((*tree-rendering* t))
      (let ((r (buffer-region *echo-area-buffer*)))
        (when (regionp r)
          (delete-region r))))))



;;;; Symbol chunk highlighting

(defun symbol-chunk-nodes (str chunks)
  "Build a list of ui-text nodes for STR with CHUNKS highlighted in face 14.
CHUNKS is a list of (offset substring) pairs from fuzzy-completions."
  (if (null chunks)
      (list (make-ui-text :content str))
      (let ((nodes nil)
            (pos 0))
        (dolist (chunk chunks)
          (let ((start (first chunk))
                (sub   (second chunk)))
            (when (< pos start)
              (push (make-ui-text :content (subseq str pos start)) nodes))
            (push (make-ui-text :content sub :face 14) nodes)
            (setf pos (+ start (length sub)))))
        (when (< pos (length str))
          (push (make-ui-text :content (subseq str pos)) nodes))
        (nreverse nodes))))


;;;; Grid building

(defun build-completion-grid (tree)
  "Build a completion grid from tree state."
  (let* ((state (ui-tree-state tree))
         (prompt (getf state :prompt ""))
         (input (input-string tree))
         (all (getf state :candidates))
         (filtered
          (cond
            ((eq (prompt-type) :file)
             (file-completion-filter input))
            ((eq (prompt-type) :symbol)
             (let* ((raw (hemlock::fuzzy-completions
                          input (or (prompt-symbol-package) "cl")))
                    (matches (first raw)))
               (setf (getf (ui-tree-state tree) :symbol-matches) matches)
               (mapcar #'first matches)))
            (t
             (hemlock::filter-completions input all))))
         (sel (getf state :selection -1))
         (msg (getf state :message ""))
         (height (getf state :height 3))
         (col-widths (getf state :col-widths))
         (n (length filtered))
         (status (if (plusp n)
                     (format nil "~D ~:*~[matches~;match~:;matches~]" n)
                     "no matches"))
         (rows nil))
    (setf (getf (ui-tree-state tree) :filtered) filtered)
    (when (and (plusp n) (>= sel n))
      (setf sel (1- n)
            (getf (ui-tree-state tree) :selection) sel))
    (let* ((max-visible (1- height))
           (offset (scroll-to-selection sel
                                        (getf state :scroll-offset 0)
                                        max-visible)))
      (setf (getf (ui-tree-state tree) :scroll-offset) offset)
      ;; row 0 — scrolling input viewport so cursor never leaves col 0
      (let* ((w0 (first col-widths))
             (avail (max 0 (- w0 (length prompt))))
             (off (getf state :cursor-offset 0))
             (view-start (max 0 (min off (- (length input) avail))))
             (visible-input (subseq input view-start
                                    (min (length input) (+ view-start avail)))))
        (setf (getf (ui-tree-state tree) :view-start) view-start)
        (push (list (make-ui-text :content (concatenate 'string prompt visible-input))
                    (make-ui-text :content status)
                    (make-ui-text :content msg))
              rows))
      ;; rows 1..N
      (loop for row from 1 below height
            for idx = (+ offset (1- row))
            do (if (< idx n)
                   (if (eq (prompt-type) :symbol)
                       (let* ((match    (nth idx (getf (ui-tree-state tree) :symbol-matches)))
                              (completed (first match))
                              (chunks   (third match))
                              (class    (fourth match))
                              (selected (= idx sel))
                              (prefix   (make-ui-text :content (if selected "> " "  ")))
                              (name-nodes (symbol-chunk-nodes completed chunks))
                              (col1     (make-ui-hstack :spacing 0
                                                        :children (cons prefix name-nodes)))
                              (class-w  (max 0 (1- (second col-widths))))
                              (class-s  (or class "")))
                         (push (list col1
                                     (make-ui-text
                                      :content (if (> (length class-s) class-w)
                                                   (subseq class-s 0 class-w)
                                                   class-s)
                                      :face 11)
                                     (make-ui-text :content ""))
                               rows))
                       (let* ((cand (nth idx filtered))
                              (display (completion-display-name cand))
                              (selected (= idx sel))
                              (prefix (if selected "> " "  ")))
                         (push (list (make-ui-text
                                      :content (concatenate 'string prefix display))
                                     (make-ui-text :content "")
                                     (make-ui-text :content ""))
                               rows)))
                   (push (list (make-ui-text :content "")
                               (make-ui-text :content "")
                               (make-ui-text :content ""))
                         rows))))
    (make-ui-grid :cells (nreverse rows) :col-widths col-widths)))


;;;; Render

(defun echo-render (tree)
  "Build grid, render, place cursor."
  (let* ((grid (build-completion-grid tree))
         (region (buffer-region (ui-tree-buffer tree)))
         (was-writable (buffer-writable (ui-tree-buffer tree))))
    (setf (buffer-writable (ui-tree-buffer tree)) t)
    (setf (ui-tree-root tree) grid)
    (render-tree tree)
    ;; cursor — account for scrolled viewport
    (let* ((prompt (getf (ui-tree-state tree) :prompt ""))
           (off (cursor-offset tree))
           (view-start (getf (ui-tree-state tree) :view-start 0))
           (line (mark-line (region-start region)))
           (pos (min (+ (length prompt) (- off view-start)) (line-length line))))
      (move-mark (buffer-point *echo-area-buffer*) (mark line pos)))
    ;; pin display
    (move-mark (window-display-start *echo-area-window*)
               (region-start region))
    (setf (buffer-writable (ui-tree-buffer tree)) was-writable)))

(defun echo-refilter (tree)
  "Reset selection and scroll, then render."
  (setf (getf (ui-tree-state tree) :selection) -1
        (getf (ui-tree-state tree) :scroll-offset) 0
        (getf (ui-tree-state tree) :message) "")
  (echo-render tree))


;;;; Echo-specific operations (delegate to ui/input.lisp + re-render)

(defun echo-type-char (char)
  (when (buffer-ui-tree *echo-area-buffer*)
    (type-char-at-cursor (buffer-ui-tree *echo-area-buffer*) char)
    (echo-refilter (buffer-ui-tree *echo-area-buffer*))))

(defun echo-delete-char ()
  (when (buffer-ui-tree *echo-area-buffer*)
    (let* ((tree (buffer-ui-tree *echo-area-buffer*))
           (did-something
            (if (and (eq (prompt-type) :file)
                     (let ((off (cursor-offset tree)))
                       (and (plusp off)
                            (char= (char (input-string tree) (1- off)) #\/))))
                (progn (kill-word-before-cursor tree #\/) t)
                (delete-char-before-cursor tree))))
      (when did-something
        (echo-refilter tree)))))

(defun echo-kill ()
  (when (buffer-ui-tree *echo-area-buffer*)
    (kill-input (buffer-ui-tree *echo-area-buffer*))
    (echo-refilter (buffer-ui-tree *echo-area-buffer*))))

(defun echo-kill-end ()
  (when (buffer-ui-tree *echo-area-buffer*)
    (kill-to-end (buffer-ui-tree *echo-area-buffer*))
    (echo-refilter (buffer-ui-tree *echo-area-buffer*))))

(defun echo-kill-word ()
  (when (buffer-ui-tree *echo-area-buffer*)
    (kill-word-before-cursor (buffer-ui-tree *echo-area-buffer*)
                             (if (eq (prompt-type) :file) #\/ #\space))
    (echo-refilter (buffer-ui-tree *echo-area-buffer*))))

(defun echo-set-input (text)
  (when (buffer-ui-tree *echo-area-buffer*)
    (set-input (buffer-ui-tree *echo-area-buffer*) text)
    (echo-refilter (buffer-ui-tree *echo-area-buffer*))))

(defun echo-select (delta)
  (when (buffer-ui-tree *echo-area-buffer*)
    (let* ((filtered (getf (ui-tree-state (buffer-ui-tree *echo-area-buffer*)) :filtered))
           (n (length filtered))
           (sel (getf (ui-tree-state (buffer-ui-tree *echo-area-buffer*)) :selection -1)))
      (when (zerop n) (return-from echo-select))
      (setf sel (if (minusp sel)
                    (if (plusp delta) 0 (1- n))
                    (mod (+ sel delta) n)))
      (setf (getf (ui-tree-state (buffer-ui-tree *echo-area-buffer*)) :selection) sel
            (getf (ui-tree-state (buffer-ui-tree *echo-area-buffer*)) :message) "")
      (echo-render (buffer-ui-tree *echo-area-buffer*)))))

(defun echo-move-cursor (delta)
  (when (buffer-ui-tree *echo-area-buffer*)
    (move-cursor (buffer-ui-tree *echo-area-buffer*) delta)
    (echo-render (buffer-ui-tree *echo-area-buffer*))))

(defun echo-cursor-start ()
  (when (buffer-ui-tree *echo-area-buffer*)
    (cursor-to-start (buffer-ui-tree *echo-area-buffer*))
    (echo-render (buffer-ui-tree *echo-area-buffer*))))

(defun echo-cursor-end ()
  (when (buffer-ui-tree *echo-area-buffer*)
    (cursor-to-end (buffer-ui-tree *echo-area-buffer*))
    (echo-render (buffer-ui-tree *echo-area-buffer*))))

(defun echo-confirm ()
  (when (buffer-ui-tree *echo-area-buffer*)
    (confirm-input (buffer-ui-tree *echo-area-buffer*))))

(defun echo-backward-word ()
  (when (buffer-ui-tree *echo-area-buffer*)
    (move-cursor-backward-word (buffer-ui-tree *echo-area-buffer*)
                               (if (eq (prompt-type) :file) #\/ #\space))
    (echo-render (buffer-ui-tree *echo-area-buffer*))))

(defun echo-set-message (text)
  (when (buffer-ui-tree *echo-area-buffer*)
    (setf (getf (ui-tree-state (buffer-ui-tree *echo-area-buffer*)) :message) (or text ""))
    (echo-render (buffer-ui-tree *echo-area-buffer*))))
