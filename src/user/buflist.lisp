;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;;    Buffer browser (Bufed).
;;;

(in-package :hemlock)


;;;; Data model

(defvar *bufed-buffer* nil)

(defun delete-bufed-buffers (buffer)
  (when (eq buffer *bufed-buffer*)
    (setf *bufed-buffer* nil)))

(defun bufed-buffer-list ()
  "Return the list of buffers to show, excluding internal ones."
  (remove-if (lambda (b)
               (or (eq b *echo-area-buffer*)
                   (eq b *bufed-buffer*)))
             *buffer-list*))

(defun delete-bufed-buffer (buf)
  "Save BUF if modified and has a pathname, then delete it."
  (when (and (buffer-modified buf)
             (buffer-pathname buf)
             (prompt :y-or-n :prompt (list "~A is modified.  Save it first? "
                                          (buffer-name buf))))
    (save-file-command nil buf))
  (delete-buffer-if-possible buf))


;;;; Display

(defun bufed-window ()
  (find-if (lambda (w) (eq (window-buffer w) *bufed-buffer*)) *window-list*))

(defun bufed-row (buf deleted-set width)
  "Build a selectable row for BUF within WIDTH columns."
  (let* ((deletedp  (member buf deleted-set :test #'eq))
         (modifiedp (buffer-modified buf))
         (name      (buffer-name buf))
         (mode      (buffer-major-mode buf))
         (file      (let ((p (buffer-pathname buf)))
                      (if p (file-namestring p) "")))
         (name-w    (max 12 (floor width 3)))
         (mode-w    12)
         (file-w    (max 0 (- width 2 name-w mode-w)))
         (name-face (cond (deletedp  12)
                          (modifiedp 12)
                          (t         10)))
         (mod-mark  (cond (deletedp  "D")
                          (modifiedp "*")
                          (t         " "))))
    (make-ui-selectable
     :data buf
     :child (make-ui-hstack
             :spacing 0
             :children (list
                        (make-ui-box :width 2 :align :left
                                     :child (make-ui-text
                                             :content mod-mark
                                             :face (when (or deletedp modifiedp) 12)))
                        (make-ui-box :width name-w :align :left
                                     :child (make-ui-text
                                             :content (shorten-string name-w name)
                                             :face name-face))
                        (make-ui-box :width mode-w :align :left
                                     :child (make-ui-text
                                             :content (shorten-string mode-w (or mode ""))
                                             :face 11))
                        (make-ui-text :content (shorten-string file-w file)
                                      :face 11))))))

(defun bufed-render (tree)
  "Render the buffer list into the tree's buffer."
  (let* ((state    (ui-tree-state tree))
         (buffers  (getf state :buffers))
         (deleted  (getf state :deleted))
         (sel      (getf state :selection 0))
         (n        (length buffers))
         (width    (ui-tree-width tree))
         (win      (bufed-window))
         (height   (if win (window-height win) 20))
         (offset   (scroll-to-selection sel (getf state :scroll-offset 0) height)))
    (setf (getf (ui-tree-state tree) :scroll-offset) offset)
    (let ((rows nil))
      (loop for i from offset below (min (+ offset height) n)
            for buf = (nth i buffers)
            do (let ((row (bufed-row buf deleted width)))
                 (setf (ui-selectable-selectedp row) (= i sel))
                 (push row rows)))
      (setf (ui-tree-root tree) (make-ui-vstack :children (nreverse rows))))
    (with-writable-buffer ((ui-tree-buffer tree))
      (render-tree tree))
    (when win
      (move-mark (window-display-start win)
                 (region-start (buffer-region (ui-tree-buffer tree)))))))

(defun bufed-open-or-refresh ()
  "Open the Bufed buffer, or refresh it if already open."
  (let ((buf (or *bufed-buffer*
                 (make-buffer "Bufed" :modes '("Bufed")
                              :delete-hook '(delete-bufed-buffers)))))
    (setf *bufed-buffer* buf)
    (when (buffer-ui-tree buf)
      (uninstall-tree (buffer-ui-tree buf)))
    (let* ((win   (or (bufed-window) (car *window-list*)))
           (width (if win (window-width win) 80))
           (tree  (make-ui-tree
                   :buffer buf
                   :width width
                   :state (list :buffers  (bufed-buffer-list)
                                :deleted  nil
                                :selection 0
                                :scroll-offset 0))))
      (install-tree tree)
      (setf (buffer-writable buf) nil)
      (bufed-render tree))
    (change-to-buffer buf)))

(defun expunge-bufed-buffers ()
  "Delete all buffers marked for deletion in the Bufed buffer."
  (unless (eq *bufed-buffer* (current-buffer))
    (editor-error "Not in the Bufed buffer."))
  (let ((tree (buffer-ui-tree *bufed-buffer*)))
    (when tree
      (let ((marked (getf (ui-tree-state tree) :deleted)))
        (when (and marked
                   (or (not (value bufed-delete-confirm))
                       (prompt :y-or-n :prompt "Delete buffers? "
                                      :default t :must-exist t :default-string "Y")))
          (dolist (b marked) (delete-bufed-buffer b))
          (bufed-open-or-refresh))))))


;;;; Commands

(defmode "Bufed" :major-p t
  :documentation
  "Bufed lists open buffers and supports deletion and navigation.")

(defhvar "Virtual Buffer Deletion"
  "When set, \"Bufed Delete\" marks a buffer for deletion instead of
   immediately deleting it."
  :value t)

(defhvar "Bufed Delete Confirm"
  "When set, \"Bufed\" commands that actually delete buffers ask for
   confirmation before taking action."
  :value t)

(defcommand "Bufed" (p)
  "Open the buffer browser, refreshing it if already open."
  "Open the buffer browser, refreshing it if already open."
  (declare (ignore p))
  (bufed-open-or-refresh))

(defcommand "Bufed Refresh" (p)
  "Refresh the buffer list."
  "Refresh the buffer list."
  (declare (ignore p))
  (bufed-open-or-refresh))

(defcommand "Bufed Next" (p)
  "Move selection to the next buffer."
  "Move selection to the next buffer."
  (declare (ignore p))
  (let ((tree (and *bufed-buffer* (buffer-ui-tree *bufed-buffer*))))
    (when tree
      (let* ((state (ui-tree-state tree))
             (n     (length (getf state :buffers)))
             (sel   (getf state :selection 0)))
        (setf (getf (ui-tree-state tree) :selection) (min (1- n) (1+ sel)))
        (bufed-render tree)))))

(defcommand "Bufed Previous" (p)
  "Move selection to the previous buffer."
  "Move selection to the previous buffer."
  (declare (ignore p))
  (let ((tree (and *bufed-buffer* (buffer-ui-tree *bufed-buffer*))))
    (when tree
      (let* ((state (ui-tree-state tree))
             (sel   (getf state :selection 0)))
        (setf (getf (ui-tree-state tree) :selection) (max 0 (1- sel)))
        (bufed-render tree)))))

(defun bufed-selected (tree)
  "Return the selected buffer, or NIL."
  (let* ((state   (ui-tree-state tree))
         (buffers (getf state :buffers))
         (sel     (getf state :selection 0)))
    (when (< -1 sel (length buffers))
      (nth sel buffers))))

(defcommand "Bufed Delete" (p)
  "Mark the selected buffer for deletion."
  "Mark the selected buffer for deletion."
  (declare (ignore p))
  (let ((tree (and *bufed-buffer* (buffer-ui-tree *bufed-buffer*))))
    (when tree
      (let ((buf (bufed-selected tree)))
        (when buf
          (if (not (value virtual-buffer-deletion))
              (when (or (not (value bufed-delete-confirm))
                        (prompt :y-or-n :prompt "Delete buffer? "
                                       :default t :must-exist t :default-string "Y"))
                (delete-bufed-buffer buf)
                (bufed-open-or-refresh))
              (let ((state (ui-tree-state tree)))
                (pushnew buf (getf state :deleted) :test #'eq)
                (setf (getf (ui-tree-state tree) :deleted)
                      (getf state :deleted))
                (bufed-render tree))))))))

(defcommand "Bufed Undelete" (p)
  "Remove the deletion mark from the selected buffer."
  "Remove the deletion mark from the selected buffer."
  (declare (ignore p))
  (let ((tree (and *bufed-buffer* (buffer-ui-tree *bufed-buffer*))))
    (when tree
      (let ((buf (bufed-selected tree)))
        (when buf
          (setf (getf (ui-tree-state tree) :deleted)
                (delete buf (getf (ui-tree-state tree) :deleted) :test #'eq))
          (bufed-render tree))))))

(defcommand "Bufed Expunge" (p)
  "Delete all buffers marked for deletion."
  "Delete all buffers marked for deletion."
  (declare (ignore p))
  (expunge-bufed-buffers))

(defcommand "Bufed Quit" (p)
  "Expunge marked buffers and kill the Bufed buffer."
  "Expunge marked buffers and kill the Bufed buffer."
  (declare (ignore p))
  (expunge-bufed-buffers)
  (when *bufed-buffer* (delete-buffer-if-possible *bufed-buffer*)))

(defcommand "Bufed Goto" (p)
  "Switch to the selected buffer."
  "Switch to the selected buffer."
  (declare (ignore p))
  (let ((tree (and *bufed-buffer* (buffer-ui-tree *bufed-buffer*))))
    (when tree
      (let ((buf (bufed-selected tree)))
        (when buf (change-to-buffer buf))))))

(defcommand "Bufed Save File" (p)
  "Save the selected buffer."
  "Save the selected buffer."
  (declare (ignore p))
  (let ((tree (and *bufed-buffer* (buffer-ui-tree *bufed-buffer*))))
    (when tree
      (let ((buf (bufed-selected tree)))
        (when buf (save-file-command nil buf))))))

(defcommand "Bufed Help" (p)
  "Show this help."
  "Show this help."
  (declare (ignore p))
  (describe-mode-command nil "Bufed"))
