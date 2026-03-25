;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;;    Connection browser (Coned).
;;;

(in-package :hemlock)


;;;; Data model

(defstruct (coned-connection
             (:constructor make-coned-connection (connection)))
  connection
  (deleted nil))

(defvar *coned-buffer* nil)

(defun delete-coned-buffers (buffer)
  (when (eq buffer *coned-buffer*)
    (setf *coned-buffer* nil)))

(defun delete-coned-connection (conn)
  (delete-connection conn))


;;;; Display

(defun coned-window ()
  (find-if (lambda (w) (eq (window-buffer w) *coned-buffer*)) *window-list*))

(defun coned-connections ()
  (mapcar #'make-coned-connection (list-all-connections)))

(defun coned-row (item width)
  "Build a selectable row for a coned-connection within WIDTH columns."
  (let* ((conn      (coned-connection-connection item))
         (deletedp  (coned-connection-deleted item))
         (name-w    (min 24 (floor width 3)))
         (type-w    16)
         (buf-w     (max 0 (- width 1 name-w type-w 2)))
         (name-face (if deletedp 12 10))
         (buf-name  (let ((b (connection-buffer conn)))
                      (if b (buffer-name b) ""))))
    (make-ui-selectable
     :data item
     :child (make-ui-hstack
             :spacing 0
             :children (list
                        (make-ui-box :width 1 :align :left
                                     :child (make-ui-text
                                             :content (if deletedp "D" " ")
                                             :face (when deletedp 12)))
                        (make-ui-box :width name-w :align :left
                                     :child (make-ui-text
                                             :content (shorten-string name-w (connection-name conn))
                                             :face name-face))
                        (make-ui-box :width type-w :align :left
                                     :child (make-ui-text
                                             :content (shorten-string type-w (princ-to-string (type-of conn)))
                                             :face 11))
                        (make-ui-text :content (shorten-string buf-w buf-name)
                                      :face 11))))))

(defun coned-render (tree)
  "Render the connection list into the tree's buffer."
  (let* ((state   (ui-tree-state tree))
         (conns   (getf state :connections))
         (sel     (getf state :selection 0))
         (n       (length conns))
         (width   (ui-tree-width tree))
         (win     (coned-window))
         (height  (if win (window-height win) 20))
         (offset  (scroll-to-selection sel (getf state :scroll-offset 0) height)))
    (setf (getf (ui-tree-state tree) :scroll-offset) offset)
    (let ((rows nil))
      (loop for i from offset below (min (+ offset height) n)
            for item = (nth i conns)
            do (let ((row (coned-row item width)))
                 (setf (ui-selectable-selectedp row) (= i sel))
                 (push row rows)))
      (setf (ui-tree-root tree) (make-ui-vstack :children (nreverse rows))))
    (with-writable-buffer ((ui-tree-buffer tree))
      (render-tree tree))
    (when win
      (move-mark (window-display-start win)
                 (region-start (buffer-region (ui-tree-buffer tree)))))))

(defun coned-open-or-refresh ()
  "Open the Coned buffer, or refresh it if already open."
  (let ((buf (or *coned-buffer*
                 (make-buffer "Coned" :modes '("Coned")
                              :delete-hook '(delete-coned-buffers)))))
    (setf *coned-buffer* buf)
    (when (buffer-ui-tree buf)
      (uninstall-tree (buffer-ui-tree buf)))
    (let* ((win   (or (coned-window) (car *window-list*)))
           (width (if win (window-width win) 80))
           (tree  (make-ui-tree
                   :buffer buf
                   :width width
                   :state (list :connections (coned-connections)
                                :selection 0
                                :scroll-offset 0))))
      (install-tree tree)
      (setf (buffer-writable buf) nil)
      (coned-render tree))
    (change-to-buffer buf)))


;;;; Commands

(defmode "Coned" :major-p t
  :documentation
  "Coned lists active connections and supports deletion.")

(defhvar "Virtual Connection Deletion"
  "When set, \"Coned Delete\" marks a connection for deletion instead of
   immediately deleting it."
  :value t)

(defhvar "Coned Delete Confirm"
  "When set, \"Coned\" commands that actually delete connections ask for
   confirmation before taking action."
  :value t)

(defcommand "Coned" (p)
  "Open the connection browser, refreshing it if already open."
  "Open the connection browser, refreshing it if already open."
  (declare (ignore p))
  (coned-open-or-refresh))

(defcommand "Coned Refresh" (p)
  "Refresh the connection list."
  "Refresh the connection list."
  (declare (ignore p))
  (let ((tree (and *coned-buffer* (buffer-ui-tree *coned-buffer*))))
    (when tree
      (setf (getf (ui-tree-state tree) :connections) (coned-connections)
            (getf (ui-tree-state tree) :selection) 0
            (getf (ui-tree-state tree) :scroll-offset) 0)
      (coned-render tree))))

(defcommand "Coned Next" (p)
  "Move selection to the next connection."
  "Move selection to the next connection."
  (declare (ignore p))
  (let ((tree (and *coned-buffer* (buffer-ui-tree *coned-buffer*))))
    (when tree
      (let* ((state (ui-tree-state tree))
             (n     (length (getf state :connections)))
             (sel   (getf state :selection 0)))
        (setf (getf (ui-tree-state tree) :selection) (min (1- n) (1+ sel)))
        (coned-render tree)))))

(defcommand "Coned Previous" (p)
  "Move selection to the previous connection."
  "Move selection to the previous connection."
  (declare (ignore p))
  (let ((tree (and *coned-buffer* (buffer-ui-tree *coned-buffer*))))
    (when tree
      (let* ((state (ui-tree-state tree))
             (sel   (getf state :selection 0)))
        (setf (getf (ui-tree-state tree) :selection) (max 0 (1- sel)))
        (coned-render tree)))))

(defun coned-selected (tree)
  "Return the selected coned-connection, or NIL."
  (let* ((state (ui-tree-state tree))
         (conns (getf state :connections))
         (sel   (getf state :selection 0)))
    (when (< -1 sel (length conns))
      (nth sel conns))))

(defcommand "Coned Delete" (p)
  "Mark the selected connection for deletion."
  "Mark the selected connection for deletion."
  (declare (ignore p))
  (let ((tree (and *coned-buffer* (buffer-ui-tree *coned-buffer*))))
    (when tree
      (let ((item (coned-selected tree)))
        (when item
          (if (not (value virtual-connection-deletion))
              (when (or (not (value coned-delete-confirm))
                        (prompt :y-or-n :prompt "Delete connection? "
                                       :default t :must-exist t :default-string "Y"))
                (delete-coned-connection (coned-connection-connection item))
                (coned-open-or-refresh))
              (progn
                (setf (coned-connection-deleted item) t)
                (coned-render tree))))))))

(defcommand "Coned Undelete" (p)
  "Unmark the selected connection."
  "Unmark the selected connection."
  (declare (ignore p))
  (let ((tree (and *coned-buffer* (buffer-ui-tree *coned-buffer*))))
    (when tree
      (let ((item (coned-selected tree)))
        (when item
          (setf (coned-connection-deleted item) nil)
          (coned-render tree))))))

(defcommand "Coned Expunge" (p)
  "Delete all connections marked for deletion."
  "Delete all connections marked for deletion."
  (declare (ignore p))
  (unless (eq *coned-buffer* (current-buffer))
    (editor-error "Not in the Coned buffer."))
  (let ((tree (buffer-ui-tree *coned-buffer*)))
    (when tree
      (let ((marked (remove-if-not #'coned-connection-deleted
                                   (getf (ui-tree-state tree) :connections))))
        (when (and marked
                   (or (not (value coned-delete-confirm))
                       (prompt :y-or-n :prompt "Delete connections? "
                                      :default t :must-exist t :default-string "Y")))
          (dolist (item marked)
            (delete-coned-connection (coned-connection-connection item)))
          (coned-open-or-refresh))))))

(defcommand "Coned Quit" (p)
  "Expunge marked connections and kill the Coned buffer."
  "Expunge marked connections and kill the Coned buffer."
  (declare (ignore p))
  (coned-expunge-command nil)
  (when *coned-buffer* (delete-buffer-if-possible *coned-buffer*)))

(defcommand "Coned Goto" (p)
  "Switch to the selected connection's buffer."
  "Switch to the selected connection's buffer."
  (declare (ignore p))
  (let ((tree (and *coned-buffer* (buffer-ui-tree *coned-buffer*))))
    (when tree
      (let ((item (coned-selected tree)))
        (when item
          (let ((buf (connection-buffer (coned-connection-connection item))))
            (if buf
                (change-to-buffer buf)
                (editor-error "Connection has no buffer."))))))))

(defcommand "Coned Help" (p)
  "Show this help."
  "Show this help."
  (declare (ignore p))
  (describe-mode-command nil "Coned"))
