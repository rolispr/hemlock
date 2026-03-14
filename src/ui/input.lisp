;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; Input handling for ui-tree buffers.
;;;
;;; - Field nodes: editable zones tracked by marks
;;; - Selectable nodes: navigation with selection index
;;; - Action nodes: callback on confirm
;;;

(in-package :hemlock.command)


;;;; Tree state accessors

(defun ui-tree-get (tree key &optional default)
  "Get a value from the tree's state plist."
  (getf (ui-tree-state tree) key default))

(defun (setf ui-tree-get) (value tree key)
  "Set a value in the tree's state plist."
  (setf (getf (ui-tree-state tree) key) value)
  value)


;;;; Finding the active field

(defun find-field-at-point (point)
  "Return the ui-field node at POINT, or nil."
  (getf (line-plist (mark-line point)) :ui-field))

(defun point-in-field-p (point field)
  "True if POINT is within the editable region of FIELD."
  (and field
       (ui-field-input-start field)
       (ui-field-input-end field)
       (mark>= point (ui-field-input-start field))
       (mark<= point (ui-field-input-end field))))

(defun field-content (field)
  "Return the current text content of a rendered field node."
  (when (and (ui-field-input-start field)
             (ui-field-input-end field))
    (region-to-string
     (region (ui-field-input-start field)
             (ui-field-input-end field)))))


;;;; Selection navigation

(defun collect-selectables (node)
  "Walk the node tree and collect all ui-selectable nodes in render order."
  (let ((result nil))
    (labels ((walk (n)
               (typecase n
                 (ui-selectable (push n result))
                 (ui-vstack (mapc #'walk (ui-vstack-children n)))
                 (ui-hstack (mapc #'walk (ui-hstack-children n)))
                 (ui-box    (when (ui-box-child n) (walk (ui-box-child n))))
                 (ui-list   ;; list children are generated; walk items if rendered
                  (when (ui-list-item-fn n)
                    (let ((items (ui-list-items n))
                          (max-v (ui-list-max-visible n)))
                      (loop for item in (if max-v
                                             (subseq items 0 (min max-v (length items)))
                                             items)
                            for idx from 0
                            for child = (funcall (ui-list-item-fn n) item idx)
                            do (walk child)))))
                 (ui-action (when (ui-action-child n) (walk (ui-action-child n))))
                 (t nil))))
      (walk node))
    (nreverse result)))

(defun update-selection (tree index)
  "Set the selection index in TREE's state and mark the appropriate
selectable nodes.  Returns the selected node or nil."
  (let* ((root (ui-tree-root tree))
         (selectables (collect-selectables root))
         (n (length selectables)))
    (when (zerop n) (return-from update-selection nil))
    ;; clamp index
    (setf index (mod index n))
    (setf (ui-tree-get tree :selection-index) index)
    ;; update selectedp flags
    (loop for s in selectables
          for i from 0
          do (setf (ui-selectable-selectedp s) (= i index)))
    (nth index selectables)))

(defun selected-node (tree)
  "Return the currently selected ui-selectable, or nil."
  (let ((idx (ui-tree-get tree :selection-index 0)))
    (let ((selectables (collect-selectables (ui-tree-root tree))))
      (when selectables
        (nth (mod idx (length selectables)) selectables)))))

(defun selection-move (tree delta)
  "Move selection by DELTA (+1 = down, -1 = up).  Returns the new selected node."
  (let ((idx (ui-tree-get tree :selection-index 0)))
    (update-selection tree (+ idx delta))))
