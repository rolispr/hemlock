;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; Tree state management, input operations, and selection navigation.
;;;

(in-package :hemlock.command)


;;;; Rendering flag

(defvar *tree-rendering* nil
  "Bound to T while a ui-tree is rendering into its buffer.")


;;;; Tree ownership

(defvar *buffer-tree-table* (make-hash-table :test #'eq))

(defun install-tree (tree)
  (setf (gethash (ui-tree-buffer tree) *buffer-tree-table*) tree))

(defun uninstall-tree (tree)
  (remhash (ui-tree-buffer tree) *buffer-tree-table*))

(defun buffer-ui-tree (buffer)
  (gethash buffer *buffer-tree-table*))


;;;; State accessors

(defun ui-tree-get (tree key &optional default)
  (getf (ui-tree-state tree) key default))

(defun (setf ui-tree-get) (value tree key)
  (setf (getf (ui-tree-state tree) key) value)
  value)


;;;; Input state operations
;;;
;;; These operate on :input (string) and :cursor-offset (fixnum) in
;;; the tree's state plist.  They modify state only — the caller is
;;; responsible for re-rendering.

(defun input-string (tree)
  (getf (ui-tree-state tree) :input ""))

(defun (setf input-string) (val tree)
  (setf (getf (ui-tree-state tree) :input) val))

(defun cursor-offset (tree)
  (let ((input (input-string tree)))
    (min (getf (ui-tree-state tree) :cursor-offset (length input))
         (length input))))

(defun (setf cursor-offset) (val tree)
  (setf (getf (ui-tree-state tree) :cursor-offset) val))

(defun type-char-at-cursor (tree char)
  "Insert CHAR at cursor position in input."
  (let* ((input (input-string tree))
         (off (cursor-offset tree)))
    (setf (input-string tree)
          (concatenate 'string (subseq input 0 off) (string char) (subseq input off)))
    (setf (cursor-offset tree) (1+ off))))

(defun delete-char-before-cursor (tree)
  "Delete one char before cursor. Returns T if deleted, NIL if at start."
  (let* ((input (input-string tree))
         (off (cursor-offset tree)))
    (when (plusp off)
      (setf (input-string tree)
            (concatenate 'string (subseq input 0 (1- off)) (subseq input off)))
      (setf (cursor-offset tree) (1- off))
      t)))

(defun kill-input (tree)
  "Clear all input."
  (setf (input-string tree) ""
        (cursor-offset tree) 0))

(defun kill-to-end (tree)
  "Kill from cursor to end of input."
  (let ((off (cursor-offset tree)))
    (setf (input-string tree) (subseq (input-string tree) 0 off))))

(defun find-word-start (input offset &optional (separator #\space))
  "Find the start of the word before OFFSET in INPUT, using SEPARATOR as boundary."
  (let* ((pos (loop for i from (1- offset) downto 0
                    while (eql (char input i) separator)
                    finally (return (1+ i)))))
    (loop for i from (1- pos) downto 0
          while (not (eql (char input i) separator))
          finally (return (1+ i)))))

(defun kill-word-before-cursor (tree &optional (separator #\space))
  "Kill the word before cursor, using SEPARATOR as word boundary."
  (let* ((input (input-string tree))
         (off (cursor-offset tree))
         (word-start (find-word-start input off separator)))
    (setf (input-string tree)
          (concatenate 'string (subseq input 0 word-start) (subseq input off)))
    (setf (cursor-offset tree) word-start)))

(defun backward-word-offset (tree &optional (separator #\space))
  "Return the cursor offset after moving back one word."
  (find-word-start (input-string tree) (cursor-offset tree) separator))

(defun move-cursor-backward-word (tree &optional (separator #\space))
  (setf (cursor-offset tree) (backward-word-offset tree separator)))

(defun set-input (tree text)
  "Replace input with TEXT, cursor at end."
  (setf (input-string tree) (or text "")
        (cursor-offset tree) (length (or text ""))))

(defun move-cursor (tree delta)
  "Move cursor by DELTA, clamped to [0, input-length]."
  (let* ((input (input-string tree))
         (off (cursor-offset tree))
         (new (max 0 (min (length input) (+ off delta)))))
    (setf (cursor-offset tree) new)))

(defun cursor-to-start (tree)
  (setf (cursor-offset tree) 0))

(defun cursor-to-end (tree)
  (setf (cursor-offset tree) (length (input-string tree))))

(defun confirm-input (tree)
  "Return the string to confirm: selected candidate or typed input."
  (let ((sel (getf (ui-tree-state tree) :selection -1))
        (filtered (getf (ui-tree-state tree) :filtered)))
    (if (and (>= sel 0) (< sel (length filtered)))
        (nth sel filtered)
        (input-string tree))))


;;;; Selection navigation

(defun collect-selectables (node)
  "Walk the node tree and collect all ui-selectable nodes."
  (let ((result nil))
    (labels ((walk (n)
               (when n
                 (typecase n
                   (ui-selectable (push n result))
                   (ui-vstack (mapc #'walk (ui-vstack-children n)))
                   (ui-hstack (mapc #'walk (ui-hstack-children n)))
                   (ui-box (walk (ui-box-child n)))
                   (ui-grid (dolist (row (ui-grid-cells n))
                              (mapc #'walk row)))
                   (ui-action (walk (ui-action-child n)))
                   (t nil)))))
      (walk node))
    (nreverse result)))

(defun update-selection (tree index)
  (let* ((root (ui-tree-root tree))
         (selectables (collect-selectables root))
         (n (length selectables)))
    (when (zerop n) (return-from update-selection nil))
    (setf index (mod index n))
    (setf (ui-tree-get tree :selection-index) index)
    (loop for s in selectables
          for i from 0
          do (setf (ui-selectable-selectedp s) (= i index)))
    (nth index selectables)))

(defun selected-node (tree)
  (let ((idx (ui-tree-get tree :selection-index 0)))
    (let ((selectables (collect-selectables (ui-tree-root tree))))
      (when selectables
        (nth (mod idx (length selectables)) selectables)))))

(defun selection-move (tree delta)
  (let ((idx (ui-tree-get tree :selection-index 0)))
    (update-selection tree (+ idx delta))))
