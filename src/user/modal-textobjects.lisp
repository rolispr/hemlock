;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; Helix-style structural text objects backed by tree-sitter.
;;;
;;; Depends on: modal.lisp (mode defs, select helpers)
;;;             ts-highlight.lisp (ts-stable-tree-for-buffer)
;;;
;;; Commands defined here:
;;;   "Expand Selection"   (Alt-o) — grow to enclosing AST node
;;;   "Shrink Selection"   (Alt-i) — shrink to first named child
;;;   "Select Next Sibling" (Alt-n) — jump to next named sibling
;;;   "Select Prev Sibling" (Alt-p) — jump to previous named sibling

(in-package :hemlock)


;;;; ─── Byte ↔ mark conversion ──────────────────────────────────────────────
;;;
;;; Tree-sitter works in UTF-8 byte offsets; hemlock uses (line, charpos) marks.

(defun %mark-to-byte-offset (mark)
  "Return the UTF-8 byte offset of MARK from the start of its buffer."
  (let ((target-line (mark-line mark))
        (target-col  (mark-charpos mark))
        (line        (mark-line (buffer-start-mark
                                 (line-buffer (mark-line mark)))))
        (offset      0))
    (loop while (and line (not (eq line target-line)))
          do (incf offset
                   (babel:string-size-in-octets
                    (line-string line) :encoding :utf-8))
             (incf offset 1)              ; newline byte
             (setf line (line-next line)))
    (when (and line (plusp target-col))
      (incf offset
            (babel:string-size-in-octets
             (subseq (line-string line) 0
                     (min target-col (length (line-string line))))
             :encoding :utf-8)))
    offset))

(defun %set-mark-to-byte-offset (mark buffer byte-offset)
  "Move MARK to the position in BUFFER corresponding to UTF-8 BYTE-OFFSET."
  (let* ((text    (hemlock.command::ts-buffer-text buffer))
         (offsets (hemlock.command::ts-build-line-offsets text)))
    (multiple-value-bind (line-num col-byte)
        (hemlock.command::ts-byte-to-line-col byte-offset offsets)
      (let ((line (mark-line (buffer-start-mark buffer))))
        (dotimes (i line-num)
          (setf line (line-next line)))
        ;; col-byte is a byte offset within the line; convert to char index.
        (let ((line-str (line-string line))
              (char-idx 0)
              (bytes-so-far 0))
          (loop for i below (length line-str)
                while (< bytes-so-far col-byte)
                do (incf bytes-so-far
                         (babel:string-size-in-octets
                          (string (char line-str i)) :encoding :utf-8))
                   (incf char-idx))
          (setf (mark-line    mark) line
                (mark-charpos mark) char-idx))))))


;;;; ─── AST node lookup ─────────────────────────────────────────────────────

(defun %node-at-mark (mark tree-ptr)
  "Return the deepest named AST node in TREE-PTR that covers MARK, or NIL."
  (when (or (null tree-ptr) (cffi:null-pointer-p tree-ptr))
    (return-from %node-at-mark nil))
  (let ((target-byte (%mark-to-byte-offset mark))
        (root-buf    (cffi:foreign-alloc
                      '(:struct tree-sitter/ffi:ts-node-raw))))
    (tree-sitter/ffi:ts-tree-root-node tree-ptr root-buf)
    (when (tree-sitter/ffi:ts-node-is-null root-buf)
      (cffi:foreign-free root-buf)
      (return-from %node-at-mark nil))
    (let ((current (make-instance 'tree-sitter/types:ts-node
                                  :tree nil :buffer root-buf)))
      ;; Descend: at each level pick the named child that contains target-byte.
      (loop
        (let ((descend nil))
          (dotimes (i (tree-sitter/node:node-named-child-count current))
            (let ((child (tree-sitter/node:node-named-child current i)))
              (when (and child
                         (<= (tree-sitter/node:node-start-byte child) target-byte)
                         (<= target-byte (tree-sitter/node:node-end-byte child)))
                (setf current child
                      descend  t)
                (return))))
          (unless descend (return))))
      current)))

(defun %smallest-node-covering-range (start-byte end-byte tree-ptr)
  "Find the smallest named AST node in TREE-PTR that spans [START-BYTE, END-BYTE]."
  (when (or (null tree-ptr) (cffi:null-pointer-p tree-ptr))
    (return-from %smallest-node-covering-range nil))
  (let ((root-buf (cffi:foreign-alloc
                   '(:struct tree-sitter/ffi:ts-node-raw))))
    (tree-sitter/ffi:ts-tree-root-node tree-ptr root-buf)
    (when (tree-sitter/ffi:ts-node-is-null root-buf)
      (cffi:foreign-free root-buf)
      (return-from %smallest-node-covering-range nil))
    (let ((current (make-instance 'tree-sitter/types:ts-node
                                  :tree nil :buffer root-buf)))
      ;; Descend to deepest named node containing start-byte.
      (loop
        (let ((descend nil))
          (dotimes (i (tree-sitter/node:node-named-child-count current))
            (let ((child (tree-sitter/node:node-named-child current i)))
              (when (and child
                         (<= (tree-sitter/node:node-start-byte child) start-byte)
                         (<= start-byte (tree-sitter/node:node-end-byte child)))
                (setf current child descend t)
                (return))))
          (unless descend (return))))
      ;; Walk up until the node's end covers end-byte too.
      (loop while (and current
                       (< (tree-sitter/node:node-end-byte current) end-byte))
            do (setf current (tree-sitter/node:node-parent current)))
      current)))


;;;; ─── Selection helper ────────────────────────────────────────────────────

(defun %select-node-bytes (start-byte end-byte buffer)
  "Set the active selection to cover [START-BYTE, END-BYTE) in BUFFER.
Point moves to end; mark anchors at start. Enters Select mode."
  (let ((point (current-point)))
    ;; Move point to node end.
    (%set-mark-to-byte-offset point buffer end-byte)
    ;; Push anchor mark at node start, activating the region.
    (let ((anchor (copy-mark point :right-inserting)))
      (%set-mark-to-byte-offset anchor buffer start-byte)
      (push-buffer-mark anchor t))
    ;; Ensure Normal mode is active and Select mode is on.
    (setf (buffer-minor-mode buffer "Normal") t)
    (setf (buffer-minor-mode buffer "Select") t)
    (update-modeline-fields buffer (current-window))))


;;;; ─── Structural selection commands ──────────────────────────────────────

(defcommand "Expand Selection" (p)
  "Expand selection to the enclosing syntax node (Alt-o).
If no selection: select the node at point.
If selection already covers the node exactly: expand to its parent."
  "Helix Alt-o — tree-sitter expand."
  (declare (ignore p))
  (let* ((buffer   (current-buffer))
         (tree-ptr (ts-stable-tree-for-buffer buffer)))
    (unless tree-ptr
      (editor-error "Tree-sitter unavailable for this buffer"))
    (let* ((point (current-point))
           (node  (if (region-active-p)
                      ;; Find node covering the full selection.
                      (let* ((mark  (buffer-mark buffer))
                             (sb    (min (%mark-to-byte-offset point)
                                        (%mark-to-byte-offset mark)))
                             (eb    (max (%mark-to-byte-offset point)
                                        (%mark-to-byte-offset mark))))
                        (%smallest-node-covering-range sb eb tree-ptr))
                      (%node-at-mark point tree-ptr))))
      (unless node
        (editor-error "No syntax node at point"))
      ;; If current selection already covers this node exactly, go to parent.
      (let ((target
             (if (region-active-p)
                 (let* ((mark  (buffer-mark buffer))
                        (sb    (min (%mark-to-byte-offset point)
                                    (%mark-to-byte-offset mark)))
                        (eb    (max (%mark-to-byte-offset point)
                                    (%mark-to-byte-offset mark)))
                        (nsb   (tree-sitter/node:node-start-byte node))
                        (neb   (tree-sitter/node:node-end-byte   node)))
                   (if (and (= sb nsb) (= eb neb))
                       (or (tree-sitter/node:node-parent node) node)
                       node))
                 node)))
        (%select-node-bytes (tree-sitter/node:node-start-byte target)
                            (tree-sitter/node:node-end-byte   target)
                            buffer)))))

(defcommand "Shrink Selection" (p)
  "Shrink selection to the first named child node (Alt-i)."
  "Helix Alt-i — tree-sitter shrink."
  (declare (ignore p))
  (let* ((buffer   (current-buffer))
         (tree-ptr (ts-stable-tree-for-buffer buffer)))
    (unless tree-ptr
      (editor-error "Tree-sitter unavailable for this buffer"))
    (let* ((point (current-point))
           (node  (%node-at-mark point tree-ptr))
           (child (when node (tree-sitter/node:node-named-child node 0))))
      (if child
          (%select-node-bytes (tree-sitter/node:node-start-byte child)
                              (tree-sitter/node:node-end-byte   child)
                              buffer)
          (editor-error "No child node to shrink to")))))

(defcommand "Select Next Sibling" (p)
  "Select the next named sibling AST node (Alt-n)."
  "Helix Alt-n — tree-sitter next sibling."
  (declare (ignore p))
  (let* ((buffer   (current-buffer))
         (tree-ptr (ts-stable-tree-for-buffer buffer)))
    (unless tree-ptr
      (editor-error "Tree-sitter unavailable for this buffer"))
    (let* ((point   (current-point))
           (node    (%node-at-mark point tree-ptr))
           (sibling (when node (tree-sitter/node:node-next-named-sibling node))))
      (if sibling
          (%select-node-bytes (tree-sitter/node:node-start-byte sibling)
                              (tree-sitter/node:node-end-byte   sibling)
                              buffer)
          (editor-error "No next sibling node")))))

(defcommand "Select Prev Sibling" (p)
  "Select the previous named sibling AST node (Alt-p)."
  "Helix Alt-p — tree-sitter prev sibling."
  (declare (ignore p))
  (let* ((buffer   (current-buffer))
         (tree-ptr (ts-stable-tree-for-buffer buffer)))
    (unless tree-ptr
      (editor-error "Tree-sitter unavailable for this buffer"))
    (let* ((point   (current-point))
           (node    (%node-at-mark point tree-ptr))
           (sibling (when node (tree-sitter/node:node-prev-named-sibling node))))
      (if sibling
          (%select-node-bytes (tree-sitter/node:node-start-byte sibling)
                              (tree-sitter/node:node-end-byte   sibling)
                              buffer)
          (editor-error "No previous sibling node")))))
