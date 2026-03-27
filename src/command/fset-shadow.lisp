;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; FSet shadow — lazily builds immutable buffer-state from mutable buffers.
;;; Rebuilds on demand when the buffer's tick has advanced.

(in-package :hemlock.command)

(defvar *buffer-states* (make-hash-table :test #'eq)
  "Map from buffer to (tick . buffer-state). Lazy — rebuilt when tick advances.")

(defun buffer-state (buffer)
  "Get the current immutable buffer-state for BUFFER, rebuilding if stale."
  (let* ((entry (gethash buffer *buffer-states*))
         (buf-tick (buffer-modified-tick buffer))
         (cached-tick (if entry (car entry) -1)))
    (if (= buf-tick cached-tick)
        (cdr entry)
        (let ((state (rebuild-buffer-state buffer)))
          (setf (gethash buffer *buffer-states*) (cons buf-tick state))
          state))))

(defun buffer-snapshot (buffer &optional window)
  "Get a snapshot for BUFFER, suitable for display or tree-sitter.
Rebuilds the immutable state if stale, then extracts a snapshot."
  (let* ((state (buffer-state buffer))
         (snap (hemlock.text::state->snapshot state)))
    ;; Overlay window-specific info if available.
    (when window
      (let* ((ds (window-display-start window))
             (ds-line-idx (mark-to-line-index buffer ds))
             (hunk (window-hunk window))
             (tty-p (and hunk (typep hunk 'hemlock.tty::tty-hunk))))
        (setf (hemlock.text::snap-start-line snap) ds-line-idx
              (hemlock.text::snap-start-charpos snap) (mark-charpos ds)
              (hemlock.text::snap-width snap) (window-width window)
              (hemlock.text::snap-height snap) (window-height window))
        (when tty-p
          (let* ((th (hemlock.tty::tty-hunk-text-height hunk))
                 (tp (hemlock.tty::tty-hunk-text-position hunk))
                 (top (1+ (- tp th)))
                 (ml-row (1+ tp)))
            (setf (hemlock.text::snap-text-height snap) th
                  (hemlock.text::snap-top-row snap) top
                  (hemlock.text::snap-modeline-row snap) ml-row)))))
    snap))

(defun rebuild-buffer-state (buffer)
  "Walk the mutable buffer and build an FSet buffer-state."
  (let* ((region (buffer-region buffer))
         (first-line (mark-line (region-start region)))
         (point (buffer-point buffer))
         (lines (fset:empty-seq))
         (point-line-idx 0)
         (idx 0)
         (line first-line))
    ;; Walk lines.
    (loop while line do
      (when (eq line (mark-line point))
        (setf point-line-idx idx))
      (setf lines (fset:with-last lines (line-string line)))
      (incf idx)
      (setf line (line-next line)))
    ;; Build state.
    (hemlock.text::make-buffer-state
     :lines lines
     :marks (fset:map (:point (list point-line-idx
                                    (mark-charpos point)
                                    :right-inserting)))
     :meta (fset:map (:name (buffer-name buffer))
                     (:modes (buffer-modes buffer))
                     (:writable (buffer-writable buffer))
                     (:modified-tick (buffer-modified-tick buffer))
                     (:unmodified-tick (buffer-unmodified-tick buffer)))
     :tick (buffer-modified-tick buffer))))

(defun mark-to-line-index (buffer mark)
  "Find the line index of MARK's line in BUFFER. O(n) but called rarely."
  (let ((target (mark-line mark))
        (line (mark-line (region-start (buffer-region buffer))))
        (idx 0))
    (loop while line do
      (when (eq line target) (return idx))
      (incf idx)
      (setf line (line-next line)))
    0))

(defun drop-buffer-state (buffer)
  "Remove cached immutable state for BUFFER (call on buffer deletion)."
  (remhash buffer *buffer-states*))
