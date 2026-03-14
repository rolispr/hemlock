;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; Render pass: walk a ui-node tree and write it into a hemlock buffer.
;;;
;;; Full re-render on every call (sufficient for small UI buffers like
;;; the echo area).  Reconciliation can be added later using node keys
;;; and marks to diff.
;;;

(in-package :hemlock.command)


;;;; Top-level entry

(defun render-tree (tree)
  "Render the ui-tree's root node into its buffer.
Clears the buffer and writes fresh content.  Sets marks on each node."
  (let* ((buffer (ui-tree-buffer tree))
         (region (buffer-region buffer))
         (width  (ui-tree-width tree)))
    ;; clear everything
    (delete-region region)
    ;; render from the start
    (let ((point (buffer-point buffer)))
      (move-mark point (region-start region))
      (render-node (ui-tree-root tree) point width)
      ;; remove trailing newline if we left one
      (let ((end (region-end region)))
        (when (and (plusp (line-length (mark-line end)))
                   (not (zerop (mark-charpos end))))
          nil)))))  ; leave as-is


;;;; Generic render dispatch

(defgeneric render-node (node point width)
  (:documentation "Render NODE into the buffer at POINT, with WIDTH columns available.
POINT is advanced past the rendered content."))


;;;; Text

(defmethod render-node ((node ui-text) point width)
  (declare (ignore width))
  (setf (ui-node-start-mark node) (copy-mark point :right-inserting))
  (let ((content (ui-text-content node)))
    (when (plusp (length content))
      (insert-string point content)))
  (setf (ui-node-end-mark node) (copy-mark point :left-inserting)))


;;;; Separator

(defmethod render-node ((node ui-separator) point width)
  (setf (ui-node-start-mark node) (copy-mark point :right-inserting))
  (insert-string point (make-string width :initial-element (ui-separator-char node)))
  (setf (ui-node-end-mark node) (copy-mark point :left-inserting)))


;;;; Field

(defmethod render-node ((node ui-field) point width)
  (declare (ignore width))
  (setf (ui-node-start-mark node) (copy-mark point :right-inserting))
  ;; input-start: right-inserting so text typed goes after the mark
  (setf (ui-field-input-start node) (copy-mark point :right-inserting))
  (let ((content (ui-field-content node)))
    (when (plusp (length content))
      (insert-string point content)))
  ;; input-end: left-inserting so text typed goes before the mark
  (setf (ui-field-input-end node) (copy-mark point :left-inserting))
  (setf (ui-node-end-mark node) (copy-mark point :left-inserting))
  ;; tag the line so input handling can find the field
  (setf (getf (line-plist (mark-line point)) :ui-field) node))


;;;; VStack

(defmethod render-node ((node ui-vstack) point width)
  (setf (ui-node-start-mark node) (copy-mark point :right-inserting))
  (let ((children (ui-vstack-children node))
        (spacing  (ui-vstack-spacing node)))
    (loop for (child . rest) on children
          do (setf (ui-node-parent child) node)
             (render-node child point width)
             (when rest
               (insert-character point #\newline)
               (dotimes (_ spacing)
                 (insert-character point #\newline)))))
  (setf (ui-node-end-mark node) (copy-mark point :left-inserting)))


;;;; HStack

(defmethod render-node ((node ui-hstack) point width)
  (setf (ui-node-start-mark node) (copy-mark point :right-inserting))
  (let ((children (ui-hstack-children node))
        (spacing  (ui-hstack-spacing node))
        (col      0))
    (loop for (child . rest) on children
          do (setf (ui-node-parent child) node)
             (let ((before (mark-charpos point)))
               (render-node child point (- width col))
               (let ((after (mark-charpos point)))
                 (incf col (- after before))))
             (when rest
               (dotimes (_ spacing)
                 (insert-character point #\space)
                 (incf col)))))
  (setf (ui-node-end-mark node) (copy-mark point :left-inserting)))


;;;; Box

(defmethod render-node ((node ui-box) point width)
  (declare (ignore width))
  (setf (ui-node-start-mark node) (copy-mark point :right-inserting))
  (let* ((box-width (ui-box-width node))
         (child     (ui-box-child node))
         (align     (ui-box-align node))
         (pad-char  (ui-box-pad node)))
    (if child
        ;; render child to a string, then pad/align into box
        (let* ((content (render-node-to-string child box-width))
               (len     (length content))
               (padded  (if (>= len box-width)
                            (subseq content 0 box-width)
                            (let ((pad-total (- box-width len)))
                              (ecase align
                                (:left
                                 (concatenate 'string content
                                              (make-string pad-total
                                                           :initial-element pad-char)))
                                (:right
                                 (concatenate 'string
                                              (make-string pad-total
                                                           :initial-element pad-char)
                                              content))
                                (:center
                                 (let ((left (floor pad-total 2))
                                       (right (ceiling pad-total 2)))
                                   (concatenate 'string
                                                (make-string left :initial-element pad-char)
                                                content
                                                (make-string right
                                                             :initial-element pad-char)))))))))
          (insert-string point padded))
        ;; no child: just pad
        (insert-string point (make-string box-width :initial-element pad-char))))
  (setf (ui-node-end-mark node) (copy-mark point :left-inserting)))


;;;; Selectable

(defmethod render-node ((node ui-selectable) point width)
  (setf (ui-node-start-mark node) (copy-mark point :right-inserting))
  (let* ((selected (ui-selectable-selectedp node))
         (prefix   (if selected
                       (ui-selectable-prefix-selected node)
                       (ui-selectable-prefix-unselected node)))
         (child    (ui-selectable-child node)))
    (insert-string point prefix)
    (when child
      (setf (ui-node-parent child) node)
      (render-node child point (- width (length prefix))))
    ;; tag line with selection state
    (setf (getf (line-plist (mark-line (ui-node-start-mark node))) :ui-selectable) node))
  (setf (ui-node-end-mark node) (copy-mark point :left-inserting)))


;;;; Action

(defmethod render-node ((node ui-action) point width)
  (setf (ui-node-start-mark node) (copy-mark point :right-inserting))
  (when (ui-action-child node)
    (setf (ui-node-parent (ui-action-child node)) node)
    (render-node (ui-action-child node) point width))
  (setf (ui-node-end-mark node) (copy-mark point :left-inserting)))


;;;; List

(defmethod render-node ((node ui-list) point width)
  (setf (ui-node-start-mark node) (copy-mark point :right-inserting))
  (let* ((items (ui-list-items node))
         (max-v (ui-list-max-visible node))
         (item-fn (ui-list-item-fn node))
         (visible (if max-v (subseq items 0 (min max-v (length items))) items)))
    (loop for (item . rest) on visible
          for idx from 0
          for child = (funcall item-fn item idx)
          do (setf (ui-node-parent child) node)
             (render-node child point width)
             (when rest
               (insert-character point #\newline))))
  (setf (ui-node-end-mark node) (copy-mark point :left-inserting)))


;;;; Utility: render a node to a string (for box alignment)

(defun render-node-to-string (node width)
  "Render NODE into a temporary buffer and return the first line as a string."
  (let ((buf (make-buffer " *ui-temp*" :delete-hook nil)))
    (unwind-protect
        (let ((point (buffer-point buf)))
          (render-node node point width)
          (line-string (mark-line (region-start (buffer-region buf)))))
      (delete-buffer buf))))
