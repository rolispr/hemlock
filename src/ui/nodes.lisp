;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; hemlock.ui — virtual node tree for structured buffer rendering.
;;;
;;; A buffer is the render target.  The node tree is the source of truth.
;;; Inspired by vui.el: layout nodes (vstack, hstack, box), content nodes
;;; (text, field, separator), and interactive nodes (selectable, action).
;;;

(in-package :hemlock.command)


;;;; Base node

(defstruct (ui-node (:constructor nil) (:copier nil))
  "Base for all virtual nodes."
  (key       nil)   ; stable identity for reconciliation
  (parent    nil)   ; back-pointer (set during render)
  (start-mark nil)  ; mark at start of rendered content in buffer
  (end-mark   nil)) ; mark at end of rendered content in buffer


;;;; Content nodes

(defstruct (ui-text (:include ui-node) (:copier nil))
  "Plain styled text."
  (content "" :type string)
  (face    nil))    ; font index or keyword for styling

(defstruct (ui-separator (:include ui-node) (:copier nil))
  "Horizontal separator line."
  (char #\─ :type character))

(defstruct (ui-field (:include ui-node) (:copier nil))
  "Editable text input zone.  Content is tracked by marks; edits
within the field update the model."
  (content  "" :type string)
  (face     nil)
  ;; populated during render:
  (input-start nil)   ; right-inserting mark at start of editable region
  (input-end   nil))  ; left-inserting mark at end of editable region


;;;; Layout nodes

(defstruct (ui-vstack (:include ui-node) (:copier nil))
  "Vertical stack: children rendered top-to-bottom."
  (children nil :type list)
  (spacing  0   :type fixnum))  ; blank lines between children

(defstruct (ui-hstack (:include ui-node) (:copier nil))
  "Horizontal stack: children rendered left-to-right on the same line."
  (children nil :type list)
  (spacing  1   :type fixnum))  ; spaces between children

(defstruct (ui-box (:include ui-node) (:copier nil))
  "Fixed-width container with alignment."
  (child  nil)                      ; single child node
  (width  0     :type fixnum)       ; column width
  (align  :left :type keyword)      ; :left :center :right
  (pad    #\space :type character)) ; padding character


;;;; Interactive nodes

(defstruct (ui-selectable (:include ui-node) (:copier nil))
  "A row that can be highlighted/selected."
  (child      nil)         ; child node to render
  (data       nil)         ; arbitrary data associated with this row
  (selectedp  nil)         ; t when this is the selected row
  (prefix-selected   "> ") ; shown when selected
  (prefix-unselected "  "))

(defstruct (ui-action (:include ui-node) (:copier nil))
  "Text that triggers a callback on confirm."
  (child    nil)
  (callback nil))   ; (lambda (node)) called on activation


;;;; List node — renders a sequence of items with keys

(defstruct (ui-list (:include ui-node) (:copier nil))
  "Renders a list of data items using a template function."
  (items     nil :type list)       ; data items
  (item-fn   nil)                  ; (lambda (item index)) -> ui-node
  (max-visible nil))               ; limit visible items (nil = all)


;;;; Tree container

(defstruct (ui-tree (:copier nil))
  "Top-level container: a root node, a target buffer, and state."
  (root   nil)        ; root ui-node (typically a vstack)
  (buffer nil)        ; hemlock buffer to render into
  (state  nil)        ; plist of arbitrary state
  (width  80))        ; available columns for rendering
