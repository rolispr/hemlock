;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; X window object-set and event-serving infrastructure.
;;; Extracted from hemlock-ext.lisp; belongs to the CLX backend.

(in-package :hemlock-ext)

(defstruct (object-set (:constructor make-object-set (name &optional default-handler)))
  name
  default-handler
  (table (make-hash-table)))

(defvar *xwindow-hash* (make-hash-table :test #'eq))

(defun hi::add-xwindow-object (window object object-set)
  (setf (gethash window *xwindow-hash*) (list object object-set)))

(defun hi::remove-xwindow-object (window)
  (remhash window *xwindow-hash*))

(defun lisp--map-xwindow (window)
  ;; -> object object-set
  (values-list (gethash window *xwindow-hash*)))


;;;; Key and button service.

(defun serve-key-press (object-set fun)
  "Associate a method in the object-set with :key-press events.  The method
   is called on the object the event occurred, event key, event window, root,
   child, same-screen-p, x, y, root-x, root-y, state, time, code, and
   send-event-p."
  (setf (gethash :key-press (object-set-table object-set)) fun))

(defun serve-key-release (object-set fun)
  "Associate a method in the object-set with :key-release events.  The method
   is called on the object the event occurred, event key, event window, root,
   child, same-screen-p, x, y, root-x, root-y, state, time, code, and
   send-event-p."
  (setf (gethash :key-release (object-set-table object-set)) fun))

(defun serve-button-press (object-set fun)
  "Associate a method in the object-set with :button-press events.  The method
   is called on the object the event occurred, event key, event window, root,
   child, same-screen-p, x, y, root-x, root-y, state, time, code, and
   send-event-p."
  (setf (gethash :button-press (object-set-table object-set)) fun))

(defun serve-button-release (object-set fun)
  "Associate a method in the object-set with :button-release events.  The
   method is called on the object the event occurred, event key, event window,
   root, child, same-screen-p, x, y, root-x, root-y, state, time, code, and
   send-event-p."
  (setf (gethash :button-release (object-set-table object-set)) fun))


;;;; Mouse service.

(defun serve-motion-notify (object-set fun)
  "Associate a method in the object-set with :motion-notify events.  The method
   is called on the object the event occurred, event key, event window, root,
   child, same-screen-p, x, y, root-x, root-y, state, time, hint-p, and
   send-event-p."
  (setf (gethash :motion-notify (object-set-table object-set)) fun))

(defun serve-enter-notify (object-set fun)
  "Associate a method in the object-set with :enter-notify events.  The method
   is called on the object the event occurred, event key, event window, root,
   child, same-screen-p, x, y, root-x, root-y, state, time, mode, kind,
   and send-event-p."
  (setf (gethash :enter-notify (object-set-table object-set)) fun))

(defun serve-leave-notify (object-set fun)
  "Associate a method in the object-set with :leave-notify events.  The method
   is called on the object the event occurred, event key, event window, root,
   child, same-screen-p, x, y, root-x, root-y, state, time, mode, kind,
   and send-event-p."
  (setf (gethash :leave-notify (object-set-table object-set)) fun))


;;;; Keyboard service.

(defun serve-focus-in (object-set fun)
  "Associate a method in the object-set with :focus-in events.  The method
   is called on the object the event occurred, event key, event window, mode,
   kind, and send-event-p."
  (setf (gethash :focus-in (object-set-table object-set)) fun))

(defun serve-focus-out (object-set fun)
  "Associate a method in the object-set with :focus-out events.  The method
   is called on the object the event occurred, event key, event window, mode,
   kind, and send-event-p."
  (setf (gethash :focus-out (object-set-table object-set)) fun))


;;;; Exposure service.

(defun serve-exposure (object-set fun)
  "Associate a method in the object-set with :exposure events.  The method
   is called on the object the event occurred, event key, event window, x, y,
   width, height, count, and send-event-p."
  (setf (gethash :exposure (object-set-table object-set)) fun))

(defun serve-graphics-exposure (object-set fun)
  "Associate a method in the object-set with :graphics-exposure events.  The
   method is called on the object the event occurred, event key, event window,
   x, y, width, height, count, major, minor, and send-event-p."
  (setf (gethash :graphics-exposure (object-set-table object-set)) fun))

(defun serve-no-exposure (object-set fun)
  "Associate a method in the object-set with :no-exposure events.  The method
   is called on the object the event occurred, event key, event window, major,
   minor, and send-event-p."
  (setf (gethash :no-exposure (object-set-table object-set)) fun))


;;;; Structure service.

(defun serve-visibility-notify (object-set fun)
  (setf (gethash :visibility-notify (object-set-table object-set)) fun))

(defun serve-create-notify (object-set fun)
  (setf (gethash :create-notify (object-set-table object-set)) fun))

(defun serve-destroy-notify (object-set fun)
  (setf (gethash :destroy-notify (object-set-table object-set)) fun))

(defun serve-unmap-notify (object-set fun)
  (setf (gethash :unmap-notify (object-set-table object-set)) fun))

(defun serve-map-notify (object-set fun)
  (setf (gethash :map-notify (object-set-table object-set)) fun))

(defun serve-map-request (object-set fun)
  (setf (gethash :map-request (object-set-table object-set)) fun))

(defun serve-reparent-notify (object-set fun)
  (setf (gethash :reparent-notify (object-set-table object-set)) fun))

(defun serve-configure-notify (object-set fun)
  (setf (gethash :configure-notify (object-set-table object-set)) fun))

(defun serve-gravity-notify (object-set fun)
  (setf (gethash :gravity-notify (object-set-table object-set)) fun))

(defun serve-resize-request (object-set fun)
  (setf (gethash :resize-request (object-set-table object-set)) fun))

(defun serve-configure-request (object-set fun)
  (setf (gethash :configure-request (object-set-table object-set)) fun))

(defun serve-circulate-notify (object-set fun)
  (setf (gethash :circulate-notify (object-set-table object-set)) fun))

(defun serve-circulate-request (object-set fun)
  (setf (gethash :circulate-request (object-set-table object-set)) fun))


;;;; Misc. service.

(defun serve-property-notify (object-set fun)
  (setf (gethash :property-notify (object-set-table object-set)) fun))

(defun serve-selection-clear (object-set fun)
  (setf (gethash :selection-clear (object-set-table object-set)) fun))

(defun serve-selection-request (object-set fun)
  (setf (gethash :selection-request (object-set-table object-set)) fun))

(defun serve-selection-notify (object-set fun)
  (setf (gethash :selection-notify (object-set-table object-set)) fun))

(defun serve-colormap-notify (object-set fun)
  (setf (gethash :colormap-notify (object-set-table object-set)) fun))

(defun serve-client-message (object-set fun)
  (setf (gethash :client-message (object-set-table object-set)) fun))
