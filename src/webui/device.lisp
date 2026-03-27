;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; WebUI display backend — device and hunk class definitions.
;;;

(in-package :hemlock.webui)

;;; Register the backend.
(pushnew :webui *available-backends*)

;;;; Device class

(defclass webui-device (device)
  ((webui-window-id
    :accessor webui-device-window-id
    :initform nil
    :documentation "libwebui window ID (size-t).")
   (lines
    :accessor webui-device-lines
    :initarg :lines
    :initform 40
    :documentation "Number of text rows.")
   (main-text-rows
    :accessor webui-device-main-text-rows
    :initform nil
    :documentation "Actual main window text rows as measured by browser.")
   (columns
    :accessor webui-device-columns
    :initarg :columns
    :initform 120
    :documentation "Number of text columns.")
   (pipe-read-fd
    :accessor webui-device-pipe-read-fd
    :initform nil
    :documentation "Read end of self-pipe (main-thread fd-handler).")
   (pipe-write-fd
    :accessor webui-device-pipe-write-fd
    :initform nil
    :documentation "Write end of self-pipe (written from webui callback thread).")
   (input-queue
    :accessor webui-device-input-queue
    :initform (sb-concurrency:make-queue)
    :documentation "Thread-safe queue of key-events from the webui callback thread.")
   (dirty-windows
    :accessor webui-device-dirty-windows
    :initform nil
    :documentation "List of window objects needing redisplay; rendered in device-force-output.")
   (cursor-x
    :accessor webui-device-cursor-x
    :initform 0)
   (cursor-y
    :accessor webui-device-cursor-y
    :initform 0)
   (resize-pending
    :accessor webui-device-resize-pending
    :initform nil
    :documentation "Set to T by the resize callback; cleared by webui-drain-events after rebuilding windows.")))

;;;; Hunk class

;;; Mirrors tty-hunk's extra slots so the portable window-split / resize
;;; arithmetic is unchanged.
(defclass webui-hunk (device-hunk)
  ((text-height
    :accessor webui-hunk-text-height
    :initarg :text-height)
   (text-position
    :accessor webui-hunk-text-position
    :initarg :text-position)
   (dom-id
    :accessor webui-hunk-dom-id
    :initarg :dom-id
    :documentation "HTML element id for this hunk's text <div>.")))

(defun make-webui-hunk (&rest args
                        &key position height text-position text-height device dom-id)
  (declare (ignore position height text-position text-height device dom-id))
  (apply #'make-instance 'webui-hunk args))
