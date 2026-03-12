;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; WebUI display backend — editor input class and key translation.
;;;

(in-package :hemlock.webui)

;;; Forward-declare webui-apply-resize defined in webui-screen.lisp.
(declaim (ftype (function (t) t) webui-apply-resize))


;;;; webui-editor-input

(defclass webui-editor-input (editor-input) ())

(defun make-webui-editor-input ()
  (make-instance 'webui-editor-input))

(defmethod get-key-event ((stream webui-editor-input)
                          &optional ignore-abort-attempts-p)
  (%editor-input-method stream ignore-abort-attempts-p))

(defmethod unget-key-event (key-event (stream webui-editor-input))
  (un-event key-event stream))

(defmethod clear-editor-input ((stream webui-editor-input))
  (hemlock-ext:without-interrupts
   (let* ((head (editor-input-head stream))
          (next (input-event-next head)))
     (when next
       (setf (input-event-next head) nil)
       (shiftf (input-event-next (editor-input-tail stream))
               *free-input-events* next)
       (setf (editor-input-tail stream) head)))))

(defmethod listen-editor-input ((stream webui-editor-input))
  ;; Return T if at least one key-event has already been drained from the
  ;; self-pipe and enqueued into the editor-input queue.  This lets
  ;; redisplay-loop abort early when there is pending input, and prevents
  ;; %editor-input-method from spinning without ever calling dispatch-events
  ;; when input arrives between redisplay passes.
  (not (null (input-event-next (editor-input-head stream)))))


;;;; Self-pipe drain — called on the main thread by the fd-handler

(defun webui-drain-events (device fd)
  ;; Drain wake bytes (content doesn't matter).
  (let ((buf (make-array 64 :element-type '(unsigned-byte 8))))
    (cffi:with-pointer-to-vector-data (p buf)
      (cffi:foreign-funcall "read" :int fd :pointer p :size 64 :long)))
  ;; Push queued key-events into hemlock's editor-input.
  (loop for ke = (sb-concurrency:dequeue (webui-device-input-queue device))
        while ke
        do (q-event *real-editor-input* ke))
  ;; If the browser reported a new window size, update hemlock's windows
  ;; on the main thread (hemlock display structures are not thread-safe).
  ;; We set *screen-image-trashed* so the next redisplay pass does a full
  ;; refresh, and we update each window's image to match the new dimensions.
  (when (webui-device-resize-pending device)
    (hemlock-ext:without-interrupts
      (setf (webui-device-resize-pending device) nil)
      (webui-apply-resize device))))


;;;; webui callback — runs in the libwebui thread

(defun webui-key-callback (device event)
  (let* ((key-str (webui:webui-get-string event))
         (ke      (webui-str->key-event key-str)))
    (when ke
      (sb-concurrency:enqueue ke (webui-device-input-queue device))
      (cffi:with-foreign-object (b :uint8)
        (setf (cffi:mem-ref b :uint8) 1)
        (cffi:foreign-funcall "write"
                              :int  (webui-device-pipe-write-fd device)
                              :pointer b
                              :size 1
                              :long)))))


;;;; Key translation

(defvar *webui-key-translations* (make-hash-table :test #'equal))

(defun init-webui-key-translations ()
  (flet ((reg (js-name hemlock-key)
           (setf (gethash js-name *webui-key-translations*) hemlock-key)))
    (reg "F1"  #k"F1")  (reg "F2"  #k"F2")  (reg "F3"  #k"F3")
    (reg "F4"  #k"F4")  (reg "F5"  #k"F5")  (reg "F6"  #k"F6")
    (reg "F7"  #k"F7")  (reg "F8"  #k"F8")  (reg "F9"  #k"F9")
    (reg "F10" #k"F10") (reg "F11" #k"F11") (reg "F12" #k"F12")
    (reg "ArrowUp"    #k"Uparrow")
    (reg "ArrowDown"  #k"Downarrow")
    (reg "ArrowRight" #k"Rightarrow")
    (reg "ArrowLeft"  #k"Leftarrow")
    (reg "Home"       #k"Home")
    (reg "End"        #k"End")
    (reg "PageUp"     #k"Pageup")
    (reg "PageDown"   #k"Pagedown")
    (reg "Insert"     #k"Insert")
    (reg "Delete"     #k"Delete")
    (reg "Backspace"  #k"Backspace")
    (reg "Tab"        #k"Tab")
    (reg "Enter"      #k"Return")
    (reg "Escape"     #k"Escape")
    (reg "Space"      #k"Space")
    ;; Modifier-only: ignore
    (reg "Control"  nil) (reg "Alt"     nil)
    (reg "Shift"    nil) (reg "Meta"    nil)
    (reg "CapsLock" nil) (reg "NumLock" nil)))

(init-webui-key-translations)


;;; Parse "Control-Meta-Shift-BaseKey" into a hemlock key-event.

(defun webui-str->key-event (str)
  (when (and str (plusp (length str)))
    (let ((ctrl  nil)
          (meta  nil)
          (shift nil))
      (loop
        (cond
          ((and (> (length str) 8) (string= str "Control-" :end1 8))
           (setf ctrl t str (subseq str 8)))
          ((and (> (length str) 5) (string= str "Meta-" :end1 5))
           (setf meta t str (subseq str 5)))
          ((and (> (length str) 6) (string= str "Shift-" :end1 6))
           (setf shift t str (subseq str 6)))
          (t (return))))
      (let ((ke (or (gethash str *webui-key-translations*)
                    (when (= (length str) 1)
                      (char-key-event (char str 0))))))
        (when ke
          (if (or ctrl meta shift)
              (let ((bits (apply #'make-key-event-bits
                                 (append (when ctrl  '("Control"))
                                         (when meta   '("Meta"))
                                         (when shift  '("Shift"))))))
                (make-key-event ke bits))
              ke))))))
