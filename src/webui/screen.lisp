;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; WebUI display backend — screen / window initialization.
;;;

(in-package :hemlock.webui)

;;; Forward-declare functions defined in webui-input.lisp and webui-display.lisp.
(declaim (ftype (function (t t) t) webui-drain-events))
(declaim (ftype (function (t t) t) webui-key-callback))
(declaim (ftype (function (t t) t) webui-resize-callback))
(declaim (ftype (function () t) make-webui-editor-input))

;;;; Initial HTML page

(defparameter *webui-initial-html*
  "<!DOCTYPE html><html><head><meta charset=\"utf-8\">
<script src=\"webui.js\"></script>
<style>
  * { box-sizing: border-box; }
  body { background:#1e1e1e; color:#d4d4d4;
         font-family:'Courier New',Courier,monospace;
         font-size:14px; line-height:1.2; overflow:hidden; margin:0; padding:0; }
  #editor-container { display:flex; flex-direction:column; height:100vh; }
  .hem-window  { flex:1; white-space:pre; overflow:hidden; font-family:inherit; }
  .hem-echo    { height:2em; flex:none; white-space:pre; overflow:hidden; font-family:inherit; }
  .hem-ml      { background:#3a3a3a; color:#aaa; white-space:pre; overflow:hidden;
                 font-family:inherit; font-size:12px; padding:0 2px; }
  .cursor      { background:#528bff; color:#1e1e1e; }
  .fg-0{color:#000} .fg-1{color:#cc0000} .fg-2{color:#00aa00} .fg-3{color:#888800}
  .fg-4{color:#3366aa} .fg-5{color:#880088} .fg-6{color:#008888} .fg-7{color:#cccccc}
  .fg-8{color:#555555} .fg-9{color:#ff5555} .fg-10{color:#55ff55} .fg-11{color:#ffff55}
  .fg-12{color:#5555ff} .fg-13{color:#ff55ff} .fg-14{color:#55ffff} .fg-15{color:#ffffff}
  .bold{font-weight:bold} .underline{text-decoration:underline}
  .reverse{filter:invert(100%)} .italic{font-style:italic}
</style></head>
<body><div id=\"editor-container\">
  <div id=\"win0\"  class=\"hem-window\"></div>
  <div id=\"win0-ml\" class=\"hem-ml\"></div>
  <div id=\"echo\"  class=\"hem-echo\"></div>
  <div id=\"echo-ml\" class=\"hem-ml\"></div>
</div><script>
function updateWindow(id, linesJson, ml) {
  var lines = JSON.parse(linesJson);
  var el = document.getElementById(id);
  if (el) el.innerHTML = lines.join('\\n');
  var m = document.getElementById(id + '-ml');
  if (m) m.textContent = ml;
}
function updateLines(id, changesJson) {
  var changes = JSON.parse(changesJson);
  var el = document.getElementById(id);
  if (!el) return;
  var kids = el.children;
  for (var i = 0; i < changes.length; i++) {
    var row = changes[i][0], html = changes[i][1];
    if (kids[row]) kids[row].outerHTML = html;
  }
}
document.addEventListener('keydown', function(e) {
  var m = '';
  if (e.ctrlKey)  m += 'Control-';
  if (e.altKey)   m += 'Meta-';
  if (e.shiftKey && e.key.length > 1) m += 'Shift-';
  var k = (e.key === ' ') ? 'Space' : e.key;
  webui.call('hemlock_key', m + k).catch(function(){});
  e.preventDefault();
});
function _measureGrid() {
  var el = document.querySelector('.hem-window');
  if (!el) return;
  var style = window.getComputedStyle(el);
  var fs  = parseFloat(style.fontSize) || 14;
  var lh  = parseFloat(style.lineHeight) || (fs * 1.2);
  var canvas = document.createElement('canvas');
  var ctx = canvas.getContext('2d');
  ctx.font = style.fontWeight + ' ' + fs + 'px ' + style.fontFamily;
  var cw = ctx.measureText('M').width || 8;
  var cols = Math.max(20, Math.floor(window.innerWidth / cw));
  var rows = Math.max(4,  Math.floor(window.innerHeight / lh));
  webui.call('hemlock_resize', cols + ' ' + rows).catch(function(){});
}
window.addEventListener('load',   _measureGrid);
window.addEventListener('resize', _measureGrid);
</script></body></html>")


;;;; Device variables

(defvar *webui-device* nil)
(defvar *webui-pipe-read-fd*  nil)
(defvar *webui-pipe-write-fd* nil)


;;;; backend-init-raw-io

(defmethod hemlock.command::backend-init-raw-io ((backend (eql :webui)) display)
  (declare (ignore display))
  (setf *editor-windowed-input* nil)
  (multiple-value-bind (r w) (sb-posix:pipe)
    (setf *webui-pipe-read-fd* r
          *webui-pipe-write-fd* w))
  (setf *editor-input* (make-webui-editor-input))
  (setf *real-editor-input* *editor-input*))


;;;; %init-screen-manager

(defmethod hemlock.command::%init-screen-manager ((backend-type (eql :webui)) (display t))
  (let ((device (make-instance 'webui-device :name "webui")))
    (setf (webui-device-pipe-read-fd  device) *webui-pipe-read-fd*)
    (setf (webui-device-pipe-write-fd device) *webui-pipe-write-fd*)
    (init-webui-screen-manager device)))


;;;; init-webui-screen-manager — mirrors init-tty-screen-manager

(defun init-webui-screen-manager (device)
  (setf *webui-device* device)
  (setf hemlock.command::*line-wrap-char* #\!)
  (setf *window-list* ())
  (let* ((width           (webui-device-columns device))
         (height          (webui-device-lines   device))
         (echo-height     (value hemlock::echo-area-height))
         (main-lines      (- height echo-height 1))
         (main-text-lines (1- main-lines))
         (last-text-line  (1- main-text-lines)))
    (setf (device-bottom-window-base device) last-text-line)
    ;; Echo area
    (let* ((echo-hunk (make-webui-hunk
                       :position      (1- height)
                       :height        echo-height
                       :text-position (- height 2)
                       :text-height   echo-height
                       :device        device
                       :dom-id        "echo"))
           (echo (internal-make-window :hunk echo-hunk)))
      (setf *echo-area-window* echo)
      (setf (device-hunk-window echo-hunk) echo)
      (setup-window-image *parse-starting-mark* echo echo-height width)
      (setup-modeline-image *echo-area-buffer* echo)
      (setf (device-hunk-previous echo-hunk) echo-hunk
            (device-hunk-next     echo-hunk) echo-hunk)
      (prepare-window-for-redisplay echo))
    ;; Main window
    (let* ((main-hunk (make-webui-hunk
                       :position      main-text-lines
                       :height        main-lines
                       :text-position last-text-line
                       :text-height   main-text-lines
                       :device        device
                       :dom-id        "win0"))
           (main (internal-make-window :hunk main-hunk)))
      (setf (device-hunk-window main-hunk) main)
      (setf *current-window* main)
      (setup-window-image (buffer-point *current-buffer*)
                          main main-text-lines width)
      (setup-modeline-image *current-buffer* main)
      (prepare-window-for-redisplay main)
      (setf (device-hunk-previous main-hunk) main-hunk
            (device-hunk-next     main-hunk) main-hunk)
      (setf (device-hunks device) main-hunk))))


;;;; device-init / device-exit

(defmethod device-init ((device webui-device))
  (sb-int:with-float-traps-masked (:invalid :overflow :inexact :divide-by-zero
                                   :underflow)
    (let ((win (webui:webui-new-window)))
      (setf (webui-device-window-id device) win)
      ;; Bind callbacks BEFORE webui-show so no events are lost if the
      ;; browser connects and fires hemlock_resize before we get here.
      (webui:webui-bind win "hemlock_key"
        (lambda (event)
          (sb-int:with-float-traps-masked (:invalid :overflow :inexact
                                           :divide-by-zero :underflow)
            (when *webui-device*
              (webui-key-callback *webui-device* event)))))
      (webui:webui-bind win "hemlock_resize"
        (lambda (event)
          (sb-int:with-float-traps-masked (:invalid :overflow :inexact
                                           :divide-by-zero :underflow)
            (when *webui-device*
              (webui-resize-callback *webui-device* event)))))
      (sb-sys:add-fd-handler
       (webui-device-pipe-read-fd device)
       :input (let ((dev device))
                (lambda (fd) (webui-drain-events dev fd))))
      ;; Show the window. webui-wait is called on the main OS thread by
      ;; run-with-webui-event-loop (required by Cocoa/WebKit on macOS).
      (webui:webui-show win *webui-initial-html*))))

(defmethod device-exit ((device webui-device))
  nil)


;;;; run-with-webui-event-loop

;;; Run the Hemlock command loop (FUN) on a background thread while
;;; webui_wait runs on the calling thread.  On macOS, webui_wait uses
;;; Cocoa and WebKit internally and must run on the main OS thread;
;;; this function is called from call-with-editor instead of the
;;; normal (catch 'hemlock-exit (funcall fun)) form.
;;;
;;; Dynamic variables that the command loop needs are captured here
;;; (inside the let-bindings set up by call-with-editor and
;;; site-wrapper-macro) and re-bound in the background thread.
;;;
(defun run-with-webui-event-loop (fun)
  (let ((in-editor      *in-the-editor*)
        (default-backend *default-backend*))
    (let ((cmd-thread
           (sb-thread:make-thread
            (lambda ()
              (let ((*in-the-editor*    in-editor)
                    (*default-backend* default-backend))
                (catch 'hemlock-exit (funcall fun)))
              ;; Command loop exited — signal webui to shut down so
              ;; webui-wait returns on the main thread.
              (webui:webui-exit))
            :name "hemlock-command-loop")))
      ;; Main thread: run the webui event loop (required on macOS).
      (sb-int:with-float-traps-masked (:invalid :overflow :inexact
                                       :divide-by-zero :underflow)
        (webui:webui-wait))
      ;; webui-wait returned (all windows closed or webui-exit called).
      ;; Stop the command loop if it is still running.
      (when (sb-thread:thread-alive-p cmd-thread)
        (handler-case
            (sb-thread:interrupt-thread cmd-thread
                                        (lambda () (throw 'hemlock-exit nil)))
          (sb-thread:interrupt-thread-error ())))
      (sb-thread:join-thread cmd-thread :timeout 5 :default nil))))


;;;; device-clear

(defmethod device-clear ((device webui-device))
  (let ((win (webui-device-window-id device)))
    (when win
      (sb-int:with-float-traps-masked (:invalid :overflow :inexact :divide-by-zero
                                       :underflow)
        (webui:webui-run win
         "document.querySelectorAll('.hem-window,.hem-echo,.hem-ml').forEach(function(e){e.innerHTML='';e.textContent='';});")))))


;;;; window operations

(defmethod device-make-window ((device webui-device)
                               start modelinep window font-family
                               ask-user x y width height proportion)
  (declare (ignore window font-family ask-user x y width height))
  (let* ((old-window   (current-window))
         (victim       (window-hunk old-window))
         (text-height  (webui-hunk-text-height victim))
         (availability (if modelinep (1- text-height) text-height)))
    (when (> availability 1)
      (let* ((new-lines    (truncate (* availability proportion)))
             (old-lines    (- availability new-lines))
             (pos          (device-hunk-position victim))
             (new-height   (if modelinep (1+ new-lines) new-lines))
             (new-text-pos (if modelinep (1- pos) pos))
             (dom-id       (format nil "win~D" (length *window-list*)))
             (new-hunk     (make-webui-hunk
                            :position      pos
                            :height        new-height
                            :text-position new-text-pos
                            :text-height   new-lines
                            :device        device
                            :dom-id        dom-id))
             (new-window   (internal-make-window :hunk new-hunk)))
        (setf (device-hunk-window new-hunk) new-window)
        (let* ((pos-diff    (- pos (webui-hunk-text-position victim)))
               (old-new-pos (- pos new-height)))
          (decf (device-hunk-height  victim) new-height)
          (setf (webui-hunk-text-height  victim) old-lines)
          (setf (device-hunk-position victim) old-new-pos)
          (setf (webui-hunk-text-position victim) (- old-new-pos pos-diff)))
        (setup-window-image start new-window new-lines (window-width old-window))
        (prepare-window-for-redisplay new-window)
        (when modelinep
          (setup-modeline-image (line-buffer (mark-line start)) new-window))
        (change-window-image-height old-window old-lines)
        (shiftf (device-hunk-previous new-hunk)
                (device-hunk-previous (device-hunk-next victim))
                new-hunk)
        (shiftf (device-hunk-next new-hunk) (device-hunk-next victim) new-hunk)
        (setf *screen-image-trashed* t)
        new-window))))

(defmethod device-delete-window ((device webui-device) window)
  (let* ((hunk (window-hunk window))
         (prev (device-hunk-previous hunk))
         (next (device-hunk-next hunk)))
    (setf (device-hunk-next prev) next)
    (setf (device-hunk-previous next) prev)
    (let ((buffer (window-buffer window)))
      (setf (buffer-windows buffer)
            (delete window (buffer-windows buffer))))
    (let ((new-lines (device-hunk-height hunk)))
      (cond ((eq hunk (device-hunks (device-hunk-device next)))
             (incf (device-hunk-height next) new-lines)
             (incf (webui-hunk-text-height next) new-lines)
             (change-window-image-height (device-hunk-window next)
                                         (+ new-lines (window-height (device-hunk-window next)))))
            (t
             (incf (device-hunk-height   prev) new-lines)
             (incf (device-hunk-position prev) new-lines)
             (incf (webui-hunk-text-height   prev) new-lines)
             (incf (webui-hunk-text-position prev) new-lines)
             (change-window-image-height (device-hunk-window prev)
                                         (+ new-lines (window-height (device-hunk-window prev)))))))
    (when (eq hunk (device-hunks device))
      (setf (device-hunks device) next)))
  (setf *screen-image-trashed* t))

(defmethod device-next-window ((device webui-device) window)
  (device-hunk-window (device-hunk-next (window-hunk window))))

(defmethod device-previous-window ((device webui-device) window)
  (device-hunk-window (device-hunk-previous (window-hunk window))))


;;;; device-show-mark

(defmethod device-show-mark ((device webui-device) window x y time)
  (declare (ignore time))
  (cond ((listen-editor-input *editor-input*))
        (x
         (internal-redisplay)
         (let* ((hunk (window-hunk window))
                (dev  (device-hunk-device hunk)))
           (device-put-cursor dev hunk x y)
           (device-force-output dev))
         t)
        (t nil)))


;;;; webui-apply-resize — called on the main thread by webui-drain-events

;;; Apply a window resize that was signalled by the browser.  The new
;;; dimensions have already been written into the device by the callback.
;;; This runs on the main editor thread (fd-handler context), so it is safe
;;; to touch hemlock display structures.
;;;
;;; Strategy: adjust hunk geometry and resize each window's image with
;;; change-window-image-height (which does not re-register the window in
;;; *window-list* or buffer-windows, unlike setup-window-image).  Then
;;; set *screen-image-trashed* so the next redisplay pass does a full repaint.
(defun webui-apply-resize (device)
  (let* ((new-rows    (webui-device-lines   device))
         (new-cols    (webui-device-columns device))
         (echo-height (value hemlock::echo-area-height))
         ;; New main text rows: total rows - echo area - 1 modeline row.
         (new-main-text (max 1 (- new-rows echo-height 1))))
    (declare (ignore new-cols))
    ;; Update the echo area hunk — stays at the bottom with fixed height.
    (let ((echo-hunk (window-hunk *echo-area-window*)))
      (setf (device-hunk-position      echo-hunk) (1- new-rows)
            (device-hunk-height        echo-hunk) echo-height
            (webui-hunk-text-position  echo-hunk) (- new-rows 2)
            (webui-hunk-text-height    echo-hunk) echo-height)
      ;; Echo area text height rarely changes; update image height in case.
      (change-window-image-height *echo-area-window* echo-height))
    ;; Update the main window hunk (only the first/primary hunk for now;
    ;; for multi-window configurations we just trash and let redisplay fix it).
    (let ((first-hunk (device-hunks device)))
      (when first-hunk
        (let ((main-win   (device-hunk-window first-hunk))
              ;; Total hunk height includes 1 modeline row.
              (new-height (+ new-main-text 1)))
          (setf (device-hunk-position      first-hunk) new-main-text
                (device-hunk-height        first-hunk) new-height
                (webui-hunk-text-position  first-hunk) (1- new-main-text)
                (webui-hunk-text-height    first-hunk) new-main-text)
          (setf (device-bottom-window-base device) (1- new-main-text))
          (change-window-image-height main-win new-main-text))))
    (setf hemlock.command::*screen-image-trashed* t)))


;;;; Resize callback (called from webui thread)

(defun webui-resize-callback (device event)
  (let* ((s  (webui:webui-get-string event))
         (sp (position #\Space s))
         (cols (when sp (parse-integer s :end sp)))
         (rows (when sp (parse-integer s :start (1+ sp)))))
    (when (and cols rows (> cols 0) (> rows 0))
      (setf (webui-device-columns device) cols
            (webui-device-lines   device) rows)
      ;; Mark that the main thread needs to adjust the window layout.
      ;; webui-drain-events reads this flag on the main thread and calls
      ;; webui-apply-resize safely (hemlock data structures are not
      ;; thread-safe, so we cannot do it here).
      (setf (webui-device-resize-pending device) t))
    ;; Wake the main thread via the self-pipe regardless of whether the
    ;; size actually changed (also wakes it for the initial page-load resize).
    (cffi:with-foreign-object (b :uint8)
      (setf (cffi:mem-ref b :uint8) 1)
      (cffi:foreign-funcall "write"
                            :int  (webui-device-pipe-write-fd device)
                            :pointer b
                            :size 1
                            :long))))
