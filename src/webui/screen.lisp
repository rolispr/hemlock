;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; WebUI display backend: screen and window initialization.
;;;

(in-package :hemlock.webui)

;;; Forward-declare functions defined in webui-input.lisp and webui-display.lisp.
(declaim (ftype (function (t t) t) webui-drain-events))
(declaim (ftype (function (t t) t) webui-key-callback))
(declaim (ftype (function (t t) t) webui-resize-callback))
(declaim (ftype (function (t t) t) webui-scroll-callback))
(declaim (ftype (function () t) make-webui-editor-input))

;;;; Initial HTML page

(defun generate-palette-css ()
  (with-output-to-string (out)
                         (dotimes (i 16)
                           (let ((rgb (hemlock.term:color-index-to-rgb i)))
                             (when rgb
                               (format out ".fg-~D{color:rgb(~D,~D,~D)} .bg-~D{background:rgb(~D,~D,~D)} "
                                       i (aref rgb 0) (aref rgb 1) (aref rgb 2)
                                       i (aref rgb 0) (aref rgb 1) (aref rgb 2)))))))

(defparameter *webui-initial-html*
              (concatenate 'string
                           "<!DOCTYPE html><html><head><meta charset=\"utf-8\">
<script src=\"webui.js\"></script>
<style>
  :root { --lh: 1.2; --fs: 14px; --line: calc(var(--fs) * var(--lh)); }
  * { box-sizing: border-box; margin:0; padding:0; }
  body { background:#1e1e1e; color:#d4d4d4;
         font-family:'Courier New',Courier,monospace;
         font-size:var(--fs); line-height:var(--lh); overflow:hidden; }
  #editor-container { height:100vh; display:flex; flex-direction:column; }
  .hem-window  { overflow-y:auto; overflow-x:hidden; white-space:pre; background:#252526; flex:1; }
  .hem-echo    { overflow:hidden; white-space:pre; background:#252526; }
  .hem-ml      { height:var(--line); background:#3a3a3a; color:#ccc;
                 white-space:pre; overflow:hidden; flex:none; }
  .cursor      { background:#528bff; color:#fff; }
  "
                           (generate-palette-css)
                           "
  .bold{font-weight:bold} .underline{text-decoration:underline}
  .reverse{filter:invert(100%)} .italic{font-style:italic}
</style></head>
<body><div id=\"editor-container\">
  <div id=\"win0\"  class=\"hem-window\"></div>
  <div id=\"win0-ml\" class=\"hem-ml\"></div>
  <div id=\"echo\"  class=\"hem-echo\"></div>
  <div id=\"echo-ml\" class=\"hem-ml\"></div>
</div><script>
var _lh, _cw;
var _rendered = {};
function _initMetrics() {
  var el = document.querySelector('.hem-window') || document.body;
  var s = window.getComputedStyle(el);
  var fs = parseFloat(s.fontSize) || 14;
  _lh = parseFloat(s.lineHeight) || (fs * 1.2);
  var c = document.createElement('canvas').getContext('2d');
  c.font = s.fontWeight + ' ' + fs + 'px ' + s.fontFamily;
  _cw = c.measureText('M').width || 8;
}
var _bufLines = {};
var _bufMl = {};
function hemSetLines(id, htmlLines, ml) {
  _bufLines[id] = htmlLines;
  _bufMl[id] = ml;
  hemPaint(id);
  var m = document.getElementById(id + '-ml');
  if (m) m.textContent = ml;
}
function hemPaint(id) {
  var el = document.getElementById(id);
  if (!el || !_lh) return;
  var lines = _bufLines[id];
  if (!lines) return;
  var total = lines.length;
  var spacer = el.querySelector('.hem-spacer');
  if (!spacer) {
    spacer = document.createElement('div');
    spacer.className = 'hem-spacer';
    spacer.style.position = 'relative';
    el.innerHTML = '';
    el.appendChild(spacer);
  }
  spacer.style.height = (total * _lh) + 'px';
  var content = spacer.querySelector('.hem-content');
  if (!content) {
    content = document.createElement('div');
    content.className = 'hem-content';
    content.style.position = 'absolute';
    content.style.left = '0';
    content.style.right = '0';
    spacer.appendChild(content);
  }
  var first = Math.max(0, Math.floor(el.scrollTop / _lh) - 20);
  var last = Math.min(total, Math.ceil((el.scrollTop + el.clientHeight) / _lh) + 20);
  content.style.top = (first * _lh) + 'px';
  content.innerHTML = lines.slice(first, last).join('');
}
function hemEnsureVisible(id, lineIdx) {
  var el = document.getElementById(id);
  if (!el || !_lh) return;
  var top = lineIdx * _lh;
  var bot = top + _lh;
  if (top < el.scrollTop) {
    el.scrollTop = top;
  } else if (bot > el.scrollTop + el.clientHeight) {
    el.scrollTop = bot - el.clientHeight;
  }
  hemPaint(id);
}
document.addEventListener('keydown', function(e) {
  var m = '';
  if (e.ctrlKey)  m += 'Control-';
  var isMeta = e.altKey || e.metaKey;
  if (isMeta) m += 'Meta-';
  if (e.shiftKey && e.key.length > 1) m += 'Shift-';
  var k;
  if (isMeta && e.code && e.code.startsWith('Key')) {
    k = e.shiftKey ? e.code.charAt(3).toUpperCase() : e.code.charAt(3).toLowerCase();
  } else if (isMeta && e.code && e.code.startsWith('Digit')) {
    k = e.code.charAt(5);
  } else {
    k = (e.key === ' ') ? 'Space' : e.key;
  }
  webui.call('hemlock_key', m + k).catch(function(){});
  e.preventDefault();
});
function _measure() {
  if (!_lh) _initMetrics();
  var wins = document.querySelectorAll('.hem-window');
  var result = [];
  wins.forEach(function(el) {
    var rows = Math.floor(el.clientHeight / _lh);
    result.push(el.id + ':' + rows);
  });
  var cols = Math.max(20, Math.floor(window.innerWidth / _cw));
  webui.call('hemlock_resize', cols + ' ' + result.join(' ')).catch(function(){});
}
function _measureRetry(n) {
  _measure();
  if (n > 0) setTimeout(function() { _measureRetry(n-1); }, 150);
}
window.addEventListener('load', function() { _initMetrics(); _measureRetry(40); });
window.addEventListener('resize', _measure);
document.addEventListener('scroll', function(e) {
  var el = e.target;
  if (el.classList && el.classList.contains('hem-window')) hemPaint(el.id);
}, true);
</script></body></html>"))


;;;; Device variables

(defvar *webui-device* nil)
(defvar *webui-pipe-read-fd*  nil)
(defvar *webui-pipe-write-fd* nil)


;;;; backend-init-raw-io

(defmethod backend-init-raw-io ((backend (eql :webui)) display)
           (declare (ignore display))
           (setf *editor-windowed-input* nil)
           (multiple-value-bind (r w) (sb-posix:pipe)
                                (setf *webui-pipe-read-fd* r
                                      *webui-pipe-write-fd* w))
           (setf *editor-input* (make-webui-editor-input))
           (setf *real-editor-input* *editor-input*))


;;;; %init-screen-manager

(defmethod %init-screen-manager ((backend-type (eql :webui)) (display t))
           (let ((device (make-instance 'webui-device :name "webui")))
             (setf (webui-device-pipe-read-fd  device) *webui-pipe-read-fd*)
             (setf (webui-device-pipe-write-fd device) *webui-pipe-write-fd*)
             (init-webui-screen-manager device)))


;;;; init-webui-screen-manager.  Mirrors init-tty-screen-manager.

(defun init-webui-screen-manager (device)
  (setf *webui-device* device)
  (setf hemlock.command::*color-support* :truecolor)
  (setf *line-wrap-char* #\!)
  (setf *window-list* ())
  (let* ((width           (webui-device-columns device))
         (height          (webui-device-lines   device))
         (echo-height     (value hemlock::echo-area-height))
         ;; height from browser = total rows for entire viewport.
         ;; Subtract: echo text lines, echo modeline, main modeline.
         (main-lines      (- height echo-height 1))
         (main-text-lines (max 1 (1- main-lines)))
         (last-text-line  (max 0 (1- main-text-lines))))
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
                                             (webui:webui-bind win "hemlock_scroll"
                                                               (lambda (event)
                                                                 (sb-int:with-float-traps-masked (:invalid :overflow :inexact
                                                                                                           :divide-by-zero :underflow)
                                                                                                 (when *webui-device*
                                                                                                   (webui-scroll-callback *webui-device* event)))))
                                             ;; Do NOT call sb-sys:add-fd-handler here.
                                             ;; *descriptor-handlers* is thread-local in SBCL.  device-init runs on
                                             ;; the main OS thread, but dispatch-events/serve-event run on the
                                             ;; hemlock command-loop background thread.  The handler is registered
                                             ;; in run-with-webui-event-loop, on the background thread, so that
                                             ;; serve-event actually polls the fd.
                                             ;;
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
                ;; sb-sys:*descriptor-handlers* is thread-local in SBCL.
                ;; Register the self-pipe handler HERE so that serve-event
                ;; (called from dispatch-events on this thread) actually
                ;; polls the fd.  device-init runs on the main OS thread,
                ;; so the registration done there would be invisible here.
                (let ((dev *webui-device*))
                  (sb-sys:add-fd-handler
                   (webui-device-pipe-read-fd dev)
                   :input (lambda (fd)
                            (webui-drain-events dev fd)))
                  ;; Also register the ioconnections wakeup pipe on this
                  ;; thread so serve-event sees TS completions and
                  ;; invoke-later callbacks.
                  (when hemlock.io::*wakeup-read-fd*
                    (sb-sys:add-fd-handler
                     hemlock.io::*wakeup-read-fd*
                     :input (lambda (fd)
                              (cffi:with-foreign-object (buf :char 64)
                                                        (cffi:foreign-funcall "read"
                                                                              :int fd :pointer buf :size 64 :long))
                              (hemlock.io:drain-pending-invocations)))))
                (catch 'hemlock-exit (funcall fun)))
              ;; command loop exited; signal webui to shut down so
              ;; webui-wait returns on the main thread
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


;;;; Sync CSS flex heights to hemlock hunk allocations.

(defun webui-sync-flex-heights (device)
  "Update CSS flex proportions for split windows."
  (let ((win-id (webui-device-window-id device)))
    (when win-id
      (sb-int:with-float-traps-masked (:invalid :overflow :inexact :divide-by-zero
                                                :underflow)
                                      (dolist (w *window-list*)
                                        (let* ((hunk (window-hunk w))
                                               (dom-id (webui-hunk-dom-id hunk))
                                               (text-h (webui-hunk-text-height hunk)))
                                          (webui:webui-run win-id
                                                           (format nil "setWindowRows('~A',~D);"
                                                                   (json-escape dom-id) text-h))))))))


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
                 ;; Create DOM elements for the new window.  Insert right after
                 ;; the old window's modeline div so visual order matches hemlock.
                 (let ((win (webui-device-window-id device))
                       (old-dom-id (webui-hunk-dom-id victim)))
                   (when win
                     (sb-int:with-float-traps-masked (:invalid :overflow :inexact
                                                               :divide-by-zero :underflow)
                                                     (webui:webui-run win
                                                                      (format nil "(function(){var c=document.getElementById('editor-container');var anchor=document.getElementById('~A-ml');var ref=anchor?anchor.nextSibling:document.getElementById('echo');var w=document.createElement('div');w.id='~A';w.className='hem-window';c.insertBefore(w,ref);~A})();"
                                                                              (json-escape old-dom-id)
                                                                              (json-escape dom-id)
                                                                              (if modelinep
                                                                                  (format nil "var m=document.createElement('div');m.id='~A-ml';m.className='hem-ml';c.insertBefore(m,ref);"
                                                                                          (json-escape dom-id))
                                                                                ""))))))
                 (webui-sync-flex-heights device)
                 (setf *screen-image-trashed* t)
                 new-window))))

(defmethod device-delete-window ((device webui-device) window)
           (let* ((hunk (window-hunk window))
                  (dom-id (webui-hunk-dom-id hunk))
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
               (setf (device-hunks device) next))
             ;; Remove DOM elements for the deleted window.
             (let ((win (webui-device-window-id device)))
               (when win
                 (sb-int:with-float-traps-masked (:invalid :overflow :inexact
                                                           :divide-by-zero :underflow)
                                                 (webui:webui-run win
                                                                  (format nil "(function(){var w=document.getElementById('~A');if(w)w.remove();var m=document.getElementById('~A-ml');if(m)m.remove();})();"
                                                                          (json-escape dom-id) (json-escape dom-id)))))))
           (webui-sync-flex-heights device)
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


;;;; webui-resize-window-width.  Reallocate dis-line arrays for new column count.

(defun webui-resize-window-width (window new-width)
  "Adjust WINDOW's dis-line arrays and width for a new column count.
Nuke all active lines back to spare, reallocate if wider, and rebuild modeline."
  (let ((old-width (window-width window)))
    ;; Move all active dis-lines back to the spare list.
    (unless (eq (cdr (window-first-line window)) the-sentinel)
      (shiftf (cdr (window-last-line window))
              (window-spare-lines window)
              (cdr (window-first-line window))
              the-sentinel))
    ;; Reallocate dis-line char arrays when the window got wider.
    (when (> new-width old-width)
      (dolist (dl (window-spare-lines window))
        (setf (dis-line-chars dl) (make-string new-width))))
    (setf (window-width window) new-width)
    ;; Rebuild modeline dis-line if present.
    (when (window-modeline-buffer window)
      (let* ((dl (window-modeline-dis-line window))
             (chars (make-string new-width))
             (len (min new-width (window-modeline-buffer-len window))))
        (setf (dis-line-old-chars dl) nil
              (dis-line-chars dl) chars)
        (replace chars (window-modeline-buffer window) :end1 len :end2 len)
        (setf (dis-line-length dl) len
              (dis-line-flags dl) changed-bit)))))


;;;; webui-apply-resize.  Called on the main thread by webui-drain-events.

;;; Apply a window resize that was signalled by the browser.  The new
;;; dimensions have already been written into the device by the callback.
;;; This runs on the main editor thread (fd-handler context), so it is safe
;;; to touch hemlock display structures.
;;;
;;; Mirrors resize-tty-layout: redistributes height proportionally among
;;; all hunks, reallocates dis-line arrays on width change, and removes
;;; extra split windows if the terminal shrinks too much.
(defun webui-apply-resize (device)
  (let* ((new-cols (webui-device-columns device))
         (width-changed (and (device-hunks device)
                             (/= new-cols (window-width
                                           (device-hunk-window
                                            (device-hunks device)))))))
    ;; Handle width changes.
    (when width-changed
      (dolist (w *window-list*)
        (webui-resize-window-width w new-cols))
      (webui-resize-window-width *echo-area-window* new-cols))
    ;; Set each window's height from the browser's per-window measurement.
    (let ((vp *pending-viewport-rows*))
      (dolist (w *window-list*)
        (let* ((hunk (window-hunk w))
               (dom-id (webui-hunk-dom-id hunk))
               (entry (assoc dom-id vp :test #'string=))
               (rows (if entry (cdr entry) (webui-hunk-text-height hunk))))
          (when (and rows (> rows 0))
            (let ((has-ml (not (null (window-modeline-buffer w)))))
              (setf (webui-hunk-text-height hunk) rows
                    (device-hunk-height hunk) (if has-ml (1+ rows) rows))
              (change-window-image-height w rows)))))
      ;; Echo area.
      (let* ((echo-entry (assoc "echo" vp :test #'string=))
             (echo-rows (if echo-entry (cdr echo-entry) 3))
             (echo-hunk (window-hunk *echo-area-window*)))
        (setf (webui-hunk-text-height echo-hunk) echo-rows
              (device-hunk-height echo-hunk) echo-rows)
        (change-window-image-height *echo-area-window* echo-rows)))
    ;; Recompute modeline fields for new width.
    (dolist (w *window-list*)
      (when (window-modeline-buffer w)
        (update-modeline-fields (window-buffer w) w)))
    (dolist (w *window-list*)
      (when (fboundp 'hemlock::terminal-resize-to-window)
        (funcall 'hemlock::terminal-resize-to-window (window-buffer w) w))))
  (webui-sync-flex-heights device)
  (setf *screen-image-trashed* t))


;;;; Resize callback (called from webui thread)

(defvar *pending-viewport-rows* nil
  "Alist of (dom-id . rows) from the latest resize callback.")

(defun webui-resize-callback (device event)
  "Parse 'cols win0:rows win1:rows ...' from browser resize."
  (let* ((s (webui:webui-get-string event))
         (parts (when (stringp s) (uiop:split-string s :separator " ")))
         (cols (when parts (parse-integer (first parts) :junk-allowed t))))
    (when (and cols (> cols 0))
      (setf (webui-device-columns device) cols)
      ;; Parse per-window viewport rows: "win0:30 win1:15 ..."
      (let ((vp nil))
        (dolist (p (rest parts))
          (let ((colon (position #\: p)))
            (when colon
              (let ((id (subseq p 0 colon))
                    (rows (parse-integer (subseq p (1+ colon)) :junk-allowed t)))
                (when (and rows (> rows 0))
                  (push (cons id rows) vp))))))
        (setf *pending-viewport-rows* (nreverse vp)))
      ;; Compute total lines for hemlock internals.
      (let ((total-text 0))
        (dolist (entry *pending-viewport-rows*)
          (unless (string= (car entry) "echo")
            (incf total-text (cdr entry))))
        (let ((echo-height (webui-hunk-text-height (window-hunk *echo-area-window*))))
          (setf (webui-device-main-text-rows device) total-text
                (webui-device-lines device) (+ total-text echo-height 2)))))
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
                                                  :long)))

(defvar *pending-scroll* nil
  "Cons of (dom-id . first-visible-line) set by scroll callback, consumed by drain.")

(defun webui-scroll-callback (device event)
  (let* ((s (webui:webui-get-string event))
         (parts (when (stringp s) (uiop:split-string s :separator " ")))
         (dom-id (first parts))
         (first-line (when (>= (length parts) 2)
                       (parse-integer (second parts) :junk-allowed t))))
    (when (and dom-id first-line)
      (setf *pending-scroll* (cons dom-id first-line))
      (cffi:with-foreign-object (b :uint8)
        (setf (cffi:mem-ref b :uint8) 1)
        (cffi:foreign-funcall "write"
                              :int  (webui-device-pipe-write-fd device)
                              :pointer b
                              :size 1
                              :long)))))
