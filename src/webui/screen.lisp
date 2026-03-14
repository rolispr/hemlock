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
  * { box-sizing: border-box; }
  body { background:#1e1e1e; color:#d4d4d4;
         font-family:'Courier New',Courier,monospace;
         font-size:14px; line-height:1.2; overflow:hidden; margin:0; padding:0; }
  #editor-container { display:flex; flex-direction:column; height:100vh; }
  .hem-window  { flex:1; white-space:pre; overflow:hidden; font-family:inherit; background:#252526; }
  .hem-echo    { height:3.6em; flex:none; white-space:pre; overflow:hidden; font-family:inherit; background:#252526; }
  .hem-ml      { height:1.2em; background:#3a3a3a; color:#ccc; white-space:pre; overflow:hidden;
                 font-family:inherit; flex:none; }
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
function updateWindow(id, lines, ml) {
  var el = document.getElementById(id);
  if (el) el.innerHTML = lines.join('');
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
function _getGridSize() {
  var el = document.querySelector('.hem-window');
  if (!el) return null;
  var style = window.getComputedStyle(el);
  var fs  = parseFloat(style.fontSize) || 14;
  var lh  = parseFloat(style.lineHeight) || (fs * 1.2);
  var canvas = document.createElement('canvas');
  var ctx = canvas.getContext('2d');
  ctx.font = style.fontWeight + ' ' + fs + 'px ' + style.fontFamily;
  var cw = ctx.measureText('M').width || 8;
  var rect = el.getBoundingClientRect();
  var cols = Math.max(20, Math.floor(rect.width / cw));
  var mainRows = Math.floor(rect.height / lh);
  var fixedPx = 0;
  document.querySelectorAll('.hem-echo, .hem-ml').forEach(function(e) {
    fixedPx += e.getBoundingClientRect().height;
  });
  var rows = Math.max(4, mainRows + Math.round(fixedPx / lh));
  return cols + ' ' + rows;
}
function _measureGrid() {
  var sz = _getGridSize();
  if (!sz) return;
  webui.call('hemlock_resize', sz).catch(function(){});
}
function _measureGridWithRetry(attemptsLeft) {
  var sz = _getGridSize();
  if (!sz) return;
  webui.call('hemlock_resize', sz).catch(function() {
    if (attemptsLeft > 0) {
      setTimeout(function() { _measureGridWithRetry(attemptsLeft - 1); }, 150);
    }
  });
}
window.addEventListener('load',   function() { _measureGridWithRetry(40); });
window.addEventListener('resize', _measureGrid);
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


;;;; init-webui-screen-manager — mirrors init-tty-screen-manager

(defun init-webui-screen-manager (device)
  (setf *webui-device* device)
  (setf *line-wrap-char* #\!)
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
      ;; NOTE: do NOT call sb-sys:add-fd-handler here.
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
                            (webui-drain-events dev fd))))
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


;;;; webui-resize-window-width — reallocate dis-line arrays for new column count

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


;;;; webui-apply-resize — called on the main thread by webui-drain-events

;;; Apply a window resize that was signalled by the browser.  The new
;;; dimensions have already been written into the device by the callback.
;;; This runs on the main editor thread (fd-handler context), so it is safe
;;; to touch hemlock display structures.
;;;
;;; Mirrors resize-tty-layout: redistributes height proportionally among
;;; all hunks, reallocates dis-line arrays on width change, and removes
;;; extra split windows if the terminal shrinks too much.
(defun webui-apply-resize (device)
  (let* ((new-lines   (webui-device-lines   device))
         (new-cols    (webui-device-columns device))
         (width-changed (/= new-cols (window-width
                                      (device-hunk-window
                                       (device-hunks device)))))
         (echo-hunk (window-hunk *echo-area-window*))
         (echo-text-height (webui-hunk-text-height echo-hunk))
         ;; Available lines for non-echo hunks: total minus echo minus
         ;; echo modeline.
         (available (- new-lines echo-text-height 1)))
    ;; If terminal is too small for even one window + echo, bail out.
    (when (< available 2)
      (return-from webui-apply-resize))
    ;; Handle width changes for all windows (including echo).
    (when width-changed
      (dolist (w *window-list*)
        (webui-resize-window-width w new-cols))
      (webui-resize-window-width *echo-area-window* new-cols))
    ;; Collect non-echo hunks in display order.
    (let* ((first (device-hunks device))
           (hunks (list first))
           (old-total 0))
      (do ((h (device-hunk-next first) (device-hunk-next h)))
          ((eq h first))
        (push h hunks))
      (setf hunks (nreverse hunks))
      (dolist (h hunks) (incf old-total (device-hunk-height h)))
      ;; Delete extra windows if terminal shrinks too much.
      ;; Each window needs at least 2 lines (1 text + 1 modeline).
      (loop while (and (> (length hunks) 1)
                       (< available (* 2 (length hunks))))
            do (let* ((victim-hunk (car (last hunks)))
                      (victim-window (device-hunk-window victim-hunk))
                      (prev (device-hunk-previous victim-hunk))
                      (next (device-hunk-next victim-hunk)))
                 ;; Unlink from the hunk ring.
                 (setf (device-hunk-next prev) next
                       (device-hunk-previous next) prev)
                 ;; Remove from window/buffer lists.
                 (setf *window-list* (delete victim-window *window-list*))
                 (let ((buf (window-buffer victim-window)))
                   (when buf
                     (setf (buffer-windows buf)
                           (delete victim-window (buffer-windows buf)))))
                 (when (eq victim-hunk (device-hunks device))
                   (setf (device-hunks device) next))
                 ;; If we just killed the current window, switch to first.
                 (when (eq victim-window *current-window*)
                   (setf *current-window*
                         (device-hunk-window (car hunks))))
                 (decf old-total (device-hunk-height victim-hunk))
                 (setf hunks (butlast hunks))))
      ;; Ensure old-total is positive for proportion calculation.
      (when (zerop old-total) (setf old-total 1))
      ;; Redistribute heights proportionally, building top-down.
      (let ((remaining available)
            (row 0))
        (loop for (hunk . rest) on hunks
              for has-modeline = (not (null (window-modeline-buffer
                                            (device-hunk-window hunk))))
              for min-h = (if has-modeline 2 1)
              for new-h = (if rest
                              (max min-h
                                   (round (* available
                                            (/ (device-hunk-height hunk)
                                               old-total))))
                              ;; Last hunk gets the remainder.
                              (max min-h remaining))
              for new-text-h = (if has-modeline (1- new-h) new-h)
              do (setf (device-hunk-height hunk) new-h
                       (webui-hunk-text-height hunk) new-text-h
                       (webui-hunk-text-position hunk) (+ row new-text-h -1)
                       (device-hunk-position hunk)
                       (if has-modeline (+ row new-h -1)
                           (+ row new-text-h -1)))
                 (change-window-image-height (device-hunk-window hunk)
                                             new-text-h)
                 (decf remaining new-h)
                 (incf row new-h))
        ;; Update device-bottom-window-base.
        (let ((last-hunk (car (last hunks))))
          (setf (device-bottom-window-base device)
                (webui-hunk-text-position last-hunk)))
        ;; Echo area: fixed at the bottom.
        (setf (device-hunk-position echo-hunk) (1- new-lines)
              (device-hunk-height echo-hunk) echo-text-height
              (webui-hunk-text-position echo-hunk) (- new-lines 2)
              (webui-hunk-text-height echo-hunk) echo-text-height)
        (change-window-image-height *echo-area-window* echo-text-height))
      ;; Recompute modeline fields for all windows so they fill the new width.
      (dolist (w *window-list*)
        (when (window-modeline-buffer w)
          (update-modeline-fields (window-buffer w) w)))
      (dolist (w *window-list*)
        (when (fboundp 'hemlock::terminal-resize-to-window)
          (funcall 'hemlock::terminal-resize-to-window (window-buffer w) w))))
    (setf *screen-image-trashed* t)))


;;;; Resize callback (called from webui thread)

(defun webui-resize-callback (device event)
  (let* ((s  (webui:webui-get-string event))
         (sp (when (stringp s) (position #\Space s)))
         (cols (when sp (parse-integer s :end sp)))
         (rows (when sp (parse-integer s :start (1+ sp)))))
    (declare (ignorable s))
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
