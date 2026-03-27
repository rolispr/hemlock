;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; WebUI display backend — redisplay methods.
;;;

(in-package :hemlock.webui)


;;;; JSON / HTML escaping helpers

(defun json-escape (str)
  "Escape STR for embedding as a JSON string value (no surrounding quotes).
   Null bytes are stripped to avoid truncating C strings."
  (with-output-to-string (out)
    (loop for ch across str do
      (case ch
        (#\Nul)                                   ; strip null bytes
        (#\\        (write-string "\\\\" out))
        (#\"        (write-string "\\\"" out))
        (#\Newline  (write-string "\\n"  out))
        (#\Return   (write-string "\\r"  out))
        (#\Tab      (write-string "\\t"  out))
        (t          (if (< (char-code ch) 32)
                        (format out "\\u~4,'0X" (char-code ch))
                        (write-char ch out)))))))

(defun html-escape-range (str start end out)
  "Write HTML-escaped characters of STR[start..end) to stream OUT.
   Null bytes and control characters are skipped."
  (loop for i from start below end
        for ch = (char str i) do
          (case ch
            (#\Nul)                                ; skip null bytes
            (#\& (write-string "&amp;"  out))
            (#\< (write-string "&lt;"   out))
            (#\> (write-string "&gt;"   out))
            (#\" (write-string "&quot;" out))
            (t   (write-char ch out)))))


;;;; Font → CSS class string

(defun font->css (font)
  (typecase font
    (integer
     (if (zerop font) "" (format nil "fg-~D" font)))
    (list
     (with-output-to-string (out)
       (let ((fg (or (getf font :color) (getf font :fg))))
         (when (and fg (integerp fg) (<= fg 15))
           (format out "fg-~D " fg)))
       (let ((bg (getf font :bg)))
         (when (and bg (integerp bg) (<= bg 15))
           (format out "bg-~D " bg)))
       (when (getf font :bold)           (write-string "bold " out))
       (when (getf font :underline)      (write-string "underline " out))
       (when (getf font :italic)         (write-string "italic " out))
       (when (getf font :inverse)        (write-string "reverse " out))
       (when (getf font :faint)          (write-string "faint " out))
       (when (getf font :strike-through) (write-string "line-through " out))))
    (t "")))


;;;; dis-line → one HTML <div>

(defun dis-line->html (dis-line cursor-col)
  "Render DIS-LINE as an HTML string (to be wrapped in <div>).
   CURSOR-COL is the column to highlight, or NIL."
  (let* ((chars   (dis-line-chars dis-line))
         (length  (dis-line-length dis-line))
         (changes (dis-line-font-changes dis-line)))
    (with-output-to-string (out)
      ;; Build a flat list of (font start end) spans.
      (let ((spans (collect-font-spans changes length))
            (pos   0))
        (dolist (span (or spans (list (list 0 0 length))))
          (destructuring-bind (font s e) span
            ;; Gap before span (font 0).
            (when (< pos s)
              (emit-range chars pos s 0 cursor-col out)
              (setf pos s))
            ;; The span itself.
            (emit-range chars s e font cursor-col out)
            (setf pos e)))
        ;; Trailing plain text.
        (when (< pos length)
          (emit-range chars pos length 0 cursor-col out))
        ;; Cursor at EOL: append a highlighted space.
        (when (and cursor-col (>= cursor-col length))
          (write-string "<span class=\"cursor\"> </span>" out))
        ;; Ensure empty lines (no content and no cursor) still occupy
        ;; a visual row.  Without this, <div></div> collapses to zero
        ;; height under white-space:pre, and surrounding lines shift
        ;; when the cursor moves onto the empty line.
        (when (and (zerop length) (null cursor-col) (zerop pos))
          (write-string " " out))))))

;;; Build flat (font start end) list from the font-change linked list.
(defun collect-font-spans (changes length)
  (let (spans)
    (do ((change changes (font-change-next change))
         (prev nil change))
        ((null change)
         (when prev
           (let ((font (font-change-font prev)))
             (when font
               (push (list font (font-change-x prev) length) spans)))))
      (when prev
        (let ((font (font-change-font prev)))
          (when font
            (push (list font (font-change-x prev) (font-change-x change)) spans)))))
    (nreverse spans)))

(defun color->css-style (font)
  (when (listp font)
    (let ((result nil))
      (let ((fg (getf font :fg)))
        (when fg
          (cond
            ((listp fg)
             (push (format nil "color:rgb(~D,~D,~D);" (first fg) (second fg) (third fg))
                   result))
            ((and (integerp fg) (> fg 15))
             (let ((rgb (hemlock.term:color-index-to-rgb fg)))
               (when rgb
                 (push (format nil "color:rgb(~D,~D,~D);" (aref rgb 0) (aref rgb 1) (aref rgb 2))
                       result)))))))
      (let ((bg (getf font :bg)))
        (when bg
          (cond
            ((listp bg)
             (push (format nil "background:rgb(~D,~D,~D);" (first bg) (second bg) (third bg))
                   result))
            ((and (integerp bg) (> bg 15))
             (let ((rgb (hemlock.term:color-index-to-rgb bg)))
               (when rgb
                 (push (format nil "background:rgb(~D,~D,~D);" (aref rgb 0) (aref rgb 1) (aref rgb 2))
                       result)))))))
      (when result
        (format nil "~{~A~}" (nreverse result))))))

;;; Emit characters [start..end) with FONT, injecting cursor span at CURSOR-COL.
(defun emit-range (chars start end font cursor-col out)
  (when (>= start end) (return-from emit-range))
  (let ((css (font->css font))
        (style (color->css-style font)))
    (flet ((emit-chars (s e cursp)
             (when (>= s e) (return-from emit-chars))
             (let ((has-class (or cursp (not (string= css ""))))
                   (has-style (and style (plusp (length style)))))
               (cond
                 ((or has-class has-style)
                  (write-string "<span" out)
                  (when has-class
                    (write-string " class=\"" out)
                    (when cursp (write-string "cursor" out))
                    (when (and cursp (not (string= css ""))) (write-char #\Space out))
                    (unless (string= css "") (write-string css out))
                    (write-string "\"" out))
                  (when has-style
                    (write-string " style=\"" out)
                    (write-string style out)
                    (write-string "\"" out))
                  (write-string ">" out)
                  (html-escape-range chars s e out)
                  (write-string "</span>" out))
                 (t
                  (html-escape-range chars s e out))))))
      (if (and cursor-col (>= cursor-col start) (< cursor-col end))
          (progn
            (emit-chars start       cursor-col       nil)
            (emit-chars cursor-col  (1+ cursor-col)  t)
            (emit-chars (1+ cursor-col) end          nil))
          (emit-chars start end nil)))))


;;;; Collect window HTML lines

(defun collect-window-lines (window cursor-hunk cursor-x cursor-y)
  "Return a vector of HTML <div> strings for each display line of WINDOW.
   Pads with empty <div> rows up to the window height so the screen is full."
  (let ((fn (and (fboundp 'hemlock::terminal-inject-font-changes-for-window)
                 (symbol-function 'hemlock::terminal-inject-font-changes-for-window))))
    (when fn (funcall fn window)))
  (let* ((hunk   (window-hunk window))
         (height (window-height window))
         (result (make-array height :fill-pointer 0))
         (i      0))
    (do ((dl (cdr (window-first-line window)) (cdr dl)))
        ((eq dl the-sentinel))
      (let* ((cursor-col (when (and (eq hunk cursor-hunk) (= i cursor-y))
                           cursor-x))
             (inner (dis-line->html (car dl) cursor-col)))
        (vector-push-extend (format nil "<div>~A</div>" inner) result))
      (incf i))
    ;; Pad remaining rows with empty divs (like ~ lines in a terminal).
    (loop while (< (fill-pointer result) height)
          do (vector-push-extend "<div> </div>" result))
    result))

(defun lines-to-json (vec)
  "Serialize a vector of HTML strings to a JSON array string."
  (with-output-to-string (out)
    (write-char #\[ out)
    (dotimes (i (length vec))
      (when (> i 0) (write-char #\, out))
      (write-char #\" out)
      (write-string (json-escape (aref vec i)) out)
      (write-char #\" out))
    (write-char #\] out)))


;;;; Snapshot-based rendering — builds HTML from immutable buffer-snapshot.
;;;; Can be called from any thread without touching mutable buffer state.

(defun snapshot-line->html (line-str colors cursor-col)
  "Render a single line from a snapshot as HTML.
LINE-STR is the line string, COLORS is the syntax color ranges (or nil),
CURSOR-COL is the cursor column on this line (or nil)."
  (let ((len (length line-str)))
    (with-output-to-string (out)
      (if (and (zerop len) (null cursor-col))
          (write-string " " out)
          (let ((pos 0))
            (when colors
              (dolist (range colors)
                (when (and (listp range) (>= (length range) 3))
                  (let ((start (max 0 (min (first range) len)))
                        (end (max 0 (min (second range) len)))
                        (font (third range)))
                    (when (< start end)
                      (when (< pos start)
                        (emit-snapshot-chars line-str pos start cursor-col out)
                        (setf pos start))
                      (let ((style (color->css-style font))
                            (css (font->css font)))
                        (write-string "<span" out)
                        (unless (string= css "")
                          (format out " class=\"~A\"" css))
                        (when style
                          (format out " style=\"~A\"" style))
                        (write-string ">" out)
                        (emit-snapshot-chars line-str start end cursor-col out)
                        (write-string "</span>" out)
                        (setf pos (max pos end))))))))
            (when (< pos len)
              (emit-snapshot-chars line-str pos len cursor-col out))
            (when (and cursor-col (>= cursor-col len))
              (write-string "<span class=\"cursor\"> </span>" out)))))))

(defun emit-snapshot-chars (str start end cursor-col out)
  "Emit characters [start..end) with cursor injection."
  (if (and cursor-col (>= cursor-col start) (< cursor-col end))
      (progn
        (when (< start cursor-col)
          (html-escape-range str start cursor-col out))
        (write-string "<span class=\"cursor\">" out)
        (html-escape-range str cursor-col (1+ cursor-col) out)
        (write-string "</span>" out)
        (when (< (1+ cursor-col) end)
          (html-escape-range str (1+ cursor-col) end out)))
      (html-escape-range str start end out)))

(defun snapshot->json (snap &key cursor-line cursor-col)
  "Build a JSON object from a buffer-snapshot for the WebUI.
Can be called from any thread — reads only immutable data."
  (let* ((lines (hemlock.text::snap-lines snap))
         (count (hemlock.text::snap-line-count snap))
         (start (hemlock.text::snap-start-line snap))
         (colors (hemlock.text::snap-colors snap))
         (height (or (hemlock.text::snap-text-height snap) (min count 40)))
         (name (hemlock.text::snap-name snap))
         (modeline (hemlock.text::snap-modeline snap))
         (cursor-line (or cursor-line (hemlock.text::snap-point-line snap)))
         (cursor-col (or cursor-col (hemlock.text::snap-point-charpos snap))))
    (with-output-to-string (out)
      (format out "{\"name\":\"~A\",\"tick\":~D,\"lines\":["
              (json-escape name)
              (hemlock.text::snap-tick snap))
      (let ((end (min count (+ start height))))
        (loop for i from start below end
              for rel = (- i start)
              do (when (> rel 0) (write-char #\, out))
                 (write-char #\" out)
                 (let* ((line-str (fset:@ lines i))
                        (lc (and colors (< i (fset:size colors)) (fset:@ colors i)))
                        (cc (when (= i cursor-line) cursor-col))
                        (html (snapshot-line->html line-str lc cc)))
                   (write-string (json-escape (format nil "<div>~A</div>" html)) out))
                 (write-char #\" out)))
      (format out "],\"modeline\":\"~A\"}" (json-escape (or modeline ""))))))


;;;; Modeline text

(defun modeline-text (window)
  "Return the modeline string for WINDOW, with any null characters stripped.
   The modeline buffer is often padded with null bytes beyond the visible text;
   these must be removed before embedding in JS (C strings truncate at null)."
  (let* ((buf (window-modeline-buffer     window))
         (len (window-modeline-buffer-len window))
         (raw (if (and (stringp buf) len (> len 0))
                  (subseq buf 0 (min len (length buf)))
                  (let ((dl (window-modeline-dis-line window)))
                    (if dl
                        (subseq (dis-line-chars dl) 0 (dis-line-length dl))
                        "")))))
    ;; Strip trailing null bytes (the modeline buffer is often padded with #\Nul).
    (let ((end (position #\Nul raw)))
      (if end (subseq raw 0 end) raw))))


;;;; Redisplay methods

(defmethod device-dumb-redisplay ((device webui-device) window)
  ;; Record the window for later rendering in device-force-output,
  ;; where the cursor position has been finalized by device-put-cursor.
  (when (window-modeline-dis-line window)
    (setf (dis-line-flags (window-modeline-dis-line window)) unaltered-bits))
  (pushnew window (webui-device-dirty-windows device) :test #'eq)
  (setf (window-first-changed window) the-sentinel
        (window-last-changed  window) (window-first-line window)))

(defmethod device-smart-redisplay ((device webui-device) window)
  (device-dumb-redisplay device window))

(defmethod device-put-cursor ((device webui-device) hunk x y)
  (declare (ignore hunk))
  (setf (webui-device-cursor-x device) x
        (webui-device-cursor-y device) y))

(defmethod device-force-output ((device webui-device))
  (let ((win (webui-device-window-id device)))
    (when win
      (sb-int:with-float-traps-masked (:invalid :overflow :inexact :divide-by-zero
                                       :underflow)
        (let* ((cw  (current-window))
               (ch  (when cw (window-hunk cw)))
               (cx  (webui-device-cursor-x device))
               (cy  (webui-device-cursor-y device))
               (windows (shiftf (webui-device-dirty-windows device) nil)))
          (dolist (window (nreverse windows))
            (handler-case
                (let* ((hunk   (window-hunk window))
                       (dom-id (webui-hunk-dom-id hunk))
                       (cursor-hunk (when (eq window cw) ch))
                       (lines  (collect-window-lines window cursor-hunk cx cy))
                       (ml     (modeline-text window)))
                  (let ((js (format nil "updateWindow(\"~A\",~A,\"~A\");"
                                    (json-escape dom-id)
                                    (lines-to-json lines)
                                    (json-escape ml))))
                    (webui:webui-run win js)))
              (error (c)
                (declare (ignore c))
                nil))))))))

(defmethod device-finish-output ((device webui-device) window)
  (declare (ignore window))
  (device-force-output device))

(defmethod device-after-redisplay ((device webui-device))
  nil)

(defmethod device-note-read-wait ((device webui-device) on-off)
  (declare (ignore on-off))
  nil)

(defmethod device-random-typeout-setup ((device webui-device) stream n)
  (declare (ignore stream n))
  nil)

(defmethod device-random-typeout-cleanup ((device webui-device) stream degree)
  (declare (ignore stream degree))
  nil)

(defmethod device-random-typeout-full-more ((device webui-device) stream)
  (declare (ignore stream))
  nil)

(defmethod device-random-typeout-line-more ((device webui-device) stream n)
  (declare (ignore stream n))
  nil)

(defmethod device-beep ((device webui-device) stream)
  (declare (ignore stream))
  nil)
