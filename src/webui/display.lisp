;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; WebUI display backend — redisplay methods.
;;;

(in-package :hemlock.webui)


;;;; JSON / HTML escaping helpers

(defun json-escape (str)
  "Escape STR for embedding as a JSON string value (no surrounding quotes)."
  (with-output-to-string (out)
    (loop for ch across str do
      (case ch
        (#\\        (write-string "\\\\" out))
        (#\"        (write-string "\\\"" out))
        (#\Newline  (write-string "\\n"  out))
        (#\Return   (write-string "\\r"  out))
        (#\Tab      (write-string "\\t"  out))
        (t          (write-char ch out))))))

(defun html-escape-range (str start end out)
  "Write HTML-escaped characters of STR[start..end) to stream OUT."
  (loop for i from start below end
        for ch = (char str i) do
    (case ch
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
       (let ((color (getf font :color)))
         (when color (format out "fg-~D " color)))
       (when (getf font :bold)      (write-string "bold "      out))
       (when (getf font :underline) (write-string "underline " out))
       (when (getf font :italic)    (write-string "italic "    out))
       (when (getf font :reverse)   (write-string "reverse "   out))))
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
          (write-string "<span class=\"cursor\"> </span>" out))))))

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

;;; Emit characters [start..end) with FONT, injecting cursor span at CURSOR-COL.
(defun emit-range (chars start end font cursor-col out)
  (when (>= start end) (return-from emit-range))
  (let ((css (font->css font)))
    (flet ((emit-chars (s e cursp)
             (when (>= s e) (return-from emit-chars))
             (cond
               (cursp
                (write-string "<span class=\"cursor" out)
                (unless (string= css "") (write-char #\Space out) (write-string css out))
                (write-string "\">" out)
                (html-escape-range chars s e out)
                (write-string "</span>" out))
               ((string= css "")
                (html-escape-range chars s e out))
               (t
                (write-string "<span class=\"" out)
                (write-string css out)
                (write-string "\">" out)
                (html-escape-range chars s e out)
                (write-string "</span>" out)))))
      (if (and cursor-col (>= cursor-col start) (< cursor-col end))
          (progn
            (emit-chars start       cursor-col       nil)
            (emit-chars cursor-col  (1+ cursor-col)  t)
            (emit-chars (1+ cursor-col) end          nil))
          (emit-chars start end nil)))))


;;;; Collect window HTML lines

(defun collect-window-lines (window cursor-hunk cursor-x cursor-y)
  "Return a vector of HTML <div> strings for each display line of WINDOW."
  (let* ((hunk   (window-hunk window))
         (result (make-array 0 :adjustable t :fill-pointer 0))
         (i      0))
    (do ((dl (cdr (window-first-line window)) (cdr dl)))
        ((eq dl the-sentinel))
      (let* ((cursor-col (when (and (eq hunk cursor-hunk) (= i cursor-y))
                           cursor-x))
             (inner (dis-line->html (car dl) cursor-col)))
        (vector-push-extend (format nil "<div>~A</div>" inner) result))
      (incf i))
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


;;;; Modeline text

(defun modeline-text (window)
  (let ((buf (window-modeline-buffer     window))
        (len (window-modeline-buffer-len window)))
    (if (and buf len (> len 0))
        (subseq buf 0 (min len (length buf)))
        (let ((dl (window-modeline-dis-line window)))
          (if dl
              (subseq (dis-line-chars dl) 0 (dis-line-length dl))
              "")))))


;;;; Redisplay methods

(defmethod device-dumb-redisplay ((device webui-device) window)
  (let* ((hunk    (window-hunk window))
         (dom-id  (webui-hunk-dom-id hunk))
         (cw      (current-window))
         (ch      (when (eq window cw) (window-hunk cw)))
         (lines   (collect-window-lines window ch
                                        (webui-device-cursor-x device)
                                        (webui-device-cursor-y device)))
         (ml      (modeline-text window)))
    (when (window-modeline-dis-line window)
      (setf (dis-line-flags (window-modeline-dis-line window)) unaltered-bits))
    (push (list dom-id lines ml)
          (webui-device-dirty-windows device))
    (setf (window-first-changed window) the-sentinel
          (window-last-changed  window) (window-first-line window))))

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
        (dolist (entry (nreverse (webui-device-dirty-windows device)))
          (destructuring-bind (dom-id lines ml) entry
            (let ((js (format nil "updateWindow(~S,~A,~S);"
                              dom-id
                              (lines-to-json lines)
                              ml)))
              (format *error-output*
                      "~&[device-force-output] win=~S dom=~S lines=~D ml=~S~%"
                      win dom-id (length lines) ml)
              (finish-output *error-output*)
              (webui:webui-run win js)))))
      (setf (webui-device-dirty-windows device) nil))))

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
