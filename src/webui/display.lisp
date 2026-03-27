;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :hemlock.webui)


;;;; HTML escaping

(defun json-escape (str)
  (with-output-to-string (out)
    (loop for ch across str do
      (case ch
        (#\Nul)
        (#\\        (write-string "\\\\" out))
        (#\"        (write-string "\\\"" out))
        (#\Newline  (write-string "\\n"  out))
        (#\Return   (write-string "\\r"  out))
        (#\Tab      (write-string "\\t"  out))
        (t          (if (< (char-code ch) 32)
                        (format out "\\u~4,'0X" (char-code ch))
                        (write-char ch out)))))))

(defun html-escape-range (str start end out)
  (loop for i from start below end
        for ch = (char str i) do
          (case ch
            (#\Nul)
            (#\& (write-string "&amp;"  out))
            (#\< (write-string "&lt;"   out))
            (#\> (write-string "&gt;"   out))
            (#\" (write-string "&quot;" out))
            (t   (write-char ch out)))))


;;;; Font → CSS

(defun font->css (font)
  (typecase font
    (integer (if (zerop font) "" (format nil "fg-~D" font)))
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

(defun index-to-rgb-css (idx property)
  (let ((rgb (hemlock.term:color-index-to-rgb idx)))
    (when rgb
      (format nil "~A:rgb(~D,~D,~D);" property (aref rgb 0) (aref rgb 1) (aref rgb 2)))))

(defun color->css-style (font)
  (when (listp font)
    (let ((result nil))
      (let ((fg (getf font :fg)))
        (when fg
          (cond
            ((listp fg)
             (push (format nil "color:rgb(~D,~D,~D);" (first fg) (second fg) (third fg)) result))
            ((integerp fg)
             (let ((css (index-to-rgb-css fg "color")))
               (when css (push css result)))))))
      (let ((bg (getf font :bg)))
        (when bg
          (cond
            ((listp bg)
             (push (format nil "background:rgb(~D,~D,~D);" (first bg) (second bg) (third bg)) result))
            ((integerp bg)
             (let ((css (index-to-rgb-css bg "background")))
               (when css (push css result)))))))
      (when result (format nil "~{~A~}" (nreverse result))))))


;;;; Line → HTML

(defun line->html (str syntax-colors cursor-col)
  (let ((len (length str)))
    (with-output-to-string (out)
      (cond
        ((and (zerop len) (null cursor-col))
         (write-string " " out))
        ((null syntax-colors)
         (if cursor-col
             (progn
               (when (> cursor-col 0)
                 (html-escape-range str 0 (min cursor-col len) out))
               (if (< cursor-col len)
                   (progn
                     (write-string "<span class=\"cursor\">" out)
                     (html-escape-range str cursor-col (1+ cursor-col) out)
                     (write-string "</span>" out)
                     (when (< (1+ cursor-col) len)
                       (html-escape-range str (1+ cursor-col) len out)))
                   (write-string "<span class=\"cursor\"> </span>" out)))
             (html-escape-range str 0 len out)))
        (t
         (let ((pos 0))
           (dolist (range syntax-colors)
             (when (and (listp range) (>= (length range) 3))
               (let ((start (max 0 (min (first range) len)))
                     (end (max 0 (min (second range) len)))
                     (font (third range)))
                 (when (< start end)
                   (when (< pos start)
                     (emit-line-chars str pos start cursor-col out)
                     (setf pos start))
                   (let ((style (color->css-style font))
                         (css (font->css font)))
                     (write-string "<span" out)
                     (unless (string= css "") (format out " class=\"~A\"" css))
                     (when style (format out " style=\"~A\"" style))
                     (write-string ">" out)
                     (emit-line-chars str start end cursor-col out)
                     (write-string "</span>" out)
                     (setf pos (max pos end)))))))
           (when (< pos len)
             (emit-line-chars str pos len cursor-col out))
           (when (and cursor-col (>= cursor-col len))
             (write-string "<span class=\"cursor\"> </span>" out))))))))

(defun emit-line-chars (str start end cursor-col out)
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


;;;; Modeline

(defun modeline-text (window)
  (let* ((buf (window-modeline-buffer window))
         (len (window-modeline-buffer-len window))
         (raw (if (and (stringp buf) len (> len 0))
                  (subseq buf 0 (min len (length buf)))
                  (let ((dl (window-modeline-dis-line window)))
                    (if dl (subseq (dis-line-chars dl) 0 (dis-line-length dl)) "")))))
    (let ((end (position #\Nul raw)))
      (if end (subseq raw 0 end) raw))))


;;;; Buffer helpers

(defun count-buffer-lines (buf)
  (let ((line (mark-line (region-start (buffer-region buf)))) (n 0))
    (loop while line do (incf n) (setf line (line-next line)))
    n))

(defun line-index-in-buffer (buf mark)
  (let ((target (mark-line mark))
        (line (mark-line (region-start (buffer-region buf))))
        (i 0))
    (loop while line do
      (when (eq line target) (return-from line-index-in-buffer i))
      (incf i) (setf line (line-next line)))
    0))

(defun nth-buffer-line (buf n)
  (let ((line (mark-line (region-start (buffer-region buf)))) (i 0))
    (loop while line do
      (when (= i n) (return-from nth-buffer-line line))
      (incf i) (setf line (line-next line)))
    nil))


;;;; Terminal font-info conversion

(defun term-font-info-to-colors (font-info line-len)
  (when (and font-info (plusp line-len))
    (let ((result nil))
      (loop for (fi . rest) on font-info
            for start = (car fi)
            for plist = (cadr fi)
            for end = (if rest (caar rest) line-len)
            when (and plist (< start end))
            do (push (list start end plist) result))
      (nreverse result))))


;;;; Redisplay methods

(defmethod device-dumb-redisplay ((device webui-device) window)
  (when (window-modeline-dis-line window)
    (setf (dis-line-flags (window-modeline-dis-line window)) unaltered-bits))
  (pushnew window (webui-device-dirty-windows device) :test #'eq)
  (setf (window-first-changed window) the-sentinel
        (window-last-changed  window) (window-first-line window)))

(defmethod device-smart-redisplay ((device webui-device) window)
  (device-dumb-redisplay device window))

(defvar *prev-point-idx* (make-hash-table :test #'eq))

(defmethod device-put-cursor ((device webui-device) hunk x y)
  (setf (webui-device-cursor-x device) x
        (webui-device-cursor-y device) y)
  (pushnew (device-hunk-window hunk) (webui-device-dirty-windows device) :test #'eq))

(defmethod device-force-output ((device webui-device))
  (let ((win (webui-device-window-id device)))
    (when win
      (sb-int:with-float-traps-masked (:invalid :overflow :inexact :divide-by-zero
                                       :underflow)
        (let* ((cw (current-window))
               (windows (shiftf (webui-device-dirty-windows device) nil)))
          (dolist (window (nreverse windows))
            (handler-case
                (let* ((buf (window-buffer window))
                       (hunk (window-hunk window))
                       (dom-id (webui-hunk-dom-id hunk))
                       (point (buffer-point buf))
                       (is-current (eq window cw))
                       (ml (modeline-text window))
                       (point-idx (line-index-in-buffer buf point))
                       (colors-fn (and (fboundp 'hemlock::terminal-inject-font-changes-for-window)
                                       (symbol-function 'hemlock::terminal-inject-font-changes-for-window))))
                  (when colors-fn (funcall colors-fn window))
                  (let ((html-parts nil)
                        (line (mark-line (region-start (buffer-region buf))))
                        (idx 0))
                    (loop while line do
                      (let* ((str (line-string line))
                             (sc (getf (line-plist line) 'hemlock.command::syntax-colors))
                             (cc (when (and is-current (= idx point-idx))
                                   (mark-charpos point))))
                        (push (format nil "<div>~A</div>" (line->html str sc cc)) html-parts))
                      (incf idx)
                      (setf line (line-next line)))
                    (setf html-parts (nreverse html-parts))
                    (webui:webui-run win
                      (format nil "hemSetLines('~A',[~{\"~A\"~^,~}],'~A');"
                              (json-escape dom-id)
                              (mapcar #'json-escape html-parts)
                              (json-escape ml))))
                  (when is-current
                    (let ((prev (gethash window *prev-point-idx*)))
                      (when (or (null prev) (/= prev point-idx))
                        (webui:webui-run win
                          (format nil "hemEnsureVisible('~A',~D);"
                                  (json-escape dom-id) point-idx))
                        (setf (gethash window *prev-point-idx*) point-idx)))))
              (error (c) (declare (ignore c)) nil))))
        (when (and *echo-area-buffer* *echo-area-window*)
          (handler-case
              (let* ((buf *echo-area-buffer*)
                     (point (buffer-point buf))
                     (ml (modeline-text *echo-area-window*))
                     (line (mark-line (region-start (buffer-region buf))))
                     (total 0)
                     (html-parts nil))
                (loop while line do
                  (let ((cc (when (eq line (mark-line point)) (mark-charpos point))))
                    (push (format nil "<div>~A</div>" (line->html (line-string line) nil cc)) html-parts))
                  (incf total)
                  (setf line (line-next line)))
                (webui:webui-run win
                  (format nil "hemSetLines('echo',[~{\"~A\"~^,~}],'~A');"
                          (mapcar #'json-escape (nreverse html-parts))
                          (json-escape ml))))
            (error (c) (declare (ignore c)) nil)))))))

(defmethod device-finish-output ((device webui-device) window)
  (declare (ignore window))
  (device-force-output device))

(defmethod device-after-redisplay ((device webui-device)) nil)
(defmethod device-note-read-wait ((device webui-device) on-off) (declare (ignore on-off)))
(defmethod device-random-typeout-setup ((device webui-device) stream n) (declare (ignore stream n)))
(defmethod device-random-typeout-cleanup ((device webui-device) stream degree) (declare (ignore stream degree)))
(defmethod device-random-typeout-full-more ((device webui-device) stream) (declare (ignore stream)))
(defmethod device-random-typeout-line-more ((device webui-device) stream n) (declare (ignore stream n)))
(defmethod device-beep ((device webui-device) stream) (declare (ignore stream)))
