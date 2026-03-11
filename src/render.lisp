;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; Pure ANSI string rendering — no I/O, no device state, no side effects.
;;;
;;; Extracted and adapted from cl-tuition (MIT, Anthony Green 2025).
;;;

(in-package :hemlock-internals)


;;;; Color constants — 16-color ANSI SGR codes

(defparameter *fg-black*          "30")
(defparameter *fg-red*            "31")
(defparameter *fg-green*          "32")
(defparameter *fg-yellow*         "33")
(defparameter *fg-blue*           "34")
(defparameter *fg-magenta*        "35")
(defparameter *fg-cyan*           "36")
(defparameter *fg-white*          "37")
(defparameter *fg-bright-black*   "90")
(defparameter *fg-bright-red*     "91")
(defparameter *fg-bright-green*   "92")
(defparameter *fg-bright-yellow*  "93")
(defparameter *fg-bright-blue*    "94")
(defparameter *fg-bright-magenta* "95")
(defparameter *fg-bright-cyan*    "96")
(defparameter *fg-bright-white*   "97")

(defparameter *bg-black*          "40")
(defparameter *bg-red*            "41")
(defparameter *bg-green*          "42")
(defparameter *bg-yellow*         "43")
(defparameter *bg-blue*           "44")
(defparameter *bg-magenta*        "45")
(defparameter *bg-cyan*           "46")
(defparameter *bg-white*          "47")
(defparameter *bg-bright-black*   "100")
(defparameter *bg-bright-white*   "107")


;;;; ANSI helpers

(defun ansi-reset () (format nil "~C[0m" #\Escape))
(defun sgr (codes) (format nil "~C[~{~A~^;~}m" #\Escape codes))


;;;; Character display width

(defun code-in-range-p (code ranges)
  (loop for (a b) in ranges thereis (and (<= a code) (<= code b))))

(defun combining-char-p (code)
  "Return T if CODE is a zero-width combining character."
  (code-in-range-p code
                   '((#x0300 #x036F)  ; Combining Diacritical Marks
                     (#x0483 #x0489)  ; Cyrillic Combining
                     (#x0591 #x05BD)  ; Hebrew vowel points
                     (#x05BF #x05BF)
                     (#x05C1 #x05C2)
                     (#x05C4 #x05C5)
                     (#x05C7 #x05C7)
                     (#x0610 #x061A)  ; Arabic extended
                     (#x064B #x065F)  ; Arabic harakat
                     (#x0670 #x0670)
                     (#x06D6 #x06DC)
                     (#x06DF #x06E4)
                     (#x06E7 #x06E8)
                     (#x06EA #x06ED)
                     (#x200B #x200F)  ; zero-width / bidi marks
                     (#x202A #x202E)
                     (#x2060 #x2064)
                     (#x2066 #x2069)  ; bidi isolates
                     (#x20D0 #x20FF)  ; Combining Diacritical for Symbols
                     (#xFE20 #xFE2F)))) ; Combining Half Marks

(defun east-asian-wide-p (code)
  "Return T if CODE is a double-width East Asian character."
  (code-in-range-p code
                   '((#x1100 #x115F)   ; Hangul Jamo
                     (#x231A #x231B)   ; watch / hourglass
                     (#x2329 #x232A)
                     (#x23E9 #x23EC)
                     (#x23F0 #x23F3)
                     (#x25FD #x25FE)
                     (#x2600 #x2605)
                     (#x2614 #x2615)
                     (#x2648 #x2653)
                     (#x2E80 #x2FFF)   ; CJK Radicals / Kangxi
                     (#x3000 #x303F)   ; CJK punctuation + ideographic space
                     (#x3040 #x309F)   ; Hiragana
                     (#x30A0 #x30FF)   ; Katakana
                     (#x3100 #x312F)   ; Bopomofo
                     (#x3130 #x318F)   ; Hangul Compatibility Jamo
                     (#x31A0 #x31EF)
                     (#x3200 #x32FF)   ; Enclosed CJK
                     (#x3400 #x4DBF)   ; CJK Extension A
                     (#x4E00 #x9FFF)   ; CJK Unified Ideographs
                     (#xA960 #xA97F)   ; Hangul Jamo Extended-A
                     (#xAC00 #xD7A3)   ; Hangul Syllables
                     (#xF900 #xFAFF)   ; CJK Compatibility Ideographs
                     (#xFE10 #xFE19)
                     (#xFE30 #xFE6B)   ; CJK Compatibility Forms
                     (#xFF01 #xFF60)   ; Fullwidth ASCII
                     (#xFFE0 #xFFE6)   ; Fullwidth symbols
                     (#x1F300 #x1F64F) ; Emoji (subset)
                     (#x1F900 #x1F9FF))))

(defun char-display-width (ch)
  "Return the number of terminal columns CH occupies: 0, 1, or 2."
  (let ((code (char-code ch)))
    (cond ((< code 32) 0)          ; control characters
          ((= code #x7F) 0)        ; DEL
          ((combining-char-p code) 0)
          ((east-asian-wide-p code) 2)
          (t 1))))

(defun visible-length (str)
  "Column width of STR: skips ANSI escape sequences, counts wide/combining chars."
  (let ((n 0) (i 0) (len (length str)))
    (loop while (< i len) do
      (let ((ch (char str i)))
        (cond
          ((char= ch #\Escape)
           (incf i)
           (when (and (< i len) (char= (char str i) #\[))
             (incf i)
             (loop while (< i len) do
               (let ((code (char-code (char str i))))
                 (incf i)
                 (when (and (>= code #x40) (<= code #x7E)) (return))))))
          (t
           (incf n (char-display-width ch))
           (incf i)))))
    n))


;;;; String utilities

(defun split-lines (str)
  "Split STR on newlines, returning a list of strings."
  (let ((result '()) (start 0))
    (loop for pos = (position #\Newline str :start start)
          while pos
          do (push (subseq str start pos) result)
             (setf start (1+ pos))
          finally (push (subseq str start) result))
    (nreverse result)))


;;;; Truncation

(defstruct (render-tok (:constructor make-render-tok)) type text)

(defun tokenize-ansi (str)
  "Tokenize STR into :ansi, :newline, and :word tokens."
  (let ((out '()) (i 0) (n (length str)))
    (labels ((push-tok (type s e)
               (when (< s e)
                 (push (make-render-tok :type type :text (subseq str s e)) out))))
      (loop while (< i n) do
        (let ((ch (char str i)))
          (cond
            ((char= ch #\Newline)
             (push (make-render-tok :type :newline :text (string ch)) out)
             (incf i))
            ((char= ch #\Escape)
             (let ((j (1+ i)))
               (loop while (and (< j n) (not (char= (char str j) #\m))) do (incf j))
               (when (< j n) (incf j))
               (push-tok :ansi i j)
               (setf i j)))
            (t
             (let ((j i))
               (loop while (and (< j n)
                                (not (char= (char str j) #\Newline))
                                (not (char= (char str j) #\Escape)))
                     do (incf j))
               (push-tok :word i j)
               (setf i j)))))))
    (nreverse out)))

(defun truncate-text (text width &key (ellipsis "…"))
  "Truncate TEXT to WIDTH visible columns, preserving ANSI sequences.
Appends ELLIPSIS if truncation occurs."
  (let* ((tokens (tokenize-ansi text))
         (ellw   (visible-length ellipsis))
         (budget (max 0 (- width ellw)))
         (out '()) (w 0) (cut nil))
    (dolist (tok tokens)
      (ecase (render-tok-type tok)
        (:ansi    (push (render-tok-text tok) out))
        (:newline (push (render-tok-text tok) out))
        (:word
         (let* ((s  (render-tok-text tok))
                (sw (visible-length s)))
           (cond
             ((<= (+ w sw) budget)
              (incf w sw) (push s out))
             (t
              (let ((need (- budget w)))
                (when (> need 0)
                  (let ((acc "") (seen 0))
                    (loop for c across s until (>= seen need) do
                      (incf seen) (setf acc (concatenate 'string acc (string c))))
                    (push acc out)))
                (setf cut t) (return))))))))
    (let ((s (apply #'concatenate 'string (nreverse out))))
      (if cut (concatenate 'string s ellipsis) s))))


;;;; Styling

(defun colored (text &key fg bg)
  "Wrap TEXT with ANSI foreground and/or background color."
  (if (or fg bg)
      (format nil "~A~A~A" (sgr (remove nil (list fg bg))) text (ansi-reset))
      text))

(defun bold (text)
  "Render TEXT bold."
  (format nil "~C[1m~A~A" #\Escape text (ansi-reset)))


;;;; Layout

(defun spaces (n) (make-string n :initial-element #\Space))

(defun join-vertical (position &rest blocks)
  "Stack text blocks vertically.  POSITION: :left :center :right."
  (let* ((all (mapcar #'split-lines blocks))
         (max-w (loop for ls in all maximize
                      (loop for l in ls maximize (visible-length l))))
         (result '()))
    (dolist (lines all)
      (dolist (line lines)
        (let ((pad (max 0 (- max-w (visible-length line)))))
          (push (ecase position
                  (:left   (format nil "~A~A" line (spaces pad)))
                  (:right  (format nil "~A~A" (spaces pad) line))
                  (:center (let* ((l (floor pad 2)) (r (- pad l)))
                             (format nil "~A~A~A" (spaces l) line (spaces r)))))
                result))))
    (format nil "~{~A~^~%~}" (nreverse result))))

(defun join-horizontal (position &rest blocks)
  "Place text blocks side by side.  POSITION: :top :middle :bottom."
  (let* ((blines  (mapcar #'split-lines blocks))
         (bwidths (mapcar (lambda (ls) (loop for l in ls maximize (visible-length l))) blines))
         (max-h   (loop for ls in blines maximize (length ls)))
         (result  '()))
    (dotimes (i max-h)
      (let ((line ""))
        (loop for lines in blines for bw in bwidths do
              (let* ((h   (length lines))
                     (off (ecase position
                            (:top    0)
                            (:middle (floor (- max-h h) 2))
                            (:bottom (- max-h h))))
                     (idx (- i off)))
                (if (and (>= idx 0) (< idx h))
                    (let* ((l (nth idx lines))
                           (p (spaces (max 0 (- bw (visible-length l))))))
                      (setf line (concatenate 'string line l p)))
                    (setf line (concatenate 'string line (spaces bw))))))
        (push line result)))
    (format nil "~{~A~^~%~}" (nreverse result))))

(defun place (width height h-pos v-pos text)
  "Position TEXT in a WIDTH×HEIGHT space.
H-POS: :left :center :right  V-POS: :top :middle :bottom."
  (let* ((lines (split-lines text))
         (th    (length lines))
         (pt    (ecase v-pos
                  (:top    0)
                  (:middle (floor (- height th) 2))
                  (:bottom (- height th))))
         (pb    (- height th pt))
         (blank (spaces width))
         (result '()))
    (dotimes (_ (max 0 pt))  (push blank result))
    (dolist (line lines)
      (let ((pad (max 0 (- width (visible-length line)))))
        (push (ecase h-pos
                (:left   (format nil "~A~A" line (spaces pad)))
                (:right  (format nil "~A~A" (spaces pad) line))
                (:center (let* ((l (floor pad 2)) (r (- pad l)))
                           (format nil "~A~A~A" (spaces l) line (spaces r)))))
              result)))
    (dotimes (_ (max 0 pb))  (push blank result))
    (format nil "~{~A~^~%~}" (nreverse result))))


;;;; Borders

(defstruct border
  top bottom left right
  top-left top-right bottom-left bottom-right)

(defparameter *border-rounded*
  (make-border :top "─" :bottom "─" :left "│" :right "│"
               :top-left "╭" :top-right "╮"
               :bottom-left "╰" :bottom-right "╯"))

(defparameter *border-normal*
  (make-border :top "─" :bottom "─" :left "│" :right "│"
               :top-left "┌" :top-right "┐"
               :bottom-left "└" :bottom-right "┘"))

(defparameter *border-ascii*
  (make-border :top "-" :bottom "-" :left "|" :right "|"
               :top-left "+" :top-right "+"
               :bottom-left "+" :bottom-right "+"))

(defun render-border (text border &key fg bg
                                       (top t) (bottom t) (left t) (right t))
  "Wrap TEXT in a box-drawing border.  FG/BG are SGR code strings."
  (let* ((lines (split-lines text))
         (max-w (loop for l in lines maximize (visible-length l)))
         (clr   (lambda (s) (if (or fg bg) (colored s :fg fg :bg bg) s)))
         (hline (lambda (fill lc rc)
                  (funcall clr
                           (format nil "~A~A~A"
                                   (if left lc "") (make-string max-w :initial-element (char fill 0))
                                   (if right rc "")))))
         (result '()))
    (when top
      (push (funcall hline (border-top border)
                     (border-top-left border) (border-top-right border))
            result))
    (dolist (line lines)
      (let* ((pad   (spaces (max 0 (- max-w (visible-length line)))))
             (lb    (if left  (funcall clr (border-left border))  ""))
             (rb    (if right (funcall clr (border-right border)) ""))
             (rpad  (funcall clr pad)))
        (push (format nil "~A~A~A~A" lb line rpad rb) result)))
    (when bottom
      (push (funcall hline (border-bottom border)
                     (border-bottom-left border) (border-bottom-right border))
            result))
    (format nil "~{~A~^~%~}" (nreverse result))))
