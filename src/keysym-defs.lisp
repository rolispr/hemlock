;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;;
;;; This file defines all the definitions of keysyms (see key-event.lisp).
;;; These keysyms match those for X11.
;;;
;;;

(in-package :hemlock-internals)


;;; The IBM RT keyboard has X11 keysyms defined for the following modifier
;;; keys, but we leave them mapped to nil indicating that they are non-events
;;; to be ignored:
;;;    ctrl             65507
;;;    meta (left)      65513
;;;    meta (right)     65514
;;;    shift (left)     65505
;;;    shift (right)    65506
;;;    lock             65509
;;;


;;; Function keys for the RT.
;;;
(define-keysym 65470 "F1")
(define-keysym 65471 "F2")
(define-keysym 65472 "F3")
(define-keysym 65473 "F4")
(define-keysym 65474 "F5")
(define-keysym 65475 "F6")
(define-keysym 65476 "F7")
(define-keysym 65477 "F8")
(define-keysym 65478 "F9")
(define-keysym 65479 "F10")
(define-keysym 65480 "F11" "L1")
(define-keysym 65481 "F12" "L2")

;;; Function keys for the Sun (and other keyboards) -- L1-L10 and R1-R15.
;;;
(define-keysym 65482 "F13" "L3")
(define-keysym 65483 "F14" "L4")
(define-keysym 65484 "F15" "L5")
(define-keysym 65485 "F16" "L6")
(define-keysym 65486 "F17" "L7")
(define-keysym 65487 "F18" "L8")
(define-keysym 65488 "F19" "L9")
(define-keysym 65489 "F20" "L10")
(define-keysym 65490 "F21" "R1")
(define-keysym 65491 "F22" "R2")
(define-keysym 65492 "F23" "R3")
(define-keysym 65493 "F24" "R4")
(define-keysym 65494 "F25" "R5")
(define-keysym 65495 "F26" "R6")
(define-keysym 65496 "F27" "R7")
(define-keysym 65497 "F28" "R8")
(define-keysym 65498 "F29" "R9")
(define-keysym 65499 "F30" "R10")
(define-keysym 65500 "F31" "R11")
(define-keysym 65501 "F32" "R12")
(define-keysym 65502 "F33" "R13")
(define-keysym 65503 "F34" "R14")
(define-keysym 65504 "F35" "R15")

;;; Upper right key bank.
;;;
(define-keysym 65377 "Printscreen")
;; Couldn't type scroll lock.
(define-keysym 65299 "Pause")

;;; Middle right key bank.
;;;
(define-keysym 65379 "Insert")
(define-keysym 65535 "Delete" "Rubout" (string (code-char 127)))
(define-keysym 65360 "Home")
(define-keysym 65365 "Pageup")
(define-keysym 65367 "End")
(define-keysym 65366 "Pagedown")

;;; Arrows.
;;;
(define-keysym 65361 "Leftarrow")
(define-keysym 65362 "Uparrow")
(define-keysym 65364 "Downarrow")
(define-keysym 65363 "Rightarrow")

;;; Number pad.
;;;
(define-keysym 65407 "Numlock")
(define-keysym 65421 "Numpad\-Return" "Numpad\-Enter")      ;num-pad-enter
(define-keysym 65455 "Numpad/")                             ;num-pad-/
(define-keysym 65450 "Numpad*")                             ;num-pad-*
(define-keysym 65453 "Numpad-")                             ;num-pad--
(define-keysym 65451 "Numpad+")                             ;num-pad-+
(define-keysym 65456 "Numpad0")                             ;num-pad-0
(define-keysym 65457 "Numpad1")                             ;num-pad-1
(define-keysym 65458 "Numpad2")                             ;num-pad-2
(define-keysym 65459 "Numpad3")                             ;num-pad-3
(define-keysym 65460 "Numpad4")                             ;num-pad-4
(define-keysym 65461 "Numpad5")                             ;num-pad-5
(define-keysym 65462 "Numpad6")                             ;num-pad-6
(define-keysym 65463 "Numpad7")                             ;num-pad-7
(define-keysym 65464 "Numpad8")                             ;num-pad-8
(define-keysym 65465 "Numpad9")                             ;num-pad-9
(define-keysym 65454 "Numpad.")                             ;num-pad-.

;;; "Named" keys.
;;;
(define-keysym 65289 "Tab")
(define-keysym 65307 "Escape" "Altmode" "Alt")              ;escape
(define-keysym 65288 "Backspace")                           ;backspace
(define-keysym 65293 "Return" "Enter")                      ;enter
#+nil
;; 65512 = #xffe8 is Meta_R for me.  As per the comment on IBM RT at the
;; to of this file, it needs to be unmapped.
(define-keysym 65512 "Linefeed" "Action" "Newline")         ;action
(define-keysym 10 "Linefeed" "Action" "Newline")            ;action
(define-keysym 32 "Space" " ")

;;; Mouse / pointer events (0xFF00-0xFF02, not used by X11 keysyms).
;;;
(define-keysym 65280 "Mouse1")
(define-keysym 65281 "WheelUp")
(define-keysym 65282 "WheelDown")

;;; Letters.
;;;
(define-keysym 97 "a") (define-keysym 65 "A")
(define-keysym 98 "b") (define-keysym 66 "B")
(define-keysym 99 "c") (define-keysym 67 "C")
(define-keysym 100 "d") (define-keysym 68 "D")
(define-keysym 101 "e") (define-keysym 69 "E")
(define-keysym 102 "f") (define-keysym 70 "F")
(define-keysym 103 "g") (define-keysym 71 "G")
(define-keysym 104 "h") (define-keysym 72 "H")
(define-keysym 105 "i") (define-keysym 73 "I")
(define-keysym 106 "j") (define-keysym 74 "J")
(define-keysym 107 "k") (define-keysym 75 "K")
(define-keysym 108 "l") (define-keysym 76 "L")
(define-keysym 109 "m") (define-keysym 77 "M")
(define-keysym 110 "n") (define-keysym 78 "N")
(define-keysym 111 "o") (define-keysym 79 "O")
(define-keysym 112 "p") (define-keysym 80 "P")
(define-keysym 113 "q") (define-keysym 81 "Q")
(define-keysym 114 "r") (define-keysym 82 "R")
(define-keysym 115 "s") (define-keysym 83 "S")
(define-keysym 116 "t") (define-keysym 84 "T")
(define-keysym 117 "u") (define-keysym 85 "U")
(define-keysym 118 "v") (define-keysym 86 "V")
(define-keysym 119 "w") (define-keysym 87 "W")
(define-keysym 120 "x") (define-keysym 88 "X")
(define-keysym 121 "y") (define-keysym 89 "Y")
(define-keysym 122 "z") (define-keysym 90 "Z")

;;; Standard number keys.
;;;
(define-keysym 49 "1") (define-keysym 33 "!")
(define-keysym 50 "2") (define-keysym 64 "@")
(define-keysym 51 "3") (define-keysym 35 "#")
(define-keysym 52 "4") (define-keysym 36 "$")
(define-keysym 53 "5") (define-keysym 37 "%")
(define-keysym 54 "6") (define-keysym 94 "^")
(define-keysym 55 "7") (define-keysym 38 "&")
(define-keysym 56 "8") (define-keysym 42 "*")
(define-keysym 57 "9") (define-keysym 40 "(")
(define-keysym 48 "0") (define-keysym 41 ")")

;;; "Standard" symbol keys.
;;;
(define-keysym 96 "`") (define-keysym 126 "~")
(define-keysym 45 "-") (define-keysym 95 "_")
(define-keysym 61 "=") (define-keysym 43 "+")
(define-keysym 91 "[") (define-keysym 123 "{")
(define-keysym 93 "]") (define-keysym 125 "}")
(define-keysym 92 "\\") (define-keysym 124 "|")
(define-keysym 59 ";") (define-keysym 58 ":")
(define-keysym 39 "'") (define-keysym 34 "\"")
(define-keysym 44 ",") (define-keysym 60 "<")
(define-keysym 46 ".") (define-keysym 62 ">")
(define-keysym 47 "/") (define-keysym 63 "?")

;;; Standard Mouse keysyms.
;;;
(define-mouse-keysym 1 25601 "Leftdown" "Super" :button-press)
(define-mouse-keysym 1 25602 "Leftup" "Super" :button-release)

(define-mouse-keysym 2 25603 "Middledown" "Super" :button-press)
(define-mouse-keysym 2 25604 "Middleup" "Super" :button-release)

(define-mouse-keysym 3 25605 "Rightdown" "Super" :button-press)
(define-mouse-keysym 3 25606 "Rightup" "Super" :button-release)

;;; Sun keyboard.
;;;
(define-keysym 65387 "break")                       ;alternate (Sun).
;(define-keysym 65290 "linefeed")



;;;; SETFs of KEY-EVANT-CHAR and CHAR-KEY-EVENT.

;;; Converting ASCII control characters to Common Lisp control characters:
;;; ASCII control character codes are separated from the codes of the
;;; "non-controlified" characters by the code of atsign.  The ASCII control
;;; character codes range from ^@ (0) through ^_ (one less than the code of
;;; space).  We iterate over this range adding the ASCII code of atsign to
;;; get the "non-controlified" character code.  With each of these, we turn
;;; the code into a Common Lisp character and set its :control bit.  Certain
;;; ASCII control characters have to be translated to special Common Lisp
;;; characters outside of the loop.
;;;    With the advent of Hemlock running under X, and all the key bindings
;;; changing, we also downcase each Common Lisp character (where normally
;;; control characters come in upcased) in an effort to obtain normal command
;;; bindings.  Commands bound to uppercase modified characters will not be
;;; accessible to terminal interaction.
;;;
(let ((@-code (char-code #\@)))
  (dotimes (i (char-code #\space))
    (setf (char-key-event (code-char i))
          (make-key-event (string (char-downcase (code-char (+ i @-code))))
                               (key-event-modifier-mask "control")))))
(setf (char-key-event (code-char 9)) (make-key-event #k"Tab"))
(setf (char-key-event (code-char 10)) (make-key-event #k"Linefeed"))
(setf (char-key-event (code-char 13)) (make-key-event #k"Return"))
(setf (char-key-event (code-char 27)) (make-key-event #k"Alt"))
;;;
;;; Other ASCII codes are exactly the same as the Common Lisp codes.
;;;
(do ((i (char-code #\space) (1+ i)))
    ((= i 128))
  (setf (char-key-event (code-char i))
        (make-key-event (string (code-char i)))))

;;; This makes KEY-EVENT-CHAR the inverse of CHAR-KEY-EVENT from the start.
;;; It need not be this way, but it is.
;;;
(dotimes (i 128)
  (let ((character (code-char i)))
    (setf (key-event-char (char-key-event character)) character)))

;;; Since we treated these characters specially above when setting
;;; HEMLOCK-EXT:CHAR-KEY-EVENT above, we must set these HEMLOCK-EXT:KEY-EVENT-CHAR's specially
;;; to make quoting characters into Hemlock buffers more obvious for users.
;;;
(setf (key-event-char #k"C-h") #\backspace)
(setf (key-event-char #k"C-i") #\tab)
(setf (key-event-char #k"C-j") #\linefeed)
(setf (key-event-char #k"C-m") #\return)
