;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;;    Editor color palette and face resolution.
;;;

(in-package :hemlock.command)


(defparameter *hemlock-syntax-palette*
  #(nil                           ; 0  = default/reset
    (:fg (106 153  85))           ; 1  = comment
    (:fg ( 68 255 221))           ; 2  = string
    (:fg (246 117  68))           ; 3  = number/constant
    (:fg (130 101 255))           ; 4  = keyword
    (:fg ( 47 177 212))           ; 5  = function
    (:fg (  0 202 255))           ; 6  = builtin/operator
    (:fg (255  61 129))           ; 7  = variable
    (:fg (255 255 115))           ; 8  = type/parameter
    (:fg (244 244 247))           ; 9  = constant.builtin
    (:fg (137 180 250) :bold t)   ; 10 = ui-header
    (:fg (108 112 134))           ; 11 = ui-dim
    (:fg (243 139 168))           ; 12 = ui-error
    (:fg (166 227 161))           ; 13 = ui-accent
    (:fg (249 226 175) :bold t))  ; 14 = ui-match
  "Syntax color palette.  Slots 1-9 are used by ts-highlight; slots 10-14
are UI chrome colors for vnode buffers.")

(defparameter *hemlock-syntax-palette-16*
  #(nil 2 6 3 5 4 6 1 3 7 4 8 1 2 3)
  "ANSI 16-color fallback indices for *hemlock-syntax-palette* slots 0-14.")

(defvar *color-support* nil
  "Cached terminal color support level: :truecolor, :256color, or :16color.")

(defun detect-color-support ()
  "Return the terminal's color support level, caching the result in *color-support*."
  (or *color-support*
      (setf *color-support*
            (let ((ct   (uiop:getenv "COLORTERM"))
                  (term (uiop:getenv "TERM")))
              (cond ((or (equal ct "truecolor") (equal ct "24bit")) :truecolor)
                    ((and term (search "256color" term))            :256color)
                    (t                                              :16color))))))

(defun resolve-font (index)
  "Resolve a palette index to a color form for the current terminal.
Returns a (:fg (R G B)) plist for truecolor/256color terminals, an integer
ANSI index for 16-color terminals, or NIL for out-of-range indices."
  (when (and (plusp index) (< index (length *hemlock-syntax-palette*)))
    (case (detect-color-support)
      ((:truecolor :256color) (aref *hemlock-syntax-palette* index))
      (t                      (aref *hemlock-syntax-palette-16* index)))))
