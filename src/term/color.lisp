;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :hemlock.term)

(defvar *color-palette* (make-array 256 :initial-element nil))

(let ((base16 #(#(0 0 0)
                #(205 0 0)
                #(0 205 0)
                #(205 205 0)
                #(0 0 238)
                #(205 0 205)
                #(0 205 205)
                #(229 229 229)
                #(127 127 127)
                #(255 0 0)
                #(0 255 0)
                #(255 255 0)
                #(92 92 255)
                #(255 0 255)
                #(0 255 255)
                #(255 255 255))))
  (dotimes (i 16)
    (setf (aref *color-palette* i) (aref base16 i))))

(let ((levels #(0 95 135 175 215 255)))
  (loop for i from 16 below 232
        for idx = (- i 16)
        for r = (aref levels (floor idx 36))
        for g = (aref levels (floor (mod idx 36) 6))
        for b = (aref levels (mod idx 6))
        do (setf (aref *color-palette* i) (vector r g b))))

(loop for i from 232 below 256
      for v = (+ 8 (* 10 (- i 232)))
      do (setf (aref *color-palette* i) (vector v v v)))

(defun color-index-to-rgb (index)
  (when (and index (<= 0 index 255))
    (aref *color-palette* index)))
