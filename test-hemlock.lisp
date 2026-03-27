;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; Hemlock PTY test harness.
;;;
;;; Spawns hemlock in a PTY subprocess, drives it via the hemlock.term
;;; VT100 emulator, and asserts on the virtual screen grid.
;;;
;;; Usage:
;;;   sbcl --script test-hemlock.lisp

(defpackage :hemlock-test-harness
  (:use :cl))
(in-package :hemlock-test-harness)


;;;; Bootstrap.

(require 'asdf)
(require 'cffi)

(let ((ocicl #P"/Users/bret.horne/.local/share/ocicl/ocicl-runtime.lisp"))
  (when (probe-file ocicl) (load ocicl)))

(asdf:initialize-source-registry
 '(:source-registry
   (:directory #P"/Users/bret.horne/git/cl/hemlock-port/")
   :inherit-configuration))

(asdf:load-system :hemlock)


;;;; Parameters.

(defparameter *hemlock-dir*    #P"/Users/bret.horne/git/cl/hemlock-port/")
(defparameter *sbcl-path*      "/opt/homebrew/bin/sbcl")
(defparameter *cols*           220)
(defparameter *rows*           50)
(defparameter *load-wait*      90)   ; seconds
(defparameter *cmd-wait*       0.4)  ; seconds
(defparameter *quiescent-ms*   200)  ; poll timeout ms

(defvar *pass*     0)
(defvar *fail*     0)
(defvar *vterm*    nil)
(defvar *proc-conn* nil)
(defvar *fd*       nil)


;;;; PTY I/O.

(defun %fd-readable-p (fd &optional (timeout-ms 0))
  "Poll FD with TIMEOUT-MS.  Return true if readable."
  (cffi:with-foreign-object (pfd :uint64)
    (setf (cffi:mem-ref pfd :int32 0) fd
          (cffi:mem-ref pfd :int16 4) 1   ; POLLIN
          (cffi:mem-ref pfd :int16 6) 0)
    (plusp (cffi:foreign-funcall "poll" :pointer pfd :uint32 1 :int timeout-ms :int))))

(defun %fd-read-chunk (fd)
  "Read available bytes from FD.  Return UTF-8 string or NIL."
  (let ((buf (make-array 8192 :element-type '(unsigned-byte 8))))
    (cffi:with-pointer-to-vector-data (ptr buf)
      (let ((n (cffi:foreign-funcall "read" :int fd :pointer ptr :size 8192 :long)))
        (when (plusp n)
          (sb-ext:octets-to-string buf :external-format :utf-8 :end n))))))

(defun %fd-write (fd str)
  "Write STR to FD as UTF-8."
  (let ((bytes (sb-ext:string-to-octets str :external-format :utf-8)))
    (sb-unix:unix-write fd bytes 0 (length bytes))))


;;;; Virtual screen.

(defun screen ()
  "Dump the virtual terminal grid as a string."
  (hemlock.term:term-dump-to-string *vterm*))

(defun drain (&optional (extra-wait *cmd-wait*))
  "Drain PTY output into *vterm*, then sleep EXTRA-WAIT."
  (loop while (%fd-readable-p *fd* *quiescent-ms*)
        do (let ((chunk (%fd-read-chunk *fd*)))
             (when chunk
               (hemlock.term:term-process-output *vterm* chunk))))
  (sleep extra-wait)
  (loop while (%fd-readable-p *fd* 0)
        do (let ((chunk (%fd-read-chunk *fd*)))
             (when chunk
               (hemlock.term:term-process-output *vterm* chunk)))))

(defun wait-for-pattern (pattern &key (timeout *load-wait*))
  "Drain until PATTERN appears on screen, or signal on timeout."
  (let ((deadline (+ (get-universal-time) timeout)))
    (loop
      (loop while (%fd-readable-p *fd* 100)
            do (let ((chunk (%fd-read-chunk *fd*)))
                 (when chunk
                   (hemlock.term:term-process-output *vterm* chunk))))
      (when (search pattern (screen))
        (return t))
      (when (>= (get-universal-time) deadline)
        (format t "  TIMEOUT waiting for: ~S~%" pattern)
        (format t "  --- screen ---~%~A~%  ---~%" (screen))
        (error "Timeout waiting for ~S" pattern))
      (sleep 0.2))))


;;;; Keystroke helpers.

(defun send-string (str)
  (%fd-write *fd* str))

(defun send-key (key)
  (send-string
   (ecase key
     (:ctrl-a  (string #\Soh))
     (:ctrl-b  (string #\Stx))
     (:ctrl-e  (string #\Enq))
     (:ctrl-f  (string #\Ack))
     (:ctrl-g  (string #\Bel))
     (:ctrl-j  (string #\Newline))
     (:ctrl-k  (string #\Vt))
     (:ctrl-n  (string #\So))
     (:ctrl-p  (string #\Dle))
     (:ctrl-r  (string #\Dc2))
     (:ctrl-u  (string #\Nak))
     (:ctrl-x  (string #\Can))
     (:ctrl-y  (string #\Em))
     (:ctrl-_  (string (code-char 31)))
     (:escape  (string #\Escape))
     (:return  (string #\Return))
     (:backspace (string (code-char 127))))))

(defun send-alt (char)
  "Send ESC + CHAR in one write."
  (%fd-write *fd* (coerce (list #\Escape char) 'string))))


;;;; Assertions.

(defun check (label pattern)
  (if (search pattern (screen))
      (progn (format t "  PASS: ~A~%" label) (incf *pass*))
      (progn (format t "  FAIL: ~A  (expected: ~S)~%" label pattern)
             (format t "  --- screen ---~%~A~%  ---~%" (screen))
             (incf *fail*))))

(defun drain-with-timeout (&optional (timeout-sec 5))
  "Drain PTY output; return :hung if hemlock never goes quiet."
  (let ((deadline (+ (get-internal-real-time)
                     (* timeout-sec internal-time-units-per-second))))
    (loop while (< (get-internal-real-time) deadline)
          do (if (%fd-readable-p *fd* 100)
                 (let ((chunk (%fd-read-chunk *fd*)))
                   (when chunk
                     (hemlock.term:term-process-output *vterm* chunk))
                   (setf deadline (+ (get-internal-real-time)
                                     (* timeout-sec internal-time-units-per-second))))
                 (return :ok)))
    :hung))

(defun check-not-hung (label)
  (let ((result (drain-with-timeout 5)))
    (if (eq result :ok)
        (progn (format t "  PASS: ~A~%" label) (incf *pass*))
        (progn (format t "  FAIL: ~A  (hung)~%" label)
               (format t "  --- screen ---~%~A~%  ---~%" (screen))
               (incf *fail*)))))

(defun check-absent (label pattern)
  (if (not (search pattern (screen)))
      (progn (format t "  PASS: ~A~%" label) (incf *pass*))
      (progn (format t "  FAIL: ~A  (should NOT contain: ~S)~%" label pattern)
             (format t "  --- screen ---~%~A~%  ---~%" (screen))
             (incf *fail*))))


;;;; Setup / teardown.

(defun start-hemlock ()
  (setf *vterm* (hemlock.term:make-term :width *cols* :height *rows*))
  (let ((cmd (format nil "~A --eval ~S --eval ~S"
                     *sbcl-path*
                     "(asdf:load-system :hemlock)"
                     "(hemlock:hemlock)")))
    (multiple-value-bind (master-fd proc-conn)
        (hemlock.term:spawn-pty-process cmd :rows *rows* :cols *cols*)
      (setf *fd* master-fd
            *proc-conn* proc-conn)))
  (format t "  Hemlock loading (up to ~Ds)...~%" *load-wait*)
  (wait-for-pattern "Hemlock")
  (sleep 1)
  (drain)
  (format t "  Hemlock loaded.~%"))

(defun stop-hemlock ()
  (when *proc-conn*
    (let ((pid (ignore-errors
                 (slot-value *proc-conn* 'hemlock::process))))
      (when (integerp pid)
        (ignore-errors (sb-posix:kill pid 15))
        (ignore-errors (sb-posix:waitpid pid 0))))
    (when *fd*
      (ignore-errors (sb-posix:close *fd*)))
    (setf *proc-conn* nil *fd* nil *vterm* nil)))


;;;; Tests.

(defun run-tests ()

  (format t "~%--- Test 1: Basic character insertion ---~%")
  (send-string "Hello World")
  (drain)
  (check "typed chars appear in buffer" "Hello World")

  (format t "~%--- Test 2: C-k Kill Line ---~%")
  (send-key :ctrl-a) (drain)
  (send-key :ctrl-k) (drain)
  (check-absent "killed text gone from buffer" "Hello World")
  (check-absent "no TYPE-ERROR after C-k"      "TYPE-ERROR")
  (check-absent "no debugger after C-k"        "Debugger")

  (format t "~%--- Test 3: Backspace ---~%")
  (send-string "abc") (drain)
  (send-key :backspace) (drain)
  (check-absent "no TYPE-ERROR after backspace" "TYPE-ERROR")
  (check-absent "no Debugger after backspace"   "Debugger")

  (format t "~%--- Test 4: C-f / C-b navigation ---~%")
  (send-string "test") (drain)
  (send-key :ctrl-b) (drain)
  (send-key :ctrl-b) (drain)
  (send-key :ctrl-f) (drain)
  (check-absent "no error on navigation" "TYPE-ERROR")
  (check-absent "no debugger on navigation" "Debugger")

  (format t "~%--- Test 5: C-a / C-e ---~%")
  (send-key :ctrl-a) (drain)
  (send-key :ctrl-e) (drain)
  (check-absent "no error C-a/C-e" "Debugger")

  (format t "~%--- Test 6: C-p / C-n ---~%")
  (send-key :ctrl-a) (drain)
  (send-key :ctrl-k) (drain)
  (send-string "line1") (drain)
  (send-key :ctrl-e) (drain)
  (send-key :ctrl-j) (drain)
  (send-string "line2") (drain)
  (send-key :ctrl-p) (drain)
  (check-absent "no error on C-p" "Debugger")
  (send-key :ctrl-n) (drain)
  (check-absent "no error on C-n" "Debugger")

  (format t "~%--- Test 7: M-x extended command ---~%")
  (send-key :escape)
  (sleep 0.15)
  (send-string "x")
  (drain)
  (check        "M-x opens prompt"                "Extended Command:")
  (check-absent "M-x does not insert literal ^[x" "^[x")
  (send-key :ctrl-g) (drain)

  (format t "~%--- Test 8: C-g abort ---~%")
  (check-absent "no error after C-g" "Debugger")

  (format t "~%--- Test 9: C-x b list buffers ---~%")
  (send-key :ctrl-x) (sleep 0.15)
  (send-string "b") (drain)
  (wait-for-pattern "Main" :timeout 5)
  (check        "buffer list appears"         "Main")
  (check-absent "no error listing buffers"    "Debugger")
  (send-key :ctrl-g) (drain)

  (format t "~%--- Test 10: C-x C-f find file ---~%")
  (send-key :ctrl-x) (sleep 0.15)
  (send-key :ctrl-f) (drain)
  (check        "find-file prompt appears"     "File:")
  (check-absent "no debugger on C-x C-f"       "Debugger")
  (send-key :ctrl-g) (drain)

  (format t "~%--- Test 11: Undo ---~%")
  (send-key :ctrl-g) (drain)
  (send-key :ctrl-x) (sleep 0.15)
  (send-string "b") (drain)
  (send-key :return) (drain)
  (send-string "undotest") (drain)
  (send-key :ctrl-a) (drain)
  (send-key :ctrl-k) (drain)
  (send-key :ctrl-_) (drain)
  (when (search "Undo the last kill?" (screen))
    (send-string "y") (drain))
  (check        "undo restores killed text" "undotest")
  (check-absent "no debugger after undo"   "Debugger")

  (format t "~%--- Test 12: Yank ---~%")
  (send-key :ctrl-a) (drain)
  (send-key :ctrl-k) (drain)
  (send-key :ctrl-y) (drain)
  (check        "yank restores text"     "undotest")
  (check-absent "no debugger after yank" "Debugger")

  (format t "~%--- Test 13: Alt key (Meta) ---~%")
  (send-key :ctrl-g) (drain)
  (send-alt #\x) (drain)
  (check        "Alt-x opens extended command" "Extended Command:")
  (check-absent "no debugger on Alt-x"         "Debugger")
  (send-key :ctrl-g) (drain)

  (format t "~%--- Test 14: Alt-o single expand ---~%")
  (send-key :ctrl-g) (drain)
  (send-alt #\o) (drain)
  (check-absent "no UNDEFINED-FUNCTION on Alt-o" "UNDEFINED-FUNCTION")
  (check-absent "no debugger on Alt-o"           "Debugger")

  (format t "~%--- Test 15: Alt-o repeated expand ---~%")
  (send-key :ctrl-x) (sleep 0.15)
  (send-key :ctrl-f) (drain)
  (send-string "src/user/modal.lisp") (drain)
  (send-key :return) (drain 2)
  (send-string "5j") (drain)
  (dotimes (i 10)
    (send-alt #\o) (drain 0.2))
  (check-not-hung "not hung after 10x Alt-o")
  (check-absent "no debugger after repeated expand" "Debugger")
  (check-absent "no UNDEFINED-FUNCTION after expand" "UNDEFINED-FUNCTION")

  (format t "~%--- Test 16: Alt-i shrink ---~%")
  (send-alt #\i) (drain 0.3)
  (check-not-hung "not hung after Alt-i shrink")
  (check-absent "no debugger after shrink" "Debugger")

  (format t "~%--- Test 17: Escape after expand ---~%")
  (send-key :escape) (drain 0.3)
  (send-string "j") (drain 0.3)
  (send-string "j") (drain 0.3)
  (check-not-hung "not hung after Escape+navigation post-expand")
  (check-absent "no debugger after Escape+navigation" "Debugger")

  (format t "~%--- Test 18: Expand/shrink cycle ---~%")
  (dotimes (i 5) (send-alt #\o) (drain 0.2))
  (dotimes (i 5) (send-alt #\i) (drain 0.2))
  (dotimes (i 3) (send-alt #\o) (drain 0.2))
  (send-key :escape) (drain 0.3)
  (send-string "itest") (drain 0.3)
  (send-key :escape) (drain 0.3)
  (check-not-hung "not hung after expand/shrink cycling")
  (check-absent "no debugger after expand/shrink cycle" "Debugger"))


;;;; Main.

(format t "=== Hemlock Lisp Test Suite ===~%")

(handler-case
    (unwind-protect
         (progn
           (start-hemlock)
           (run-tests))
      (stop-hemlock))
  (error (e)
    (format t "~%FATAL: ~A~%" e)
    (stop-hemlock)))

(format t "~%=============================~%")
(format t "Results: ~D passed, ~D failed~%" *pass* *fail*)
(format t "=============================~%")

(sb-ext:exit :code (if (zerop *fail*) 0 1))
