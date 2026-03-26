;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; Hemlock Lisp-native test harness
;;;
;;; Replaces test-hemlock.sh: no tmux, no bash, no external tools.
;;; Spawns hemlock in a PTY subprocess, drives it via the hemlock.term
;;; VT100 emulator, and asserts on the virtual screen grid.
;;;
;;; Usage:
;;;   cd /Users/bret.horne/git/cl/hemlock-port
;;;   sbcl --script test-hemlock.lisp
;;;

;; ─── Package ─────────────────────────────────────────────────────────────────

(defpackage :hemlock-test-harness
  (:use :cl))
(in-package :hemlock-test-harness)


;; ─── Bootstrap ──────────────────────────────────────────────────────────────

(require 'asdf)
(require 'cffi)

;; Replicate what .sbclrc does (--script skips user init).
(let ((ocicl #P"/Users/bret.horne/.local/share/ocicl/ocicl-runtime.lisp"))
  (when (probe-file ocicl) (load ocicl)))

(asdf:initialize-source-registry
 '(:source-registry
   (:directory #P"/Users/bret.horne/git/cl/hemlock-port/")
   :inherit-configuration))

;; Load just enough for the virtual screen API. hemlock.term pulls in
;; hemlock.base + cffi; the Terminal mode file (mode.lisp) needs the
;; full hemlock package, so load it.
(asdf:load-system :hemlock)


;; ─── Parameters ─────────────────────────────────────────────────────────────

(defparameter *hemlock-dir*    #P"/Users/bret.horne/git/cl/hemlock-port/")
(defparameter *sbcl-path*      "/opt/homebrew/bin/sbcl")
(defparameter *cols*           220)
(defparameter *rows*           50)
(defparameter *load-wait*      90)   ; seconds to wait for hemlock to load
(defparameter *cmd-wait*       0.4)  ; seconds to sleep after each command
(defparameter *quiescent-ms*   200)  ; poll timeout for "no more output"

(defvar *pass*     0)
(defvar *fail*     0)
(defvar *vterm*    nil)   ; hemlock.term:term object
(defvar *proc-conn* nil)  ; hemlock process-connection (for cleanup)
(defvar *fd*       nil)   ; integer — PTY master file descriptor


;; ─── PTY I/O (raw fd, mirrors terminal-drain-pty in src/term/mode.lisp) ────

(defun %fd-readable-p (fd &optional (timeout-ms 0))
  "True if FD has bytes waiting (poll with TIMEOUT-MS timeout)."
  ;; struct pollfd: fd:int32 events:int16 revents:int16  (total 8 bytes)
  (cffi:with-foreign-object (pfd :uint64)
    (setf (cffi:mem-ref pfd :int32 0) fd
          (cffi:mem-ref pfd :int16 4) 1  ; POLLIN
          (cffi:mem-ref pfd :int16 6) 0)
    (plusp (cffi:foreign-funcall "poll" :pointer pfd :uint32 1 :int timeout-ms :int))))

(defun %fd-read-chunk (fd)
  "Non-blocking read from FD; return UTF-8 string or NIL when empty."
  (let ((buf (make-array 8192 :element-type '(unsigned-byte 8))))
    (cffi:with-pointer-to-vector-data (ptr buf)
      (let ((n (cffi:foreign-funcall "read" :int fd :pointer ptr :size 8192 :long)))
        (when (plusp n)
          (sb-ext:octets-to-string buf :external-format :utf-8 :end n))))))

(defun %fd-write (fd str)
  "Write STR to FD as UTF-8 bytes."
  (let ((bytes (sb-ext:string-to-octets str :external-format :utf-8)))
    (sb-unix:unix-write fd bytes 0 (length bytes))))


;; ─── Virtual screen ──────────────────────────────────────────────────────────

(defun screen ()
  "Current grid content as a plain-text string."
  (hemlock.term:term-dump-to-string *vterm*))

(defun drain (&optional (extra-wait *cmd-wait*))
  "Drain all available PTY output into *vterm*, then wait EXTRA-WAIT seconds."
  ;; Read until fd goes quiet for *quiescent-ms* ms.
  (loop while (%fd-readable-p *fd* *quiescent-ms*)
        do (let ((chunk (%fd-read-chunk *fd*)))
             (when chunk
               (hemlock.term:term-process-output *vterm* chunk))))
  (sleep extra-wait)
  ;; One final pass after the sleep to catch any trailing output.
  (loop while (%fd-readable-p *fd* 0)
        do (let ((chunk (%fd-read-chunk *fd*)))
             (when chunk
               (hemlock.term:term-process-output *vterm* chunk)))))

(defun wait-for-pattern (pattern &key (timeout *load-wait*))
  "Keep draining until PATTERN appears on screen, or error on timeout."
  (let ((deadline (+ (get-universal-time) timeout)))
    (loop
      ;; Quick drain pass
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


;; ─── Send keystrokes ─────────────────────────────────────────────────────────

(defun send-string (str)
  "Send raw string to PTY (characters typed literally)."
  (%fd-write *fd* str))

(defun send-key (key)
  "Send a named key to PTY."
  (send-string
   (ecase key
     (:ctrl-a  (string #\Soh))          ;  1
     (:ctrl-b  (string #\Stx))          ;  2
     (:ctrl-e  (string #\Enq))          ;  5
     (:ctrl-f  (string #\Ack))          ;  6
     (:ctrl-g  (string #\Bel))          ;  7
     (:ctrl-j  (string #\Newline))      ; 10
     (:ctrl-k  (string #\Vt))           ; 11
     (:ctrl-n  (string #\So))           ; 14
     (:ctrl-p  (string #\Dle))          ; 16
     (:ctrl-r  (string #\Dc2))          ; 18
     (:ctrl-u  (string #\Nak))          ; 21
     (:ctrl-x  (string #\Can))          ; 24
     (:ctrl-y  (string #\Em))           ; 25
     (:ctrl-_  (string (code-char 31))) ; 31
     (:escape  (string #\Escape))       ; 27
     (:return  (string #\Return))       ; 13
     (:backspace (string (code-char 127)))))

(defun send-alt (char)
  "Send Alt+CHAR as ESC followed by CHAR in a single write (meta key)."
  (%fd-write *fd* (coerce (list #\Escape char) 'string))))


;; ─── Assertions ──────────────────────────────────────────────────────────────

(defun check (label pattern)
  (if (search pattern (screen))
      (progn (format t "  PASS: ~A~%" label) (incf *pass*))
      (progn (format t "  FAIL: ~A  (expected: ~S)~%" label pattern)
             (format t "  --- screen ---~%~A~%  ---~%" (screen))
             (incf *fail*))))

(defun drain-with-timeout (&optional (timeout-sec 5))
  "Drain PTY output. If hemlock keeps producing output past TIMEOUT-SEC,
return :hung instead of blocking forever."
  (let ((deadline (+ (get-internal-real-time)
                     (* timeout-sec internal-time-units-per-second))))
    (loop while (< (get-internal-real-time) deadline)
          do (if (%fd-readable-p *fd* 100)
                 (let ((chunk (%fd-read-chunk *fd*)))
                   (when chunk
                     (hemlock.term:term-process-output *vterm* chunk))
                   ;; Reset deadline on new output — give it more time.
                   (setf deadline (+ (get-internal-real-time)
                                     (* timeout-sec internal-time-units-per-second))))
                 ;; No output for 100ms — hemlock is quiescent.
                 (return :ok)))
    ;; If we got here, we timed out — hemlock is still producing output.
    :hung))

(defun check-not-hung (label)
  "Drain with timeout and fail if hemlock appears hung."
  (let ((result (drain-with-timeout 5)))
    (if (eq result :ok)
        (progn (format t "  PASS: ~A~%" label) (incf *pass*))
        (progn (format t "  FAIL: ~A  (hemlock hung — no quiescence in 5s)~%" label)
               (format t "  --- screen ---~%~A~%  ---~%" (screen))
               (incf *fail*)))))

(defun check-absent (label pattern)
  (if (not (search pattern (screen)))
      (progn (format t "  PASS: ~A~%" label) (incf *pass*))
      (progn (format t "  FAIL: ~A  (should NOT contain: ~S)~%" label pattern)
             (format t "  --- screen ---~%~A~%  ---~%" (screen))
             (incf *fail*))))


;; ─── Setup / teardown ────────────────────────────────────────────────────────

(defun start-hemlock ()
  ;; hemlock.term:spawn-pty-process uses find-a-pty → spawn_with_ctty
  ;; which properly calls setsid()+TIOCSCTTY so the child gets /dev/tty.
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
    ;; connection-process slot holds the integer PID when spawned via spawn_with_ctty.
    (let ((pid (ignore-errors
                 (slot-value *proc-conn* 'hemlock::process))))
      (when (integerp pid)
        (ignore-errors (sb-posix:kill pid 15))
        (ignore-errors (sb-posix:waitpid pid 0))))
    (when *fd*
      (ignore-errors (sb-posix:close *fd*)))
    (setf *proc-conn* nil *fd* nil *vterm* nil)))


;; ─── Test suite ─────────────────────────────────────────────────────────────

(defun run-tests ()

  ;; ── Test 1: Basic character insertion ────────────────────────────────────
  (format t "~%--- Test 1: Basic character insertion ---~%")
  (send-string "Hello World")
  (drain)
  (check "typed chars appear in buffer" "Hello World")

  ;; ── Test 2: C-k Kill Line ────────────────────────────────────────────────
  (format t "~%--- Test 2: C-k Kill Line ---~%")
  (send-key :ctrl-a) (drain)
  (send-key :ctrl-k) (drain)
  (check-absent "killed text gone from buffer" "Hello World")
  (check-absent "no TYPE-ERROR after C-k"      "TYPE-ERROR")
  (check-absent "no debugger after C-k"        "Debugger")

  ;; ── Test 3: Backspace ────────────────────────────────────────────────────
  (format t "~%--- Test 3: Backspace ---~%")
  (send-string "abc") (drain)
  (send-key :backspace) (drain)
  (check-absent "no TYPE-ERROR after backspace" "TYPE-ERROR")
  (check-absent "no Debugger after backspace"   "Debugger")

  ;; ── Test 4: C-f / C-b navigation ─────────────────────────────────────────
  (format t "~%--- Test 4: C-f / C-b navigation ---~%")
  (send-string "test") (drain)
  (send-key :ctrl-b) (drain)
  (send-key :ctrl-b) (drain)
  (send-key :ctrl-f) (drain)
  (check-absent "no error on navigation" "TYPE-ERROR")
  (check-absent "no debugger on navigation" "Debugger")

  ;; ── Test 5: C-a / C-e ────────────────────────────────────────────────────
  (format t "~%--- Test 5: C-a / C-e beginning/end of line ---~%")
  (send-key :ctrl-a) (drain)
  (send-key :ctrl-e) (drain)
  (check-absent "no error C-a/C-e" "Debugger")

  ;; ── Test 6: C-p / C-n line navigation ────────────────────────────────────
  (format t "~%--- Test 6: C-p / C-n line navigation ---~%")
  (send-key :ctrl-a) (drain)
  (send-key :ctrl-k) (drain)
  (send-string "line1") (drain)
  (send-key :ctrl-e) (drain)
  (send-key :ctrl-j) (drain)          ; insert newline
  (send-string "line2") (drain)
  (send-key :ctrl-p) (drain)
  (check-absent "no error on C-p" "Debugger")
  (send-key :ctrl-n) (drain)
  (check-absent "no error on C-n" "Debugger")

  ;; ── Test 7: M-x extended command ─────────────────────────────────────────
  (format t "~%--- Test 7: M-x extended command ---~%")
  (send-key :escape)
  (sleep 0.15)
  (send-string "x")
  (drain)
  (check        "M-x opens prompt"                "Extended Command:")
  (check-absent "M-x does not insert literal ^[x" "^[x")
  (send-key :ctrl-g) (drain)

  ;; ── Test 8: C-g abort ────────────────────────────────────────────────────
  (format t "~%--- Test 8: C-g abort ---~%")
  (check-absent "no error after C-g" "Debugger")

  ;; ── Test 9: C-x C-b list buffers ─────────────────────────────────────────
  (format t "~%--- Test 9: C-x C-b list buffers ---~%")
  (send-key :ctrl-x) (sleep 0.15)
  (send-string "b") (drain)            ; Note: C-x b not C-x C-b (same as shell script)
  (wait-for-pattern "Main" :timeout 5)
  (check        "buffer list appears"         "Main")
  (check-absent "no error listing buffers"    "Debugger")
  (send-key :ctrl-g) (drain)

  ;; ── Test 10: C-x C-f find file prompt ────────────────────────────────────
  (format t "~%--- Test 10: C-x C-f find file prompt ---~%")
  (send-key :ctrl-x) (sleep 0.15)
  (send-key :ctrl-f) (drain)
  (check        "find-file prompt appears"     "File:")
  (check-absent "no debugger on C-x C-f"       "Debugger")
  (send-key :ctrl-g) (drain)

  ;; ── Test 11: Undo ─────────────────────────────────────────────────────────
  (format t "~%--- Test 11: Undo ---~%")
  (send-key :ctrl-g) (drain)
  ;; Switch back to Main buffer.
  (send-key :ctrl-x) (sleep 0.15)
  (send-string "b") (drain)
  (send-key :return) (drain)
  ;; Type, kill, undo.
  (send-string "undotest") (drain)
  (send-key :ctrl-a) (drain)
  (send-key :ctrl-k) (drain)
  (send-key :ctrl-_) (drain)
  (when (search "Undo the last kill?" (screen))
    (send-string "y") (drain))
  (check        "undo restores killed text" "undotest")
  (check-absent "no debugger after undo"   "Debugger")

  ;; ── Test 12: Yank ────────────────────────────────────────────────────────
  (format t "~%--- Test 12: Yank ---~%")
  (send-key :ctrl-a) (drain)
  (send-key :ctrl-k) (drain)          ; kill "undotest" into kill ring
  (send-key :ctrl-y) (drain)          ; yank back
  (check        "yank restores text"     "undotest")
  (check-absent "no debugger after yank" "Debugger")

  ;; ── Test 13: Alt key (Meta) via send-alt ───────────────────────────────
  (format t "~%--- Test 13: Alt key (Meta) ---~%")
  (send-key :ctrl-g) (drain)
  (send-alt #\x) (drain)
  (check        "Alt-x opens extended command" "Extended Command:")
  (check-absent "no debugger on Alt-x"         "Debugger")
  (send-key :ctrl-g) (drain)

  ;; ── Test 14: Alt-o single expand ─────────────────────────────────────
  (format t "~%--- Test 14: Alt-o single expand ---~%")
  (send-key :ctrl-g) (drain)
  (send-alt #\o) (drain)
  (check-absent "no UNDEFINED-FUNCTION on Alt-o" "UNDEFINED-FUNCTION")
  (check-absent "no debugger on Alt-o"           "Debugger")

  ;; ── Test 15: Alt-o repeated expand until top-level ──────────────────
  (format t "~%--- Test 15: Alt-o repeated expand to top-level ---~%")
  ;; Open a Lisp file so tree-sitter has something to parse.
  (send-key :ctrl-x) (sleep 0.15)
  (send-key :ctrl-f) (drain)
  ;; Open a file we know exists.
  (send-string "src/user/modal.lisp") (drain)
  (send-key :return) (drain 2)  ; give tree-sitter time to parse
  ;; Navigate into some code.
  (send-string "5j") (drain)    ; move down a few lines in Normal mode
  ;; Expand repeatedly — should hit top-level and stop.
  (dotimes (i 10)
    (send-alt #\o) (drain 0.2))
  (check-not-hung "not hung after 10x Alt-o")
  (check-absent "no debugger after repeated expand" "Debugger")
  (check-absent "no UNDEFINED-FUNCTION after expand" "UNDEFINED-FUNCTION")

  ;; ── Test 16: Alt-i shrink after expand ──────────────────────────────
  (format t "~%--- Test 16: Alt-i shrink after expand ---~%")
  (send-alt #\i) (drain 0.3)
  (check-not-hung "not hung after Alt-i shrink")
  (check-absent "no debugger after shrink" "Debugger")

  ;; ── Test 17: Escape after expand, then type ─────────────────────────
  (format t "~%--- Test 17: Escape after expand, then normal use ---~%")
  (send-key :escape) (drain 0.3)
  (send-string "j") (drain 0.3)
  (send-string "j") (drain 0.3)
  (check-not-hung "not hung after Escape+navigation post-expand")
  (check-absent "no debugger after Escape+navigation" "Debugger")

  ;; ── Test 18: Expand/shrink cycle ────────────────────────────────────
  (format t "~%--- Test 18: Expand/shrink cycle ---~%")
  ;; Alt-o 5x, Alt-i 5x, Alt-o 3x, Escape, type
  (dotimes (i 5) (send-alt #\o) (drain 0.2))
  (dotimes (i 5) (send-alt #\i) (drain 0.2))
  (dotimes (i 3) (send-alt #\o) (drain 0.2))
  (send-key :escape) (drain 0.3)
  (send-string "itest") (drain 0.3)  ; enter insert mode and type
  (send-key :escape) (drain 0.3)     ; back to normal
  (check-not-hung "not hung after expand/shrink cycling")
  (check-absent "no debugger after expand/shrink cycle" "Debugger"))


;; ─── Main ────────────────────────────────────────────────────────────────────

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
