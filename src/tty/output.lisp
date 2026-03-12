;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;

(in-package :hemlock.tty)

(pushnew :tty hemlock.command:*available-backends*)

(defvar *pending-resize* nil
  "Set to T by the SIGWINCH handler; cleared by maybe-resize-tty-device.")

(defvar *pending-resize-lines* nil
  "Number of terminal rows captured in the SIGWINCH handler, or NIL.")

(defvar *pending-resize-cols* nil
  "Number of terminal columns captured in the SIGWINCH handler, or NIL.")


;;;; Get terminal attributes:

(defvar *terminal-baud-rate* nil)
(declaim (type (or (unsigned-byte 24) null) *terminal-baud-rate*))

;;;;
;;;; CFFI definitions replacing osicat-posix
;;;;

;;; struct winsize
(cffi:defcstruct (winsize :size 8)
  (row    :uint16 :offset 0)
  (col    :uint16 :offset 2)
  (xpixel :uint16 :offset 4)
  (ypixel :uint16 :offset 6))

(defconstant tiocgwinsz
  #+darwin 1074295912
  #+linux  #x00005413)

(cffi:defcfun ("ioctl" ioctl-syscall) :int
  (fd :int) (request :unsigned-long) &rest)

;;; struct termios
(cffi:defcstruct (termios :size 72)
  (iflag :uint32 :offset  0)
  (oflag :uint32 :offset  8)
  (cflag :uint32 :offset 16)
  (lflag :uint32 :offset 24)
  (cc    :uint8  :count 20 :offset 32))

;;; tcsetattr action codes
(defconstant tcsanow   0)
(defconstant tcsadrain 1)
(defconstant tcsaflush 2)

;;; _POSIX_VDISABLE
(defconstant posix-vdisable 255)

;;; c_cc indices
#+darwin
(progn
  (defconstant cflag-veof    0)
  (defconstant cflag-veol    1)
  (defconstant cflag-veol2   2)
  (defconstant cflag-verase  3)
  (defconstant cflag-vwerase 4)
  (defconstant cflag-vkill   5)
  (defconstant cflag-vintr   8)
  (defconstant cflag-vquit   9)
  (defconstant cflag-vsusp  10)
  (defconstant cflag-vdsusp 11)
  (defconstant cflag-vstart 12)
  (defconstant cflag-vstop  13)
  (defconstant cflag-vmin   16)
  (defconstant cflag-vtime  17))

#+linux
(progn
  (defconstant cflag-vintr   0)
  (defconstant cflag-vquit   1)
  (defconstant cflag-verase  2)
  (defconstant cflag-vkill   3)
  (defconstant cflag-veof    4)
  (defconstant cflag-vtime   5)
  (defconstant cflag-vmin    6)
  (defconstant cflag-vstart  8)
  (defconstant cflag-vstop   9)
  (defconstant cflag-vsusp  10)
  (defconstant cflag-veol   11)
  (defconstant cflag-vmin   16))

;;; c_iflag bits
#+darwin
(progn
  (defconstant tty-ignbrk #x0001)
  (defconstant tty-brkint #x0002)
  (defconstant tty-istrip #x0020)
  (defconstant tty-icrnl  #x0100)
  (defconstant tty-ixon   #x0200))

#+linux
(progn
  (defconstant tty-ignbrk #x0001)
  (defconstant tty-brkint #x0002)
  (defconstant tty-istrip #x0020)
  (defconstant tty-icrnl  #x0100)
  (defconstant tty-ixon   #x0400))

;;; c_oflag bits
#+darwin
(progn
  (defconstant tty-opost #x00001)
  (defconstant tty-onlcr #x00002)
  (defconstant tty-ocrnl #x00010))

#+linux
(progn
  (defconstant tty-opost #x00001)
  (defconstant tty-onlcr #x00004)
  (defconstant tty-ocrnl #x00008))

;;; c_lflag bits
#+darwin
(progn
  (defconstant tty-icanon #x0100)
  (defconstant tty-echo   #x0008)
  (defconstant tty-echonl #x0010))

#+linux
(progn
  (defconstant tty-icanon #x0002)
  (defconstant tty-echo   #x0008)
  (defconstant tty-echonl #x0040))

;;; syscall wrappers
(cffi:defcfun ("isatty"    isatty)    :int (fd :int))
(cffi:defcfun ("tcgetattr" tcgetattr) :int (fd :int) (termios :pointer))
(cffi:defcfun ("tcsetattr" tcsetattr) :int (fd :int) (action :int) (termios :pointer))

;;; GET-TERMINAL-ATTRIBUTES  --  Interface
;;;
;;;    Get terminal attributes from Unix.  Return as values, the lines,
;;; columns and speed.  If any value is inaccessible, return NIL for that
;;; value.  We also sleazily cache the speed in *terminal-baud-rate*, since I
;;; don't want to figure out how to get my hands on the TTY-DEVICE at the place
;;; where I need it.  Currently, there really can only be one TTY anyway, since
;;; the buffer is in a global.
;;;
(defun get-terminal-attributes (&optional fd)
  "Return (values lines cols baud-rate).
Query the controlling terminal via /dev/tty if no FD is given.
Falls back to stty(1) if the ioctl fails."
  (let ((baud-rate 4800))
    (setf *terminal-baud-rate* baud-rate)
    (handler-case
        (let ((tty-fd (or fd (sb-posix:open "/dev/tty" sb-posix:o-rdwr))))
          (unwind-protect
               (cffi:with-foreign-object (ws '(:struct winsize))
                 (ioctl-syscall tty-fd tiocgwinsz :pointer ws)
                 (cffi:with-foreign-slots ((row col) ws (:struct winsize))
                   (values row col baud-rate)))
            (unless fd
              (sb-posix:close tty-fd))))
      (error ()
        (handler-case
            (let ((out (uiop:run-program '("stty" "size")
                                        :output :string
                                        :error-output nil
                                        :input "/dev/tty")))
              (let ((parts (uiop:split-string out)))
                (when (= 2 (length parts))
                  (values (parse-integer (first parts))
                          (parse-integer (second parts))
                          baud-rate))))
          (error () (values nil nil baud-rate)))))))


;;;; Output routines and buffering.

(defconstant redisplay-output-buffer-length 256)

(defvar *redisplay-output-buffer*
  (make-string redisplay-output-buffer-length))
(declaim (simple-string *redisplay-output-buffer*))

(defvar *redisplay-output-buffer-index* 0)
(declaim (fixnum *redisplay-output-buffer-index*))

;;; WRITE-AND-MAYBE-WAIT  --  Internal
;;;
;;;    Write the first Count characters in the redisplay output buffer.  If
;;; *terminal-baud-rate* is set, then sleep for long enough to allow the
;;; written text to be displayed.  We multiply by 10 to get the baud-per-byte
;;; conversion, which assumes 7 character bits + 1 start bit + 2 stop bits, no
;;; parity.
;;;
(defun write-and-maybe-wait (count)
  (declare (fixnum count))
  ;; dispatch-events-no-hang is NOT called here (per chunk) — it is called
  ;; once per redisplay cycle from device-force-output instead, after all
  ;; chunks are queued.  This avoids firing fd-handlers between chunks.
  (connection-write (subseq *redisplay-output-buffer* 0 count)
                    *tty-connection*))


;;; TTY-WRITE-STRING blasts the string into the redisplay output buffer.
;;; If the string overflows the buffer, then segments of the string are
;;; blasted into the buffer, dumping the buffer, until the last piece of
;;; the string is stored in the buffer.  The buffer is always dumped if
;;; it is full, even if the last piece of the string just fills the buffer.
;;;
(defun tty-write-string (string &optional (start 0) length)
  (declare (fixnum start)
           (type (or fixnum null) length))
  (let ((buffer-space (- redisplay-output-buffer-length
                         *redisplay-output-buffer-index*))
        (length (or length (length string))))
    (declare (fixnum buffer-space))
    (cond ((<= length buffer-space)
           (let ((dst-index (+ *redisplay-output-buffer-index* length)))
             #+(or)
             (%primitive byte-blt
                         string             ;src
                         start              ;src-start
                         *redisplay-output-buffer* ;dst
                         *redisplay-output-buffer-index* ;dst-start
                         dst-index                       ;dst-end
                         )
             (replace *redisplay-output-buffer*
                      string
                      :start1 *redisplay-output-buffer-index*
                      :end1 dst-index
                      :start2 start)
             (cond ((= length buffer-space)
                    (write-and-maybe-wait redisplay-output-buffer-length)
                    (setf *redisplay-output-buffer-index* 0))
                   (t
                    (setf *redisplay-output-buffer-index* dst-index)))))
          (t
           (let ((remaining (- length buffer-space)))
             (declare (fixnum remaining))
             (loop
              #+(or)
                (%primitive byte-blt
                            string                        ;src
                            start                         ;src-start
                            *redisplay-output-buffer*     ;dst
                            *redisplay-output-buffer-index* ;dst-start
                            redisplay-output-buffer-length  ;dst-end
                            )
                (replace *redisplay-output-buffer*
                         string
                         :start1 *redisplay-output-buffer-index*
                         :end1 redisplay-output-buffer-length
                         :start2 start)
              (write-and-maybe-wait redisplay-output-buffer-length)
              (when (< remaining redisplay-output-buffer-length)
                #+(or)
                (%primitive byte-blt
                            string                    ;src
                            (+ start buffer-space)    ;src-start
                            *redisplay-output-buffer* ;dst
                            0                         ;dst-start
                            remaining                 ;dst-end
                            )
                (replace *redisplay-output-buffer*
                         string
                         :start1 0
                         :end1 remaining
                         :start2 (+ start buffer-space))
                (setf *redisplay-output-buffer-index* remaining)
                (return t))
              (incf start buffer-space)
              (setf *redisplay-output-buffer-index* 0)
              (setf buffer-space redisplay-output-buffer-length)
              (decf remaining redisplay-output-buffer-length)))))))


;;; TTY-WRITE-CHAR stores a character in the redisplay output buffer,
;;; dumping the buffer if it becomes full.
;;;
(defun tty-write-char (char)
  (setf (schar *redisplay-output-buffer* *redisplay-output-buffer-index*)
        char)
  (incf *redisplay-output-buffer-index*)
  (when (= *redisplay-output-buffer-index* redisplay-output-buffer-length)
    (write-and-maybe-wait redisplay-output-buffer-length)
    (setf *redisplay-output-buffer-index* 0)))

;;; Write a terminfo string returned by 'tputs which is a list of
;;; strings and delays.
(defun tty-write-cmd (cmd)
  (declare (type (or string list) cmd))
  (etypecase cmd
    (string
     (tty-write-string cmd))
    (list
     (dolist (string-or-delay cmd)
       (cond ((stringp string-or-delay)
              (tty-write-string string-or-delay))
             ((numberp string-or-delay)
              ;; Not yet supported, but could pass the delay to the
              ;; event handler.
              ))))))

;;; Return the total number of characters in the command list returned
;;; by 'tputs.
(defun tty-cmd-length (cmd)
  (declare (type (or string list) cmd))
  (etypecase cmd
    (string
     (length cmd))
    (list
     (let ((len 0))
       (dolist (string-or-delay cmd)
         (when (stringp string-or-delay)
           (incf len (length string-or-delay))))
       len))))


;;; TTY-FORCE-OUTPUT dumps the redisplay output buffer.  This is called
;;; out of terminal device structures in multiple places -- the device
;;; exit method, random typeout methods, out of tty-hunk-stream methods,
;;; after calls to REDISPLAY or REDISPLAY-ALL.
;;;
(defmethod device-force-output ((device tty-device))
  (unless (zerop *redisplay-output-buffer-index*)
    (write-and-maybe-wait *redisplay-output-buffer-index*)
    (setf *redisplay-output-buffer-index* 0))
  ;; Flush the async write queue once per redisplay cycle rather than
  ;; once per chunk, to avoid serving fd-handlers mid-output.
  (dispatch-events-no-hang))


;;; TTY-FINISH-OUTPUT simply dumps output.
;;;
(defmethod device-finish-output ((device tty-device) window)
  (declare (ignore window))
  (device-force-output device))



;;;; Terminal init and exit methods.

;;;; Terminal feature helpers (cursor shape, mouse, focus, window title)

(defun tty-set-cursor-shape (shape)
  "Set terminal cursor shape. SHAPE is :block, :underline, or :beam.
Uses the DECSCUSR escape sequence (xterm and most modern terminals)."
  (let ((code (ecase shape
                (:block     1)   ; blinking block
                (:underline 3)   ; blinking underline
                (:beam      5)   ; blinking bar / I-beam
                )))
    (tty-write-cmd (format nil "~C[~D q" #\Escape code))))

(defun tty-set-window-title (title)
  "Set the terminal window/tab title via OSC 2."
  (tty-write-cmd (format nil "~C]2;~A~C\\" #\Escape title #\Escape)))

(defun tty-enable-mouse ()
  "Enable SGR 1006 mouse button+motion reporting."
  (tty-write-cmd (format nil "~C[?1002h~C[?1006h" #\Escape #\Escape)))

(defun tty-disable-mouse ()
  "Disable mouse tracking."
  (tty-write-cmd (format nil "~C[?1002l~C[?1006l" #\Escape #\Escape)))

(defun tty-enable-focus-events ()
  "Enable focus-in / focus-out event reporting (ESC[I / ESC[O)."
  (tty-write-cmd (format nil "~C[?1004h" #\Escape)))

(defun tty-disable-focus-events ()
  "Disable focus event reporting."
  (tty-write-cmd (format nil "~C[?1004l" #\Escape)))


(defmethod device-init ((device tty-device))
  (setup-input)
  (tty-write-cmd (tty-device-init-string device))
  ;; Enter alternate screen buffer — gives us a clean slate and preserves
  ;; the user's shell scrollback.
  (tty-write-cmd (format nil "~C[?1049h" #\Escape))
  ;; Enable bracketed paste mode.
  (tty-write-cmd (format nil "~C[?2004h" #\Escape))
  ;; Enable mouse button+motion tracking (SGR 1006 format).
  (tty-enable-mouse)
  ;; Enable focus in/out events.
  (tty-enable-focus-events)
  ;; Set block cursor for normal mode.
  (tty-set-cursor-shape :block)
  (tty-set-window-title "Hemlock")
  ;; Flush the terminal init sequences immediately.
  (device-force-output device)
  ;; Mark screen as trashed so the command loop does the initial repaint.
  (setf *screen-image-trashed* t))

(defmethod device-exit ((device tty-device))
  ;; Disable mouse, focus events, bracketed paste.
  (tty-disable-mouse)
  (tty-disable-focus-events)
  (tty-write-cmd (format nil "~C[?2004l" #\Escape))
  ;; Restore default cursor shape.
  (tty-write-cmd (format nil "~C[0 q" #\Escape))
  ;; Exit alternate screen buffer — restores the user's terminal state.
  (tty-write-cmd (format nil "~C[?1049l" #\Escape))
  (device-force-output device)
  (reset-input)
  ;; Restore global output streams so post-exit printing goes to the terminal.
  (when hemlock::*original-standard-output*
    (setf *standard-output* hemlock::*original-standard-output*
          *error-output*    hemlock::*original-error-output*
          *trace-output*    hemlock::*original-trace-output*)
    (setf hemlock::*original-standard-output* nil)))


;;;; Screen image line hacks.

(defun replace-si-line (dst-string src-string src-start dst-start dst-end)
;;;   `(%primitive byte-blt ,src-string ,src-start ,dst-string ,dst-start ,dst-end)
  (replace dst-string
           src-string
           :start1 dst-start
           :end1 dst-end
           :start2 src-start))

(defvar *old-c-iflag*)
(defvar *old-c-oflag*)
(defvar *old-c-cflag*)
(defvar *old-c-lflag*)
(defvar *old-c-cc*)

;;; The TTY erase character.
(defvar *tty-erase-char* nil)

(defun setup-input ()
  (let ((fd 1))
    (when (plusp (isatty fd))
      (cffi:with-foreign-object (tios '(:struct termios))
        (tcgetattr fd tios)
        (cffi:with-foreign-slots ((iflag oflag cflag lflag cc)
                                  tios (:struct termios))
          (setf *old-c-iflag* iflag)
          (setf *old-c-oflag* oflag)
          (setf *old-c-cflag* cflag)
          (setf *old-c-lflag* lflag)
          (macrolet ((ccref (slot)
                       `(cffi:mem-ref cc :uint8 ,slot)))
            (setf *old-c-cc*
                  (vector (ccref cflag-vsusp)
                          (ccref cflag-veof)
                          (ccref cflag-verase)
                          (ccref cflag-vintr)
                          (ccref cflag-vquit)
                          (ccref cflag-vstart)
                          (ccref cflag-vstop)
                          (ccref cflag-vsusp)
                          (when (boundp 'cflag-vdsusp)
                            (ccref cflag-vdsusp))
                          (ccref cflag-vmin)
                          (ccref cflag-vtime)))
            (setf lflag
                  (logandc2 lflag
                            (logior tty-echo
                                    tty-icanon)))
            (setf iflag
                  (logandc2 (logior iflag
                                    tty-ignbrk)
                            (logior tty-icrnl
                                    tty-istrip
                                    tty-ixon)))
            (setf oflag
                  (logandc2 oflag
                            (logior #-bsd tty-ocrnl
                                    #+bsd tty-onlcr)))
            (setf (ccref cflag-vsusp) posix-vdisable)
            (setf (ccref cflag-veof) posix-vdisable)
            (setf *tty-erase-char* (ccref cflag-verase))
            (setf (ccref cflag-vintr)
                  (if *editor-windowed-input* posix-vdisable 28))
            (setf (ccref cflag-vquit) posix-vdisable)
            (setf (ccref cflag-vstart) posix-vdisable)
            (setf (ccref cflag-vstop) posix-vdisable)
            (setf (ccref cflag-vsusp) posix-vdisable)
            (when (boundp 'cflag-vdsusp)
              (setf (ccref cflag-vdsusp) posix-vdisable))
            (setf (ccref cflag-vmin) 1)
            (setf (ccref cflag-vtime) 0))
          (tcsetattr fd tcsaflush tios))))))

;;; #+nil ;; #-(or hpux irix bsd glibc2)
;;;       (alien:with-alien ((sg (alien:struct unix:sgttyb)))
;;;     (multiple-value-bind
;;;         (val err)
;;;         (unix:unix-ioctl fd unix:TIOCGETP (alien:alien-sap sg))
;;;       (unless val
;;;         (error "Could not get tty information, unix error ~S."
;;;                (unix:get-unix-error-msg err))))
;;;     (let ((flags (alien:slot sg 'unix:sg-flags)))
;;;       (setq old-flags flags)
;;;       (setf (alien:slot sg 'unix:sg-flags)
;;;             (logand #-(or hpux irix bsd glibc2) (logior flags unix:tty-cbreak)
;;;                     (lognot unix:tty-echo)
;;;                     (lognot unix:tty-crmod)))
;;;       (multiple-value-bind
;;;           (val err)
;;;           (unix:unix-ioctl fd unix:TIOCSETP (alien:alien-sap sg))
;;;         (if (null val)
;;;             (error "Could not set tty information, unix error ~S."
;;;                    (unix:get-unix-error-msg err))))))
;;;       #+nil ;; #-(or hpux irix bsd glibc2)
;;;       (alien:with-alien ((tc (alien:struct unix:tchars)))
;;;     (multiple-value-bind
;;;         (val err)
;;;         (unix:unix-ioctl fd unix:TIOCGETC (alien:alien-sap tc))
;;;       (unless val
;;;         (error "Could not get tty tchars information, unix error ~S."
;;;                (unix:get-unix-error-msg err))))
;;;     (setq old-tchars
;;;           (vector (alien:slot tc 'unix:t-intrc)
;;;                   (alien:slot tc 'unix:t-quitc)
;;;                   (alien:slot tc 'unix:t-startc)
;;;                   (alien:slot tc 'unix:t-stopc)
;;;                   (alien:slot tc 'unix:t-eofc)
;;;                   (alien:slot tc 'unix:t-brkc)))
;;;     (setf (alien:slot tc 'unix:t-intrc)
;;;           (if *editor-windowed-input* -1 28))
;;;     (setf (alien:slot tc 'unix:t-quitc) -1)
;;;     (setf (alien:slot tc 'unix:t-startc) -1)
;;;     (setf (alien:slot tc 'unix:t-stopc) -1)
;;;     (setf (alien:slot tc 'unix:t-eofc) -1)
;;;     (setf (alien:slot tc 'unix:t-brkc) -1)
;;;     (multiple-value-bind
;;;         (val err)
;;;         (unix:unix-ioctl fd unix:TIOCSETC (alien:alien-sap tc))
;;;       (unless val
;;;         (error "Failed to set tchars, unix error ~S."
;;;                (unix:get-unix-error-msg err)))))

;;;       ;; Needed even under HpUx to suppress dsuspc.
;;;       #+nil
;;;       ;; #-(or glibc2 irix)
;;;       (alien:with-alien ((tc (alien:struct unix:ltchars)))
;;;     (multiple-value-bind
;;;         (val err)
;;;         (unix:unix-ioctl fd unix:TIOCGLTC (alien:alien-sap tc))
;;;       (unless val
;;;         (error "Could not get tty ltchars information, unix error ~S."
;;;                (unix:get-unix-error-msg err))))
;;;     (setq old-ltchars
;;;           (vector (alien:slot tc 'unix:t-suspc)
;;;                   (alien:slot tc 'unix:t-dsuspc)
;;;                   (alien:slot tc 'unix:t-rprntc)
;;;                   (alien:slot tc 'unix:t-flushc)
;;;                   (alien:slot tc 'unix:t-werasc)
;;;                   (alien:slot tc 'unix:t-lnextc)))
;;;     (setf (alien:slot tc 'unix:t-suspc) -1)
;;;     (setf (alien:slot tc 'unix:t-dsuspc) -1)
;;;     (setf (alien:slot tc 'unix:t-rprntc) -1)
;;;     (setf (alien:slot tc 'unix:t-flushc) -1)
;;;     (setf (alien:slot tc 'unix:t-werasc) -1)
;;;     (setf (alien:slot tc 'unix:t-lnextc) -1)
;;;     (multiple-value-bind
;;;         (val err)
;;;         (unix:unix-ioctl fd unix:TIOCSLTC (alien:alien-sap tc))
;;;       (unless val
;;;         (error "Failed to set ltchars, unix error ~S."
;;;                (unix:get-unix-error-msg err)))))

(defun reset-input ()
  (let ((fd 1))
    (when (plusp (isatty fd))
      (cffi:with-foreign-object (tios '(:struct termios))
        (tcgetattr fd tios)
        (cffi:with-foreign-slots ((iflag oflag cflag lflag cc)
                                  tios (:struct termios))
          (setf iflag *old-c-iflag*)
          (setf oflag *old-c-oflag*)
          (setf cflag *old-c-cflag*)
          (setf lflag *old-c-lflag*)
          (macrolet ((ccref (slot)
                       `(cffi:mem-ref cc :uint8 ,slot)))
            (assert (= (length *old-c-cc*) 11))
            (setf (ccref cflag-vsusp)  (elt *old-c-cc* 0)
                  (ccref cflag-veof)   (elt *old-c-cc* 1)
                  (ccref cflag-verase) (elt *old-c-cc* 2)
                  (ccref cflag-vintr)  (elt *old-c-cc* 3)
                  (ccref cflag-vquit)  (elt *old-c-cc* 4)
                  (ccref cflag-vstart) (elt *old-c-cc* 5)
                  (ccref cflag-vstop)  (elt *old-c-cc* 6)
                  (ccref cflag-vsusp)  (elt *old-c-cc* 7))
            (when (boundp 'cflag-vdsusp)
              (setf (ccref cflag-vdsusp) (elt *old-c-cc* 8)))
            (setf (ccref cflag-vmin)  (elt *old-c-cc* 9)
                  (ccref cflag-vtime) (elt *old-c-cc* 10)))
          (tcsetattr fd tcsaflush tios))))))

#+(or)
(defun pause-hemlock ()
  "Pause hemlock and pop out to the Unix Shell."
  (without-hemlock
   (unix:unix-kill (unix:unix-getpid) :sigstop))
  t)
