;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :hemlock-internals)

;;;; Editor input from a tty.

(defclass tty-editor-input (editor-input)
  ((fd :initarg :fd
       :accessor tty-editor-input-fd)))

(defun make-tty-editor-input (&rest args)
  (apply #'make-instance 'tty-editor-input args))

(defmethod get-key-event
    ((stream tty-editor-input) &optional ignore-abort-attempts-p)
  (%editor-input-method stream ignore-abort-attempts-p))

(defmethod unget-key-event (key-event (stream tty-editor-input))
  (un-event key-event stream))

(defmethod clear-editor-input ((stream tty-editor-input))
  (hemlock-ext:without-interrupts
   (let* ((head (editor-input-head stream))
          (next (input-event-next head)))
     (when next
       (setf (input-event-next head) nil)
       (shiftf (input-event-next (editor-input-tail stream))
               *free-input-events* next)
       (setf (editor-input-tail stream) head)))))

;;; Note that we never return NIL as long as there are events to be served with
;;; SERVE-EVENT.  Thus non-keyboard input (i.e. process output)
;;; effectively causes LISTEN to block until either all the non-keyboard input
;;; has happened, or there is some real keyboard input.
;;;
(defmethod listen-editor-input ((stream tty-editor-input))
  (process-editor-tty-input)
  nil)

(defvar *tty-translations* (make-hash-table :test #'equal))

(defun register-tty-translations ()
  (assert hemlock.terminfo:*terminfo*)
  (flet ((reg (string keysym)
           (let ((string (etypecase string
                           (character (string string))
                           (list (coerce string 'simple-string))
                           (string string))))
              (setf (gethash string *tty-translations*) keysym))))
    ;; KLUDGE: There seems to be no way get F1-F4 reliably transmit
    ;; things in the terminfo db, since some terminals transmit them
    ;; as if they were vt100 PF1-PF4, so, register these aliases here.
    ;; If they double as something else, that will override these.
    (reg '(#\Esc #\O #\P) #k"F1")
    (reg '(#\Esc #\O #\Q) #k"F2")
    (reg '(#\Esc #\O #\R) #k"F3")
    (reg '(#\Esc #\O #\S) #k"F4")
    ;; Terminfo definitions for F1-F12
    (reg hemlock.terminfo:key-f1 #k"F1")
    (reg hemlock.terminfo:key-f2 #k"F2")
    (reg hemlock.terminfo:key-f3 #k"F3")
    (reg hemlock.terminfo:key-f4 #k"F4")
    (reg hemlock.terminfo:key-f5 #k"F5")
    (reg hemlock.terminfo:key-f6 #k"F6")
    (reg hemlock.terminfo:key-f7 #k"F7")
    (reg hemlock.terminfo:key-f8 #k"F8")
    (reg hemlock.terminfo:key-f9 #k"F9")
    (reg hemlock.terminfo:key-f10 #k"F10")
    (reg hemlock.terminfo:key-f11 #k"F11")
    (reg hemlock.terminfo:key-f12 #k"F12")
    ;; Terminfo definitions for movement keys
    (reg hemlock.terminfo:key-up #k"Uparrow")
    (reg hemlock.terminfo:key-down #k"Downarrow")
    (reg hemlock.terminfo:key-right #k"Rightarrow")
    (reg hemlock.terminfo:key-left #k"Leftarrow")
    (reg hemlock.terminfo:key-home #k"Home")
    (reg hemlock.terminfo:key-end #k"End")
    (reg hemlock.terminfo:key-ic #k"Insert")
    (reg hemlock.terminfo:key-dc #k"Delete")
    (reg hemlock.terminfo:key-ppage #k"Pageup")
    (reg hemlock.terminfo:key-npage #k"Pagedown")
    (reg hemlock.terminfo:key-backspace #k"Backspace")
    ;; Many modern terminals (including macOS Terminal) send DEL (0x7F) for
    ;; the physical Backspace key rather than BS (0x08).  Register both.
    (reg (string (code-char 127)) #k"Backspace")

    (reg hemlock.terminfo:key-sr #k"Shift-Uparrow")
    (reg hemlock.terminfo:key-sf #k"Shift-Downarrow")
    (reg hemlock.terminfo:key-sright #k"Shift-Rightarrow")
    (reg hemlock.terminfo:key-sleft #k"Shift-Leftarrow")
    (reg hemlock.terminfo:key-shome #k"Shift-Home")
    (reg hemlock.terminfo:key-send #k"Shift-End")
    (reg hemlock.terminfo:key-sic #k"Shift-Insert")
    (reg hemlock.terminfo:key-sdc #k"Shift-Delete")
    (reg hemlock.terminfo:key-sprevious #k"Shift-Pageup")
    (reg hemlock.terminfo:key-snext #k"Shift-Pagedown")
    
    ;; Xterm definitions, not in terminfo.

    (reg "[1;5A" #k"Control-Uparrow")
    (reg "[1;5B" #k"Control-Downarrow")
    (reg "[1;5C" #k"Control-Rightarrow")
    (reg "[1;5D" #k"Control-Leftarrow")
    (reg "[1;5H" #k"Control-Home")
    (reg "[1;5F" #k"Control-End")
    (reg "[2;3~" #k"Control-Insert")
    (reg "[3;5~" #k"Control-Delete")
    (reg "[5;3~" #k"Control-Pageup")
    (reg "[6;3~" #k"Control-Pagedown")

    (reg "[1;3A" #k"Meta-Uparrow")
    (reg "[1;3B" #k"Meta-Downarrow")
    (reg "[1;3C" #k"Meta-Rightarrow")
    (reg "[1;3D" #k"Meta-Leftarrow")
    (reg "[1;3H" #k"Meta-Home")
    (reg "[1;3F" #k"Meta-End")
    (reg "[2;3~" #k"Meta-Insert")
    (reg "[3;3~" #k"Meta-Delete")
    (reg "[5;3~" #k"Meta-Pageup")
    (reg "[6;3~" #k"Meta-Pagedown")
    
    (reg "[1;6A" #k"Shift-Control-Uparrow")
    (reg "[1;6B" #k"Shift-Control-Downarrow")
    (reg "[1;6C" #k"Shift-Control-Rightarrow")
    (reg "[1;6D" #k"Shift-Control-Leftarrow")
    (reg "[1;6H" #k"Shift-Control-Home")
    (reg "[1;6F" #k"Shift-Control-End")
    (reg "[2;6~" #k"Shift-Control-Insert")
    (reg "[3;6~" #k"Shift-Control-Delete")
    (reg "[5;6~" #k"Shift-Control-PageUp")
    (reg "[6;6~" #k"Shift-Control-PageDown")
    (reg "[3;6~" #k"Shift-Control-Delete")

    (reg "[1;4A" #k"Shift-Meta-Uparrow")
    (reg "[1;4B" #k"Shift-Meta-Downarrow")
    (reg "[1;4C" #k"Shift-Meta-Rightarrow")
    (reg "[1;4D" #k"Shift-Meta-Leftarrow")
    (reg "[1;4H" #k"Shift-Meta-Home")
    (reg "[1;4F" #k"Shift-Meta-End")
    (reg "[2;4~" #k"Shift-Meta-Insert")
    (reg "[3;4~" #k"Shift-Meta-Delete")
    (reg "[5;4~" #k"Shift-Meta-PageUp")
    (reg "[6;4~" #k"Shift-Meta-PageDown")

    (reg "[1;7A" #k"Meta-Control-Uparrow")
    (reg "[1;7B" #k"Meta-Control-Downarrow")
    (reg "[1;7C" #k"Meta-Control-Rightarrow")
    (reg "[1;7D" #k"Meta-Control-Leftarrow")
    (reg "[1;7H" #k"Meta-Control-Home")
    (reg "[1;7F" #k"Meta-Control-End")
    (reg "[2;7~" #k"Meta-Control-Insert")
    (reg "[3;7~" #k"Meta-Control-Delete")
    (reg "[5;7~" #k"Meta-Control-PageUp")
    (reg "[6;7~" #k"Meta-Control-PageDown")

    (reg "[1;8A" #k"Shift-Meta-Control-Uparrow")
    (reg "[1;8B" #k"Shift-Meta-Control-Downarrow")
    (reg "[1;8C" #k"Shift-Meta-Control-Rightarrow")
    (reg "[1;8D" #k"Shift-Meta-Control-Leftarrow")
    (reg "[1;8H" #k"Shift-Meta-Control-Home")
    (reg "[1;8F" #k"Shift-Meta-Control-End")
    (reg "[2;8~" #k"Shift-Meta-Control-Insert")
    (reg "[3;8~" #k"Shift-Meta-Control-Delete")
    (reg "[5;8~" #k"Shift-Meta-Control-PageUp")
    (reg "[6;8~" #k"Shift-Meta-Control-PageDown")

    ;; Misc.
    ;;
    ;; Not #\return, because then C-j turns into return aka C-m.
    ;; Is this translation needed at all?
    (reg #\newline #k"Linefeed")
    ;;
    (reg #\tab #k"Tab")
    (reg #\escape #k"Escape")
    ;; Kludge: This shouldn't be needed, but otherwise C-c M-i doesn't work.
    (reg '(#\Esc #\i) #k"meta-i")))

(defun translate-tty-event (data)
  (let ((string (coerce data 'string)))
    (or (gethash string *tty-translations*)
        (when (= 1 (length string))
          (hemlock-ext:char-key-event (char string 0))))))

;;; Bracketed paste markers (xterm protocol).
(defvar +paste-begin+ (coerce #(#\Esc #\[ #\2 #\0 #\0 #\~) 'string))
(defvar +paste-end+   (coerce #(#\Esc #\[ #\2 #\0 #\1 #\~) 'string))
(defconstant +paste-begin-length+ 6)
(defconstant +paste-end-length+   6)

;;; Focus event markers (xterm protocol, enabled with ESC[?1004h).
(defvar +focus-in+  (coerce #(#\Esc #\[ #\I) 'string))
(defvar +focus-out+ (coerce #(#\Esc #\[ #\O) 'string))

(defvar *tty-focused* t
  "True when the terminal window has focus (updated by focus in/out events).")

(defvar *last-mouse-x* 0 "Terminal column of the last mouse click (0-based).")
(defvar *last-mouse-y* 0 "Terminal row of the last mouse click (0-based).")

;;; SGR mouse sequence prefix: ESC[<
(defconstant +sgr-mouse-prefix-length+ 3)

(defun parse-sgr-mouse (data start length)
  "Try to parse an SGR mouse sequence starting at START.
  SGR format: ESC [ < Pb ; Px ; Py M|m
  Returns NIL if incomplete/invalid, or the index past the sequence."
  ;; Need at least ESC [ < digit ... M|m
  (when (< (- length start) 4) (return-from parse-sgr-mouse nil))
  (let ((i (+ start 3))) ; past ESC [ <
    (flet ((read-num ()
             (let ((n 0) (found nil))
               (loop while (< i length)
                     for c = (char data i)
                     while (digit-char-p c)
                     do (setf n (+ (* n 10) (digit-char-p c))
                              found t)
                        (incf i))
               (when found n))))
      (let* ((b  (read-num))
             (_1 (when (and b (< i length) (char= (char data i) #\;)) (incf i)))
             (px (read-num))
             (_2 (when (and px (< i length) (char= (char data i) #\;)) (incf i)))
             (py (read-num))
             (final (when (and py (< i length)) (char data i))))
        (declare (ignore _1 _2))
        (when (and b px py (member final '(#\M #\m)))
          (incf i)
          ;; Dispatch mouse event as a Hemlock key event.
          ;; Column px and row py are 1-based in the SGR protocol.
          (let ((pressed (char= final #\M))
                (button  (logand b 3))
                (scroll  (or (= (logand b 63) 64) (= (logand b 63) 65))))
            (cond
              (scroll
               (let ((sym (if (= (logand b 63) 64) #k"WheelUp" #k"WheelDown")))
                 (when sym (q-event *real-editor-input* sym))))
              ((and pressed (zerop button))
               ;; Left button press — store coords and fire Mouse1.
               (setf *last-mouse-x* (1- px)
                     *last-mouse-y* (1- py))
               (q-event *real-editor-input* #k"Mouse1")))
            i))))))

(defun tty-key-event (data)
  (loop with start = 0
        with length = (length data)
        while (< start length)
        do
          (let ((remaining (- length start)))
            (cond
              ;; Bracketed paste: ESC[200~ ... content ... ESC[201~
              ((and (>= remaining +paste-begin-length+)
                    (string= data +paste-begin+
                             :start1 start :end1 (+ start +paste-begin-length+)))
               (let* ((content-start (+ start +paste-begin-length+))
                      (end-pos (search +paste-end+ data :start2 content-start)))
                 (if end-pos
                     (progn
                       (loop for i from content-start below end-pos
                             for event = (hemlock-ext:char-key-event (char data i))
                             when event do (q-event *real-editor-input* event))
                       (setf start (+ end-pos +paste-end-length+)))
                     (incf start +paste-begin-length+))))
              ;; Focus in: ESC[I
              ((and (>= remaining 3)
                    (string= data +focus-in+ :start1 start :end1 (+ start 3)))
               (setf *tty-focused* t)
               (incf start 3))
              ;; Focus out: ESC[O
              ((and (>= remaining 3)
                    (string= data +focus-out+ :start1 start :end1 (+ start 3)))
               (setf *tty-focused* nil)
               (incf start 3))
              ;; SGR mouse: ESC[<...
              ((and (>= remaining 4)
                    (char= (char data start) #\Escape)
                    (< (1+ start) length) (char= (char data (1+ start)) #\[)
                    (< (+ 2 start) length) (char= (char data (+ 2 start)) #\<))
               (let ((next (parse-sgr-mouse data start length)))
                 (if next
                     (setf start next)
                     ;; Incomplete — fall through to normal longest-match.
                     (loop for end from length downto (1+ start)
                           do (let ((event (translate-tty-event (subseq data start end))))
                                (when event
                                  (q-event *real-editor-input* event)
                                  (setf start end)
                                  (return)))))))
              (t
               (loop for end from length downto (1+ start)
                     do (let ((event (translate-tty-event (subseq data start end))))
                          (when event
                            (q-event *real-editor-input* event)
                            (setf start end)
                            (return)))))))))

;;; TTY Mouse support commands.

;;; Move point to the absolute terminal (col, row) click position.
;;; Switches the current window if the click lands in a different window.
(defun tty-move-point-to-click (abs-col abs-row)
  (dolist (window *window-list*)
    (let* ((hunk (window-hunk window)))
      (when (typep hunk 'tty-hunk)
        (let* ((text-pos (tty-hunk-text-position hunk))
               (text-height (tty-hunk-text-height hunk))
               ;; Absolute screen row of the first text line of this window.
               (top-row (1+ (- text-pos text-height))))
          (when (and (>= abs-row top-row)
                     (< abs-row (+ top-row text-height)))
            (let ((rel-row (- abs-row top-row)))
              ;; Find the dis-line for this relative row.
              (loop for dl on (cdr (window-first-line window))
                    do
                (let ((dis (car dl)))
                  (when (= (dis-line-position dis) rel-row)
                    (let* ((chars (dis-line-chars dis))
                           (hemline (dis-line-line dis))
                           (end (dis-line-end dis))
                           ;; Walk displayed chars to find charpos at abs-col.
                           (col 0)
                           (charpos 0))
                      (loop for i from 0 below end
                            do (let ((w (char-display-width (char chars i))))
                                 (when (> (+ col w) abs-col)
                                   (return))
                                 (incf col w)
                                 (setf charpos (1+ i))))
                      ;; Switch window if needed.
                      (unless (eq window (current-window))
                        (setf (current-window) window))
                      (move-to-position (current-point) charpos hemline)
                      (return-from tty-move-point-to-click t))))))))))))

(defcommand "TTY Mouse Click" (p)
  "Move point to the terminal position of the last mouse click."
  "Move point to the terminal position of the last mouse click."
  (declare (ignore p))
  (tty-move-point-to-click *last-mouse-x* *last-mouse-y*))

(defcommand "TTY Mouse Wheel Up" (p)
  "Scroll the current window up by mouse wheel lines."
  "Scroll the current window up by mouse wheel lines."
  (declare (ignore p))
  (scroll-window-up-command 3))

(defcommand "TTY Mouse Wheel Down" (p)
  "Scroll the current window down by mouse wheel lines."
  "Scroll the current window down by mouse wheel lines."
  (declare (ignore p))
  (scroll-window-down-command 3))

(defun register-tty-translation (string keysym &key kludge)
  (when kludge
    ;; FIXME: This is pretty terrible, but for some reason my *terminfo* has
    ;; Esc,O,<foo> for arrow keys, whereas terminal actually sends Esc,[,<foo>
    ;; -- either I don't understand how terminfo stuff is supposed to work,
    ;; Apple ships with a broken terminfo db, or something is wrong with
    ;; the terminfo code. I'm inclined to blame me...
    (assert (eq #\O (char string 1)))
    (setf string (format nil "~A[~A" (char string 0) (subseq string 2))))
  (setf (gethash (string string) *tty-translations*) keysym))

;;; Mouse key bindings — must be after the commands are defined above.
(bind-key "TTY Mouse Click"      #k"Mouse1")
(bind-key "TTY Mouse Wheel Up"   #k"WheelUp")
(bind-key "TTY Mouse Wheel Down" #k"WheelDown")

