;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :hemlock)

(defmode "Terminal" :major-p t)

(defvar *terminal-buffers* (make-hash-table :test 'eq))

(defstruct terminal-state
  (term nil)
  (process nil)
  (connection nil)
  (pty-fd nil :type (or null fixnum))
  (dirty nil :type boolean))

(defun terminal-state-for-buffer (buffer)
  (gethash buffer *terminal-buffers*))

(defun terminal-state-for-current-buffer ()
  (terminal-state-for-buffer (current-buffer)))

(defun terminal-write-to-pty (state str)
  (let ((fd (terminal-state-pty-fd state)))
    (when fd
      (let ((bytes (sb-ext:string-to-octets str :external-format :utf-8)))
        (sb-unix:unix-write fd bytes 0 (length bytes))))))

(defun terminal-send (str)
  (let ((state (terminal-state-for-current-buffer)))
    (when (and state (terminal-state-pty-fd state))
      (terminal-write-to-pty state str)
      (dispatch-events-no-hang)
      (terminal-sync-buffer (current-buffer))
      (redisplay))))

(defun terminal-output-filter (buffer bytes)
  (let ((state (terminal-state-for-buffer buffer)))
    (when state
      (let ((str (sb-ext:octets-to-string bytes :external-format :utf-8)))
        (hemlock.term:term-process-output (terminal-state-term state) str)
        (setf (terminal-state-dirty state) t))
      nil)))

(defun terminal-sync-buffer (buffer)
  (let ((state (terminal-state-for-buffer buffer)))
    (when (and state (terminal-state-dirty state))
      (let* ((term (terminal-state-term state))
             (h (hemlock.term:term-height term)))
        (with-writable-buffer (buffer)
          (let ((line (mark-line (buffer-start-mark buffer))))
            (dotimes (y h)
              (when line
                (let ((row-str (hemlock.term:term-dump-row-string term y)))
                  (unless (string= (line-string line) row-str)
                    (setf (line-string line) row-str)))
                (setf line (line-next line))))))
        (setf (terminal-state-dirty state) nil)))))

(defun terminal-redisplay-hook (window)
  (let* ((buffer (window-buffer window))
         (state (terminal-state-for-buffer buffer)))
    (when (and state (terminal-state-dirty state))
      (terminal-sync-buffer buffer))))

(add-hook hemlock::redisplay-hook #'terminal-redisplay-hook)

(defun make-terminal-buffer (&key (command "/bin/bash --norc --noprofile") (rows 24) (cols 80))
  (let* ((term (hemlock.term:make-term
                :width cols :height rows
                :input-fn (lambda (term str)
                            (declare (ignore term))
                            (let ((state (terminal-state-for-current-buffer)))
                              (when (and state (terminal-state-pty-fd state))
                                (terminal-write-to-pty state str))))
                :bell-fn (lambda (term)
                           (declare (ignore term)))))
         (name (format nil "Terminal ~D" (hash-table-count *terminal-buffers*))))
    (multiple-value-bind (master-fd proc-conn)
        (hemlock.term:spawn-pty-process command :rows rows :cols cols)
      (let* ((buffer (make-buffer name :modes '("Terminal")))
             (conn (make-pipelike-connection
                    master-fd master-fd
                    :name name
                    :process-connection proc-conn
                    :filter (lambda (connection bytes)
                              (declare (ignore connection))
                              (terminal-output-filter buffer bytes))
                    :buffer nil))
             (state (make-terminal-state :term term
                                         :process nil
                                         :connection conn
                                         :pty-fd master-fd)))
        (setf (gethash buffer *terminal-buffers*) state)
        (with-writable-buffer (buffer)
          (let ((point (buffer-point buffer)))
            (dotimes (y rows)
              (insert-string point (make-string cols :initial-element #\Space))
              (when (< y (1- rows))
                (insert-character point #\Newline)))))
        (setf (buffer-writable buffer) nil)
        buffer))))

(defun terminal-kill-process (buffer)
  (let ((state (terminal-state-for-buffer buffer)))
    (when state
      (when (terminal-state-connection state)
        (delete-connection (terminal-state-connection state)))
      (remhash buffer *terminal-buffers*))))

(defcommand "Terminal Send Self" (p)
  "Send the last typed key to the terminal process."
  "Send the last typed key to the terminal process."
  (declare (ignore p))
  (let ((char (hemlock-ext:key-event-char *last-key-event-typed*)))
    (when char
      (terminal-send (string char)))))

(defcommand "Terminal Send Return" (p)
  "Send return to the terminal process."
  "Send return to the terminal process."
  (declare (ignore p))
  (terminal-send (string #\Return)))

(defcommand "Terminal Send Tab" (p)
  "Send tab to the terminal process."
  "Send tab to the terminal process."
  (declare (ignore p))
  (terminal-send (string #\Tab)))

(defcommand "Terminal Send Backspace" (p)
  "Send backspace to the terminal process."
  "Send backspace to the terminal process."
  (declare (ignore p))
  (terminal-send (string #\Rubout)))

(defcommand "Terminal Send Escape" (p)
  "Send escape to the terminal process."
  "Send escape to the terminal process."
  (declare (ignore p))
  (terminal-send (string #\Escape)))

(defcommand "Terminal Send Interrupt" (p)
  "Send C-c (interrupt) to the terminal process."
  "Send C-c (interrupt) to the terminal process."
  (declare (ignore p))
  (terminal-send (string (code-char 3))))

(defcommand "Terminal Send EOF" (p)
  "Send C-d (EOF) to the terminal process."
  "Send C-d (EOF) to the terminal process."
  (declare (ignore p))
  (terminal-send (string (code-char 4))))

(defcommand "Terminal Send Arrow Up" (p)
  "Send up arrow to the terminal process."
  "Send up arrow to the terminal process."
  (declare (ignore p))
  (terminal-send (format nil "~C[A" #\Escape)))

(defcommand "Terminal Send Arrow Down" (p)
  "Send down arrow to the terminal process."
  "Send down arrow to the terminal process."
  (declare (ignore p))
  (terminal-send (format nil "~C[B" #\Escape)))

(defcommand "Terminal Send Arrow Right" (p)
  "Send right arrow to the terminal process."
  "Send right arrow to the terminal process."
  (declare (ignore p))
  (terminal-send (format nil "~C[C" #\Escape)))

(defcommand "Terminal Send Arrow Left" (p)
  "Send left arrow to the terminal process."
  "Send left arrow to the terminal process."
  (declare (ignore p))
  (terminal-send (format nil "~C[D" #\Escape)))

(defcommand "Terminal Quit" (p)
  "Kill the terminal process and buffer."
  "Kill the terminal process and buffer."
  (declare (ignore p))
  (let ((buffer (current-buffer)))
    (terminal-kill-process buffer)
    (delete-buffer-if-possible buffer)))

(defcommand "Terminal" (p)
  "Open a terminal emulator buffer."
  "Open a terminal emulator buffer."
  (declare (ignore p))
  (let ((buffer (make-terminal-buffer)))
    (change-to-buffer buffer)))

(defcommand "Terminal With Command" (p)
  "Open a terminal emulator buffer running a specific command."
  "Open a terminal emulator buffer running a specific command."
  (declare (ignore p))
  (let* ((command (prompt-for-string
                   :default "/bin/bash" :trim t
                   :prompt "Command: "
                   :help "Command to run in terminal."))
         (buffer (make-terminal-buffer :command command)))
    (change-to-buffer buffer)))

;;; Key bindings — MUST come after defcommand forms
(do-alpha-key-events (key-event :both)
  (bind-key "Terminal Send Self" key-event :mode "Terminal"))
(dolist (ch (list #\Space #\! #\@ #\# #\$ #\% #\^ #\& #\* #\( #\) #\- #\_
                  #\= #\+ #\[ #\] #\{ #\} #\\ #\| #\; #\: #\' #\"
                  #\, #\. #\< #\> #\/ #\? #\` #\~
                  #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
  (let ((ke (hemlock-ext:char-key-event ch)))
    (when ke (bind-key "Terminal Send Self" ke :mode "Terminal"))))

(bind-key "Terminal Send Return" #k"return" :mode "Terminal")
(bind-key "Terminal Send Tab" #k"tab" :mode "Terminal")
(bind-key "Terminal Send Backspace" #k"backspace" :mode "Terminal")
(bind-key "Terminal Send Backspace" #k"delete" :mode "Terminal")
(bind-key "Terminal Send Interrupt" #k"hyper-c" :mode "Terminal")
(bind-key "Terminal Send EOF" #k"hyper-d" :mode "Terminal")
(bind-key "Terminal Send Arrow Up" #k"uparrow" :mode "Terminal")
(bind-key "Terminal Send Arrow Down" #k"downarrow" :mode "Terminal")
(bind-key "Terminal Send Arrow Right" #k"rightarrow" :mode "Terminal")
(bind-key "Terminal Send Arrow Left" #k"leftarrow" :mode "Terminal")
(bind-key "Terminal Quit" #k"hyper-q" :mode "Terminal")
