;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :hemlock)

(defmode "Terminal" :major-p t)

(defvar *terminal-buffers* (make-hash-table :test 'eq))

(defstruct terminal-state
  (term nil)
  (process nil)
  (connection nil)
  (pty-fd nil :type (or null fixnum))
  (dirty nil :type boolean)
  (font-info-cache nil))

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

(defun terminal-font-changes-to-font-info (font-changes line-length)
  (let ((result nil))
    (loop for (fc . rest) on font-changes
          for x = (first fc)
          for plist = (second fc)
          for next-x = (if rest (first (first rest)) line-length)
          when (and plist (> next-x x))
          do (push (list* plist x next-x) result))
    (nreverse result)))

(defun terminal-sync-buffer (buffer)
  (let ((state (terminal-state-for-buffer buffer)))
    (when (and state (terminal-state-dirty state))
      (let* ((term (terminal-state-term state))
             (h (hemlock.term:term-height term))
             (cache (or (terminal-state-font-info-cache state)
                        (setf (terminal-state-font-info-cache state)
                              (make-array h :initial-element nil)))))
        (with-writable-buffer (buffer)
          (let ((line (mark-line (buffer-start-mark buffer)))
                (cy (hemlock.term:term-cursor-y term))
                (cx (hemlock.term:term-cursor-x term))
                (cursor-line nil))
            (dotimes (y h)
              (when line
                (multiple-value-bind (row-str font-changes)
                    (hemlock.term:term-render-line term y)
                  (unless (string= (line-string line) row-str)
                    (setf (line-string line) row-str))
                  (setf (aref cache y)
                        (terminal-font-changes-to-font-info
                         font-changes (length row-str))))
                (when (= y cy)
                  (setf cursor-line line))
                (setf line (line-next line))))
            (when cursor-line
              (move-mark (buffer-point buffer)
                         (mark cursor-line
                               (min cx (line-length cursor-line)))))))
        (setf (terminal-state-dirty state) nil)))))

(defun terminal-font-info-for-dis-line (dis-line)
  (let* ((line (dis-line-line dis-line))
         (buffer (and line (line-buffer line)))
         (state (and buffer (terminal-state-for-buffer buffer))))
    (when state
      (let ((cache (terminal-state-font-info-cache state))
            (pos (dis-line-position dis-line)))
        (when (and cache (< pos (length cache)))
          (aref cache pos))))))

(defun terminal-inject-font-changes-for-window (window)
  (let* ((buffer (window-buffer window))
         (state (terminal-state-for-buffer buffer)))
    (when state
      (let ((cache (terminal-state-font-info-cache state)))
        (when cache
          (do ((dl (cdr (window-first-line window)) (cdr dl))
               (y 0 (1+ y)))
              ((or (eq dl the-sentinel) (>= y (length cache))))
            (let ((font-info (aref cache y))
                  (dis-line (car dl)))
              (let ((changes (dis-line-font-changes dis-line)))
                (when changes
                  (do ((prev changes current)
                       (current (font-change-next changes) (font-change-next current)))
                      ((null current)
                       (setf (dis-line-font-changes dis-line) nil)
                       (shiftf (font-change-next prev)
                               hemlock.command::*free-font-changes* changes))
                    (setf (font-change-mark current) nil))))
              (let ((head nil) (tail nil))
                (dolist (fi font-info)
                  (let ((node (hemlock.command::alloc-font-change
                               (cadr fi) (car fi) nil)))
                    (if tail
                        (setf (font-change-next tail) node
                              tail node)
                        (setf head node tail node))))
                (setf (dis-line-font-changes dis-line) head)))))))))

(defun terminal-resize-to-window (buffer window)
  (let ((state (terminal-state-for-buffer buffer)))
    (when state
      (let* ((new-h (window-height window))
             (new-w (window-width window))
             (term (terminal-state-term state))
             (old-h (hemlock.term:term-height term))
             (old-w (hemlock.term:term-width term)))
        (when (or (/= new-h old-h) (/= new-w old-w))
          (hemlock.term:term-resize term new-w new-h)
          (let ((fd (terminal-state-pty-fd state)))
            (when fd
              (hemlock.term:set-pty-size fd new-h new-w)))
          (with-writable-buffer (buffer)
            (delete-region (buffer-region buffer))
            (let ((point (buffer-point buffer)))
              (dotimes (y new-h)
                (insert-string point (make-string new-w :initial-element #\Space))
                (when (< y (1- new-h))
                  (insert-character point #\Newline)))))
          (setf (terminal-state-font-info-cache state)
                (make-array new-h :initial-element nil))
          (setf (terminal-state-dirty state) t))))))

(defun terminal-redisplay-hook (window)
  (let* ((buffer (window-buffer window))
         (state (terminal-state-for-buffer buffer)))
    (when state
      (let ((term (terminal-state-term state)))
        (when (or (/= (window-height window) (hemlock.term:term-height term))
                  (/= (window-width window) (hemlock.term:term-width term)))
          (terminal-resize-to-window buffer window)))
      (when (terminal-state-dirty state)
        (terminal-sync-buffer buffer)))))

(add-hook hemlock::redisplay-hook #'terminal-redisplay-hook)

(defun make-terminal-buffer (&key (command "/bin/bash --norc --noprofile")
                                  (rows (if *current-window* (window-height *current-window*) 24))
                                  (cols (if *current-window* (window-width *current-window*) 80)))
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
        (setf (terminal-state-font-info-cache state)
              (make-array rows :initial-element nil))
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

(defcommand "Terminal Send Control" (p)
  "Send the control-character version of the last typed key to the terminal."
  "Send the control-character version of the last typed key to the terminal."
  (declare (ignore p))
  (let* ((keysym (key-event-keysym *last-key-event-typed*))
         (name (keysym-preferred-name keysym)))
    (when (and name (= (length name) 1))
      (let* ((ch (char-upcase (char name 0)))
             (code (- (char-code ch) 64)))
        (when (<= 1 code 31)
          (terminal-send (string (code-char code))))))))

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
(let ((ctrl-bits (make-key-event-bits "Control")))
  (do-alpha-key-events (key-event :both)
    (bind-key "Terminal Send Control"
              (make-key-event key-event ctrl-bits)
              :mode "Terminal")))
(bind-key "Terminal Send Arrow Up" #k"uparrow" :mode "Terminal")
(bind-key "Terminal Send Arrow Down" #k"downarrow" :mode "Terminal")
(bind-key "Terminal Send Arrow Right" #k"rightarrow" :mode "Terminal")
(bind-key "Terminal Send Arrow Left" #k"leftarrow" :mode "Terminal")
(bind-key "Terminal Quit" #k"hyper-q" :mode "Terminal")
