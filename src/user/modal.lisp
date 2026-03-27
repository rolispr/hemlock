;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;;
;;; Helix-style modal editing for Hemlock.
;;;
;;; Two minor modes:
;;;   "Normal"  (precedence 5.0) --navigation + operators; motions collapse selection
;;;   "Select"  (precedence 10.0) --shadows Normal motions with raw equivalents;
;;;                                  moving point extends the active region naturally
;;;
;;; Insert mode = neither Normal nor Select is active (hemlock default state).
;;; Escape is handled by "Helix Escape" bound globally.
;;;

(in-package :hemlock)


;;;; Mode Definitions.

(defmode "Normal"
  :major-p nil
  :precedence 5.0
  :documentation
  "Helix-style normal mode. Navigate and manipulate text.
   Press i/a/o to enter insert mode, v to select, Escape to return here.")

(defmode "Select"
  :major-p nil
  :precedence 10.0
  :documentation
  "Helix-style select mode. Motions extend the active selection.
   Press v or Escape to return to normal mode.")


;;;; Hemlock Variables.

(defhvar "Default Editing Style"
  "Global default editing style.
   :modal activates Normal mode on every new buffer.
   :emacs leaves hemlock in its default state."
  :value :modal)

(defhvar "Modal Buffer"
  "Per-buffer modal editing state.
   :on    --this buffer is in modal editing mode.
   :off   --modal editing suppressed for this buffer (user opted out).
   :unset --follows the Default Editing Style global setting."
  :value :unset)


;;;; Variable Helpers.
;;;
;;; variable-value :buffer only works when a buffer-local binding exists (created
;;; via defhvar :buffer).  The global default does NOT fall through automatically.
;;; Use these helpers everywhere to safely read/write the per-buffer modal state.

(defun modal-buffer-state (buffer)
  "Return the modal-buffer state for BUFFER.
   Returns the buffer-local value if set, else the global default."
  (if (hemlock-bound-p 'modal-buffer :buffer buffer)
      (variable-value 'modal-buffer :buffer buffer)
      (variable-value 'modal-buffer :global)))

(defun (setf modal-buffer-state) (value buffer)
  "Set the modal-buffer state for BUFFER, creating a buffer-local binding if needed."
  (unless (hemlock-bound-p 'modal-buffer :buffer buffer)
    (defhvar "Modal Buffer"
      "Per-buffer modal editing state."
      :buffer buffer
      :value value)
    (return-from modal-buffer-state value))
  (setf (variable-value 'modal-buffer :buffer buffer) value))


;;;; Modeline Field.

(make-modeline-field
 :name :modal-indicator
 :width 8
 :function (lambda (buffer window)
             "Returns INSERT/NORMAL/SELECT indicator for modal editing buffers."
             (declare (ignore window))
             (cond ((buffer-minor-mode buffer "Select") "SELECT  ")
                   ((buffer-minor-mode buffer "Normal") "NORMAL  ")
                   ((eq (modal-buffer-state buffer) :on) "INSERT  ")
                   (t ""))))

;;; Insert at front of the default modeline fields list AND update the hemlock
;;; variable that screen.lisp reads when the TTY device is initialized.
;;; (defhvar "Default Modeline Fields" in main.lisp captures *default-modeline-fields*
;;;  by value, so the hemlock variable must be explicitly updated after pushnew.)
(pushnew (modeline-field :modal-indicator) *default-modeline-fields*)
(setf (variable-value 'hemlock::default-modeline-fields :global)
      *default-modeline-fields*)


;;;; Internal Helper.

(defmacro %leave-modal-modes (buf)
  "Remove Normal and Select minor modes from BUF and deactivate region."
  `(progn
     (setf (buffer-minor-mode ,buf "Select") nil)
     (setf (buffer-minor-mode ,buf "Normal") nil)
     (deactivate-region ,buf)))

(defun %set-normal-cursor ()
  "Set/update the Helix 1-char cursor selection at current point.
   Reuses the top mark via move-mark rather than pushing (no ring growth).
   Falls back to push-buffer-mark if the ring is empty (first use on buffer)."
  (let* ((ring  (value buffer-mark-ring))
         (point (current-point)))
    (if (plusp (ring-length ring))
        (let ((mark (current-mark)))
          (move-mark mark point)
          (when (mark-after mark)
            (activate-region)))
        (let ((after (copy-mark point :right-inserting)))
          (when (mark-after after)
            (push-buffer-mark after t))))))


;;;; Mode-Switch Commands.

(defcommand "Normal Mode" (p)
  "Enter Normal mode: disable Select mode, activate Normal mode, deactivate region."
  "Enter Normal mode."
  (declare (ignore p))
  (let ((buf (current-buffer)))
    (setf (buffer-minor-mode buf "Select") nil)
    (setf (buffer-minor-mode buf "Normal") t)
    (setf (modal-buffer-state buf) :on)
    (deactivate-region buf)
    (%set-normal-cursor)))

(defcommand "Enter Select Mode" (p)
  "Enter Select mode: set mark at current point and activate the region."
  "Enter Select mode."
  (declare (ignore p))
  (let ((buf (current-buffer)))
    (push-buffer-mark (copy-mark (current-point)) t)
    (setf (buffer-minor-mode buf "Select") t)))

(defcommand "Exit Select Mode" (p)
  "Exit Select mode: deactivate region, return to Normal mode."
  "Exit Select mode."
  (declare (ignore p))
  (let ((buf (current-buffer)))
    (setf (buffer-minor-mode buf "Select") nil)
    (deactivate-region buf)
    (%set-normal-cursor)))

(defcommand "Helix Escape" (p)
  "Context-sensitive Escape handler.
   In Select mode: collapse selection to cursor, stay in Normal.
   In Normal mode: deactivate any active region, restore 1-char cursor selection.
   In Insert (modal buffer): return to Normal mode.
   In non-modal buffer: keyboard quit (abort)."
  "Helix escape."
  (declare (ignore p))
  (let ((buf (current-buffer)))
    (cond
      ((buffer-minor-mode buf "Select")
       (setf (buffer-minor-mode buf "Select") nil)
       (deactivate-region buf)
       (%set-normal-cursor))
      ((buffer-minor-mode buf "Normal")
       (deactivate-region buf)
       (%set-normal-cursor))
      ((eq (modal-buffer-state buf) :on)
       (normal-mode-command nil))
      (t
       (beep)
       (throw 'editor-top-level-catcher nil)))))

(defcommand "Toggle Modal Editing" (p)
  "Toggle Normal mode on/off in the current buffer.
   Also sets the buffer-local Modal Buffer variable so the creation hook
   does not re-enable it automatically."
  "Toggle modal editing for current buffer."
  (declare (ignore p))
  (let ((buf (current-buffer)))
    (if (buffer-minor-mode buf "Normal")
        (progn
          (setf (buffer-minor-mode buf "Select") nil)
          (setf (buffer-minor-mode buf "Normal") nil)
          (setf (modal-buffer-state buf) :off)
          (message "Modal editing off."))
        (progn
          (normal-mode-command nil)
          (message "Modal editing on.")))))


;;;; Insert Mode Transitions.

(defcommand "Helix Insert Before" (p)
  "Enter insert mode before the cursor (i)."
  "Helix i."
  (declare (ignore p))
  (%leave-modal-modes (current-buffer)))

(defcommand "Helix Append After" (p)
  "Enter insert mode after the cursor (a)."
  "Helix a."
  (declare (ignore p))
  (let ((buf (current-buffer)))
    (%leave-modal-modes buf)
    (mark-after (current-point))))

(defcommand "Helix Insert Line Start" (p)
  "Enter insert mode at the first non-whitespace character of the line (I)."
  "Helix I."
  (declare (ignore p))
  (let ((buf (current-buffer))
        (point (current-point)))
    (%leave-modal-modes buf)
    (line-start point)
    (find-attribute point :whitespace #'zerop)))

(defcommand "Helix Append Line End" (p)
  "Enter insert mode at the end of the line (A)."
  "Helix A."
  (declare (ignore p))
  (let ((buf (current-buffer)))
    (%leave-modal-modes buf)
    (line-end (current-point))))

(defcommand "Helix Open Below" (p)
  "Open a new line below the current line and enter insert mode (o)."
  "Helix o."
  (declare (ignore p))
  (let ((buf (current-buffer))
        (point (current-point)))
    (%leave-modal-modes buf)
    (line-end point)
    (insert-character point #\newline)))

(defcommand "Helix Open Above" (p)
  "Open a new line above the current line and enter insert mode (O)."
  "Helix O."
  (declare (ignore p))
  (let ((buf (current-buffer))
        (point (current-point)))
    (%leave-modal-modes buf)
    (line-start point)
    (insert-character point #\newline)
    (line-offset point -1 0)))


;;;; Normal Mode Motions.
;;;
;;; Each motion deactivates the region before moving, collapsing any selection.
;;; In Select mode these keys are shadowed by raw hemlock commands that naturally
;;; extend the region (since moving point with a live mark always extends it).

(defmacro define-helix-motion (name doc &body body)
  "Define a Normal-mode motion. Anchors selection at current position before
   moving, spanning from here to the destination --the Kakoune/Helix model.
   Reuses the top mark via move-mark (no ring growth per keypress).
   Falls back to push-buffer-mark only on first use per buffer.
   BODY may reference P (the prefix argument)."
  `(defcommand ,name (p)
     ,doc ,doc
     (let ((ring (value buffer-mark-ring)))
       (if (plusp (ring-length ring))
           (progn
             (move-mark (current-mark) (current-point))
             (activate-region))
           (push-buffer-mark (copy-mark (current-point) :right-inserting) t)))
     ,@body))

(define-helix-motion "Helix Forward Character"
  "Move right one character, collapsing selection."
  (character-offset (current-point) (or p 1)))

(define-helix-motion "Helix Backward Character"
  "Move left one character, collapsing selection."
  (character-offset (current-point) (- (or p 1))))

(define-helix-motion "Helix Next Line"
  "Move down one line, collapsing selection."
  (let* ((point (current-point))
         (target (set-target-column point)))
    (unless (line-offset point (or p 1))
      (line-end point))
    (unless (move-to-column point target)
      (line-end point))
    (setf (last-command-type) :line-motion)))

(define-helix-motion "Helix Previous Line"
  "Move up one line, collapsing selection."
  (let* ((point (current-point))
         (target (set-target-column point)))
    (unless (line-offset point (- (or p 1)))
      (line-start point))
    (unless (move-to-column point target)
      (line-end point))
    (setf (last-command-type) :line-motion)))

(define-helix-motion "Helix Forward Word"
  "Move to the start of the next word, collapsing selection."
  (unless (word-offset (current-point) (or p 1))
    (buffer-end (current-point))))

(define-helix-motion "Helix Backward Word"
  "Move to the start of the previous word, collapsing selection."
  (unless (word-offset (current-point) (- (or p 1)))
    (buffer-start (current-point))))

(define-helix-motion "Helix Forward Word End"
  "Move to the end of the current/next word, collapsing selection."
  ;; Skip any leading delimiters to reach the word, then skip to after it,
  ;; then back up one to sit on the last word character.
  (let ((point (current-point)))
    (find-attribute point :word-delimiter #'zerop)
    (unless (find-attribute point :word-delimiter)
      (buffer-end point))
    (mark-before point)))

(define-helix-motion "Helix Beginning of Line"
  "Move to beginning of line, collapsing selection."
  (line-start (current-point)))

(define-helix-motion "Helix End of Line"
  "Move to end of line, collapsing selection."
  (line-end (current-point)))

(define-helix-motion "Helix First Non-Whitespace"
  "Move to the first non-whitespace character on the line, collapsing selection."
  (let ((point (current-point)))
    (line-start point)
    (find-attribute point :whitespace #'zerop)))

(define-helix-motion "Helix Buffer Start"
  "Move to start of buffer, collapsing selection."
  (buffer-start (current-point)))

(define-helix-motion "Helix Buffer End"
  "Move to end of buffer, collapsing selection."
  (buffer-end (current-point)))

(define-helix-motion "Helix Forward Paragraph"
  "Move forward one paragraph, collapsing selection."
  (forward-paragraph-command p))

(define-helix-motion "Helix Backward Paragraph"
  "Move backward one paragraph, collapsing selection."
  (backward-paragraph-command p))


;;;; WORD Motion Helpers (W / B / E).
;;;
;;; Helix W/B/E treat any run of non-whitespace as one WORD, unlike w/b/e which
;;; respect hemlock :word-delimiter character attributes.

(defun %skip-forward-nonws (point)
  (loop for c = (next-character point)
        while (and c (not (member c '(#\space #\tab #\newline))))
        do (mark-after point)))

(defun %skip-forward-ws (point)
  (loop for c = (next-character point)
        while (and c (member c '(#\space #\tab #\newline)))
        do (mark-after point)))

(defun %skip-backward-nonws (point)
  (loop for c = (previous-character point)
        while (and c (not (member c '(#\space #\tab #\newline))))
        do (mark-before point)))

(defun %skip-backward-ws (point)
  (loop for c = (previous-character point)
        while (and c (member c '(#\space #\tab #\newline)))
        do (mark-before point)))

(define-helix-motion "Helix Forward Upcase Word"
  "Move to the start of the next WORD (non-whitespace run) (W)."
  (let ((point (current-point)))
    (%skip-forward-nonws point)
    (%skip-forward-ws    point)))

(define-helix-motion "Helix Backward Upcase Word"
  "Move to the start of the previous WORD (B)."
  (let ((point (current-point)))
    (%skip-backward-ws    point)
    (%skip-backward-nonws point)))

(define-helix-motion "Helix Forward Upcase Word End"
  "Move to the end of the current/next WORD (E)."
  (let ((point (current-point)))
    (mark-after point)
    (%skip-forward-ws    point)
    (%skip-forward-nonws point)
    (when (previous-character point)
      (mark-before point))))


;;;; Scroll Motions (collapse selection before scrolling).

(define-helix-motion "Helix Scroll Down"
  "Scroll window down, collapsing selection."
  (scroll-window-down-command p))

(define-helix-motion "Helix Scroll Up"
  "Scroll window up, collapsing selection."
  (scroll-window-up-command p))


;;;; Goto Mode Commands (g submap).

(defcommand "Helix Goto Window Top" (p)
  "Move cursor to the top line of the window (g t)."
  "Helix g t."
  (declare (ignore p))
  (let* ((win   (current-window))
         (start (window-display-start win)))
    (move-mark (current-point) start)
    (line-start (current-point))))

(defcommand "Helix Goto Window Center" (p)
  "Move cursor to the center line of the window (g c)."
  "Helix g c."
  (declare (ignore p))
  (let* ((win   (current-window))
         (start (copy-mark (window-display-start win) :temporary))
         (half  (floor (window-height win) 2)))
    (line-offset start half)
    (move-mark (current-point) start)
    (line-start (current-point))
    (delete-mark start)))

(defcommand "Helix Goto Window Bottom" (p)
  "Move cursor to the bottom line of the window (g b)."
  "Helix g b."
  (declare (ignore p))
  (let* ((win   (current-window))
         (start (copy-mark (window-display-start win) :temporary))
         (h     (max 0 (1- (window-height win)))))
    (line-offset start h)
    (move-mark (current-point) start)
    (line-start (current-point))
    (delete-mark start)))

(defcommand "Helix Goto Next Buffer" (p)
  "Switch to the next buffer in the buffer list (g n)."
  "Helix g n."
  (declare (ignore p))
  (let ((bufs nil))
    (do-strings (name buf *buffer-names*)
      (declare (ignore name))
      (unless (string= (buffer-name buf) "Echo Area")
        (push buf bufs)))
    (setf bufs (nreverse bufs))
    (when bufs
      (let* ((cur  (current-buffer))
             (idx  (or (position cur bufs) -1))
             (next (nth (mod (1+ idx) (length bufs)) bufs)))
        (when next
          (change-to-buffer next))))))

(defcommand "Helix Goto Previous Buffer" (p)
  "Switch to the previous buffer in the buffer list (g p)."
  "Helix g p."
  (declare (ignore p))
  (let ((bufs nil))
    (do-strings (name buf *buffer-names*)
      (declare (ignore name))
      (unless (string= (buffer-name buf) "Echo Area")
        (push buf bufs)))
    (setf bufs (nreverse bufs))
    (when bufs
      (let* ((cur  (current-buffer))
             (idx  (or (position cur bufs) 0))
             (prev (nth (mod (1- idx) (length bufs)) bufs)))
        (when prev
          (change-to-buffer prev))))))

(defcommand "Helix Goto Column" (p)
  "Move to column P (0-indexed) on the current line (g |)."
  "Helix g |."
  (let ((col   (or p 0))
        (point (current-point)))
    (line-start point)
    (dotimes (i col)
      (when (end-line-p point) (return))
      (mark-after point))))


(defcommand "Helix Goto Line" (p)
  "With prefix argument, go to that line number.  Without, go to end of buffer (G)."
  "Helix G / <n>G."
  ;; Anchor selection at current position (same pattern as define-helix-motion).
  (let ((ring (value buffer-mark-ring)))
    (if (plusp (ring-length ring))
        (progn (move-mark (current-mark) (current-point))
               (activate-region))
        (push-buffer-mark (copy-mark (current-point) :right-inserting) t)))
  (if p
      (let ((point (current-point)))
        (buffer-start point)
        (unless (line-offset point (1- p))
          (buffer-end point)))
      (buffer-end (current-point))))


;;;; Find-Char Commands (f / F / t / T).

(defun helix-find-char-on-line (point char forwardp tillp)
  "Scan from POINT for CHAR on the same line.
   FORWARDP: T = scan forward, NIL = scan backward.
   TILLP: if T, place cursor one position before (forward) or after (backward) the char.
   Moves POINT on success.  Returns T if found, NIL otherwise."
  (with-mark ((scan point :temporary))
    (let ((original-line (mark-line scan))
          (found nil))
      (loop
        (unless (if forwardp (mark-after scan) (mark-before scan))
          (return))
        (unless (eq (mark-line scan) original-line)
          (return))
        (when (eql (next-character scan) char)
          (setf found t)
          (return)))
      (when found
        (move-mark point scan)
        (when tillp
          (if forwardp
              (mark-before point)
              (mark-after point))))
      found)))

(defcommand "Helix Find Char" (p)
  "Move to the next occurrence of a typed character on the current line (f)."
  "Helix f."
  (declare (ignore p))
  (deactivate-region)
  (let* ((ke (get-key-event *editor-input*))
         (char (key-event-char ke)))
    (when char
      (unless (helix-find-char-on-line (current-point) char t nil)
        (editor-error "Character ~C not found on line." char)))))

(defcommand "Helix Till Char" (p)
  "Move to one position before the next occurrence of a typed character (t)."
  "Helix t."
  (declare (ignore p))
  (deactivate-region)
  (let* ((ke (get-key-event *editor-input*))
         (char (key-event-char ke)))
    (when char
      (unless (helix-find-char-on-line (current-point) char t t)
        (editor-error "Character ~C not found on line." char)))))

(defcommand "Helix Find Char Back" (p)
  "Move to the previous occurrence of a typed character on the current line (F)."
  "Helix F."
  (declare (ignore p))
  (deactivate-region)
  (let* ((ke (get-key-event *editor-input*))
         (char (key-event-char ke)))
    (when char
      (unless (helix-find-char-on-line (current-point) char nil nil)
        (editor-error "Character ~C not found on line." char)))))

(defcommand "Helix Till Char Back" (p)
  "Move to one position after the previous occurrence of a typed character (T)."
  "Helix T."
  (declare (ignore p))
  (deactivate-region)
  (let* ((ke (get-key-event *editor-input*))
         (char (key-event-char ke)))
    (when char
      (unless (helix-find-char-on-line (current-point) char nil t)
        (editor-error "Character ~C not found on line." char)))))


;;;; Operators.

(defcommand "Helix Delete" (p)
  "Delete the current selection into the kill ring.
   With no selection, delete the character at point (d)."
  "Helix d."
  (declare (ignore p))
  (cond
    ((region-active-p)
     (kill-region (current-region nil nil) :kill-forward))
    (t
     (with-mark ((end (current-point) :right-inserting))
       (if (mark-after end)
           (kill-region (region (current-point) end) :kill-forward)
           (editor-error "End of buffer.")))))
  (setf (buffer-minor-mode (current-buffer) "Select") nil)
  (deactivate-region))

(defcommand "Helix Change" (p)
  "Delete the current selection and enter insert mode (c)."
  "Helix c."
  (declare (ignore p))
  (let ((buf (current-buffer)))
    (cond
      ((region-active-p)
       (kill-region (current-region nil nil) :kill-forward))
      (t
       (with-mark ((end (current-point) :right-inserting))
         (when (mark-after end)
           (kill-region (region (current-point) end) :kill-forward)))))
    (%leave-modal-modes buf)))

(defcommand "Helix Yank" (p)
  "Copy the current selection to the kill ring without deleting (y)."
  "Helix y."
  (declare (ignore p))
  (when (region-active-p)
    (ring-push (copy-region (current-region nil nil)) *kill-ring*)
    (setf (buffer-minor-mode (current-buffer) "Select") nil)
    (deactivate-region)
    (message "Yanked.")))

(defcommand "Helix Paste After" (p)
  "Paste kill ring contents after the cursor (p)."
  "Helix p."
  (declare (ignore p))
  (when (> (ring-length *kill-ring*) 0)
    (mark-after (current-point))
    (un-kill-command nil)))

(defcommand "Helix Paste Before" (p)
  "Paste kill ring contents before the cursor (P)."
  "Helix P."
  (declare (ignore p))
  (un-kill-command nil))

(defcommand "Helix Select Line" (p)
  "Select current line; on repeated presses extend selection down one line (x).
   Helix extend_line_below --anchor stays, head grows down."
  "Helix x."
  (declare (ignore p))
  (let ((buffer (current-buffer))
        (point  (current-point)))
    (unless (region-active-p)
      ;; First press: anchor mark at start of current line.
      (let ((ring (value buffer-mark-ring)))
        (if (plusp (ring-length ring))
            (progn (move-mark (current-mark) point)
                   (line-start (current-mark))
                   (activate-region))
            (push-buffer-mark
             (let ((m (copy-mark point :left-inserting)))
               (line-start m) m) t)))
      (line-start point))
    ;; Every press: extend head one line down (or to end of buffer).
    (unless (line-offset point 1)
      (line-end point))
    (setf (buffer-minor-mode buffer "Select") t)
    (update-modeline-fields buffer (current-window))))

(defcommand "Helix Select All" (p)
  "Select the entire buffer (%)."
  "Helix %."
  (declare (ignore p))
  (let ((buffer (current-buffer))
        (point  (current-point)))
    (buffer-start point)
    (let ((ring (value buffer-mark-ring)))
      (if (plusp (ring-length ring))
          (progn (move-mark (current-mark) point)
                 (activate-region))
          (push-buffer-mark (copy-mark point) t)))
    (buffer-end point)
    (setf (buffer-minor-mode buffer "Select") t)
    (update-modeline-fields buffer (current-window))))

(defcommand "Helix Extend To Line Bounds" (p)
  "Snap selection so anchor is at line-start and head is at start of next line (X).
   If no region, selects the current full line."
  "Helix X."
  (declare (ignore p))
  (let ((buffer (current-buffer))
        (point  (current-point)))
    (if (region-active-p)
        (let ((mark (current-mark)))
          ;; Normalize: mark to start of its line.
          (line-start mark)
          ;; Extend point to start of next line (or EOL if last line).
          (unless (line-offset point 1)
            (line-end point)))
        (helix-select-line-command nil))
    (setf (buffer-minor-mode buffer "Select") t)
    (update-modeline-fields buffer (current-window))))

(defcommand "Helix Shrink To Line Bounds" (p)
  "Shrink selection: if head is at BOL, back it up to EOL of previous line (Alt-x).
   Removes trailing newline from a line-wise selection."
  "Helix Alt-x."
  (declare (ignore p))
  (when (region-active-p)
    (let* ((point  (current-point))
           (mark   (current-mark))
           (end    (if (mark< mark point) point mark)))
      ;; If end is at start of a line, back it up to end of previous line.
      (when (zerop (mark-charpos end))
        (mark-before end)))))

(defcommand "Helix Collapse Selection" (p)
  "Collapse selection to the cursor position without deactivating Normal mode (;)."
  "Helix ;."
  (declare (ignore p))
  (setf (buffer-minor-mode (current-buffer) "Select") nil)
  (deactivate-region))

(defcommand "Helix Flip Selection" (p)
  "Swap anchor and head of the active selection (Alt-;)."
  "Helix Alt-;."
  (declare (ignore p))
  (when (region-active-p)
    (let ((point (current-point))
          (mark (current-mark)))
      (with-mark ((temp point))
        (move-mark point mark)
        (move-mark mark temp)))))

(defcommand "Helix Join Lines" (p)
  "Join the next line to the current line, normalizing whitespace at the join (J)."
  "Helix J."
  (declare (ignore p))
  (let ((point (current-point)))
    (line-end point)
    (when (next-character point)         ; not at end of buffer
      (delete-characters point 1)        ; delete the newline
      ;; Remove any extra leading spaces from the joined line, leave one space
      (do ()
          ((not (and (next-character point)
                     (char= (next-character point) #\space)))
           nil)
        (delete-characters point 1))
      (insert-character point #\space))))

(defcommand "Helix Join Lines Space" (p)
  "Join next line to current line, inserting one space without trimming (Alt-J)."
  "Helix Alt-J."
  (declare (ignore p))
  (let ((point (current-point)))
    (line-end point)
    (when (next-character point)
      (delete-characters point 1)
      (insert-character point #\space))))

(defcommand "Helix Toggle Case" (p)
  "Toggle case of the selection or the character at point (~)."
  "Helix ~."
  (declare (ignore p))
  (flet ((toggle (c)
           (cond ((upper-case-p c) (char-downcase c))
                 ((lower-case-p c) (char-upcase c))
                 (t c))))
    (if (region-active-p)
        (let* ((reg (current-region nil t))
               (start (region-start reg))
               (end (region-end reg)))
          (with-mark ((mark start :left-inserting))
            (loop while (mark< mark end) do
              (let ((c (next-character mark)))
                (when c
                  (setf (next-character mark) (toggle c))
                  (mark-after mark))))))
        (let* ((point (current-point))
               (c (next-character point)))
          (when c
            (setf (next-character point) (toggle c)))))))


;;;; Additional Helix Operators.

(defcommand "Helix Replace Char" (p)
  "Replace the character at point with the next typed key (r)."
  "Helix r."
  (declare (ignore p))
  (let* ((ke (get-key-event *editor-input*))
         (ch (key-event-char ke)))
    (when ch
      (setf (next-character (current-point)) ch)
      (%set-normal-cursor))))

(defcommand "Helix Replace With Yanked" (p)
  "Replace selection or char at point with top of kill ring (R)."
  "Helix R."
  (declare (ignore p))
  (if (region-active-p)
      (progn
        (delete-region (current-region nil nil))
        (un-kill-command nil))
      (progn
        (with-mark ((end (current-point) :right-inserting))
          (when (mark-after end)
            (delete-region (region (current-point) end))))
        (un-kill-command nil)))
  (deactivate-region))

(defcommand "Helix Lowercase" (p)
  "Convert selection to lowercase (`)."
  "Helix `."
  (declare (ignore p))
  (when (region-active-p)
    (let* ((reg   (current-region nil t))
           (start (region-start reg))
           (end   (region-end   reg)))
      (with-mark ((m start :left-inserting))
        (loop while (mark< m end)
              do (let ((c (next-character m)))
                   (when c
                     (setf (next-character m) (char-downcase c))
                     (mark-after m)))))))
  (deactivate-region))

(defcommand "Helix Uppercase" (p)
  "Convert selection to uppercase (Alt-`)."
  "Helix Alt-`."
  (declare (ignore p))
  (when (region-active-p)
    (let* ((reg   (current-region nil t))
           (start (region-start reg))
           (end   (region-end   reg)))
      (with-mark ((m start :left-inserting))
        (loop while (mark< m end)
              do (let ((c (next-character m)))
                   (when c
                     (setf (next-character m) (char-upcase c))
                     (mark-after m)))))))
  (deactivate-region))

(defcommand "Helix Delete No Yank" (p)
  "Delete selection without adding to kill ring (Alt-d)."
  "Helix Alt-d."
  (declare (ignore p))
  (if (region-active-p)
      (delete-region (current-region nil nil))
      (with-mark ((end (current-point) :right-inserting))
        (when (mark-after end)
          (delete-region (region (current-point) end)))))
  (setf (buffer-minor-mode (current-buffer) "Select") nil)
  (deactivate-region))

(defcommand "Helix Change No Yank" (p)
  "Delete selection without yanking, then enter insert mode (Alt-c)."
  "Helix Alt-c."
  (declare (ignore p))
  (helix-delete-no-yank-command nil)
  (setf (buffer-minor-mode (current-buffer) "Normal") nil))

(defcommand "Helix Increment" (p)
  "Increment the integer at/near point by P (default 1) (Ctrl-a)."
  "Helix Ctrl-a."
  (let ((count (or p 1)))
    (with-mark ((start (current-point) :left-inserting)
                (end   (current-point) :right-inserting))
      ;; Extend start backward to beginning of digit run.
      (loop while (let ((c (previous-character start)))
                    (and c (digit-char-p c)))
            do (mark-before start))
      ;; Extend end forward to end of digit run.
      (loop while (let ((c (next-character end)))
                    (and c (digit-char-p c)))
            do (mark-after end))
      (let ((str (region-to-string (region start end))))
        (when (and (plusp (length str)) (every #'digit-char-p str))
          (delete-region (region start end))
          (insert-string start (format nil "~D" (+ (parse-integer str) count))))))))

(defcommand "Helix Decrement" (p)
  "Decrement the integer at/near point by P (default 1) (Ctrl-x)."
  "Helix Ctrl-x."
  (helix-increment-command (- (or p 1))))

(defcommand "Helix Search Word" (p)
  "Search forward for the word under cursor (*)."
  "Helix *."
  (declare (ignore p))
  (with-mark ((start (current-point) :left-inserting)
              (end   (current-point) :right-inserting))
    (word-offset start -1)
    (word-offset end 1)
    (let* ((word    (region-to-string (region start end)))
           (pattern (new-search-pattern :string-sensitive :forward word)))
      (when (plusp (length word))
        (with-mark ((found (current-point) :right-inserting))
          (if (find-pattern found pattern)
              (progn
                (move-mark (current-point) found)
                (%set-normal-cursor))
              (editor-error "~A: not found" word)))))))

(defcommand "Helix Search Selection" (p)
  "Search forward for the current selection verbatim (Alt-*)."
  "Helix Alt-*."
  (declare (ignore p))
  (when (region-active-p)
    (let* ((str     (region-to-string (current-region nil nil)))
           (pattern (new-search-pattern :string-sensitive :forward str)))
      (when (plusp (length str))
        (deactivate-region)
        (with-mark ((found (current-point) :right-inserting))
          (if (find-pattern found pattern)
              (progn
                (move-mark (current-point) found)
                (%set-normal-cursor))
              (editor-error "~A: not found" str)))))))

(defcommand "Helix Trim Selection" (p)
  "Remove leading and trailing whitespace from the selection (_)."
  "Helix _."
  (declare (ignore p))
  (when (region-active-p)
    (let* ((point (current-point))
           (mark  (current-mark))
           (start (if (mark< mark point) mark point))
           (end   (if (mark< mark point) point mark)))
      (loop while (and (mark< start end)
                       (let ((c (next-character start)))
                         (and c (member c '(#\space #\tab #\newline)))))
            do (mark-after start))
      (loop while (and (mark< start end)
                       (let ((c (previous-character end)))
                         (and c (member c '(#\space #\tab #\newline)))))
            do (mark-before end)))))

(defcommand "Helix Ensure Selection Forward" (p)
  "Ensure the selection goes forward: anchor before head (Alt-:)."
  "Helix Alt-:."
  (declare (ignore p))
  (when (region-active-p)
    (let ((point (current-point))
          (mark  (current-mark)))
      (when (mark< point mark)
        ;; Swap point and mark so selection goes forward.
        (let ((pl (mark-line    point))
              (pc (mark-charpos point)))
          (setf (mark-line    point) (mark-line    mark)
                (mark-charpos point) (mark-charpos mark)
                (mark-line    mark)  pl
                (mark-charpos mark)  pc))))))

(defcommand "Helix Keep Primary" (p)
  "Keep only the primary selection (,). No-op in single-cursor hemlock."
  "Helix ,."
  (declare (ignore p)))

(defcommand "Helix Toggle Comment" (p)
  "Toggle Lisp ;; line comments on the selection or current line (Ctrl-c)."
  "Helix Ctrl-c."
  (declare (ignore p))
  (flet ((toggle-line (m)
           (line-start m)
           (if (and (not (end-line-p m))
                    (char= (next-character m) #\;))
               ;; Remove leading comment chars and optional space.
               (progn
                 (delete-characters m 1)
                 (when (and (not (end-line-p m))
                            (char= (next-character m) #\;))
                   (delete-characters m 1))
                 (when (and (not (end-line-p m))
                            (char= (next-character m) #\space))
                   (delete-characters m 1)))
               ;; Add comment prefix.
               (insert-string m ";; "))))
    (if (region-active-p)
        (let* ((reg   (current-region nil t))
               (start (region-start reg))
               (end   (region-end   reg)))
          (with-mark ((m start :left-inserting))
            (loop do
              (toggle-line m)
              (when (or (mark>= m end) (not (line-offset m 1)))
                (return)))))
        (with-mark ((m (current-point) :left-inserting))
          (toggle-line m)))))

(defcommand "Helix Add Newline Below" (p)
  "Insert a blank line below the current line (] Space)."
  "Helix ] Space."
  (declare (ignore p))
  (let ((point (current-point)))
    (line-end point)
    (insert-character point #\newline)
    (line-offset point -1 0)))

(defcommand "Helix Add Newline Above" (p)
  "Insert a blank line above the current line ([ Space)."
  "Helix [ Space."
  (declare (ignore p))
  (let ((point (current-point)))
    (line-start point)
    (insert-character point #\newline)
    (line-offset point -1 0)))


;;;; Buffer Creation Hook.

(defvar *non-modal-major-modes*
  '("Dired" "Bufed" "Debugger" "Debug" "Apropos" "Xref" "Coned"
    "Grep" "Typeout" "View" "Fuzzylist" "Lisp-Lib")
  "Major modes where Normal mode is automatically suppressed.
   These buffers use their own single-key bindings directly.")

(defun %non-modal-mode-p (buffer)
  "Return T if BUFFER's major mode is in *non-modal-major-modes*."
  (member (buffer-major-mode buffer) *non-modal-major-modes* :test #'string=))

(defun %activate-normal-mode-if-needed (buffer)
  "Activate Normal mode on BUFFER if the global style is :modal and the
   buffer has not explicitly opted out or is in a non-modal major mode."
  (when (and (not (string= (buffer-name buffer) "Echo Area"))
             (eq (variable-value 'default-editing-style :global) :modal)
             (not (eq (modal-buffer-state buffer) :off))
             (not (%non-modal-mode-p buffer)))
    (setf (buffer-minor-mode buffer "Normal") t)
    (setf (modal-buffer-state buffer) :on)))

(defun %maybe-deactivate-normal-on-mode-change (buffer mode-name)
  "Deactivate Normal mode when a buffer switches to a non-modal major mode."
  (when (member mode-name *non-modal-major-modes* :test #'string=)
    (setf (buffer-minor-mode buffer "Normal") nil)
    (setf (buffer-minor-mode buffer "Select") nil)
    (setf (modal-buffer-state buffer) :off)))

(add-hook make-buffer-hook #'%activate-normal-mode-if-needed)
(add-hook buffer-major-mode-hook #'%maybe-deactivate-normal-on-mode-change)

;;; The "Main" buffer is created by setup-initial-buffer during %init-hemlock,
;;; which runs when main.lisp is loaded --before this (user-1) module loads.
;;; setup-initial-buffer temporarily installs a fake make-buffer-hook that it
;;; removes immediately, so our hook never fires for "Main".  Iterate over all
;;; buffers that already exist and activate Normal mode on them now.
(do-strings (name buffer *buffer-names*)
  (declare (ignore name))
  (%activate-normal-mode-if-needed buffer))
