;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;;
;;; Echo area commands.
;;;
;;;
(in-package :hemlock)

(defhvar "Beep on Ambiguity"
  "If non-NIL, beep when completion of a parse is ambiguous."
  :value t)

(defhvar "Ignore File Types"
  "File types to ignore when trying to complete a filename."
  :value
  (list "fasl" "pmaxf" "sparcf" "rtf" "hpf" "axpf" "sgif" "err"
        "x86f" "lbytef" "core" "trace"      ; Lisp
        "BAK" "CKP"                         ; Backups & Checkpoints
        "PS" "ps" "press" "otl" "dvi" "toc" ; Formatting
        "bbl" "lof" "idx" "lot" "aux"       ; Formatting
        "mo" "elc"                          ; Other editors
        "bin" "lbin"                        ; Obvious binary extensions.
        "o" "a" "aout" "out"                ; UNIXY stuff
        "bm" "onx" "snf"                    ; X stuff
        "UU" "uu" "arc" "Z" "gz" "tar"      ; Binary encoded files
        ))


;;; Field separator characters separate fields for TOPS-20 ^F style
;;; completion.
(defattribute "Parse Field Separator"
  "A value of 1 for this attribute indicates that the corresponding character
  should be considered to be a field separator by the prompting commands.")
(setf (character-attribute :parse-field-separator #\space) 1)


;;; Find-All-Completions  --  Internal
;;;
;;;    Return as a list of all the possible completions of String in the
;;; list of string-tables Tables.
;;;
(defun find-all-completions (string tables)
  (do ((table tables (cdr table))
       (res ()
            (merge 'list (find-ambiguous string (car table))
                   res #'string-lessp)))
      ((null table) res)))

(defcommand "Help on Parse" (p)
  "Display help for parse in progress.
  If there are a limited number of options then display them."
  "Display the *Parse-Help* and any possibly completions of the current
  input."
  (declare (ignore p))
  (let ((help (typecase *parse-help*
                (list (unless *parse-help* (error "There is no parse help."))
                 (apply #'format nil *parse-help*))
                (string *parse-help*)
                (t (error "Parse help is not a string or list: ~S" *parse-help*))))
        (input (region-to-string *parse-input-region*)))
    (cond
      ((eq *parse-type* :keyword)
       (let ((strings (find-all-completions input *parse-string-tables*)))
         (with-pop-up-display (s :height (+ (length strings) 2))
           (write-line help s)
           (finish-output s)
           (cond (strings
                  (write-line "Possible completions of what you have typed:" s)
                  (dolist (string strings)
                    (write-line string s)))
                 (t
                  (write-line
                   "There are no possible completions of what you have typed." s))))))
      ((and (eq *parse-type* :file) (not (zerop (length input))))
       (let ((pns (ambiguous-files (region-to-string *parse-input-region*)
                                   *parse-default*)))
         (declare (list pns))
         (with-pop-up-display(s :height (+ (length pns) 2))
           (write-line help s)
           (cond (pns
                  (write-line "Possible completions of what you have typed:" s)
                  (let ((width (- (window-width (current-window)) 27)))
                    (dolist (pn pns)
                      (let* ((dir (directory-namestring pn))
                             (len (length dir)))
                        (unless (<= len width)
                          (let ((slash (position #\/ dir
                                                 :start (+ (- len width) 3))))
                            (setf dir
                                  (if slash
                                      (concatenate 'string "..."
                                                   (subseq dir slash))
                                      "..."))))
                        (format s " ~A~25T ~A~%"
                                (file-namestring pn) dir)))))
                 (t
                  (write-line
                   "There are no possible completions of what you have typed." s))))))
      (t
       (with-mark ((m (buffer-start-mark *echo-area-buffer*) :left-inserting))
         (insert-string m help)
         (insert-character m #\newline))))))

(defun file-completion-action (typein)
  (declare (simple-string typein))
  (when (zerop (length typein)) (editor-error))
  (multiple-value-bind
        (result win)
      (complete-file typein
                                 :defaults (directory-namestring *parse-default*)
                                 :ignore-types (value ignore-file-types))
    (when result
      (delete-region *parse-input-region*)
      (insert-string (region-start *parse-input-region*)
                     (namestring result)))
    (when (and (not win) (value beep-on-ambiguity))
      (editor-error))))

(defcommand "Complete Keyword" (p)
  "Complete the keyword being parsed as far as possible."
  "Complete the keyword, using grid state if active."
  (declare (ignore p))
  (cond
    ((buffer-ui-tree *echo-area-buffer*)
     (let ((input (echo-confirm)))
       (multiple-value-bind (prefix key value fld ambig)
           (complete-string input *parse-string-tables*)
         (declare (ignore value fld ambig))
         (when prefix (echo-set-input prefix))
         (when (and (or (eq key :ambiguous) (eq key :none))
                    (value beep-on-ambiguity))
           (echo-set-message "Ambiguous")))))
    (t
     (let ((typein (region-to-string *parse-input-region*)))
       (declare (simple-string typein))
       (case *parse-type*
         (:keyword
          (multiple-value-bind (prefix key value field ambig)
              (complete-string typein *parse-string-tables*)
            (declare (ignore value field))
            (when prefix
              (delete-region *parse-input-region*)
              (insert-string (region-start *parse-input-region*) prefix)
              (when (eq key :ambiguous)
                (let ((point (current-point)))
                  (move-mark point (region-start *parse-input-region*))
                  (unless (character-offset point ambig)
                    (buffer-end point)))))
            (when (and (or (eq key :ambiguous) (eq key :none))
                       (value beep-on-ambiguity))
              (editor-error))))
         (:file (file-completion-action typein))
         (t (editor-error "Cannot complete input for this prompt.")))))))

(defun field-separator-p (x)
  (plusp (character-attribute :parse-field-separator x)))

(defcommand "Complete Field" (p)
  "Complete a field in a keyword parse."
  "Complete a field, using grid state if active."
  (cond
    ((buffer-ui-tree *echo-area-buffer*)
     (let* ((char (key-event-char *last-key-event-typed*))
            (sel (getf (ui-tree-state (buffer-ui-tree *echo-area-buffer*)) :selection -1))
            (filtered (getf (ui-tree-state (buffer-ui-tree *echo-area-buffer*)) :filtered))
            (input (echo-confirm))
            (spacep (eql char #\space)))
       (cond
         ;; space or tab with selection: fill in selected candidate
         ((and (>= sel 0) (< sel (length filtered)))
          (echo-set-input (nth sel filtered)))
         ;; space/tab for keyword: field completion
         ((eq *parse-type* :keyword)
          (multiple-value-bind (prefix key value field ambig)
              (complete-string input *parse-string-tables*)
            (declare (ignore value ambig))
            (cond
              ((eq key :none)
               (if spacep
                   (echo-type-char #\space)
                   (echo-set-message "No possible completion.")))
              (prefix
               (let ((text (if spacep
                               (if (or field (not (eq key :unique)))
                                   (concatenate 'string (subseq prefix 0 field) " ")
                                   (subseq prefix 0 field))
                               prefix)))
                 (echo-set-input text)
                 ;; tab didn't extend input — select first match so next tab confirms
                 (when (and (not spacep)
                            (string-equal text input)
                            (eq key :ambiguous))
                   (echo-select 1)))))))
         ;; space/tab for file: file completion
         ((eq *parse-type* :file)
          (multiple-value-bind (result win)
              (complete-file input
                             :defaults (directory-namestring *parse-default*)
                             :ignore-types (value ignore-file-types))
            (cond
              (result
               (echo-set-input (namestring result))
               ;; tab didn't extend input — select first match
               (when (and (not spacep) (not win)
                          (string= (namestring result) input))
                 (echo-select 1)))
              (spacep (echo-type-char #\space))
              (t (echo-set-message "No possible completion.")))))
         ;; non-keyword/file: just insert the character
         (t (echo-type-char char)))))
    (t
     (let ((typein (region-to-string *parse-input-region*)))
       (declare (simple-string typein))
       (case *parse-type*
         (:string (self-insert-command p))
         (:file (file-completion-action typein))
         (:keyword
          (setf *completion-candidates* nil)
          (let ((spacep (eql (key-event-char *last-key-event-typed*) #\space)))
            (when spacep
              (let ((point (current-point)))
                (unless (blank-after-p point)
                  (insert-character point #\space))))
            (multiple-value-bind (prefix key value field ambig)
                (complete-string typein *parse-string-tables*)
              (declare (ignore value ambig))
              (when (eq key :none) (editor-error "No possible completion."))
              (delete-region *parse-input-region*)
              (let ((new-typein (if (and spacep (or field (not (eq key :unique))))
                                    (concatenate 'string (subseq prefix 0 field) " ")
                                    (subseq prefix 0 field))))
                (insert-string (region-start *parse-input-region*) new-typein)))))
         (t (editor-error "Cannot complete input for this prompt.")))))))


(defvar *echo-area-history* (make-ring 10)
  "This ring-buffer contains strings which were previously input in the
  echo area.")

(defvar *echo-history-pointer* 0
  "This is our current position to the ring during a historical exploration.")

(defvar *completion-candidates* nil
  "List of completion candidates for cycling.")
(defvar *completion-index* -1
  "Current index into *completion-candidates*.")
(defvar *completion-base-input* nil
  "The input text that generated the current candidate list.")

(defun ensure-completion-candidates ()
  "Ensure *completion-candidates* is populated for the current input.
   Recomputes if the input has changed from *completion-base-input*."
  (let ((typein (region-to-string *parse-input-region*)))
    (unless (and *completion-base-input*
                 (string= typein *completion-base-input*))
      (setf *completion-base-input* typein
            *completion-candidates* (find-all-completions typein *parse-string-tables*)
            *completion-index* -1))
    *completion-candidates*))

(defcommand "Next Completion" (p)
  "Cycle to the next completion candidate."
  "Move selection down in the completion list, or cycle old-style."
  (declare (ignore p))
  (cond
    ((buffer-ui-tree *echo-area-buffer*)
     (echo-select 1))
    (t
     (unless (eq *parse-type* :keyword)
       (editor-error "Cannot cycle completions for this prompt."))
     (let ((candidates (ensure-completion-candidates)))
       (unless candidates (editor-error "No completions."))
       (setf *completion-index*
             (mod (1+ *completion-index*) (length candidates)))
       (let ((candidate (nth *completion-index* candidates)))
         (delete-region *parse-input-region*)
         (insert-string (region-start *parse-input-region*) candidate)
         (setf *completion-base-input* candidate))))))

(defcommand "Previous Completion" (p)
  "Cycle to the previous completion candidate."
  "Move selection up in the completion list, or cycle old-style."
  (declare (ignore p))
  (cond
    ((buffer-ui-tree *echo-area-buffer*)
     (echo-select -1))
    (t
     (unless (eq *parse-type* :keyword)
       (editor-error "Cannot cycle completions for this prompt."))
     (let ((candidates (ensure-completion-candidates)))
       (unless candidates (editor-error "No completions."))
       (setf *completion-index*
             (mod (1- *completion-index*) (length candidates)))
       (let ((candidate (nth *completion-index* candidates)))
         (delete-region *parse-input-region*)
         (insert-string (region-start *parse-input-region*) candidate)
         (setf *completion-base-input* candidate))))))

(defcommand "Confirm Parse" (p)
  "Terminate echo-area input.
  If the input is invalid then an editor-error will signalled."
  "If no input has been given, exits the recursive edit with the default,
  otherwise calls the verification function."
  (declare (ignore p))
  (let* ((string (if (buffer-ui-tree *echo-area-buffer*)
                     (echo-confirm)
                     (region-to-string *parse-input-region*)))
         (empty (zerop (length string))))
    (declare (simple-string string))
    ;; when using the grid, clean up and put confirmed text into parse region
    (when (buffer-ui-tree *echo-area-buffer*)
      (cleanup-echo-completions)
      (let ((*tree-rendering* t))
        (delete-region *parse-input-region*)
        (insert-string (region-start *parse-input-region*) string)))
    (if empty
        (when *parse-default* (setq string *parse-default*))
        (when (or (zerop (ring-length *echo-area-history*))
                  (string/= string (ring-ref *echo-area-history* 0)))
          (ring-push string *echo-area-history*)))
    (multiple-value-bind (res flag)
        (funcall *parse-verification-function* string)
      (unless (or res flag) (editor-error))
      (exit-recursive-edit res))))

(defcommand "Previous Parse" (p)
  "Rotate the echo-area history forward.
  If current input is non-empty and different from what is on the top
  of the ring then push it on the ring before inserting the new input."
  "Pop the *echo-area-history* ring buffer."
  (let ((length (ring-length *echo-area-history*))
        (p (or p 1)))
    (when (zerop length) (editor-error))
    (cond
      ((eq (last-command-type) :echo-history)
       (let ((base (mod (+ *echo-history-pointer* p) length)))
         (delete-region *parse-input-region*)
         (insert-string (region-end *parse-input-region*)
                        (ring-ref *echo-area-history* base))
         (setq *echo-history-pointer* base)))
      (t
       (let ((current (region-to-string *parse-input-region*))
             (base (mod (if (minusp p) p (1- p)) length)))
         (delete-region *parse-input-region*)
         (insert-string (region-end *parse-input-region*)
                        (ring-ref *echo-area-history* base))
         (when (and (plusp (length current))
                    (string/= (ring-ref *echo-area-history* 0) current))
           (ring-push current *echo-area-history*)
           (incf base))
         (setq *echo-history-pointer* base))))
    (setf (last-command-type) :echo-history)))

(defcommand "Next Parse" (p)
  "Rotate the echo-area history backward.
  If current input is non-empty and different from what is on the top
  of the ring then push it on the ring before inserting the new input."
  "Push the *echo-area-history* ring buffer."
  (previous-parse-command (- (or p 1))))

(defcommand "Illegal" (p)
  "This signals an editor-error.
  It is useful for making commands locally unbound."
  "Just signals an editor-error."
  (declare (ignore p))
  (editor-error))

(add-hook window-buffer-hook
          #'(lambda (window new-buff)
              (when (and (eq window *echo-area-window*)
                         (not (eq new-buff *echo-area-buffer*)))
                (editor-error "Can't change echo area window."))))

(defcommand "Beginning Of Parse" (p)
  "Moves to immediately after the prompt when in the echo area."
  "Move point to start of editable input."
  (declare (ignore p))
  (if (buffer-ui-tree *echo-area-buffer*)
      (echo-cursor-start)
      (move-mark (buffer-point *echo-area-buffer*) *parse-starting-mark*)))

(defcommand "End Of Parse" (p)
  "Moves to end of input in the echo area."
  "Move point to end of editable input."
  (declare (ignore p))
  (if (buffer-ui-tree *echo-area-buffer*)
      (echo-cursor-end)
      (move-mark (buffer-point *echo-area-buffer*)
                 (region-end *parse-input-region*))))

(defcommand "Echo Area Self Insert" (p)
  "Insert a character and update completion display."
  "Self-insert for echo area with inline completion refresh."
  (declare (ignore p))
  (if (buffer-ui-tree *echo-area-buffer*)
      (echo-type-char (key-event-char *last-key-event-typed*))
      (self-insert-command p)))

(defcommand "Echo Area Delete Previous Character" (p)
  "Delete the previous character."
  "Delete previous char."
  (declare (ignore p))
  (if (buffer-ui-tree *echo-area-buffer*)
      (echo-delete-char)
      (with-mark ((tem (buffer-point *echo-area-buffer*)))
        (unless (character-offset tem (- (or p 1))) (editor-error))
        (when (mark< tem *parse-starting-mark*) (editor-error))
        (delete-previous-character-command p))))

(defcommand "Echo Area Kill Previous Word" (p)
  "Kill the previous word."
  "Kill previous word."
  (declare (ignore p))
  (if (buffer-ui-tree *echo-area-buffer*)
      (echo-kill-word)
      (with-mark ((tem (buffer-point *echo-area-buffer*)))
        (unless (word-offset tem (- (or p 1))) (editor-error))
        (when (mark< tem *parse-starting-mark*) (editor-error))
        (kill-previous-word-command p))))

(declaim (special *kill-ring*))

(defcommand "Kill Parse" (p)
  "Kills any input so far."
  "Kills input region."
  (declare (ignore p))
  (if (buffer-ui-tree *echo-area-buffer*)
      (echo-kill-end)
      (if (end-line-p (current-point))
          (kill-region *parse-input-region* :kill-backward)
          (ring-push (delete-and-save-region *parse-input-region*)
                     *kill-ring*))))

(defcommand "Insert Parse Default" (p)
  "Inserts the default for the parse in progress."
  "Inserts *parse-default* into the input."
  (declare (ignore p))
  (unless *parse-default* (editor-error))
  (if (buffer-ui-tree *echo-area-buffer*)
      (echo-set-input *parse-default*)
      (insert-string (buffer-point *echo-area-buffer*) *parse-default*)))

(defcommand "Echo Area Forward Character" (p)
  "Go forward one character."
  "Move cursor forward in input."
  (declare (ignore p))
  (if (buffer-ui-tree *echo-area-buffer*)
      (echo-move-cursor 1)
      (forward-character-command p)))

(defcommand "Echo Area Backward Character" (p)
  "Go back one character."
  "Move cursor back in input."
  (declare (ignore p))
  (if (buffer-ui-tree *echo-area-buffer*)
      (echo-move-cursor -1)
      (progn
        (backward-character-command p)
        (when (mark< (buffer-point *echo-area-buffer*) *parse-starting-mark*)
          (beginning-of-parse-command ())
          (editor-error)))))

(defcommand "Echo Area Backward Word" (p)
  "Go back one word."
  "Move cursor back one word in input."
  (declare (ignore p))
  (if (buffer-ui-tree *echo-area-buffer*)
      (echo-backward-word)
      (progn
        (backward-word-command p)
        (when (mark< (buffer-point *echo-area-buffer*) *parse-starting-mark*)
          (beginning-of-parse-command ())
          (editor-error)))))
