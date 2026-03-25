;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;;
;;; Hemlock Echo Area stuff.
;;;
(in-package :hemlock.command)

(defmode "Echo Area" :major-p t)
(defvar *echo-area-buffer*
  (let ((b (make-buffer "Echo Area" :modes '("Echo Area"))))
    (setf (buffer-undo-p b) nil)
    b)
  "Buffer used to hack text for the echo area.")
(defvar *echo-area-region* (buffer-region *echo-area-buffer*)
  "Internal thing that's the *echo-area-buffer*'s region.")
(defvar *echo-area-stream*
  (make-hemlock-output-stream (region-end *echo-area-region*) :full)
  "Buffered stream that prints into the echo area.")
(defvar *echo-area-window* ()
  "Window used to display stuff in the echo area.")
(defvar *parse-starting-mark*
  (copy-mark (buffer-point *echo-area-buffer*) :right-inserting)
  "Mark that points to the beginning of the text that'll be parsed.")
(defvar *parse-input-region*
  (region *parse-starting-mark* (region-end *echo-area-region*))
  "Region that contains the text typed in.")


;;;; Prompt state — replaces the old special variables (*parse-type*, etc.)

(defstruct prompt-state
  "Holds all state for the current prompt session."
  (source nil)              ; dispatch object passed to the prompt generic
  (type :string)            ; :keyword, :file, :symbol, :string, :integer, :expression
  (tables nil)              ; list of string-tables (for keyword completion)
  (verify nil)              ; verification function: (lambda (string) → (list result) or nil)
  (must-exist t)            ; whether input must match an existing entry
  (default nil)             ; default value string
  (default-string nil)      ; display override for default
  (prompt-text nil)         ; prompt directive string
  (help-text nil)           ; help text for Help on Parse
  (symbol-package nil))     ; package name for symbol completion

(defvar *current-prompt* nil
  "The active prompt-state during a prompt session, or NIL if not prompting.")

;;; Accessors for echo-commands and echo-completion to use instead of specials.
;;; These return safe defaults when not in a prompt session.
(declaim (inline prompt-type prompt-tables prompt-verify prompt-must-exist
                 prompt-default prompt-default-string prompt-text prompt-help
                 prompt-symbol-package))

(defun prompt-type ()          (if *current-prompt* (prompt-state-type *current-prompt*) :string))
(defun prompt-tables ()        (if *current-prompt* (prompt-state-tables *current-prompt*)))
(defun prompt-verify ()        (if *current-prompt* (prompt-state-verify *current-prompt*) #'%not-inside-a-parse))
(defun prompt-must-exist ()    (if *current-prompt* (prompt-state-must-exist *current-prompt*)))
(defun prompt-default ()       (if *current-prompt* (prompt-state-default *current-prompt*)))
(defun prompt-default-string () (if *current-prompt* (prompt-state-default-string *current-prompt*)))
(defun prompt-text ()          (if *current-prompt* (prompt-state-prompt-text *current-prompt*)))
(defun prompt-help ()          (if *current-prompt* (prompt-state-help-text *current-prompt*)))
(defun prompt-symbol-package () (if *current-prompt* (prompt-state-symbol-package *current-prompt*)))

(defun %not-inside-a-parse (quaz)
  "Called when a parse confirmation happens outside a prompt session."
  (declare (ignore quaz))
  (let* ((bufs (remove *echo-area-buffer* *buffer-list*))
         (buf (or (find-if #'buffer-windows bufs)
                  (car bufs)
                  (make-buffer "Main"))))
    (setf (current-buffer) buf)
    (dolist (w *window-list*)
      (when (and (eq (window-buffer w) *echo-area-buffer*)
                 (not (eq w *echo-area-window*)))
        (setf (window-buffer w) buf)))
    (setf (current-window)
          (or (car (buffer-windows buf))
              (make-window (buffer-start-mark buf)))))
  (editor-error "Wham!  We tried to confirm a parse that wasn't in progress?"))


;;;; MESSAGE and CLEAR-ECHO-AREA:

(defhvar "Message Pause" "The number of seconds to pause after a Message."
  :value 0.5s0)

(defvar *last-message-time* 0
  "Internal-Real-Time the last time we displayed a message.")

(defun clear-echo-area ()
  "You guessed it."
  (clear-output *echo-area-stream*)
  (delete-region *echo-area-region*)
  (setf (buffer-modified *echo-area-buffer*) nil))

;;; Message  --  Public
;;;
;;;    Display the stuff on *echo-area-stream*.
;;;
(defun message (string &rest args)
  "Nicely display a message in the echo-area.
  Put the message on a fresh line and wait for \"Message Pause\" seconds
  to give the luser a chance to see it.  String and Args are a format
  control string and format arguments, respectively."
  (cond
    ;; When completion tree is active, show message in grid cell (R0,C2).
    ((buffer-ui-tree *echo-area-buffer*)
     (echo-set-message (apply #'format nil string args)))
    ((eq *current-window* *echo-area-window*)
     (let ((point (buffer-point *echo-area-buffer*)))
       (with-mark ((m point :left-inserting))
         (line-start m)
         (with-output-to-mark (s m :full)
           (apply #'format s string args)
           (fresh-line s)))))
    (t
     (let ((mark (region-end *echo-area-region*)))
       (cond ((buffer-modified *echo-area-buffer*)
              (clear-echo-area))
             ((not (zerop (mark-charpos mark)))
              (insert-character mark #\newline)
              (unless (displayed-p mark *echo-area-window*)
                (clear-echo-area))))
       (apply #'format *echo-area-stream* string args)
       (force-output *echo-area-stream*)
       (setf (buffer-modified *echo-area-buffer*) nil))))
  (force-output *echo-area-stream*)
  (setq *last-message-time* (get-internal-real-time))
  nil)

;;; LOUD-MESSAGE -- Public.
;;;
;;; Like message, only more provocative.
;;;
(defun loud-message (&rest args)
  "This is the same as MESSAGE, but it beeps and clears the echo area before
   doing anything else."
  (beep)
  (clear-echo-area)
  (apply #'message args))


;;;; DISPLAY-PROMPT-NICELY and PARSE-FOR-SOMETHING.

(defun display-prompt-nicely (&optional (prompt (prompt-text)) default)
  (declare (ignore default))
  (clear-echo-area)
  (let ((point (buffer-point *echo-area-buffer*)))
    (cond ((listp prompt)
           (apply #'format *echo-area-stream* prompt)
           (finish-output *echo-area-stream*))
          (t
           (insert-string point prompt)))
    #+(or)                              ;we insert the default directly now
    (when default
      (insert-character point #\[)
      (insert-string point default)
      (insert-string point "] "))))

(defun parse-for-something (state)
  "Run the echo area command loop for a prompt session.
   STATE is a prompt-state holding all prompt configuration."
  (let ((*current-prompt* state))
    (display-prompt-nicely)
    (let ((start-window (current-window))
          (start-buffer (current-buffer))
          (point (buffer-point *echo-area-buffer*)))
      (move-mark *parse-starting-mark* point)
      (insert-string point (or (prompt-default-string) (prompt-default)))
      (when (or (prompt-tables)
                (eq (prompt-type) :file)
                (eq (prompt-type) :symbol))
        (make-echo-completions))
      (setf (current-window) *echo-area-window*)
      (unwind-protect
          (use-buffer *echo-area-buffer*
            (recursive-edit nil))
        (when (buffer-ui-tree *echo-area-buffer*)
          (cleanup-echo-completions))
        (setf (current-window) start-window)
        (change-to-buffer start-buffer)))))


;;;; ─── prompt generic ──────────────────────────────────────────────────────────

(defgeneric prompt (source &key prompt help default default-string must-exist)
  (:documentation "Prompt the user, completing against SOURCE.
   Dispatches on the type of SOURCE to determine completion behavior.
   Returns the selected/validated result."))

(defmethod prompt ((source string-table) &key
                   (prompt "Keyword: ") (help "Type a keyword.")
                   default default-string (must-exist t))
  (parse-for-something
   (make-prompt-state :source source :type :keyword
                      :tables (list source)
                      :verify #'keyword-verification-function
                      :must-exist must-exist
                      :default default :default-string default-string
                      :prompt-text prompt :help-text help)))

(defmethod prompt ((source cons) &key
                   (prompt "Keyword: ") (help "Type a keyword.")
                   default default-string (must-exist t))
  (parse-for-something
   (make-prompt-state :source source :type :keyword
                      :tables source
                      :verify #'keyword-verification-function
                      :must-exist must-exist
                      :default default :default-string default-string
                      :prompt-text prompt :help-text help)))

(defmethod prompt ((source pathname) &key
                   (prompt "Filename: ") (help "Type a file name.")
                   default default-string (must-exist t))
  (parse-for-something
   (make-prompt-state :source source :type :file
                      :verify #'file-verification-function
                      :must-exist must-exist
                      :default (namestring (or default source))
                      :default-string default-string
                      :prompt-text prompt :help-text help)))

(defmethod prompt ((source string) &key
                   (prompt "Filename: ") (help "Type a file name.")
                   default default-string (must-exist t))
  "Coerce string to pathname and delegate to the pathname method."
  (prompt (pathname source) :prompt prompt :help help
          :default default :default-string default-string
          :must-exist must-exist))

(defmethod prompt ((source package) &key
                   (prompt "Symbol: ") (help "Complete a symbol.")
                   default default-string must-exist)
  (declare (ignore must-exist))
  (parse-for-something
   (make-prompt-state :source source :type :symbol
                      :verify (lambda (string) (list string))
                      :default-string (or default-string default)
                      :prompt-text prompt :help-text help
                      :symbol-package (package-name source))))

(defmethod prompt ((source (eql :string)) &key
                   (prompt "String: ") (help "Type a string.")
                   default default-string must-exist trim)
  (declare (ignore must-exist))
  (parse-for-something
   (make-prompt-state :source source :type :string
                      :verify (lambda (string)
                                (list (string-trim
                                       (if (eq trim t) '(#\space #\tab)
                                           (or trim '()))
                                       string)))
                      :default default :default-string default-string
                      :prompt-text prompt :help-text help)))

(defmethod prompt ((source (eql :integer)) &key
                   (prompt "Integer: ") (help "Type an integer.")
                   default default-string (must-exist t))
  (parse-for-something
   (make-prompt-state :source source :type :integer
                      :verify (lambda (string)
                                (let ((number (parse-integer string :junk-allowed t)))
                                  (if must-exist
                                      (if number (list number))
                                      (list (or number string)))))
                      :must-exist must-exist
                      :default (if default (write-to-string default :base 10))
                      :default-string default-string
                      :prompt-text prompt :help-text help)))

(defmethod prompt ((source (eql :expression)) &key
                   (prompt "Expression: ") (help "Type a Lisp expression.")
                   (default nil defaultp) default-string (must-exist t))
  (parse-for-something
   (make-prompt-state :source source :type :expression
                      :verify (lambda (string)
                                (declare (ignore string))
                                (let ((expr (with-input-from-region
                                                (stream *parse-input-region*)
                                              (handler-case (read stream nil hemlock-eof)
                                                (error () hemlock-eof)))))
                                  (if must-exist
                                      (if (not (eq expr hemlock-eof))
                                          (values (list expr) t))
                                      (if (eq expr hemlock-eof)
                                          (list (region-to-string *parse-input-region*))
                                          (values (list expr) t)))))
                      :must-exist must-exist
                      :default (if defaultp (prin1-to-string default))
                      :default-string default-string
                      :prompt-text prompt :help-text help)))

(defmethod prompt ((source (eql :yes-or-no)) &key
                   (prompt "Yes or No? ") (help "Type Yes or No.")
                   (default nil defaultp) default-string (must-exist t))
  (parse-for-something
   (make-prompt-state :source source :type :keyword
                      :tables (list *yes-or-no-string-table*)
                      :verify (lambda (string)
                                (multiple-value-bind (prefix key value field ambig)
                                    (complete-string string
                                                     (list *yes-or-no-string-table*))
                                  (declare (ignore prefix field ambig))
                                  (let ((won (or (eq key :complete) (eq key :unique))))
                                    (if must-exist
                                        (if won (values (list value) t))
                                        (list (if won (values value t) string))))))
                      :must-exist must-exist
                      :default (if defaultp (if default "Yes" "No"))
                      :default-string default-string
                      :prompt-text prompt :help-text help)))

(defmethod prompt ((source (eql :y-or-n)) &key
                   (prompt "Y or N? ") (help "Type Y or N.")
                   (default nil defaultp) default-string (must-exist t))
  (declare (ignore default-string))
  (let ((*current-prompt* (make-prompt-state :type :y-or-n
                                             :help-text help
                                             :prompt-text prompt))
        (old-window (current-window)))
    (unwind-protect
        (progn
          (setf (current-window) *echo-area-window*)
          (display-prompt-nicely prompt (if defaultp (if default "Y" "N")))
          (loop
            (let ((key-event (get-key-event *editor-input*)))
              (cond ((or (eq key-event #k"y") (eq key-event #k"Y"))
                     (return t))
                    ((or (eq key-event #k"n") (eq key-event #k"N"))
                     (return nil))
                    ((logical-key-event-p key-event :confirm)
                     (if defaultp (return default) (beep)))
                    ((logical-key-event-p key-event :help)
                     (hemlock::help-on-parse-command ()))
                    (t
                     (unless must-exist (return key-event))
                     (beep))))))
      (setf (current-window) old-window))))

(defmethod prompt ((source (eql :key-event)) &key
                   (prompt "Key-event: ") help default default-string
                   must-exist (change-window t))
  (declare (ignore help default default-string must-exist))
  (prompt-for-key-event* prompt change-window))

(defmethod prompt ((source (eql :key)) &key
                   (prompt "Key: ") (help "Type a key.")
                   default default-string (must-exist t))
  (prompt-for-key :prompt prompt :help help
                  :default default :default-string default-string
                  :must-exist must-exist))


(defun file-verification-function (string)
  (let ((pn (pathname-or-lose string)))
    (if pn
        (let ((merge
               (cond ((not (prompt-default)) nil)
                     ((directoryp pn)
                      (merge-pathnames pn (prompt-default)))
                     (t
                      (merge-pathnames pn
                                       (or (directory-namestring
                                            (prompt-default))
                                           ""))))))
          (cond ((probe-file pn) (list pn))
                ((and merge (probe-file merge)) (list merge))
                ((not (prompt-must-exist)) (list (or merge pn)))
                (t nil))))))

;;; PATHNAME-OR-LOSE tries to convert string to a pathname using
;;; PARSE-NAMESTRING.  If it succeeds, this returns the pathname.  Otherwise,
;;; this deletes the offending characters from *parse-input-region* and signals
;;; an editor-error.
;;;
(defun pathname-or-lose (string)
  (declare (simple-string string))
  (multiple-value-bind (pn idx)
                       (parse-namestring string nil *default-pathname-defaults*
                                         :junk-allowed t)
    (cond (pn)
          (t (delete-characters (region-end *echo-area-region*)
                                (- idx (length string)))
             nil))))


(defun current-variable-tables ()
  "Returns a list of all the variable tables currently established globally,
   by the current buffer, and by any modes for the current buffer."
  (do ((tables (list (buffer-variables *current-buffer*)
                     *global-variable-names*)
               (cons (mode-object-variables (car mode)) tables))
       (mode (buffer-mode-objects *current-buffer*) (cdr mode)))
      ((null mode) tables)))

(defun keyword-verification-function (string)
  (declare (simple-string string))
  (multiple-value-bind
      (prefix key value field ambig)
      (complete-string string (prompt-tables))
    (declare (ignore field))
    (cond ((prompt-must-exist)
           (ecase key
             (:none nil)
             ((:unique :complete)
              (list prefix value))
             (:ambiguous
              (delete-region *parse-input-region*)
              (insert-string (region-start *parse-input-region*) prefix)
              (let ((point (current-point)))
                (move-mark point (region-start *parse-input-region*))
                (unless (character-offset point ambig)
                  (buffer-end point)))
              nil)))
          (t
           ;; HACK: If it doesn't have to exist, and the completion does not
           ;; add anything, then return the completion's capitalization,
           ;; instead of the user's input.
           (list (if (= (length string) (length prefix)) prefix string))))))


(defvar hemlock-eof '(())
  "An object that won't be EQ to anything read.")


(defvar *yes-or-no-string-table*
  (make-string-table :initial-contents '(("Yes" . t) ("No" . nil))))

(defun listify (x) (if (listp x) x (list x)))

(defun key-event-dispatch (&rest clauses)
  (let ((key-event (get-key-event *editor-input*)))
    (loop for (keys body) on clauses by #'cddr
          when (or (eq keys t) (find key-event keys))
            do (return (funcall body)))))

(defmacro key-event-case (&rest clauses)
  `(key-event-dispatch
    ,@(loop for (keys . body) in clauses
            collect (if (eq keys t) t `(list ,@keys))
            collect `(lambda () ,@body))))

(defun prompt-for-key-event* (prompt change-window)
  (let ((old-window (current-window)))
    (unwind-protect
        (progn
          (when change-window
            (setf (current-window) *echo-area-window*))
          (display-prompt-nicely prompt)
          (get-key-event *editor-input* t))
      (when change-window (setf (current-window) old-window)))))

(defvar *prompt-key* (make-array 10 :adjustable t :fill-pointer 0))
(defun prompt-for-key (&key ((:must-exist must-exist) t)
                            default default-string
                            (prompt "Key: ")
                            ((:help *parse-help*) "Type a key."))
  (let ((old-window (current-window))
        (string (if default
                    (or default-string
                        (let ((l (coerce default 'list)))
                          (format nil "~:C~{ ~:C~}" (car l) (cdr l)))))))

    (unwind-protect
        (progn
          (setf (current-window) *echo-area-window*)
          (display-prompt-nicely prompt string)
          (setf (fill-pointer *prompt-key*) 0)
          (prog ((key *prompt-key*) key-event)
                (declare (vector key))
                TOP
                (setf key-event (get-key-event *editor-input*))
                (cond ((logical-key-event-p key-event :quote)
                       (setf key-event (get-key-event *editor-input* t)))
                      ((logical-key-event-p key-event :confirm)
                       (cond ((and default (zerop (length key)))
                              (let ((res (get-command default :current)))
                                (unless (commandp res) (go FLAME))
                                (return (values default res))))
                             ((and (not must-exist) (plusp (length key)))
                              (return (copy-seq key)))
                             (t
                              (go FLAME))))
                      ((logical-key-event-p key-event :help)
                       (hemlock::help-on-parse-command ())
                       (go TOP)))
                (vector-push-extend key-event key)
                (when must-exist
                  (let ((res (get-command key :current)))
                    (cond ((commandp res)
                           (print-pretty-key-event key-event
                                                       *echo-area-stream*
                                                       t)
                           (write-char #\space *echo-area-stream*)
                           (force-output *echo-area-stream*)
                           (return (values (copy-seq key) res)))
                          ((not (eq res :prefix))
                           (vector-pop key)
                           (go FLAME)))))
                (print-pretty-key key-event *echo-area-stream* t)
                (write-char #\space *echo-area-stream*)
                (force-output *echo-area-stream*)
                (go TOP)
                FLAME
                (beep)
                (go TOP)))
      (force-output *echo-area-stream*)
      (setf (current-window) old-window))))


;;;; Logical key-event stuff.

(defvar *real-to-logical-key-events* (make-hash-table :test #'eql)
  "A hashtable from real key-events to their corresponding logical
   key-event keywords.")

(defvar *logical-key-event-descriptors* (make-hash-table :test #'eq)
  "A hashtable from logical-key-events to logical-key-event-descriptors.")

(defstruct (logical-key-event-descriptor
            (:constructor make-logical-key-event-descriptor ()))
  name
  key-events
  documentation)

;;; LOGICAL-KEY-EVENT-P  --  Public
;;;
(defun logical-key-event-p (key-event keyword)
  "Return true if key-event has been defined to have Keyword as its
   logical key-event.  The relation between logical and real key-events
   is defined by using SETF on LOGICAL-KEY-EVENT-P.  If it is set to
   true then calling LOGICAL-KEY-EVENT-P with the same key-event and
   Keyword, will result in truth.  Setting to false produces the opposite
   result.  See DEFINE-LOGICAL-KEY-EVENT and COMMAND-CASE."
  (not (null (member keyword (gethash key-event *real-to-logical-key-events*)))))

;;; GET-LOGICAL-KEY-EVENT-DESC  --  Internal
;;;
;;;    Return the descriptor for the logical key-event keyword, or signal
;;; an error if it isn't defined.
;;;
(defun get-logical-key-event-desc (keyword)
  (let ((res (gethash keyword *logical-key-event-descriptors*)))
    (unless res
      (error "~S is not a defined logical-key-event keyword." keyword))
    res))

;;; %SET-LOGICAL-KEY-EVENT-P  --  Internal
;;;
;;;    Add or remove a logical key-event link by adding to or deleting from
;;; the list in the from-char hashtable and the descriptor.
;;;
(defun (setf logical-key-event-p) (new-value key-event keyword)
  "Change what Logical-Char= returns for the specified arguments."
  (let ((entry (get-logical-key-event-desc keyword)))
    (cond
     (new-value
      (pushnew keyword (gethash key-event *real-to-logical-key-events*))
      (pushnew key-event (logical-key-event-descriptor-key-events entry)))
     (t
      (setf (gethash key-event *real-to-logical-key-events*)
            (delete keyword (gethash key-event *real-to-logical-key-events*)))
      (setf (logical-key-event-descriptor-key-events entry)
            (delete keyword (logical-key-event-descriptor-key-events entry))))))
  new-value)

(defun logical-key-event-key-events (keyword)
  "Return the list of key-events for which Keyword is the logical key-event."
  (logical-key-event-descriptor-key-events
   (get-logical-key-event-desc keyword)))

;;; DEFINE-LOGICAL-KEY-EVENT  --  Public
;;;
;;;    Make the entries in the two hashtables and the string-table.
;;;
(defun define-logical-key-event (name documentation)
  "Define a logical key-event having the specified Name and Documentation.
  See LOGICAL-KEY-EVENT-P and COMMAND-CASE."
  (check-type name string)
  (check-type documentation (or string function))
  (let* ((keyword (string-to-keyword name))
         (entry (or (gethash keyword *logical-key-event-descriptors*)
                    (setf (gethash keyword *logical-key-event-descriptors*)
                          (make-logical-key-event-descriptor)))))
    (setf (logical-key-event-descriptor-documentation entry) documentation)))


;;;; Some standard logical-key-events:

(define-logical-key-event "Forward Search"
  "This key-event is used to indicate that a forward search should be made.")
(define-logical-key-event "Backward Search"
  "This key-event is used to indicate that a backward search should be made.")
(define-logical-key-event "Recursive Edit"
  "This key-event indicates that a recursive edit should be entered.")
(define-logical-key-event "Cancel"
  "This key-event is used  to cancel a previous key-event of input.")
(define-logical-key-event "Abort"
  "This key-event is used to abort the command in progress.")
(define-logical-key-event "Exit"
  "This key-event is used to exit normally the command in progress.")
(define-logical-key-event "Yes"
  "This key-event is used to indicate a positive response.")
(define-logical-key-event "No"
  "This key-event is used to indicate a negative response.")
(define-logical-key-event "Do All"
  "This key-event means do it as many times as you can.")
(define-logical-key-event "Do Once"
  "This key-event means, do it this time, then exit.")
(define-logical-key-event "Help"
  "This key-event is used to ask for help.")
(define-logical-key-event "Confirm"
  "This key-event is used to confirm some choice.")
(define-logical-key-event "Quote"
  "This key-event is used to quote the next key-event of input.")
(define-logical-key-event "Keep"
  "This key-event means exit but keep something around.")


;;;; COMMAND-CASE help message printing.

(defvar *my-string-output-stream* (make-string-output-stream))

(defun chars-to-string (chars)
  (do ((s *my-string-output-stream*)
       (chars chars (cdr chars)))
      ((null chars)
       (get-output-stream-string s))
    (let ((char (car chars)))
      (if (characterp char)
          (write-char char s)
          (do ((key-events
                (logical-key-event-key-events char)
                (cdr key-events)))
              ((null key-events))
            (print-pretty-key (car key-events) s)
            (unless (null (cdr key-events))
              (write-string ", " s))))
      (unless (null (cdr chars))
        (write-string ", " s)))))

;;; COMMAND-CASE-HELP  --  Internal
;;;
;;;    Print out a help message derived from the options in a
;;; random-typeout window.
;;;
(defun command-case-help (help options)
  (let ((help (if (listp help)
                  (apply #'format nil help) help)))
    (with-pop-up-display (s)
      (write-string help s)
      (fresh-line s)
      (do ((o options (cdr o)))
          ((null o))
        (let ((string (chars-to-string (caar o))))
          (declare (simple-string string))
          (if (= (length string) 1)
              (write-char (char string 0) s)
              (write-line string s))
          (write-string "  - " s)
          (write-line (cdar o) s))))))
