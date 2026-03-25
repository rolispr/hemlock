;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;;
;;; This file contains code for connecting to eval servers and some command
;;; level stuff too.
;;;
;;;

(in-package :hemlock)



;;;; Structures.

(defstruct (server-info (:print-function print-server-info))
  name                        ; String name of this server.
  wire                        ; Wire connected to this server.
  notes                       ; List of note objects for operations
                              ;  which have not yet completed.
  agent-info                  ; Ts-Info used in "Agent Lisp" buffer
                              ;  (formerly the "Lisp Listener" buffer).
  agent-buffer                ; "Agent Lisp" buffer for agent's *terminal-io*.
  background-info             ; Ts-Info structure for the compilation buffer.
  background-buffer           ; Buffer for compilation/background output.
  (errors                     ; Array of errors while compiling
   (make-array 16 :adjustable t :fill-pointer 0))
  error-index                 ; Index of current error.
  implementation-type
  implementation-version)
;;;
(defun print-server-info (obj stream n)
  (declare (ignore n))
  (format stream "#<Server-info for ~A>" (server-info-name obj)))


(defstruct (error-info (:print-function print-error-info))
  buffer                      ; Buffer this error is for.
  message                     ; Error Message
  line                        ; Pointer to message in compilation buffer.
  region)                     ; Region of faulty text
;;;
(defun print-error-info (obj stream n)
  (declare (ignore n))
  (format stream "#<Error: ~A>" (error-info-message obj)))


(defvar *server-names* (make-string-table)
  "A string-table of the name of all Eval servers and their corresponding
   server-info structures.")

(defun list-server-infos ()
  (map-string-table 'list #'identity *server-names*))

(defvar *abort-operations* nil
  "T iff we should ignore any operations sent to us.")

(defvar *inside-operation* nil
  "T iff we are currenly working on an operation. A catcher for the tag
   abort-operation will be established whenever this is T.")

(defparameter *agent-connect-wait* 300)

;;; Used internally for communications.
;;;
(defvar *newly-created-agent* nil)
(defvar *compiler-wire* nil)
(defvar *compiler-error-stream* nil)
(defvar *compiler-note* nil)



;;;; Hemlock Variables

(defhvar "Current Compile Server"
  "The Server-Info object for the server currently used for compilation
   requests."
  :value nil)

(defhvar "Current Package"
  "This variable holds the name of the package currently used for Lisp
   evaluation and compilation.  If it is Nil, the value of *Package* is used
   instead."
  :value nil)

(defhvar "Agent Utility"
  "This is the pathname of the utility to fire up agent Lisps.  It defaults
   to \"cmucl\"."
  :value "cmucl")

(defhvar "Agent Utility Switches"
  "These are additional switches to pass to the Agent Utility.
   For example, (list \"-core\" <core-file-name>).  The agent
   switch and the editor name are always supplied, and they should
   not be present in this variable."
  :value nil)

(defhvar "Ask About Old Servers"
  "When set (the default), Hemlock will prompt for an existing server's name
   in preference to prompting for a new agent's name and creating it."
  :value t)

(defhvar "Confirm Agent Creation"
  "When set, Hemlock always confirms an agent's creation for whatever reason."
  ;; I'm not certain what this reason would be, so I'm disabling it.
  ;; In any case, I think the user should determine interactively whether
  ;; he wants this kind of question or not using the command prefix --
  ;; or by using a different command in the first place.
  ;; --dfl
  :value nil)


(defhvar "Agent GC Alarm"
  "Determines what is done when the agent notifies that it is GCing.
  :MESSAGE prints a message in the echo area, :LOUD-MESSAGE beeps as well.
  NIL does nothing."
  :value :message)


;;;; Agent destruction.

;;; WIRE-DIED -- Internal.
;;;
;;; The routine is called whenever a wire dies.  We roll through all the
;;; Scan all servers looking for any that use this wire and clean them up.
;;;
(defun wire-died (wire)
  (let ((servers nil))
    (do-strings (name info *server-names*)
      (declare (ignore name))
      (when (eq wire (server-info-wire info))
        (push info servers)))
    (dolist (server servers)
      (agent-died server))))

;;; SERVER-DIED -- Internal.
;;;
;;; Clean up the server. Remove any references to it from variables, etc.
;;;
(defun agent-died (server)
  (declare (special *breakpoints*))
  (let ((name (server-info-name server)))
    (delete-string name *server-names*)
    (message "Agent ~A just died." name))
  (when (server-info-wire server)
    (setf (server-info-wire server) nil))
  (when (server-info-agent-info server)
    (session-buffer-wire-died (server-info-agent-info server))
    (setf (server-info-agent-info server) nil))
  (when (server-info-background-info server)
    (session-buffer-wire-died (server-info-background-info server))
    (setf (server-info-background-info server) nil))
  (clear-server-errors server)
  (when (eq server (variable-value 'current-eval-server :global))
    (setf (variable-value 'current-eval-server :global) nil))
  (when (eq server (variable-value 'current-compile-server :global))
    (setf (variable-value 'current-compile-server :global) nil))
  (dolist (buffer *buffer-list*)
    (dolist (var '(current-eval-server current-compile-server server-info))
      (when (and (hemlock-bound-p var :buffer buffer)
                 (eq (variable-value var :buffer buffer) server))
        (delete-variable var :buffer buffer))))
  (setf *breakpoints* (delete-if #'(lambda (b)
                                     (eq (breakpoint-info-agent b) server))
                                 *breakpoints*)))

;;; SERVER-CLEANUP -- Internal.
;;;
;;; This routine is called as a buffer delete hook.  It takes care of any
;;; per-buffer cleanup that is necessary.  It clears out all references to the
;;; buffer from server-info structures and that any errors that refer to this
;;; buffer are finalized.
;;;
(defun server-cleanup (buffer)
  (let ((info (if (hemlock-bound-p 'server-info :buffer buffer)
                  (variable-value 'server-info :buffer buffer))))
    (when info
      (when (eq buffer (server-info-agent-buffer info))
        (setf (server-info-agent-buffer info) nil)
        (setf (server-info-agent-info info) nil))
      (when (eq buffer (server-info-background-buffer info))
        (setf (server-info-background-buffer info) nil)
        (setf (server-info-background-info info) nil))))
  (do-strings (string server *server-names*)
    (declare (ignore string))
    (clear-server-errors server
                         #'(lambda (error)
                             (eq (error-info-buffer error) buffer)))))
;;;
(add-hook delete-buffer-hook 'server-cleanup)

;;; CLEAR-SERVER-ERRORS -- Public.
;;;
;;; Clears all known errors for the given server and resets it so more can
;;; accumulate.
;;;
(defun clear-server-errors (server &optional test-fn)
  "This clears compiler errors for server cleaning up any pointers for GC
   purposes and allowing more errors to register."
  (let ((array (server-info-errors server))
        (current nil))
    (dotimes (i (fill-pointer array))
      (let ((error (aref array i)))
        (when (or (null test-fn)
                  (funcall test-fn error))
          (let ((region (error-info-region error)))
            (when (regionp region)
              (delete-mark (region-start region))
              (delete-mark (region-end region))))
          (setf (aref array i) nil))))
    (let ((index (server-info-error-index server)))
      (when index
        (setf current
              (or (aref array index)
                  (find-if-not #'null array
                               :from-end t
                               :end current)))))
    (setf array (delete nil array))
    (setf (server-info-error-index server)
          (position current array))))



;;;; Agent creation.

;;; INITIALIZE-SERVER-STUFF -- Internal.
;;;
;;; Reinitialize stuff when a core file is saved.
;;;
(defun initialize-server-stuff ()
  (clrstring *server-names*))


(defvar *editor-name* nil "Name of this editor.")

#+(or) ;disabled.  If the original switch was important, let's find a
       ; different way to to this.  -dfl
(defvar *accept-connections* t
  "When set, allow agents to connect to the editor.")

;;; GET-EDITOR-NAME -- Internal.
;;;
;;; Pick a name for the editor.  Names consist of machine-name:port-number.  If
;;; in ten tries we can't get an unused port, choak.  We don't save the result
;;; of HEMLOCK.WIRE:CREATE-REQUEST-SERVER because we don't think the editor needs to
;;; ever kill the request server, and we can always inhibit connection with
;;; "Accept Connections".
;;;
(defun get-editor-name ()
  (if *editor-name*
      *editor-name*
      (multiple-value-bind (request-server port)
          (create-request-server)
        (declare (ignore request-server))
        (setf *editor-name*
              (format nil "~A:~D"
                      #+nil (machine-instance)
                      "127.0.0.1"
                      port)))))


;;; MAKE-BUFFERS-FOR-TYPESCRIPT -- Internal.
;;;
(defun make-buffers-for-session (agent-name background-name)
  "Make the interactive and compilation buffers agent-name and background-name.
   If either is nil, then prompt the user."
  (multiple-value-bind (agent-name background-name)
                       (cond ((not (and agent-name background-name))
                              (pick-agent-buffer-names))
                             ((getstring agent-name *server-names*)
                              (multiple-value-bind
                                  (new-sn new-bn)
                                  (pick-agent-buffer-names)
                                (message "~S is already an eval server; ~
                                          using ~S instead."
                                         agent-name new-sn)
                                (values new-sn new-bn)))
                             (t (values agent-name background-name)))
    (let* ((agent-buffer (or (getstring agent-name *buffer-names*)
                             (make-buffer agent-name :modes '("Lisp"))))
           (background-buffer (or (getstring background-name *buffer-names*)
                                  (make-buffer background-name
                                               :modes '("Lisp"))))
           (server-info (make-server-info :name agent-name
                                          :wire :wire-not-yet-established
                                          :agent-buffer agent-buffer
                                          :background-buffer background-buffer))
           (agent-info (sessionify-buffer agent-buffer server-info
                                             :wire-not-yet-established))
           (background-info (sessionify-buffer background-buffer server-info
                                                  :wire-not-yet-established)))
      (setf (server-info-agent-info server-info) agent-info)
      (setf (server-info-background-info server-info) background-info)
      (setf (getstring agent-name *server-names*) server-info)
      (setf (variable-value 'current-eval-server :global) server-info)
      server-info)))


;;; CREATE-AGENT -- Public.
;;;
;;; clbuild command variables deleted — clbuild is dead.

(defun create-agent (command &optional name)
  "This creates an agent that tries to connect to the editor.  A preliminary
   server-info structure is returned immediately, whose details will
   be filled in later by the agent once the wire has been established.
   Name is the name of the interactive buffer.  If name is nil, this generates
   a name.  If name is supplied, and a buffer with that name already exists,
   this signals an error."
  (when (and name (getstring name *buffer-names*))
    (editor-error "Buffer ~A is already in use." name))
  (multiple-value-bind (agent background)
      (if name
          (values name (format nil "Compilation ~A" name))
          (pick-agent-buffer-names))
    (when (value confirm-agent-creation)
      (setf agent (prompt :string
                        :prompt "New agent name? "
                        :help "Enter the name to use for the newly created agent."
                        :default agent
                        :default-string agent))
      (setf background (format nil "Compilation ~A" agent))
      (when (getstring agent *buffer-names*)
        (editor-error "Buffer ~A is already in use." agent))
      (when (getstring background *buffer-names*)
        (editor-error "Buffer ~A is already in use." background)))
    (message "Spawning agent ... ")
    (let ((server-info (make-buffers-for-session agent background)))
      (make-process-connection
       command
       :filter (let ((ts (server-info-agent-info server-info)))
                 (lambda (connection bytes)
                   (session-buffer-output-string
                    ts
                    (default-filter connection bytes))
                   nil)))
      server-info)))

;;; CREATE-AGENT-IN-THREAD -- Public.
;;;
(defun create-local-eval (&optional name)
  "Create the master's own eval server.
   Evaluates directly in hemlock's image — no wire, no remote process."
  (when (and name (getstring name *buffer-names*))
    (editor-error "Buffer ~A is already in use." name))
  (multiple-value-bind (agent background)
      (if name
          (values name (format nil "Compilation ~A" name))
          (pick-agent-buffer-names))
    (when (value confirm-agent-creation)
      (setf agent (prompt :string
                          :prompt "New agent name? "
                          :help "Enter the name to use for the newly created agent."
                          :default agent
                          :default-string agent))
      (setf background (format nil "Compilation ~A" agent))
      (when (getstring agent *buffer-names*)
        (editor-error "Buffer ~A is already in use." agent))
      (when (getstring background *buffer-names*)
        (editor-error "Buffer ~A is already in use." background)))
    (let ((server-info (make-buffers-for-session agent background)))
      ;; Mark the server as immediately ready — no wire handshake needed.
      ;; The wire slot holds :local to distinguish from process agents.
      (setf (server-info-wire server-info) :local)
      ;; Update session-data wire slots so session input works.
      (let ((ts (server-info-agent-info server-info))
            (bg (server-info-background-info server-info)))
        (when ts (setf (session-data-wire ts) :local))
        (when bg (setf (session-data-wire bg) :local)))
      server-info)))

;;; MAYBE-CREATE-SERVER -- Internal interface.
;;;
(defun maybe-create-server ()
  "If there is an existing server and \"Ask about Old Servers\" is set, then
   prompt for a server's name and return that server's info.  Otherwise,
   create a new server."
  (if (value ask-about-old-servers)
      (multiple-value-bind (first-server-name first-server-info)
                           (do-strings (name info *server-names*)
                             (return (values name info)))
        (if first-server-info
            (multiple-value-bind
                (name info)
                (prompt *server-names*
                       :prompt "Existing server name: "
                       :default first-server-name
                       :default-string first-server-name
                       :help
                       "Enter the name of an existing eval agent."
                       :must-exist t)
              (declare (ignore name))
              (or info (create-local-eval)))
            (create-local-eval)))
      (create-local-eval)))


(defvar *next-agent-index* 0
  "Number to use when creating the next agent.")

;;; PICK-AGENT-BUFFER-NAMES -- Internal.
;;;
;;; Return two unused names to use for the agent and compilation buffers.
;;;
(defun pick-agent-buffer-names (&optional info)
  "Generate buffer names for an eval server connection pair."
  (let* ((parent (or info
                     (buffer-name (current-buffer))))
         (type "M"))
    (loop
      (let* ((n (incf *next-agent-index*))
             (fg (format nil "REPL ~A ~A:~D" parent type n))
             (bg (format nil "Compilation ~A ~A:~D" parent type n)))
        (unless (or (getstring fg *buffer-names*)
                    (getstring bg *buffer-names*))
          (return (values fg bg)))))))



;;;; Agent selection.

;;; GET-CURRENT-EVAL-SERVER -- Public.
;;;
(defun get-current-eval-server (&optional errorp)
  "Returns the server-info struct for the current eval server.  If there is
   none, and errorp is non-nil, then signal an editor error.  If there is no
   current server, and errorp is nil, then create one, prompting the user for
   confirmation.  Also, set the current server to be the newly created one."
  (setf errorp t)
  (let ((info (value current-eval-server)))
    (cond (info)
          (errorp
           (editor-error "No current eval agent."))
          (t
           (setf (value current-eval-server) (maybe-create-server))))))

;;; GET-CURRENT-COMPILE-SERVER -- Public.
;;;
;;; If a current compile server is defined, return it, otherwise return the
;;; current eval server using get-current-eval-server.
;;;
(defun get-current-compile-server (&optional errorp)
  "Returns the server-info struct for the current compile server. If there is
   no current compile server, return the current eval server."
  (or (value current-compile-server)
      (get-current-eval-server errorp)))



;;;; Server Manipulation commands.

;;; clbuild command helpers and process-spawn commands deleted — clbuild is dead.

(defcommand "Eval Async" (p)
  "Create a local eval agent backed by a eval thread.
   No wire protocol — evaluates directly in the master image."
  ""
  (let ((info (create-local-eval (pick-agent-buffer-names "Eval"))))
    (change-to-buffer (server-info-agent-buffer info))))

(defcommand "Select Agent" (p)
  "" ""
  (let* ((info (or (get-current-eval-server)
                   (editor-error "No current eval agent yet")))
         (agent (server-info-agent-buffer info)))
    (unless agent
      (editor-error "The current eval agent doesn't have an agent buffer!"))
    (change-to-buffer agent)))

(defcommand "Select Compilation" (p)
  "Switch to the current eval server's compilation buffer."
  "Switch to the current eval server's compilation buffer."
  (let* ((info (if p
                 (get-current-compile-server t)
                 (get-current-eval-server t)))
         (background (server-info-background-buffer info)))
    (unless background
      (editor-error "The current ~A server doesn't have a compilation buffer!"
                    (if p "compile" "eval")))
    (change-to-buffer background)))

#+(or)
(defcommand "Accept Agent Connections" (p)
  "This causes Hemlock to accept agent connections and displays the port of
   the editor's connections request server.  Given an argument, this inhibits
   agent connections."
  "This causes Hemlock to accept agent connections and displays the port of
   the editor's connections request server.  Given an argument, this inhibits
   agent connections."
  (let ((accept (not p)))
    (setf *accept-connections* accept)
    (message "~:[Inhibiting~;Accepting~] connections to ~S"
             accept (get-editor-name))))



;;;; Agent initialization.

(defvar *original-beep-function* nil
  "Handle on original beep function.")

(defvar *original-gc-notify-before* nil
  "Handle on original before-GC notification function.")

(defvar *original-gc-notify-after* nil
  "Handle on original after-GC notification function.")

(defvar *original-terminal-io* nil
  "Handle on original *terminal-io* so we can restore it.")

(defvar *original-standard-input* nil
  "Handle on original *standard-input* so we can restore it.")

(defvar *original-standard-output* nil
  "Handle on original *standard-output* so we can restore it.")

(defvar *original-error-output* nil
  "Handle on original *error-output* so we can restore it.")

(defvar *original-debug-io* nil
  "Handle on original *debug-io* so we can restore it.")

(defvar *original-query-io* nil
  "Handle on original *query-io* so we can restore it.")

(defvar *original-trace-output* nil
  "Handle on original *trace-output* so we can restore it.")

(defvar *background-io* nil
  "Stream connected to the editor's compilation buffer in case we want to use it
  in the future.")

;;; CONNECT-STREAM -- internal
;;;
;;; Run in the agent to create a new stream and connect it to the supplied
;;; buffer.  Returns the stream.
;;;
(defun connect-stream (remote-buffer)
  (let ((stream (make-session-stream hemlock.wire:*current-wire* remote-buffer)))
    (hemlock.wire:remote hemlock.wire:*current-wire*
      (session-buffer-set-stream remote-buffer
                            (hemlock.wire:make-remote-object stream)))
    stream))

;;; MADE-BUFFERS-FOR-TYPESCRIPT -- Internal Interface.
;;;
;;; Run in the agent by the editor with the two buffers' info structures,
;;; actually remote-objects in the agent.  Does any necessary stream hacking.
;;; Return nil to make sure no weird objects try to go back over the wire
;;; since the editor calls this in the agent for value.  The editor does this
;;; for synch'ing, not for values.
;;;
(defvar cl-user::*io* nil)
(defun made-buffers-for-session (agent-info background-info)
  (setf *original-terminal-io* *terminal-io*)
  (macrolet ((frob (symbol new-value)
               `(setf ,(intern (concatenate 'simple-string
                                            (symbol-name '#:*original-)
                                            (subseq (string symbol) 1)))
                 ,symbol
                 ,symbol ,new-value)))
    (frob *terminal-io* (connect-stream agent-info))
    (frob *standard-input* (make-synonym-stream '*terminal-io*))
    (frob *standard-output* *standard-input*)
    (frob *error-output* *standard-input*)
    (frob *debug-io* *standard-input*)
    (frob *query-io* *standard-input*)
    (frob *trace-output* *standard-input*)
    )
  (setf *background-io* (connect-stream background-info))
  (setf cl-user::*io* *terminal-io*)
  nil)

;;; AGENT-GC-NOTIFY-BEFORE and AGENT-GC-NOTIFY-AFTER -- internal
;;;
;;; These two routines are run in the editor by the agent's gc notify routines.
;;;
(defun agent-gc-notify-before (remote-ts message)
  (let ((ts (hemlock.wire:remote-object-value remote-ts)))
    (session-buffer-output-string ts message t)
    (when (value agent-gc-alarm)
      (message "~A is GC'ing." (buffer-name (session-data-buffer ts)))
      (when (eq (value agent-gc-alarm) :loud-message)
        (beep)))))

(defun agent-gc-notify-after (remote-ts message)
  (let ((ts (hemlock.wire:remote-object-value remote-ts)))
    (session-buffer-output-string ts message t)
    (when (value agent-gc-alarm)
      (message "~A is done GC'ing." (buffer-name (session-data-buffer ts)))
      (when (eq (value agent-gc-alarm) :loud-message)
        (beep)))))

;;; EDITOR-DIED -- internal
;;;
;;; Run in the agent when the editor goes belly up.
;;;
(defun editor-died ()
  (macrolet ((frob (symbol)
               (let ((orig (intern (concatenate 'simple-string
                                                (symbol-name '#:*original-)
                                                (subseq (string symbol) 1)))))
                 `(when ,orig
                    (setf ,symbol ,orig)))))
    (frob *terminal-io*)
    (frob *standard-input*)
    (frob *standard-output*)
    (frob *error-output*)
    (frob *debug-io*)
    (frob *query-io*)
    (frob *trace-output*))
  (setf *background-io* nil)
  (format t "~2&Connection to editor died.~%"))

;;; *MASTER-MACHINE-AND-PORT* -- internal
;;;
(defvar *master-machine-and-port*)

(defun install-thread-variable-default (var fun)
  (push (cons var `(funcall ',fun)) bt:*default-special-bindings*))

(defun make-variable-thread-local (var)
  (install-thread-variable-default var (lambda () (symbol-value var))))

(defun install-special-variables-for-background-threads ()
  (install-thread-variable-default
   '*connection-backend*
   (constantly *connection-backend*))
  (install-thread-variable-default
   '*default-backend*
   (constantly *default-backend*))
  (install-thread-variable-default
   '*original-terminal-io*
   (constantly *original-terminal-io*)))

;;; START-AGENT -- internal
;;;
;;; Initiate the process by which a lisp becomes an agent.
;;;
(defun %start-agent
       (&key editor
             agent
             agent-buffer
             background-buffer
             (backend-type *default-backend*))
  (assert agent)
  (let ((*connection-backend*
         (ecase backend-type
           ((:tty :clx :webui :mini) :sb-sys)))
        (seperator (position #\: editor :test #'char=)))
    (unless seperator
      (error "Editor name ~S invalid. ~
              Must be of the form \"MachineName:PortNumber\"."
             editor))
    (install-special-variables-for-background-threads)
    (let ((machine (subseq editor 0 seperator))
          (port (parse-integer editor :start (1+ seperator)))
          (*in-hemlock-agent-p* t)
          ;; override --disable-debugger from this point on:
          (*debugger-hook*
           (lambda (c orig)
             (declare (ignore orig))
             (invoke-debugger c)))
          #+sbcl
          (sb-ext:*invoke-debugger-hook*
           (lambda (c orig)
             (declare (ignore orig))
             (invoke-debugger c))))
      (setf *master-machine-and-port* (list machine port))
      (format t "Connecting to ~A:~D~%" machine port)
      (with-new-event-loop ()
        (let ((hemlock.wire::*current-wire* :wire-not-yet-known))
          (connect-to-editor machine port agent-buffer background-buffer)
          (dispatch-events-no-hang)
          (loop until cl-user::*io*
                do (dispatch-events)
                   (write-line "Waiting for typestream buffer..."
                               *original-terminal-io*)
                   (force-output *original-terminal-io*))
          (loop (dispatch-events)))))))

(defun simple-backtrace (&optional (stream *standard-output*))
  (let ((i 0))
    (sb-debug:map-backtrace
     (lambda (frame)
       (format stream "~D: " i)
       (sb-debug::print-frame-call frame stream)
       (terpri stream)
       (incf i)))))

(defun start-agent (&rest args)
  (let ((*original-terminal-io* *terminal-io*))
    (block nil
      (handler-bind
          ((serious-condition
            (lambda (c)
              ;; The streams having changed indicates that the agent has
              ;; started up successfully.  From that point on, don't
              ;; keep it from entering the debugger.
              (when (eq *original-terminal-io* *terminal-io*)
                (format *original-terminal-io* "Error: ~A~%" c)
                (simple-backtrace *original-terminal-io*)
                (force-output *original-terminal-io*)
                (return)))))
        (apply #'%start-agent args)))))




;;; CONNECT-TO-EDITOR -- internal
;;;
;;; Do the actual connect to the editor.
;;;
(defun connect-to-editor (machine port &optional (agent nil) (background nil))
  (declare (ignore agent background))
  (connect-to-remote-server
   machine
   port
   (lambda (wire)
     (let ()
       (setf hemlock.wire::*current-wire* wire)
       (hemlock.wire:remote-value-bind wire
         (agent background)
         (set-up-buffers-for-agent (lisp-implementation-type)
                                   (lisp-implementation-version))
         (made-buffers-for-session agent background))))
   'editor-died))

(defun set-up-buffers-for-agent
    (type version &optional (wire hemlock.wire:*current-wire*))
  (let* ((server-info (variable-value 'current-eval-server :global))
         (agent-info (server-info-agent-info server-info))
         (background-info (server-info-background-info server-info)))
    (setf (server-info-wire server-info) wire)
    (session-buffer-wire-connected agent-info wire)
    (session-buffer-wire-connected background-info wire)
    (setf (server-info-implementation-type server-info) type)
    (setf (server-info-implementation-version server-info) version)
    (let* ((buf (session-data-buffer agent-info))
           (name (format nil "~A ~A" (buffer-name buf) type)))
      (maybe-rename-buffer buf name))
    (values (hemlock.wire:make-remote-object agent-info)
            (hemlock.wire:make-remote-object background-info))))

;;; CONNECT-TO-EDITOR-FOR-BACKGROUND-THREAD -- internal
;;;
;;; Do the actual connect to the editor.
;;;
(defun connect-to-editor-for-background-thread (machine port)
  (connect-to-remote-server
   machine
   port
   (lambda (wire)
     (setf hemlock.wire::*current-wire* wire))
   'editor-died))



;;;; Eval server evaluation functions.

(defvar *eval-form-stream*
  (make-two-way-stream
   (make-concatenated-stream)
   (make-broadcast-stream)))

;;; SERVER-EVAL-FORM -- Public.
;;;   Evaluates the given form (which is a string to be read from in the given
;;; package) and returns the results as a list.
;;;
(defun server-eval-form (package form)
  (declare (type (or string null) package) (simple-string form))
  (handler-bind
      ((error #'(lambda (condition)
                  (hemlock.wire:remote hemlock.wire:*current-wire*
                               (eval-form-error (format nil "~A~&" condition)))
                  (return-from server-eval-form nil))))
    (let ((*package* (if package
                         (or (find-package package)
                             (error "no such package: ~A" package))
                         *package*))
          (*terminal-io* *eval-form-stream*))
      (stringify-list (multiple-value-list (eval (read-from-string form)))))))


;;; DO-OPERATION -- Internal.
;;;   Checks to see if we are aborting operations. If not, do the operation
;;; wrapping it with operation-started and operation-completed calls. Also
;;; deals with setting up *terminal-io* and *package*.
;;;
(defmacro do-operation ((note package terminal-io) &body body)
  `(let ((aborted t)
         (*terminal-io* (if ,terminal-io
                          (hemlock.wire:remote-object-value ,terminal-io)
                          *terminal-io*))
         (*package* (maybe-make-package ,package)))
     (unwind-protect
         (unless *abort-operations*
           (when (eq :was-in-debugger
                     (catch 'abort-operation
                       (let ((*inside-operation* t))
                         (hemlock.wire:remote hemlock.wire:*current-wire*
                                      (operation-started ,note))
                         (hemlock.wire:wire-force-output hemlock.wire:*current-wire*)
                         ,@body
                         (setf aborted nil))))
             (format t
                     "~&[Operation aborted.  ~
                      You are no longer in this instance of the debugger.]~%")))
       (hemlock.wire:remote hemlock.wire:*current-wire*
         (operation-completed ,note aborted))
       (hemlock.wire:wire-force-output hemlock.wire:*current-wire*))))


;;; unique-thingie is a unique eof-value for READ'ing.  Its a parameter, so
;;; we can reload the file.
;;;
(defparameter unique-thingie (gensym)
  "Used as eof-value in reads to check for the end of a file.")

;;; SERVER-EVAL-TEXT -- Public.
;;;
;;;   Evaluate all the forms read from text in the given package, and send the
;;; results back.  The error handler bound does not handle any errors.  It
;;; simply notifies the client that an error occurred and then returns.
;;;
(defun server-eval-text (note package text terminal-io)
  (do-operation (note package terminal-io)
    (with-input-from-string (stream text)
      (let ((last-pos 0))
        (handler-bind
            ((error
              #'(lambda (condition)
                  (hemlock.wire:remote hemlock.wire:*current-wire*
                               (lisp-error note last-pos
                                           (file-position stream)
                                           (format nil "~A~&" condition))))))
          (loop
            (let ((form (read stream nil unique-thingie)))
              (when (eq form unique-thingie)
                (return nil))
              (let* ((values (stringify-list (multiple-value-list (eval form))))
                     (pos (file-position stream)))
                (hemlock.wire:remote hemlock.wire:*current-wire*
                  (eval-text-result note last-pos pos values))
                (setf last-pos pos)))))))))

(defun stringify-list (list)
  (mapcar #'prin1-to-string list))
#|
(defun stringify-list (list)
  (mapcar #'(lambda (thing)
              (with-output-to-string (stream)
                (write thing
                       :stream stream :radix nil :base 10 :circle t
                       :pretty nil :level nil :length nil :case :upcase
                       :array t :gensym t)))
          list))
|#


;;;; Eval server compilation stuff.

;;; DO-COMPILER-OPERATION -- Internal.
;;;
;;; Useful macro that does the operation with *compiler-note* and
;;; *compiler-wire* bound.
;;;
(defmacro do-compiler-operation ((note package terminal-io error) &body body)
  `(let ((*compiler-note* ,note)
         (*compiler-error-stream* ,error)
         (*compiler-wire* hemlock.wire:*current-wire*)
         #+nil
         (c:*compiler-notification-function* #'compiler-note-in-editor))
     (do-operation (*compiler-note* ,package ,terminal-io)
                   (unwind-protect
                       (handler-bind ((error #'compiler-error-handler))
                         ,@body)
                     (when *compiler-error-stream*
                       (force-output *compiler-error-stream*))))))

;;; COMPILER-NOTE-IN-EDITOR -- Internal.
;;;
;;; DO-COMPILER-OPERATION binds c:*compiler-notification-function* to this, so
;;; interesting observations in the compilation can be propagated back to the
;;; editor.  If there is a notification point defined, we send information
;;; about the position and kind of error.  The actual error text is written out
;;; using session operations.
;;;
;;; Start and End are the compiler's best guess at the file position where the
;;; error occurred.  Function is some string describing where the error was.
;;;
(defun compiler-note-in-editor (severity function name pos)
  (declare (ignore name))
  (when *compiler-wire*
    (force-output *compiler-error-stream*)
    (hemlock.wire:remote *compiler-wire*
      (compiler-error *compiler-note* pos pos function severity)))
    (hemlock.wire:wire-force-output *compiler-wire*))


;;; COMPILER-ERROR-HANDLER -- Internal.
;;;
;;;    The error handler function for the compiler interfaces.
;;; DO-COMPILER-OPERATION binds this as an error handler while evaluating the
;;; compilation form.
;;;
(defun compiler-error-handler (condition)
  (when *compiler-wire*
    (hemlock.wire:remote *compiler-wire*
      (lisp-error *compiler-note* nil nil
                  (format nil "~A~&" condition)))))


;;; SERVER-COMPILE-TEXT -- Public.
;;;
;;;    Similar to server-eval-text, except that the stuff is compiled.
;;;
(defmacro with-temporary-file-name ((var) &body body)
  `(invoke-with-temporary-file-name (lambda (,var) ,@body)))

(defun invoke-with-temporary-file-name (fun)
  (multiple-value-bind (fd pathname)
                       (sb-posix:mkstemp "/tmp/hemlockXXXXXX")
    (sb-posix:close fd)
    (funcall fun pathname)
    (when (probe-file pathname)
      (sb-posix:unlink pathname))))

(defun server-compile-text (note package text defined-from
                            terminal-io error-output)
  (declare (ignore defined-from))
  (let ((error-output (if error-output
                        (hemlock.wire:remote-object-value error-output))))
    (do-compiler-operation (note package terminal-io error-output)
      (with-temporary-file-name (tmp)
        (with-open-file (s tmp :direction :output :if-exists :supersede)
          (write-string text s))
        (terpri error-output)
        (load (compile-file tmp))))))


;;; SERVER-COMPILE-FILE -- Public.
;;;
;;;    Compiles the file sending error info back to the editor.
;;;
(defun server-compile-file (note package input output error trace
                            load terminal background-stream)
  (declare (ignore output error trace load))
  (macrolet ((frob (x)
               `(if (hemlock.wire:remote-object-p ,x)
                  (hemlock.wire:remote-object-value ,x)
                  ,x)))
    (let ((error-stream (frob background-stream)))
      (do-compiler-operation (note package terminal error-stream)
        (multiple-value-bind (fasl warning-free-p)
            (compile-file (frob input))
          (when fasl
            (load fasl))
          (format nil "~A ~A" fasl warning-free-p))))))


;;;; Other random eval server stuff.

;;; MAYBE-MAKE-PACKAGE -- Internal.
;;;
;;; Returns a package for a name.  Creates it if it doesn't already exist.
;;;
(defun maybe-make-package (name)
  (cond ((null name) *package*)
        ((find-package name))
        (t
         (hemlock.wire:remote-value (session-stream-wire *terminal-io*)
           (session-buffer-output-string
            (session-stream-session *terminal-io*)
            (format nil "~&Creating package ~A.~%" name)
            t))
         (make-package name))))

;;; SERVER-SET-PACKAGE -- Public.
;;;
;;;   Serves package setting requests.  It simply sets
;;; *package* to an already existing package or newly created one.
;;;
(defun server-set-package (package)
  (setf *package* (maybe-make-package package)))

;;; SERVER-ACCEPT-OPERATIONS -- Public.
;;;
;;;   Start accepting operations again.
;;;
(defun server-accept-operations ()
  (setf *abort-operations* nil))



;;;; Command line switches.



;;;;

(defcommand "Start Swank Server"
    (p &optional (port (prompt :integer
                              :prompt "Port: "
                              :default-string "4005")))
  "" ""
  (declare (ignore p))
  (asdf:operate 'asdf:load-op :swank)
  (eval (read-from-string (format nil "(swank:create-server :port ~D)" port))))


;;;;

(defun make-extra-session-buffer
    (name &optional (server-info (get-current-eval-server t))
                    (wire (server-info-wire server-info)))
  (let ((buffer
         (make-buffer-with-unique-name name :modes '("Lisp"))))
    (sessionify-buffer buffer server-info wire)
    buffer))

(defun wire-to-server-info (&optional (wire hemlock.wire:*current-wire*)
                                      (errorp t)
                                      (error-value nil))
  (or (find wire (list-server-infos) :key #'server-info-wire)
      (if errorp
          (error "no server info for wire: ~A" wire)
          error-value)))

(defun %make-extra-session-buffer (name)
  (let* ((wire hemlock.wire:*current-wire*)
         (info ;; hmm, do we need the server info?
          :server-info-for-extra-buffer-not-set)
         (buffer (make-extra-session-buffer
                  name
                  (wire-to-server-info wire nil info)
                  wire))
         (session-data (variable-value 'session-data :buffer buffer)))
    (change-to-buffer buffer)
    (hemlock.wire:make-remote-object session-data)))


;;;; Wire-readiness predicate.

(defun eval-server-ready-p (info)
  "Return true when INFO is a server-info with an established connection.
   Accepts both wire-backed (process agents) and :local (thread agents)."
  (and info
       (let ((wire (server-info-wire info)))
         (or (eq wire :local)
             (hemlock.wire:wire-p wire)))))

;; Alias for prepl.lisp which calls this from eval threads
(setf (fdefinition 'make-extra-repl-buffer-impl)
      #'%make-extra-session-buffer)


;;;; Auto-start agent on editor entry.

(add-hook entry-hook
  (lambda ()
    (ignore-errors (create-local-eval))))
