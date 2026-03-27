;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :cl-user)

;; Note: I want real relative package names like the Symbolics has
;; them. In the mean time:



;;;
;;; hemlock.text — core data model: types, structs, primitives, text operations
;;; No dependencies beyond Common Lisp and trivial-gray-streams.
;;;
(defpackage :hemlock.text
  (:use :common-lisp :trivial-gray-streams)
  (:shadow #:char-code-limit)
  (:export
   ;; basic list utilities (struct.lisp) — used throughout all layers
   #:delq #:memq #:assq
   ;; string primitives (struct.lisp / char-attrs.lisp)
   #:%sp-byte-blt
   #:%sp-find-character-with-attribute
   #:%sp-reverse-find-character-with-attribute

   ;; line.lisp / text-primitives.lisp
   #:neq
   #:byte-blt
   #:close-line
   ;; open-line gap buffer variables (text-primitives.lisp)
   #:open-line
   #:open-chars
   #:left-open-pos
   #:right-open-pos
   #:line-cache-length
   #:modifying-buffer
   #:search-hash-code
   #:map-string-table
   #:tick
   #:find-character-with-attribute
   #:reverse-find-character-with-attribute
   #:internal-make-font-mark
   #:character-attribute
   #:print-attribute-descriptor
   #:print-hcommand
   #:print-hwindow
   #:%print-hwindow
   #:print-modeline-field
   #:print-modeline-field-info
   #:strlen
   ;; internal temp vars (text-delete.lisp)
   #:*internal-temp-region*
   #:*internal-temp-mark*
   ;; fast compilation flag and declfun macro (decls.lisp)
   #:*fast*
   #:declfun
   ;; device-hunk class and accessors (struct.lisp)
   #:device-hunk
   ;; value-node struct (table.lisp)
   #:value-node
   #:value-node-proper
   #:value-node-folded
   #:value-node-value
   #:make-value-node
   ;; tty-device class and accessors (defined in struct.lisp, must be exported
   ;; from hemlock.text because the class is defined here — hemlock.tty USEs this)
   #:tty-device #:%make-tty-device
   #:tty-device-dumbp #:tty-device-lines #:tty-device-columns
   #:tty-device-display-string #:tty-device-standout-init #:tty-device-standout-end
   #:tty-device-clear-lines #:tty-device-clear-to-eol #:tty-device-clear-to-eow
   #:tty-device-open-line #:tty-device-delete-line #:tty-device-insert-string
   #:tty-device-delete-char #:tty-device-cursor-x #:tty-device-cursor-y
   #:tty-device-standout-init-string #:tty-device-standout-end-string
   #:tty-device-clear-to-eol-string #:tty-device-clear-string
   #:tty-device-open-line-string #:tty-device-delete-line-string
   #:tty-device-insert-init-string #:tty-device-insert-char-init-string
   #:tty-device-insert-char-end-string #:tty-device-insert-end-string
   #:tty-device-delete-init-string #:tty-device-delete-char-string
   #:tty-device-delete-end-string #:tty-device-init-string
   #:tty-device-screen-image #:tty-device-speed
   #:tty-device-cm-string #:tty-device-cm-one-origin #:tty-device-cm-reversep
   #:tty-device-cm-x-pad #:tty-device-cm-y-pad #:tty-device-cm-end-string
   #:tty-device-cm-x-add-char #:tty-device-cm-y-add-char
   #:tty-device-cm-x-condx-char #:tty-device-cm-y-condx-char
   #:tty-device-cm-x-condx-add-char #:tty-device-cm-y-condx-add-char
   ;; string-table internals (table.lisp)
   #:string-table-num-nodes
   #:string-table-value-nodes
   #:linep
   #:make-line
   #:line-string
   #:line-previous
   #:line-next
   #:line-buffer
   #:line-length
   #:line-character
   #:line-plist
   #:line-signature
   ;; line internals used across packages
   #:line-%buffer
   #:%line-tag
   #:line-chars
   #:line-marks
   #:line-number
   #:line-tag-direct
   #:copy-line
   #:line-length*
   ;; syntax-info struct (sy- prefix)
   #:make-syntax-info
   #:sy-signature
   #:sy-from-state
   #:sy-to-state
   #:sy-font-marks
   ;; tag struct
   #:make-tag
   #:tag-ticks
   #:tag-line-number
   #:tag-syntax-info
   #:tag-package
   #:markp
   #:mark-line
   #:mark-charpos
   #:mark-kind
   #:mark
   #:previous-character
   #:next-character
   #:copy-mark
   #:delete-mark
   #:move-to-position
   #:move-mark
   #:region
   #:regionp
   #:make-empty-region
   #:region-start
   #:region-end
   #:region-bounds
   #:set-region-bounds
   #:start-line-p
   #:end-line-p
   #:empty-line-p
   #:blank-line-p
   #:blank-before-p
   #:blank-after-p
   #:same-line-p
   #:mark<
   #:mark<=
   #:mark=
   #:mark/=
   #:mark>=
   #:mark>
   #:line<
   #:line<=
   #:line>=
   #:line>
   #:lines-related
   #:first-line-p
   #:last-line-p
   #:buffer-signature

   ;; text-ops.lisp
   #:region-to-string
   #:string-to-region
   #:line-to-region
   #:line-start
   #:line-end
   #:buffer-start
   #:buffer-end
   #:mark-before
   #:mark-after
   #:character-offset
   #:line-offset
   #:count-lines
   #:count-characters
   #:*print-region*

   ;; text-insert.lisp
   #:insert-character
   #:insert-string
   #:insert-region
   #:ninsert-region

   ;; text-delete.lisp
   #:delete-characters
   #:delete-region
   #:delete-and-save-region
   #:copy-region
   #:filter-region

   ;; search1.lisp / search2.lisp
   #:new-search-pattern
   #:search-pattern-p
   #:get-search-pattern
   #:find-pattern
   #:replace-pattern
   #:search-pattern

   ;; ring.lisp
   #:make-ring
   #:ringp
   #:ring-length
   #:ring-ref
   #:ring-push
   #:ring-pop
   #:rotate-ring

   ;; table.lisp
   #:make-string-table
   #:string-table-p
   #:string-table-separator
   #:delete-string
   #:clrstring
   #:getstring
   #:complete-string
   #:find-ambiguous
   #:find-containing

   ;; buffer (buffer operations at text level)
   #:buffer                             ; type name
   #:current-buffer
   #:current-point
   #:bufferp
   #:buffer-name
   #:buffer-pathname
   #:buffer-write-date
   #:buffer-point
   #:buffer-mark
   #:buffer-writable
   #:buffer-modified
   #:buffer-variables
   #:buffer-modes
   #:buffer-windows
   #:buffer-delete-hook
   #:buffer-undo-p
   #:buffer-start-mark
   #:buffer-end-mark
   #:make-buffer
   #:delete-buffer
   #:delete-buffer-if-possible
   #:buffer-region

   ;; struct.lisp — mark/region/buffer/window struct types and display
   ;; buffer struct internals
   #:internal-make-buffer
   #:buffer-region
   #:buffer-mode-objects
   #:buffer-bindings
   #:buffer-modified-tick
   #:buffer-unmodified-tick
   #:buffer-var-values
   #:buffer-display-start
   #:buffer-modeline-fields-list
   #:buffer-tag-line-number
   #:buffer-undo-list
   ;; internal %-prefixed buffer slot accessors
   #:buffer-%name
   #:buffer-%region
   #:buffer-%pathname
   #:buffer-%writable
   #:buffer-%modeline-fields
   #:buffer-active-region-p
   #:buffer-variables
   #:buffer-windows
   #:buffer-write-date
   #:buffer-delete-hook
   #:buffer-widget
   #:buffer-undo-p
   #:virtual-buffer-p
   ;; region struct internals
   #:internal-make-region
   #:line
   #:ring
   #:string-table
   #:font-mark
   #:font-mark-p
   #:fast-font-mark-p
   #:font-mark-font
   #:delete-font-mark
   #:delete-line-font-marks
   #:move-font-mark
   #:window-font
   ;; modeline-field struct
   #:modeline-field                     ; type name
   #:alloc-modeline-field
   #:%make-modeline-field
   #:modeline-field-p
   #:modeline-field-%name
   #:modeline-field-%function
   #:modeline-field-%width
   #:modeline-field-name
   #:modeline-field-function
   #:modeline-field-width
   ;; modeline-field-info struct
   #:make-ml-field-info
   #:ml-field-info-field
   #:ml-field-info-start
   #:ml-field-info-end
   ;; mode-object struct
   #:make-mode-object
   #:modep
   #:mode-object-name
   #:mode-object-setup-function
   #:mode-object-cleanup-function
   #:mode-object-bindings
   #:mode-object-transparent-p
   #:mode-object-hook-name
   #:mode-object-major-p
   #:mode-object-precedence
   #:mode-object-character-attributes
   #:mode-object-variables
   #:mode-object-var-values
   #:mode-object-documentation
   ;; variable-object struct
   #:make-variable-object
   #:variable-object-documentation
   #:variable-object-name
   #:variable-object-value
   #:variable-object-hooks
   #:variable-object-down
   ;; option-object struct
   #:make-option-object
   #:option-object-value
   #:option-object-hooks
   #:option-object-down
   #:option-object-documentation
   #:option-object-name
   ;; window struct
   #:window
   #:windowp
   #:internal-make-window
   #:window-tick
   #:window-buffer
   #:window-%buffer                     ; port's internal slot accessor
   #:window-height
   #:window-width
   #:window-old-start
   #:window-first-line
   #:window-last-line
   #:window-first-changed
   #:window-last-changed
   #:window-spare-lines
   #:window-old-lines
   #:window-hunk
   #:window-display-start
   #:window-display-end
   #:window-point
   #:window-modeline-dis-line           ; port's name (vs modernize's window-modeline-display-line)
   #:window-modeline-buffer
   #:window-modeline-buffer-len
   #:window-display-recentering
   ;; attribute-descriptor struct
   #:make-attribute-descriptor
   #:attribute-descriptor-name
   #:attribute-descriptor-keyword
   #:attribute-descriptor-documentation
   #:attribute-descriptor-char-set
   #:attribute-descriptor-hooks
   #:attribute-descriptor-end-value
   ;; command struct
   #:command                            ; type name
   #:commandp
   #:internal-make-command
   #:command-name
   #:command-documentation
   #:command-function
   #:command-bindings
   #:command-%name
   #:command-%bindings
   ;; random-typeout-stream class
   #:random-typeout-stream
   #:make-random-typeout-stream
   #:random-typeout-stream-mark
   #:random-typeout-stream-window
   #:random-typeout-stream-line-buffered-p
   #:random-typeout-stream-no-prompt
   #:random-typeout-stream-first-more-p
   #:random-typeout-stream-more-mark
   ;; device class and generics
   #:device
   #:device-name
   #:device-init
   #:device-make-window
   #:device-exit
   #:device-smart-redisplay
   #:device-dumb-redisplay
   #:device-after-redisplay
   #:device-clear
   #:device-note-read-wait
   #:device-force-output
   #:device-finish-output
   #:device-hide-cursor
   #:device-show-cursor
   #:device-put-cursor
   #:device-show-mark
   #:device-next-window
   #:device-previous-window
   #:device-delete-window
   #:device-random-typeout-full-more
   #:device-random-typeout-line-more
   #:device-random-typeout-setup
   #:device-random-typeout-cleanup
   #:device-beep
   ;; device class slot accessors (struct.lisp)
   #:device-bottom-window-base
   #:device-hunks
   ;; device-hunk class slot accessors (struct.lisp)
   #:device-hunk-device
   #:device-hunk-height
   #:device-hunk-next
   #:device-hunk-position
   #:device-hunk-previous
   #:device-hunk-window
   ;; bitmap-hunk CLX accessors
   #:bitmap-hunk-font-family
   #:bitmap-hunk-trashed
   #:bitmap-hunk-modeline-dis-line
   #:bitmap-hunk-modeline-pos
   #:bitmap-hunk-window
   #:bitmap-hunk-window-group
   #:bitmap-hunk-xwindow
   #:bitmap-device-display
   ;; font-family struct
   #:font-family
   #:make-font-family
   #:copy-font-family
   #:font-family-map
   #:font-family-height
   #:font-family-width
   #:font-family-baseline
   #:font-family-cursor-width
   #:font-family-cursor-height
   #:font-family-cursor-x-offset
   #:font-family-cursor-y-offset
   ;; font-change struct
   #:make-font-change
   #:font-change-x
   #:font-change-font
   #:font-change-next
   #:font-change-mark
   ;; dis-line struct (port uses dis-line- prefix, not display-line-)
   #:dis-line
   #:window-dis-line
   #:make-window-dis-line
   #:dis-line-chars
   #:dis-line-delta
   #:dis-line-flags
   #:dis-line-font-changes
   #:dis-line-length
   #:dis-line-position
   #:dis-line-old-chars
   #:dis-line-line
   #:dis-line-end
   #:dis-line-tick
   #:dis-line-tag
   #:dis-line-tag-ticks
   ;; char-attrs.lisp
   #:comment
   #:char-code-limit
   #:character-set
   #:make-character-set
   #:character-set-p
   #:character-set-page0
   #:character-set-table
   #:character-set-default
   #:char-set-ref
   #:do-alpha-chars

   ;; decls.lisp — error and mark utilities needed by text-layer files
   #:editor-error
   #:editor-error-format-string
   #:editor-error-format-arguments
   #:with-mark
   #:parse-forms
   #:*buffer-modified-notifier*
   #:*ts-text-change-hook*

   ;; key-event.lisp — key event type and accessors
   #:key-event
   #:key-event-p
   #:key-event-bits
   #:key-event-keysym
   #:char-key-event
   #:key-event-char
   #:key-event-bit-p
   #:do-alpha-key-events
   #:make-key-event
   #:make-key-event-bits
   #:key-event-modifier-mask
   #:key-event-bits-modifiers
   #:define-key-event-modifier
   #:*all-modifier-names*
   #:*modifier-translations*
   #:*modifiers-to-internal-masks*
   #:define-keysym
   #:define-mouse-keysym
   #:name-keysym
   #:keysym-names
   #:keysym-preferred-name
   #:translate-mouse-key-event
   #:print-pretty-key
   #:print-pretty-key-event

   ;; lispdep.lisp — SBCL portability primitives
   #:getenv
   #:without-interrupts
   #:fixnump
   #:file-writable
   ))


;;;
;;; hemlock.command — editor framework: commands system, modes, keys,
;;;                   display, windows, streams
;;;
(defpackage :hemlock.command
  (:use :common-lisp :trivial-gray-streams :hemlock.text)
  (:shadowing-import-from :hemlock.text #:char-code-limit)
  (:import-from :hemlock.wire #:dispatch-events #:dispatch-events-no-hang)
  (:import-from :act  #:! #:*self* #:*state*)
  (:import-from :ac   #:actor-of #:shutdown)
  (:import-from :asys #:make-actor-system)
  (:export
   #:dispatch-events #:dispatch-events-no-hang #:dispatch-events-timeout

   #:fset-state #:fset-snapshot #:drop-fset-state

   #:current-mark
   #:pop-buffer-mark
   #:push-buffer-mark
   #:change-to-buffer
   #:previous-buffer
   #:buffer-active-region-p

   #:make-modeline-field
   #:buffer-modeline-fields
   #:buffer-modeline-field-p
   #:update-modeline-fields
   #:update-modeline-field

   #:fetch-cut-string
   #:store-cut-string
   #:check-region-query-size

   #:kill-region
   #:kill-characters
   #:activate-region
   #:deactivate-region
   #:region-active-p
   #:check-region-active
   #:current-region

   #:current-variable-tables
   #:defhvar
   #:variable-value
   #:variable-documentation
   #:variable-hooks
   #:variable-name
   #:string-to-variable
   #:hemlock-bound-p
   #:delete-variable

   #:make-command
   #:bind-key
   #:delete-key-binding
   #:get-command
   #:map-bindings
   #:key-translation
   #:interactive
   #:last-command-type
   #:prefix-argument
   #:recursive-edit
   #:in-recursive-edit
   #:exit-recursive-edit
   #:abort-recursive-edit

   #:defmode
   #:mode-documentation
   #:buffer-major-mode
   #:buffer-minor-mode
   #:mode-variables
   #:mode-major-p

   ;; syntax.lisp — character attribute system
   #:defattribute
   #:character-attribute-name
   #:character-attribute-documentation
   #:character-attribute-p
   #:shadow-attribute
   #:unshadow-attribute
   #:find-attribute
   #:reverse-find-attribute
   #:character-attribute-hooks

   #:current-window
   #:make-window
   #:delete-window
   #:window-display-start
   #:window-display-end
   #:window-display-recentering
   #:center-window
   #:scroll-window
   #:displayed-p
   #:next-window
   #:previous-window
   #:mark-to-cursorpos
   #:cursorpos-to-mark
   #:last-key-event-cursorpos
   #:mark-column
   #:move-to-column
   #:show-mark
   #:redisplay
   #:redisplay-all
   #:editor-finish-output

   #:define-logical-key-event
   #:logical-key-event-key-events
   #:logical-key-event-p

   #:clear-echo-area
   #:message
   #:loud-message
   ;; prompt system
   #:prompt
   #:prompt-state #:make-prompt-state #:prompt-state-p
   #:*current-prompt*
   #:prompt-type #:prompt-tables #:prompt-verify #:prompt-must-exist
   #:prompt-default #:prompt-default-string #:prompt-text #:prompt-help
   #:prompt-symbol-package
   ;; legacy prompt functions
   #:prompt-for-buffer
   #:prompt-for-key-event
   #:prompt-for-key
   #:prompt-for-file
   #:prompt-for-integer
   #:prompt-for-keyword
   #:prompt-for-expression
   #:prompt-for-string
   #:prompt-for-symbol
   #:prompt-for-variable
   #:prompt-for-y-or-n
   #:prompt-for-yes-or-no

   #:process-file-options
   #:pathname-to-buffer-name
   #:buffer-default-pathname
   #:read-file
   #:write-file
   #:write-buffer-file
   #:read-buffer-file
   #:find-file-buffer

   #:get-key-event
   #:unget-key-event
   #:clear-editor-input
   #:listen-editor-input

   #:make-hemlock-output-stream
   #:hemlock-output-stream-p
   #:make-hemlock-region-stream
   #:hemlock-region-stream-p
   #:make-kbdmac-stream
   #:modify-kbdmac-stream

   #:add-definition-dir-translation
   #:delete-definition-dir-translation
   #:schedule-event
   #:remove-scheduled-event
   #:in-lisp

   #:indent-region
   #:indent-region-for-commands
   #:delete-horizontal-space
   #:pre-command-parse-check
   #:form-offset
   #:top-level-offset
   #:mark-top-level-form
   #:defun-region
   #:inside-defun-p
   #:start-defun-p
   #:forward-up-list
   #:backward-up-list
   #:valid-spot
   #:defindent
   #:word-offset
   #:sentence-offset
   #:paragraph-offset
   #:mark-paragraph
   #:goto-page
   #:page-offset
   #:page-directory
   #:display-page-directory
   #:fill-region
   #:fill-region-by-paragraphs

   #:save-for-undo
   #:make-region-undo
   #:supply-generic-pointer-up-function

   ;; Macros from the CIM:
   #:with-writable-buffer
   #:value
   #:setv
   #:add-hook
   #:remove-hook
   #:invoke-hook
   #:*inhibit-hooks*
   #:defcommand
   #:use-buffer
   #:command-case
   #:define-file-option
   #:define-file-type-hook
   #:do-active-group
   #:with-input-from-region
   #:with-output-to-mark
   #:with-pop-up-display
   #:handle-lisp-errors
   #:do-strings

   ;; Later, possibly ill-advised additions
   #:goto-buffer-start
   #:goto-buffer-end

   ;; device interface
   #:device                             ;[class]
   #:device-init
   #:device-make-window
   #:device-exit
   #:device-smart-redisplay
   #:device-dumb-redisplay
   #:device-after-redisplay
   #:device-clear
   #:device-note-read-wait
   #:device-force-output
   #:device-finish-output
   #:device-hide-cursor
   #:device-show-cursor
   #:device-put-cursor
   #:device-show-mark
   #:device-next-window
   #:device-previous-window
   #:device-delete-window
   #:device-random-typeout-full-more
   #:device-random-typeout-line-more
   #:device-random-typeout-setup
   #:device-random-typeout-cleanup
   #:device-beep
   ;;
   #:random-typeout-stream
   #:with-mark

   ;; variables
   #:*in-the-editor*
   #:*current-buffer*
   #:*current-window*
   #:*echo-area-buffer*
   #:*random-typeout-buffers*
   #:*random-typeout-ml-fields*
   #:*window-list*

   ;; functions
   #:hlet
   #:random-typeout-stream-mark
   #:default-font
   #:window                             ;as a type

   #:editor-input
   #:*editor-input*
   #:*real-editor-input*

   #:list-all-connections
   #:connection
   #:connection-name
   #:connection-buffer
   #:connection-sentinel
   #:connection-filter
   #:connection-encoding
   #:delete-connection
   #:connection-listen
   #:connection-write
   #:tcp-connection
   #:make-tcp-connection
   #:process-connection
   #:connection-command
   #:make-process-connection
   #:make-pipelike-connection
   #:make-process-with-pty-connection
   #:find-a-pty
   #:connection-exit-status
   #:connection-exit-code
   #:file-connection
   #:connection-port
   #:connection-host
   #:make-file-connection
   #:conection-filename
   #:descriptor-connection
   #:connection-descriptor
   #:make-descriptor-connection
   #:io-connection
   #:tcp-connection-mixin
   #:process-connection-mixin
   #:tcp-listener-mixin
   #:connection-initargs
   #:listening-connection
   #:make-tcp-listener
   #:make-connection-device
   #:connection-note-event
   #:filter-connection-output
   #:note-connected
   #:process-incoming-data
   #:connection-input-buffer
   #:%read
   #:process-incoming-connection
   #:convert-pending-connection

   ;; additional public API
   #:directoryp
   #:reprompt
   #:exit-hemlock
   #:pause-hemlock
   #:merge-relative-pathnames
   #:*beep-function* #:beep
   #:*last-key-event-typed* #:*key-event-history*
   #:*global-variable-names* #:*character-attribute-names*
   #:*mode-names* #:*buffer-names* #:*command-names* #:*buffer-list*
   #:*default-modeline-fields*
   #:maximum-modeline-pathname-length-hook
   #:site-init
   #:%init-syntax-table
   #:%init-line-image
   ;; platform.lisp — entry-point functions used from main.lisp (hemlock pkg)
   ;; display constants (winimage.lisp) — re-exported through hemlock.display to hemlock.tty
   #:unaltered-bits #:changed-bit #:moved-bit #:new-bit #:the-sentinel
   #:site-wrapper-macro
   #:init-raw-io
   #:%init-screen-manager
   #:backend-init-raw-io
   #:*illegal-read-stream*
   #:*default-backend*
   #:*available-backends*
   #:*editor-has-been-entered*
   #:validate-backend-type
   #:choose-backend-type
   #:get-terminal-name
   #:*editor-file-descriptor*
   #:process-editor-tty-input
   ;; macros.lisp — used from main.lisp
   #:*connection-backend*
   #:with-existing-event-loop
   #:with-new-event-loop
   #:invoke-with-new-event-loop
   #:invoke-with-existing-event-loop
   #:make-event-loop
   #:dispatch-events-with-backend
   #:dispatch-events-no-hang-with-backend
   #:dispatch-events-with-timeout-backend
   #:invoke-later
   #:later
   #:lisp-error-error-handler
   ;; catch/throw tags shared across hemlock and hemlock.command
   #:command-loop-catcher
   #:editor-top-level-catcher
   #:hemlock-exit
   ;; key-dispatch.lisp
   #:*invoke-hook*
   #:%command-loop
   ;; streams.lisp — internal state for lispbuf input
   #:*reading-lispbuf-input*
   #:lispbuf-input
   ;; window.lisp — used from main.lisp
   #:%init-redisplay
   ;; echo.lisp
   #:display-prompt-nicely
   #:key-event-case
   #:after-editor-initializations
   #:line-tag
   #:default-filter
   #:queue-buffer-change
   #:make-buffer-with-unique-name
   #:*prompt-key*
   #:update-modelines-for-buffer
   #:*debug-on-error*
   #:*editor-input*
   #:stream-fd
   #:*event-base*
   #:make-input-event
   #:class-for
   #:pipelike-connection-mixin
   #:process-with-pty-connection-mixin
   #:device-enlarge-window
   #:*background-image*

   ;; tree-sitter
   #:ts-stable-tree-for-buffer

   ;; parse/echo variables
   #:*echo-area-stream*
   #:*echo-area-window*
   #:*parse-starting-mark*
   #:*parse-input-region*
   #:*parse-verification-function*
   #:*parse-string-tables*
   #:*parse-value-must-exist*
   #:*parse-default*
   #:*parse-default-string*
   #:*parse-prompt*
   #:*parse-help*
   #:*parse-type*
   #:*parse-symbol-package*

   ;; editor-input internals (input.lisp) — used by tty backend
   #:*real-editor-input*
   #:*free-input-events*
   #:*screen-image-trashed*
   #:*editor-windowed-input*
   #:%editor-input-method
   #:q-event #:un-event #:dq-event
   #:editor-input-head #:editor-input-tail
   #:input-event-next #:input-event-key-event
   #:input-event-x #:input-event-y #:input-event-hunk #:input-event-unread-p
   #:new-event #:editor-abort-key-events #:abort-key-event-p
   #:more-read-key-event
   #:*in-hemlock-stream-input-method*

   ;; window image internals
   #:setup-modeline-image
   #:canonical-case
   #:setup-window-image
   #:*line-wrap-char*
   #:change-window-image-height
   #:internal-redisplay
   #:prepare-window-for-redisplay

   ;; display functions forward-declared
   #:random-typeout-redisplay
   #:update-window-image
   #:maybe-recenter-window
   #:wait-for-more

   ;; misc
   #:input-waiting
   #:concat
   #:setup-initial-buffer
   ;; eval-server.lisp — used by lispeval.lisp
   #:eval-server-ready-p

   ;; backend-facing internals (used by tty, term, webui backends)
   #:agent-fd
   #:connection-environment
   #:*free-font-changes*
   #:alloc-font-change

   ;; hemlock-ext.lisp — framework operations (formerly in hemlock-ext package)
   #:skip-whitespace
   #:quit
   #:default-directory
   #:find-buffer
   #:maybe-rename-buffer
   #:rename-buffer-uniquely
   #:with-clx-event-handling
   #:complete-file
   #:complete-file-directory
   #:ambiguous-files
   #:set-file-permissions

   ;; ui node tree (src/ui/)
   #:ui-node #:ui-text #:ui-field #:ui-vstack #:ui-hstack #:ui-box
   #:ui-selectable #:ui-action #:ui-list #:ui-separator #:ui-tree
   #:make-ui-text #:make-ui-field #:make-ui-vstack #:make-ui-hstack
   #:make-ui-box #:make-ui-selectable #:make-ui-action #:make-ui-list
   #:make-ui-separator #:make-ui-tree
   #:ui-tree-root #:ui-tree-buffer #:ui-tree-state #:ui-tree-width
   #:ui-selectable-data #:ui-selectable-selectedp
   #:ui-node-start-mark #:ui-node-end-mark
   #:render-node #:render-tree
   #:ui-tree-get #:selected-node #:selection-move
   #:collect-selectables #:update-selection
   #:scroll-to-selection

   ;; ui-tree buffer ownership and edit protection
   #:*tree-rendering*
   #:install-tree #:uninstall-tree #:buffer-ui-tree

   ;; input state operations (src/ui/input.lisp)
   #:input-string #:cursor-offset
   #:type-char-at-cursor #:delete-char-before-cursor
   #:kill-input #:kill-to-end #:kill-word-before-cursor #:find-word-start
   #:move-cursor #:cursor-to-start #:cursor-to-end
   #:move-cursor-backward-word #:backward-word-offset
   #:set-input #:confirm-input

   ;; grid node
   #:ui-grid #:make-ui-grid #:ui-grid-cells #:ui-grid-col-widths

   ;; echo area completion (src/user/echo-completion.lisp)
   #:completion-candidates #:completion-display-name
   #:file-list-directory #:file-display-name #:file-completion-filter
   #:make-echo-completions
   #:cleanup-echo-completions
   #:build-completion-grid
   #:echo-render #:echo-refilter
   #:echo-type-char #:echo-delete-char
   #:echo-kill #:echo-kill-end #:echo-kill-word
   #:echo-set-input #:echo-select
   #:echo-move-cursor #:echo-cursor-start #:echo-cursor-end
   #:echo-confirm #:echo-backward-word #:echo-set-message
   ))


;;;
;;; hemlock-interface — stable API for command writers.
;;; Use this package when writing new modes, commands, or extensions.
;;; Re-exports curated symbols from hemlock.text and hemlock.command.
;;;
(defpackage :hemlock-interface
    (:use :hemlock.text :hemlock.command)
  (:shadowing-import-from :hemlock.text #:char-code-limit)
  (:export
   ;; Functions from the CIM:
   #:linep
   #:line-string
   #:line-previous
   #:line-next
   #:line-buffer
   #:line-length
   #:line-character
   #:line-plist
   #:line-signature
   #:markp
   #:mark-line
   #:mark-charpos
   #:mark-kind
   #:previous-character
   #:next-character
   #:mark
   #:copy-mark
   #:delete-mark
   #:move-to-position
   #:move-mark
   #:line-start
   #:line-end
   #:buffer-start
   #:buffer-end
   #:mark-before
   #:mark-after
   #:character-offset
   #:line-offset
   #:region
   #:regionp
   #:make-empty-region
   #:copy-region
   #:region-to-string
   #:string-to-region
   #:line-to-region
   #:region-start
   #:region-end
   #:region-bounds
   #:set-region-bounds
   #:count-lines
   #:count-characters
   #:check-region-query-size
   #:current-buffer
   #:current-point
   #:current-mark
   #:pop-buffer-mark
   #:push-buffer-mark
   #:change-to-buffer
   #:previous-buffer
   #:make-buffer
   #:bufferp
   #:buffer-name
   #:buffer-region
   #:buffer-pathname
   #:buffer-write-date
   #:buffer-point
   #:buffer-mark
   #:buffer-active-region-p
   #:buffer-start-mark
   #:buffer-end-mark
   #:buffer-writable
   #:buffer-modified
   #:buffer-signature
   #:buffer-variables
   #:buffer-modes
   #:buffer-windows
   #:buffer-delete-hook
   #:buffer-undo-p
   #:delete-buffer
   #:delete-buffer-if-possible
   #:make-modeline-field
   #:modeline-field-p
   #:modeline-field-name
   #:modeline-field
   #:modeline-field-function
   #:modeline-field-width
   #:buffer-modeline-fields
   #:buffer-modeline-field-p
   #:update-modeline-fields
   #:update-modeline-field
   #:insert-character
   #:insert-string
   #:insert-region
   #:ninsert-region
   #:delete-characters
   #:delete-region
   #:delete-and-save-region
   #:fetch-cut-string
   #:store-cut-string
   #:filter-region
   #:start-line-p
   #:end-line-p
   #:empty-line-p
   #:blank-line-p
   #:blank-before-p
   #:blank-after-p
   #:same-line-p
   #:mark<
   #:mark<=
   #:mark=
   #:mark/=
   #:mark>=
   #:mark>
   #:line<
   #:line<=
   #:line>=
   #:line>
   #:lines-related
   #:first-line-p
   #:last-line-p
   #:kill-region
   #:kill-characters
   #:activate-region
   #:deactivate-region
   #:region-active-p
   #:check-region-active
   #:current-region
   #:new-search-pattern
   #:search-pattern-p
   #:get-search-pattern
   #:find-pattern
   #:replace-pattern
   #:current-variable-tables
   #:defhvar
   #:variable-value
   #:variable-documentation
   #:variable-hooks
   #:variable-name
   #:string-to-variable
   #:hemlock-bound-p
   #:delete-variable
   #:make-command
   #:commandp
   #:command-documentation
   #:command-function
   #:command-name
   #:bind-key
   #:command-bindings
   #:delete-key-binding
   #:get-command
   #:map-bindings
   #:key-translation
   #:interactive
   #:last-command-type
   #:prefix-argument
   #:recursive-edit
   #:in-recursive-edit
   #:exit-recursive-edit
   #:abort-recursive-edit
   #:defmode
   #:mode-documentation
   #:buffer-major-mode
   #:buffer-minor-mode
   #:mode-variables
   #:mode-major-p
   #:defattribute
   #:character-attribute-name
   #:character-attribute-documentation
   #:character-attribute
   #:character-attribute-p
   #:shadow-attribute
   #:unshadow-attribute
   #:find-attribute
   #:reverse-find-attribute
   #:character-attribute-hooks
   #:current-window
   #:make-window
   #:windowp
   #:delete-window
   #:window-buffer
   #:window-display-start
   #:window-display-end
   #:window-display-recentering
   #:window-point
   #:center-window
   #:scroll-window
   #:displayed-p
   #:window-height
   #:window-width
   #:next-window
   #:previous-window
   #:mark-to-cursorpos
   #:cursorpos-to-mark
   #:last-key-event-cursorpos
   #:mark-column
   #:move-to-column
   #:show-mark
   #:redisplay
   #:redisplay-all
   #:editor-finish-output
   #:define-logical-key-event
   #:logical-key-event-key-events
   #:logical-key-event-p
   #:clear-echo-area
   #:message
   #:loud-message
   #:prompt
   #:prompt-for-buffer
   #:prompt-for-key-event
   #:prompt-for-key
   #:prompt-for-file
   #:prompt-for-integer
   #:prompt-for-keyword
   #:prompt-for-expression
   #:prompt-for-string
   #:prompt-for-variable
   #:prompt-for-y-or-n
   #:prompt-for-yes-or-no
   #:process-file-options
   #:pathname-to-buffer-name
   #:buffer-default-pathname
   #:read-file
   #:write-file
   #:write-buffer-file
   #:read-buffer-file
   #:find-file-buffer
   ;;   #:ed
   #:exit-hemlock
   #:pause-hemlock
   #:get-key-event
   #:unget-key-event
   #:clear-editor-input
   #:listen-editor-input
   #:make-hemlock-output-stream
   #:hemlock-output-stream-p
   #:make-hemlock-region-stream
   #:hemlock-region-stream-p
   #:editor-error-format-string
   #:editor-error-format-arguments
   #:editor-error
   #:add-definition-dir-translation
   #:delete-definition-dir-translation
   #:schedule-event
   #:remove-scheduled-event
   #:in-lisp
   #:indent-region
   #:indent-region-for-commands
   #:delete-horizontal-space
   #:pre-command-parse-check
   #:form-offset
   #:top-level-offset
   #:mark-top-level-form
   #:defun-region
   #:inside-defun-p
   #:start-defun-p
   #:forward-up-list
   #:backward-up-list
   #:valid-spot
   #:defindent
   #:word-offset
   #:sentence-offset
   #:paragraph-offset
   #:mark-paragraph
   #:goto-page
   #:page-offset
   #:page-directory
   #:display-page-directory
   #:fill-region
   #:fill-region-by-paragraphs
   #:make-string-table
   #:string-table-p
   #:string-table-separator
   #:delete-string
   #:clrstring
   #:getstring
   #:complete-string
   #:find-ambiguous
   #:find-containing
   #:make-ring
   #:ringp
   #:ring-length
   #:ring-ref
   #:ring-push
   #:ring-pop
   #:rotate-ring
   #:save-for-undo
   #:make-region-undo
   #:supply-generic-pointer-up-function

   ;; Macros from the CIM:
   #:with-writable-buffer
   #:value
   #:setv
   #:add-hook
   #:remove-hook
   #:invoke-hook
   #:*inhibit-hooks*
   #:defcommand
   #:use-buffer
   #:command-case
   #:define-file-option
   #:define-file-type-hook
   #:do-active-group
   #:with-input-from-region
   #:with-output-to-mark
   #:with-pop-up-display
   #:handle-lisp-errors
   #:do-alpha-chars
   #:do-strings

   ;; Later, possibly ill-adviced additions
   #:goto-buffer-start
   #:goto-buffer-end

   ;; Display device protocol (public interface for backends)
   #:device
   #:device-init #:device-exit #:device-make-window #:device-delete-window
   #:device-smart-redisplay #:device-dumb-redisplay #:device-after-redisplay
   #:device-clear #:device-force-output #:device-finish-output
   #:device-put-cursor #:device-show-mark #:device-beep
   #:device-next-window #:device-previous-window
   #:device-note-read-wait #:device-enlarge-window
   #:device-random-typeout-full-more #:device-random-typeout-line-more
   #:device-random-typeout-setup #:device-random-typeout-cleanup
   #:device-hunk #:device-hunk-device #:device-hunks
   #:default-font
   #:define-window-cursor
   #:*delete-window-hook*
   #:dis-line-chars
   #:dis-line-delta
   #:dis-line-flags
   #:dis-line-font-changes
   #:dis-line-length
   #:dis-line-position
   #:do-tty-full-more
   #:*echo-area-window*
   #:font-change-font
   #:font-change-next
   #:font-change-x
   #:font-family-baseline
   #:font-family-cursor-height
   #:font-family-cursor-width
   #:font-family-cursor-x-offset
   #:*foreground-background-xor*
   #:hunk-width-limit
   #:line-buffered-p
   #:make-xwindow-like-hwindow
   #:minimum-window-columns
   #:minimum-window-lines
   #:*more-prompt-action*
   #:moved-bit
   #:prepare-window-for-redisplay
   #:raise-echo-area-when-modified
   #:*random-typeout-hook*
   #:random-typeout-redisplay
   #:random-typeout-stream-first-more-p
   #:random-typeout-stream-more-mark
   #:random-typeout-stream-no-prompt
   #:set-window-name-for-buffer-name
   #:set-window-name-for-window-buffer
   #:unaltered-bits
   #:update-tty-line-buffered-stream
   #:wait-for-more
   #:window-first-changed
   #:window-first-line
   #:window-group
   #:window-group-p
   #:window-last-changed
   #:window-last-line
   #:window-modeline-dis-line
   #:window-old-lines
   #:dummy-line
   #:setup-modeline-image
   #:tick
   #:dis-line-old-chars
   #:update-window-image
   #:make-window-dis-line
   #:internal-make-window
   #:maybe-recenter-window
   #:window-modeline-buffer-len
   #:window-tick
   #:window-spare-lines
   #:maybe-recenter-window
   #:window                             ;as a type

   #:editor-input
   #:*editor-input*
   #:window-%buffer
   #:window-old-start
   #:device-name
   #:device-bottom-window-base
   #:device-hunks
   #:device-hunk-window
   #:device-hunk-position
   #:device-hunk-height
   #:device-hunk-next
   #:device-hunk-previous
   #:device-hunk-device

   #:list-all-connections
   #:connection
   #:connection-name
   #:connection-buffer
   #:connection-sentinel
   #:connection-filter
   #:connection-encoding
   #:delete-connection
   #:connection-listen
   #:connection-write
   #:tcp-connection
   #:make-tcp-connection
   #:process-connection
   #:connection-command
   #:make-process-connection
   #:make-pipelike-connection
   #:make-process-with-pty-connection
   #:find-a-pty
   #:connection-exit-status
   #:connection-exit-code
   #:file-connection
   #:connection-port
   #:connection-host
   #:make-file-connection
   #:conection-filename
   #:descriptor-connection
   #:connection-descriptor
   #:make-descriptor-connection
   #:listening-connection
   #:make-tcp-listener
   #:make-connection-device

   ;; From hemlock.text — portability primitives (lispdep.lisp)
   #:without-interrupts
   #:getenv
   #:fixnump
   #:file-writable

   ;; From hemlock.text — key event API
   #:define-keysym #:define-mouse-keysym #:name-keysym #:keysym-names
   #:keysym-preferred-name #:define-key-event-modifier #:define-clx-modifier
   #:make-key-event-bits #:key-event-modifier-mask #:key-event-bits-modifiers
   #:*all-modifier-names* #:translate-mouse-key-event
   #:make-key-event #:key-event #:key-event-p #:key-event-bits #:key-event-keysym
   #:char-key-event #:key-event-char #:key-event-bit-p #:do-alpha-key-events
   #:print-pretty-key #:print-pretty-key-event

   ;; From hemlock.text — list utilities
   #:delq #:memq #:assq

   ;; From hemlock.command — framework operations (hemlock-ext.lisp)
   #:concat
   #:quit
   #:skip-whitespace
   #:default-directory
   #:find-buffer
   #:maybe-rename-buffer
   #:rename-buffer-uniquely
   #:with-clx-event-handling
   #:complete-file
   #:complete-file-directory
   #:ambiguous-files
   #:set-file-permissions)
  (:import-from :hemlock.wire #:dispatch-events #:dispatch-events-no-hang)
  (:export #:dispatch-events #:dispatch-events-no-hang #:dispatch-events-timeout))

;;;
;;; hemlock-ext — external extension API.
;;; For code outside hemlock that needs to interact with the editor:
;;; key event construction, portability primitives, event dispatch.
;;;
(defpackage :hemlock-ext
  (:use :common-lisp :hemlock-interface)
  (:shadowing-import-from :hemlock-interface #:char-code-limit)
  (:export
   ;; Re-exported from hemlock-interface — portability primitives
   #:without-interrupts
   #:getenv
   #:fixnump
   #:file-writable

   ;; Re-exported from hemlock-interface — key event API
   #:define-keysym #:define-mouse-keysym #:name-keysym #:keysym-names
   #:keysym-preferred-name #:define-key-event-modifier #:define-clx-modifier
   #:make-key-event-bits #:key-event-modifier-mask #:key-event-bits-modifiers
   #:*all-modifier-names* #:translate-mouse-key-event
   #:make-key-event #:key-event #:key-event-p #:key-event-bits #:key-event-keysym
   #:char-key-event #:key-event-char #:key-event-bit-p #:do-alpha-key-events
   #:print-pretty-key #:print-pretty-key-event

   ;; Re-exported from hemlock-interface — list utilities
   #:delq #:memq #:assq

   ;; Re-exported from hemlock-interface — framework operations
   #:concat
   #:quit
   #:default-directory
   #:find-buffer
   #:maybe-rename-buffer
   #:rename-buffer-uniquely
   #:with-clx-event-handling
   #:complete-file
   #:ambiguous-files
   #:set-file-permissions
   #:skip-whitespace

   ;; System-level re-exports
   #:serve-event
   #:sap-ref-8
   #:print-directory
   ))

;;;
;;; hemlock-internals — implementation details needed by the hemlock package
;;; but not part of the public command-writer API (hemlock-interface).
;;;
;;; If a symbol is already exported from hemlock.text or hemlock.command
;;; (and thus available via hemlock-interface), it does NOT belong here.
;;;
(defpackage :hemlock-internals
  (:use :common-lisp :hemlock.text :hemlock.command
        :command-line-arguments
        :trivial-gray-streams)
  (:nicknames :hi)
  (:shadowing-import-from :hemlock.text #:char-code-limit)
  (:shadow #:show-option-help)
  (:shadowing-import-from :hemlock-ext #:concat)
  (:export
   ;; platform.lisp — internal functions called from hemlock package code
   #:fun-defined-from-pathname
   #:editor-describe-function
   #:schedule-event #:remove-scheduled-event
   #:enter-window-autoraise
   #:*input-transcript*
   #:*beep-function* #:beep
   #:default-font

   ;; Internal editor state
   #:*global-variable-names* #:*mode-names* #:*buffer-names*
   #:*character-attribute-names* #:*command-names*
   #:*background-image*
   #:after-editor-initializations

   ;; Window creation hooks (used by backends)
   #:*create-window-hook* #:*delete-window-hook*
   #:*random-typeout-hook* #:*create-initial-windows-hook*
   #:make-xwindow-like-hwindow


   ;; Entry points
   #:hemlock #:main #:with-editor #:call-with-editor
   #:linedit #:formedit #:repl

   ;; Re-exported for hemlock package use
   #:concat))


(defpackage :hemlock
  (:use :common-lisp :hemlock.text :hemlock.command :hi :hemlock-ext
        :trivial-gray-streams)
;;;  (:import-from :hemlock-ext #:delq #:memq #:assq)
;;;  (:import-from :hemlock-internals #:*fast*)
  (:import-from :hemlock-internals #:hemlock)
  (:export #:hemlock
           #:main
           #:with-editor
           #:call-with-editor
           #:start-agent
           #:*background-image*
           #:linedit
           #:formedit
           #:repl
           #:command-loop
           ;; filesystem utilities (prepl.lisp) used by hemlock.tty
           #:file-kind
           #:read-link
           #:user-info
           #:relative-pathname-p
           #:with-directory-iterator
           #:get-init-file-string
           ;; repl utilities
           #:call-with-typeout-pop-up-in-master
           #:with-typeout-pop-up-in-master
           #:call-with-standard-synonym-streams)
  (:shadowing-import-from #:hemlock-ext
                          #:char-code-limit)
  (:shadowing-import-from :hemlock-ext
   #:*all-modifier-names*
   #:assq
   #:concat
   #:char-key-event
   #:default-directory
   #:define-clx-modifier
   #:define-key-event-modifier
   #:define-keysym
   #:define-mouse-keysym
   #:delq
   #:do-alpha-key-events
   #:file-writable
   #:fixnump
   #:key-event
   #:key-event-bit-p
   #:key-event-bits
   #:key-event-bits-modifiers
   #:key-event-char
   #:key-event-keysym
   #:key-event-modifier-mask
   #:key-event-p
   #:keysym-names
   #:keysym-preferred-name
   #:make-key-event
   #:make-key-event-bits
   #:memq
   #:name-keysym
   #:print-pretty-key
   #:print-pretty-key-event
   #:quit

   ;; these are from system package
   #:sap-ref-8
   #:serve-event
   #:without-interrupts

   #:translate-mouse-key-event))


(defpackage :hemlock.display
  (:use :common-lisp :hemlock.text :hemlock.command)
  (:shadowing-import-from :hemlock.text #:char-code-limit)
  (:export
   ;; Change-tracking flags
   #:unaltered-bits
   #:changed-bit
   #:moved-bit
   ;; Window display sentinel
   #:the-sentinel
   ;; Window image update
   #:update-window-image
   #:setup-modeline-image
   ;; Redisplay
   #:redisplay
   #:redisplay-all
   #:random-typeout-redisplay
   #:prepare-window-for-redisplay
   #:maybe-recenter-window
   ;; Display state
   #:random-typeout-stream #:random-typeout-stream-window
   #:random-typeout-stream-mark
   #:*random-typeout-buffers* #:*random-typeout-ml-fields*
   #:*default-font-family* #:font-family-cursor-y-offset
   #:font-family-height #:font-family-map #:font-family-width
   #:window-for-hunk #:window-hunk #:window-input-handler
   #:window-modeline-buffer))


(defpackage :hemlock.x11
  (:use :common-lisp :hemlock.text :hemlock.command :hemlock :trivial-gray-streams)
  (:shadowing-import-from :hemlock.text #:char-code-limit)
  (:export
   ;; Object-set infrastructure (defined in clx/object-set.lisp)
   #:make-object-set #:object-set #:object-set-name #:object-set-default-handler
   #:object-set-table
   #:add-xwindow-object #:remove-xwindow-object #:lisp--map-xwindow
   ;; Event service (defined in clx/object-set.lisp)
   #:serve-key-press #:serve-key-release
   #:serve-button-press #:serve-button-release
   #:serve-motion-notify #:serve-enter-notify #:serve-leave-notify
   #:serve-focus-in #:serve-focus-out
   #:serve-exposure #:serve-graphics-exposure #:serve-no-exposure
   #:serve-visibility-notify #:serve-create-notify #:serve-destroy-notify
   #:serve-unmap-notify #:serve-map-notify #:serve-map-request
   #:serve-reparent-notify #:serve-configure-notify #:serve-gravity-notify
   #:serve-resize-request #:serve-configure-request
   #:serve-circulate-notify #:serve-circulate-request
   #:serve-property-notify #:serve-selection-clear
   #:serve-selection-request #:serve-selection-notify
   #:serve-colormap-notify #:serve-client-message
   ;; CLX event handling
   #:call-with-clx-event-handling
   #:object-set-event-handler
   #:flush-display-events
   #:disable-clx-event-handling
   #:default-clx-event-handler
   ;; Bitmap device/hunk (CLX-specific)
   #:bitmap-device-display
   #:bitmap-hunk-font-family #:bitmap-hunk-modeline-dis-line
   #:bitmap-hunk-modeline-pos #:bitmap-hunk-trashed
   #:bitmap-hunk-window #:bitmap-hunk-window-group #:bitmap-hunk-xwindow
   #:init-bitmap-screen-manager #:reverse-video-hook-fun
   ;; CLX window management
   #:make-window-group #:window-group-height #:window-group-width
   #:window-group-xparent #:hemlock-window
   #:get-hemlock-cursor #:get-hemlock-grey-pixmap
   #:make-black-color #:make-white-color
   #:child-interesting-xevents-mask #:group-interesting-xevents-mask
   #:random-typeout-xevents-mask
   #:*default-foreground-pixel* #:*default-background-pixel*
   #:*hemlock-cursor* #:*cursor-background-color* #:*cursor-foreground-color*
   #:windowed-monitor-p))

(defpackage :hemlock-user
    (:use :common-lisp :hemlock.text :hemlock.command :hemlock)
  (:shadowing-import-from :hemlock.text #:char-code-limit))

(defpackage :hemlock.io
  (:use :common-lisp :hemlock.text :hemlock.command)
  (:shadowing-import-from :hemlock.text #:char-code-limit)
  (:export
   ;; fd utilities
   #:fd-readable-p
   #:drain-pending-invocations
   #:setup-wakeup-pipe
   #:wake-event-loop))

(defpackage :hemlock.terminfo
  (:use :common-lisp)
  (:export #:*terminfo-directories*
           #:*terminfo*
           #:capability
           #:tparm
           #:tputs
           #:set-terminal))

(defpackage :hemlock.tty
  (:use :common-lisp :hemlock.text :hemlock.command :hemlock :hemlock.display
        :hemlock.io :trivial-gray-streams)
  (:shadowing-import-from :hemlock.text #:char-code-limit)
  (:nicknames :tty))

;;;
;;; hemlock.actor — actor system, agent registry, agent protocols.
;;; Uses sento packages (ac, act, asys, rem) for actor primitives.
;;;
(defpackage :hemlock.actor
  (:use :common-lisp :ac :act :asys :rem)
  (:export
   ;; system lifecycle
   #:*actor-system*
   #:*remoting-port*
   #:start-actor-system
   #:stop-actor-system
   ;; agent registry
   #:*agent-registry*
   #:agent-info
   #:agent-info-name #:agent-info-type #:agent-info-actor
   #:agent-info-buffers #:agent-info-port
   #:start-agent-registry
   #:register-agent #:unregister-agent #:find-agent #:list-agents
   #:agent-eval #:agent-compile
   ;; local agent
   #:*local-agent*
   #:start-local-agent
   ;; process agent spawning
   #:spawn-process-agent
   #:kill-process-agent))

;;;
;;; hemlock.agent — agent-side package. Loaded in agent processes only.
;;; Separate actor-system that connects to the master via sento remoting.
;;;
(defpackage :hemlock.agent
  (:use :common-lisp :ac :act :asys :rem)
  (:export #:connect))

(defpackage :hemlock.webui
  (:use :common-lisp :hemlock.text :hemlock.command :hemlock :hemlock.display
        :hemlock.io)
  (:shadowing-import-from :hemlock.text #:char-code-limit))
