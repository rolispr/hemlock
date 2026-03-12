;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :cl-user)

;; Note: I want real relative package names like the Symbolics has
;; them. In the mean time:

#+CMU
(eval-when (:compile-toplevel :load-toplevel :execute)
  (progn
    ;; Just in case the original Hemlock is loaded.
    (dolist (p '("HEMLOCK" "HEMLOCK-INTERNALS"))
      (when (find-package p)
        (delete-package p)))))


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
   ;; tty-device class and accessors (struct.lisp)
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
   ;; value-node struct (table.lisp)
   #:value-node
   #:value-node-proper
   #:value-node-folded
   #:value-node-value
   #:make-value-node
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
   ))


;;;
;;; hemlock.command — editor framework: commands system, modes, keys,
;;;                   display, windows, streams
;;;
(defpackage :hemlock.command
  (:use :common-lisp :trivial-gray-streams :hemlock.text)
  (:shadowing-import-from :hemlock.text #:char-code-limit)
  (:import-from :hemlock.wire #:dispatch-events #:dispatch-events-no-hang)
  (:export
   #:dispatch-events #:dispatch-events-no-hang #:dispatch-events-timeout

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
   ;; rompsite.lisp — entry-point functions used from main.lisp (hemlock pkg)
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
   ;; key-dispatch.lisp — used from main.lisp
   #:*invoke-hook*
   #:%command-loop
   ;; window.lisp — used from main.lisp
   #:%init-redisplay
   ;; echo.lisp — referenced via hi:: from user-layer files
   #:display-prompt-nicely
   #:key-event-case
   #:after-editor-initializations
   ;; symbols referenced via hi:: from user-layer or tty files
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
   ))


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

   ;;;; !!!!
   ;;;; !!!! Everything below here is because putting bit-screen.lisp,
   ;;;; !!!! bit-display.lisp and hunk-draw.lisp into its own package.
   ;;;; !!!! Besides the DEVICE-xyz entries this list should be empty.
   ;;;; !!!! --GB 2004-05-26
   ;;;; !!!!

   #:device                             ;[class]
   ;; their methods
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
   #:device-hunk
   #:random-typeout-stream
   #:with-mark
   #:init-bitmap-screen-manager
   #:reverse-video-hook-fun             ;### only defined in bitmap device, refered to from rompsite.lisp

   ;; variables, due to bit-screen.lisp
   #:*current-buffer*
   #:*current-window*
   #:*cursor-background-color*
   #:*cursor-foreground-color*
   #:*default-font-family*
   #:*echo-area-buffer*
   #:*hemlock-cursor*
   #:*random-typeout-buffers*
   #:*random-typeout-ml-fields*
   #:*window-list*
   #:child-interesting-xevents-mask
   #:group-interesting-xevents-mask
   #:random-typeout-xevents-mask
   #:the-sentinel
   #:*default-foreground-pixel*         ;### rompsite.lisp
   #:*default-background-pixel*         ;### rompsite.lisp
   ;; functions, due to bit-screen.lisp
   #:device-hunks
   #:random-typeout-stream-window
   #:window-group-height
   #:window-group-width
   #:add-xwindow-object
   #:device-hunk-device
   #:device-hunks
   #:font-family-cursor-y-offset
   #:font-family-height
   #:font-family-map
   #:font-family-width
   #:get-hemlock-cursor
   #:get-hemlock-grey-pixmap
   #:hemlock-window
   #:hlet
   #:make-black-color
   #:make-white-color
   #:make-window-group
   #:random-typeout-stream-mark
   #:random-typeout-stream-window
   #:remove-xwindow-object
   #:window-for-hunk
   #:window-group-height
   #:window-group-width
   #:window-group-xparent
   #:window-hunk
   #:window-input-handler
   #:window-modeline-buffer
   #:windowed-monitor-p
   ;;;; grrr
   #:bitmap-device-display
   #:bitmap-hunk-font-family
   #:bitmap-hunk-modeline-dis-line
   #:bitmap-hunk-modeline-pos
   #:bitmap-hunk-trashed
   #:bitmap-hunk-window
   #:bitmap-hunk-window-group
   #:bitmap-hunk-xwindow
   #:changed-bit
   #:*create-initial-windows-hook*
   #:*create-window-hook*
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
   #:make-connection-device)
  (:import-from :hemlock.wire #:dispatch-events #:dispatch-events-no-hang)
  (:export #:dispatch-events #:dispatch-events-no-hang #:dispatch-events-timeout))


(defpackage :hemlock-ext
  (:use :common-lisp
        :hemlock-interface)
  (:shadow #:char-code-limit)
  ;; Import key-event symbols from hemlock.text for backward compat
  (:import-from :hemlock.text
                #:key-event #:key-event-p #:key-event-bits #:key-event-keysym
                #:char-key-event #:key-event-char #:key-event-bit-p #:do-alpha-key-events
                #:make-key-event #:make-key-event-bits #:key-event-modifier-mask
                #:key-event-bits-modifiers #:define-key-event-modifier
                #:*all-modifier-names* #:translate-mouse-key-event
                #:define-keysym #:define-mouse-keysym #:name-keysym #:keysym-names
                #:keysym-preferred-name #:print-pretty-key #:print-pretty-key-event)
  ;;
  (:export
   #:without-interrupts
   #:define-setf-method
   #:getenv

   #:delq #:memq #:assq
   #:concat
   #:fixnump
   #:file-writable

   #:define-keysym #:define-mouse-keysym #:name-keysym #:keysym-names
   #:keysym-preferred-name #:define-key-event-modifier #:define-clx-modifier
   #:make-key-event-bits #:key-event-modifier-mask #:key-event-bits-modifiers
   #:*all-modifier-names* #:translate-key-event #:translate-mouse-key-event
   #:make-key-event #:key-event #:key-event-p #:key-event-bits #:key-event-keysym
   #:char-key-event #:key-event-char #:key-event-bit-p #:do-alpha-key-events
   #:print-pretty-key #:print-pretty-key-event

   ;; hemlock-ext.lisp
   #:disable-clx-event-handling
   #:quit
   #:serve-event
   #:sap-ref-8
   #:make-object-set
   #:default-clx-event-handler
   #:serve-exposure
   #:serve-graphics-exposure
   #:serve-no-exposure
   #:serve-configure-notify
   #:serve-destroy-notify
   #:serve-unmap-notify
   #:serve-map-notify
   #:serve-reparent-notify
   #:serve-gravity-notify
   #:serve-circulate-notify
   #:serve-client-message
   #:serve-key-press
   #:serve-button-press
   #:serve-button-release
   #:serve-enter-notify
   #:serve-leave-notify
   #:flush-display-events
   #:object-set-event-handler
   #:with-clx-event-handling
   #:complete-file
   #:default-directory
   #:set-file-permissions
   #:ambiguous-files
   #:print-directory
   ))

(defpackage :hemlock-internals
  (:use :common-lisp :hemlock.text :hemlock.command
        :command-line-arguments
        :trivial-gray-streams)
  (:nicknames :hi)
  (:shadow #:char-code-limit #:show-option-help)
  (:shadowing-import-from :hemlock-ext #:concat)
  ;;
  (:export
   #:*fast*                             ;hmm not sure about this one

   ;; rompsite.lisp
   #:show-mark #:*input-transcript* #:fun-defined-from-pathname
   #:editor-describe-function #:pause-hemlock #:store-cut-string
   #:fetch-cut-string #:schedule-event #:remove-scheduled-event
   #:enter-window-autoraise #:directoryp #:merge-relative-pathnames
   ;;
   ;; Export default-font to prevent a name conflict that occurs due to
   ;; the Hemlock variable "Default Font" defined in SITE-INIT below.
   ;;
   #:default-font
   #:*beep-function* #:beep

   ;;
   #:mark #:mark-line #:mark-charpos #:markp #:region #:region-start #:region-end
   #:regionp #:buffer #:bufferp #:buffer-modes #:buffer-point #:buffer-writable
   #:buffer-delete-hook #:buffer-windows #:buffer-variables #:buffer-write-date
   #:region #:regionp #:region-start #:region-end #:window #:windowp #:window-height
   #:window-width #:window-display-start #:window-display-end #:window-point
   #:window-display-recentering #:commandp #:command #:command-function
   #:command-documentation #:modeline-field #:modeline-field-p

   ;; from input.lisp
   #:get-key-event #:unget-key-event #:clear-editor-input #:listen-editor-input
   #:*last-key-event-typed* #:*key-event-history* #:*editor-input*
   #:*real-editor-input* #:input-waiting #:last-key-event-cursorpos

   ;; from macros.lisp
   #:invoke-hook #:value #:setv #:hlet #:string-to-variable #:add-hook #:remove-hook
   #:defcommand #:with-mark #:use-buffer #:editor-error #:canonical-case
   #:editor-error-format-string #:editor-error-format-arguments #:do-strings
   #:command-case #:reprompt #:with-output-to-mark #:with-input-from-region
   #:handle-lisp-errors #:with-pop-up-display #:*random-typeout-buffers*

   ;; from line.lisp
   #:line #:linep #:line-previous #:line-next #:line-plist #:line-signature

   ;; from ring.lisp
   #:ring #:ringp #:make-ring #:ring-push #:ring-pop #:ring-length #:ring-ref
   #:rotate-ring

   ;; from table.lisp
   #:string-table #:string-table-p #:make-string-table
   #:string-table-separator #:getstring
   #:find-ambiguous #:complete-string #:find-containing
   #:delete-string #:clrstring #:do-strings

   ;; bit-display.lisp
   #:redisplay #:redisplay-all

   ;; bit-screen.lisp
   #:make-xwindow-like-hwindow          ;used in input.lisp
   #:*create-window-hook* #:*delete-window-hook*
   #:*random-typeout-hook* #:*create-initial-windows-hook*

   ;; buffer.lisp
   #:buffer-modified #:buffer-region #:buffer-name #:buffer-pathname
   #:buffer-major-mode #:buffer-minor-mode #:buffer-modeline-fields
   #:buffer-modeline-field-p #:current-buffer #:current-point
   #:in-recursive-edit #:exit-recursive-edit #:abort-recursive-edit
   #:recursive-edit #:defmode #:mode-major-p #:mode-variables #:mode-documentation
   #:make-buffer #:delete-buffer #:with-writable-buffer #:buffer-start-mark
   #:buffer-end-mark #:*buffer-list*

   ;; charmacs.lisp
   #:do-alpha-chars

   ;; cursor.lisp
   #:mark-to-cursorpos #:center-window #:displayed-p #:scroll-window
   #:mark-column #:cursorpos-to-mark #:move-to-column

   ;; display.lisp
   #:redisplay #:redisplay-all

   ;; echo.lisp
   #:*echo-area-buffer* #:*echo-area-stream* #:*echo-area-window*
   #:*parse-starting-mark* #:*parse-input-region*
   #:*parse-verification-function* #:*parse-string-tables*
   #:*parse-value-must-exist* #:*parse-default* #:*parse-default-string*
   #:*parse-prompt* #:*parse-help* #:clear-echo-area #:message #:loud-message
   #:prompt-for-buffer #:prompt-for-file #:prompt-for-integer
   #:prompt-for-keyword #:prompt-for-expression #:prompt-for-string
   #:prompt-for-variable #:prompt-for-yes-or-no #:prompt-for-y-or-n
   #:prompt-for-key-event #:prompt-for-key
   #:logical-key-event-p
   #:logical-key-event-key-events
   #:define-logical-key-event #:*parse-type* #:current-variable-tables

   ;; files.lisp
   #:read-file #:write-file


   ;; font.lisp
   #:font-mark #:delete-font-mark #:delete-line-font-marks #:move-font-mark
   #:window-font

   ;; htext1.lisp
   #:line-length #:line-buffer #:line-string #:line-character #:mark #:mark-kind
   #:copy-mark #:delete-mark #:move-to-position #:region #:make-empty-region
   #:start-line-p #:end-line-p #:empty-line-p #:blank-line-p #:blank-before-p
   #:blank-after-p #:same-line-p #:mark< #:mark<= #:mark> #:mark>= #:mark= #:mark/=
   #:line< #:line<= #:line> #:line>= #:first-line-p #:last-line-p #:buffer-signature
   #:lines-related


   ;; htext2.lisp
   #:region-to-string #:string-to-region #:line-to-region
   #:previous-character #:next-character #:count-lines
   #:count-characters #:line-start #:line-end #:buffer-start
   #:buffer-end #:move-mark #:mark-before #:mark-after
   #:character-offset #:line-offset #:region-bounds
   #:set-region-bounds #:*print-region*


   ;; htext3.lisp
   #:insert-character #:insert-string #:insert-region #:ninsert-region


   ;; htext4.lisp
   #:delete-characters #:delete-region #:delete-and-save-region #:copy-region
   #:filter-region


   ;; interp.lisp
   #:bind-key #:delete-key-binding #:get-command #:map-bindings
   #:make-command #:command-name #:command-bindings #:last-command-type
   #:prefix-argument #:exit-hemlock #:*invoke-hook* #:key-translation


   ;; main.lisp
   #:*global-variable-names* #:*mode-names* #:*buffer-names*
   #:*character-attribute-names* #:*command-names* #:*buffer-list*
   #:*window-list* #:*last-key-event-typed* #:after-editor-initializations
   #:*background-image*

   ;; screen.lisp
   #:make-window #:delete-window #:next-window #:previous-window


   ;; search1.lisp
   #:search-pattern #:search-pattern-p #:find-pattern #:replace-pattern
   #:new-search-pattern


   ;; streams.lisp
   #:make-hemlock-output-stream
   #:hemlock-region-stream #:hemlock-region-stream-p
   #:hemlock-output-stream #:make-hemlock-region-stream
   #:hemlock-output-stream-p #:make-kbdmac-stream
   #:modify-kbdmac-stream

   ;; syntax.lisp
   #:character-attribute-name
   #:defattribute #:character-attribute-documentation #:character-attribute
   #:character-attribute-hooks #:character-attribute-p #:shadow-attribute
   #:unshadow-attribute #:find-attribute #:reverse-find-attribute

   ;; vars.lisp
   #:variable-value #:variable-hooks #:variable-documentation #:variable-name
   #:hemlock-bound-p #:defhvar #:delete-variable

   ;; window.lisp
   #:current-window #:window-buffer #:modeline-field-width
   #:modeline-field-function #:make-modeline-field #:update-modeline-fields
   #:update-modeline-field #:modeline-field-name #:modeline-field
   #:editor-finish-output #:*window-list*

   ;; start hemlock
   #:hemlock
   #:main
   #:with-editor
   #:call-with-editor
   #:linedit
   #:formedit
   #:repl

   #:concat))


(defpackage :hemlock
  (:use :common-lisp :hemlock.text :hemlock.command :hi :hemlock-ext)
;;;  (:import-from :hemlock-ext #:delq #:memq #:assq)
;;;  (:import-from :hemlock-internals #:*fast*)
  (:import-from :hemlock-internals #:hemlock)
  (:export #:hemlock
           #:main
           #:with-editor
           #:call-with-editor
           #:start-slave
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
  ;;  #+cmu
  ;; These are defined in EXTENSONS package in CMUCL
  (:shadowing-import-from :hemlock-ext
   #:*all-modifier-names*
   #:assq
   #:concat
   #:char-key-event
   #:default-clx-event-handler
   #:default-directory
   #:define-clx-modifier
   #:define-key-event-modifier
   #:define-keysym
   #:define-mouse-keysym
   #:delq
   #:disable-clx-event-handling
   #:do-alpha-key-events
   #:file-writable
   #:fixnump
   #:flush-display-events
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
   #:object-set-event-handler
   #:print-pretty-key
   #:print-pretty-key-event
   #:quit

   ;; these four are from system package
   #:make-object-set
   #:sap-ref-8
   #:serve-event
   #:without-interrupts

   #:translate-key-event
   #:translate-mouse-key-event
   #:with-clx-event-handling))


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
   #:maybe-recenter-window))


(defpackage :hemlock.x11
  (:use :common-lisp :hemlock.text :hemlock.command :hemlock :trivial-gray-streams)
  (:shadowing-import-from :hemlock.text #:char-code-limit)

  (:import-from :hemlock-ext
   #:serve-button-press
   #:serve-button-release
   #:serve-circulate-notify
   #:serve-client-message
   #:serve-configure-notify
   #:serve-destroy-notify
   #:serve-enter-notify
   #:serve-exposure
   #:serve-graphics-exposure
   #:serve-gravity-notify
   #:serve-key-press
   #:serve-leave-notify
   #:serve-map-notify
   #:serve-no-exposure
   #:serve-reparent-notify
   #:serve-unmap-notify))

(defpackage :hemlock-user
    (:use :common-lisp :hemlock))

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
        :trivial-gray-streams)
  (:shadowing-import-from :hemlock.text #:char-code-limit)
  (:nicknames :tty))

(defpackage :hemlock.webui
  (:use :common-lisp :hemlock.text :hemlock.command :hemlock :hemlock.display)
  (:shadowing-import-from :hemlock.text #:char-code-limit))


;; $Log: package.lisp,v $
;; Revision 1.4  2004-09-03 23:06:51  abakic
;; Changes to get rid of warnings and notes. As a side-effect, more code
;; has been commented out. There should be no more warnings nor notes
;; with CMUCL, and only two style warnings with SBCL. Not tested with
;; other implementations yet. TODO: spread key bindings to different
;; files.
;;
;; Revision 1.3  2004/08/10 05:58:04  rstrandh
;; Removed logical-key-event-name and logical-key-event-documentation
;; as they were never used.
;;
;; Revision 1.2  2004/08/10 05:24:16  rstrandh
;; Removed the string table *logical-key-event-names* as it was never
;; used, only written to.
;;
;; Added #k"control=[" as an alias for ESCAPE, because that is what
;; I use all the time (rather than trying to find the META key).
;;
;; Revision 1.1  2004/07/09 15:00:36  gbaumann
;; Let us see if this works.
;;
;; Revision 1.9  2003/08/05 19:58:21  gilbert
;; - we now have a HEMLOCK-INTERFACE package which exports symbols mentioned
;;   in the Command Implementors Manual.
;;
;; Revision 1.8  2003/07/28 20:35:32  jdz
;; BEEP function now works.
;;
;; Revision 1.7  2003/07/27 10:11:06  jdz
;; HEMLOCK-EXT package is now used by HEMLOCK.  Conflicting symbols from
;; EXTENSIONS package in CMUCL are shadowed.
;;
;; Revision 1.6  2003/05/12 11:01:48  gbyers
;; Conditionalize (Gray streams package) for OpenMCL.
;;
;; Revision 1.5  2003/03/26 07:50:10  gilbert
;; Port to SCL by Douglas Crosher
;;
;; Revision 1.4  2003/03/06 21:38:58  gilbert
;; The symbol *FAST* is now exported from HI (no idea if that is the
;; right thing to do) and imported into HEMLOCK. Fixes bug:
;; auto-save.lisp was not compiling.
;;
