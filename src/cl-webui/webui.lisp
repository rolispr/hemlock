(defpackage :webui
  (:use :cl :cffi)
  (:export
   ;; Browser enum
   :+webui-no-browser+
   :+webui-any-browser+
   :+webui-chrome+
   :+webui-firefox+
   :+webui-edge+
   :+webui-safari+
   :+webui-chromium+
   :+webui-opera+
   :+webui-brave+
   :+webui-vivaldi+
   :+webui-epic+
   :+webui-yandex+
   :+webui-chromium-based+
   :+webui-webview+
   ;; Event enum
   :+webui-event-disconnected+
   :+webui-event-connected+
   :+webui-event-mouse-click+
   :+webui-event-navigation+
   :+webui-event-callback+
   ;; Runtime enum
   :+webui-runtime-none+
   :+webui-runtime-deno+
   :+webui-runtime-nodejs+
   :+webui-runtime-bun+
   ;; Config option enum
   :+show-wait-connection+
   :+ui-event-blocking+
   :+folder-monitor+
   :+multi-client+
   :+use-cookies+
   :+asynchronous-response+
   ;; Logger level enum
   :+webui-logger-debug+
   :+webui-logger-info+
   :+webui-logger-error+
   ;; Event struct and accessors
   :webui-event-t
   :webui-event-window
   :webui-event-event-type
   :webui-event-element
   :webui-event-event-number
   :webui-event-bind-id
   :webui-event-client-id
   :webui-event-connection-id
   :webui-event-cookies
   ;; Window lifecycle
   :webui-new-window
   :webui-new-window-id
   :webui-get-new-window-id
   :webui-bind
   :webui-get-best-browser
   :webui-browser-exist
   ;; Show/display
   :webui-show
   :webui-show-browser
   :webui-show-wv
   :webui-show-client
   :webui-set-kiosk
   :webui-start-server
   :webui-open-url
   ;; Wait/close
   :webui-wait
   :webui-close
   :webui-close-client
   :webui-destroy
   :webui-exit
   ;; Window appearance/control
   :webui-minimize
   :webui-maximize
   :webui-set-resizable
   :webui-set-frameless
   :webui-set-transparent
   :webui-set-center
   :webui-set-minimum-size
   :webui-set-high-contrast
   :webui-is-high-contrast
   :webui-set-event-blocking
   :webui-set-custom-parameters
   ;; Context
   :webui-set-context
   :webui-get-context
   ;; File/profile/root
   :webui-set-root-folder
   :webui-set-default-root-folder
   :webui-set-file-handler
   :webui-set-file-handler-window
   :webui-set-browser-folder
   :webui-is-shown
   :webui-set-timeout
   :webui-set-icon
   ;; Encode/decode/memory
   :webui-encode
   :webui-decode
   :webui-free
   :webui-malloc
   :webui-memcpy
   ;; Raw data
   :webui-send-raw
   :webui-send-raw-client
   ;; Window settings
   :webui-set-hide
   :webui-set-size
   :webui-set-position
   :webui-set-profile
   :webui-set-proxy
   :webui-get-url
   :webui-set-public
   ;; Navigation
   :webui-navigate
   :webui-navigate-client
   ;; Cleanup
   :webui-clean
   :webui-delete-all-profiles
   :webui-delete-profile
   ;; Process
   :webui-get-parent-process-id
   :webui-get-child-process-id
   ;; Networking
   :webui-set-port
   :webui-get-port
   :webui-get-free-port
   ;; Config/TLS/runtime/logging
   :webui-set-config
   :webui-set-tls-certificate
   :webui-set-runtime
   :webui-get-mime-type
   ;; JavaScript
   :webui-run
   :webui-run-client
   :webui-script
   :webui-script-client
   :webui-get-count
   :webui-get-int-at
   :webui-get-int
   :webui-get-float-at
   :webui-get-float
   :webui-get-string-at
   :webui-get-string
   :webui-get-bool-at
   :webui-get-bool
   :webui-get-size-at
   :webui-get-size
   :webui-return-int
   :webui-return-float
   :webui-return-string
   :webui-return-bool
   ;; Error handling
   :webui-get-last-error-number
   :webui-get-last-error-message
   ;; Interface
   :webui-interface-get-context
   :webui-interface-bind
   :webui-interface-set-response
   :webui-interface-is-app-running
   :webui-interface-get-window-id
   :webui-interface-get-string-at
   :webui-interface-get-int-at
   :webui-interface-get-float-at
   :webui-interface-get-bool-at
   :webui-interface-get-size-at
   :webui-interface-show-client
   :webui-interface-close-client
   :webui-interface-send-raw-client
   :webui-interface-navigate-client
   :webui-interface-run-client
   :webui-interface-script-client
   :webui-interface-set-response-file-handler))

(in-package :webui)

(define-foreign-library libwebui
  (:darwin (:or "libwebui-2.dylib" "webui-2.dylib"))
  (:unix (:or "libwebui-2.so" "webui-2.so" "libwebui.so")))

(use-foreign-library libwebui)

;;; Correct 64-bit size_t for macOS/Linux
(defctype size-t :unsigned-long)

;;; --- Enums ---

(defcenum webui-browsers
  (+webui-no-browser+ 0)
  (+webui-any-browser+ 1)
  +webui-chrome+
  +webui-firefox+
  +webui-edge+
  +webui-safari+
  +webui-chromium+
  +webui-opera+
  +webui-brave+
  +webui-vivaldi+
  +webui-epic+
  +webui-yandex+
  +webui-chromium-based+
  +webui-webview+)

(defcenum webui-event
  +webui-event-disconnected+
  +webui-event-connected+
  +webui-event-mouse-click+
  +webui-event-navigation+
  +webui-event-callback+)

(defcenum webui-runtime
  +webui-runtime-none+
  +webui-runtime-deno+
  +webui-runtime-nodejs+
  +webui-runtime-bun+)

(defcenum webui-config-option
  +show-wait-connection+
  +ui-event-blocking+
  +folder-monitor+
  +multi-client+
  +use-cookies+
  +asynchronous-response+)

(defcenum webui-logger-level
  +webui-logger-debug+
  +webui-logger-info+
  +webui-logger-error+)

;;; --- Event struct ---
;;; Matches webui_event_t from webui.h exactly (8 fields, all size_t or char*)

(defcstruct webui-event-t
  (window size-t)
  (event-type size-t)
  (element :pointer)       ; char*
  (event-number size-t)
  (bind-id size-t)
  (client-id size-t)
  (connection-id size-t)
  (cookies :pointer))      ; char*

(defun webui-event-window (self)
  (foreign-slot-value self '(:struct webui-event-t) 'window))

(defun webui-event-event-type (self)
  (foreign-slot-value self '(:struct webui-event-t) 'event-type))

(defun webui-event-element (self)
  (foreign-slot-value self '(:struct webui-event-t) 'element))

(defun webui-event-event-number (self)
  (foreign-slot-value self '(:struct webui-event-t) 'event-number))

(defun webui-event-bind-id (self)
  (foreign-slot-value self '(:struct webui-event-t) 'bind-id))

(defun webui-event-client-id (self)
  (foreign-slot-value self '(:struct webui-event-t) 'client-id))

(defun webui-event-connection-id (self)
  (foreign-slot-value self '(:struct webui-event-t) 'connection-id))

(defun webui-event-cookies (self)
  (foreign-slot-value self '(:struct webui-event-t) 'cookies))

;;; --- Window lifecycle ---

(defcfun "webui_new_window" size-t)

(defcfun "webui_new_window_id" size-t
  (window-number size-t))

(defcfun "webui_get_new_window_id" size-t)

;;; --- Bind with trampoline ---
;;; Single top-level defcallback dispatches via hash table, enabling real
;;; Lisp closures and making webui-bind a proper function (not a macro).

(defvar *bind-callbacks* (make-hash-table))

(defcallback %bind-trampoline :void ((event :pointer))
  (let* ((bind-id (foreign-slot-value event '(:struct webui-event-t) 'bind-id))
         (fn (gethash bind-id *bind-callbacks*)))
    (when fn (funcall fn event))))

(defun webui-bind (window element func)
  (let ((id (foreign-funcall "webui_bind"
                             size-t window
                             :string element
                             :pointer (callback %bind-trampoline)
                             size-t)))
    (setf (gethash id *bind-callbacks*) func)
    id))

;;; --- Context ---

(defcfun "webui_set_context" :void
  (window size-t)
  (element :string)
  (context :pointer))

(defcfun "webui_get_context" :pointer
  (event :pointer))

;;; --- Best browser / browser existence ---

(defcfun "webui_get_best_browser" size-t
  (window size-t))

(defcfun "webui_browser_exist" :bool
  (browser size-t))

;;; --- Show / display ---

(defcfun "webui_show" :bool
  (window size-t)
  (content :string))

(defcfun "webui_show_client" :bool
  (event :pointer)
  (content :string))

(defcfun "webui_show_browser" :bool
  (window size-t)
  (content :string)
  (browser size-t))

(defcfun "webui_show_wv" :bool
  (window size-t)
  (content :string))

(defcfun "webui_set_kiosk" :void
  (window size-t)
  (status :bool))

(defcfun "webui_start_server" :string
  (window size-t)
  (content :string))

(defcfun "webui_open_url" :void
  (url :string))

;;; --- Wait / close ---

(defcfun "webui_wait" :void)

(defcfun "webui_close" :void
  (window size-t))

(defcfun "webui_close_client" :void
  (event :pointer))

(defcfun "webui_minimize" :void
  (window size-t))

(defcfun "webui_maximize" :void
  (window size-t))

(defcfun "webui_destroy" :void
  (window size-t))

(defcfun "webui_exit" :void)

;;; --- Window appearance / control ---

(defcfun "webui_set_high_contrast" :void
  (window size-t)
  (status :bool))

(defcfun "webui_set_resizable" :void
  (window size-t)
  (status :bool))

(defcfun "webui_is_high_contrast" :bool)

(defcfun "webui_set_center" :void
  (window size-t))

(defcfun "webui_set_minimum_size" :void
  (window size-t)
  (width :unsigned-int)
  (height :unsigned-int))

(defcfun "webui_set_event_blocking" :void
  (window size-t)
  (status :bool))

(defcfun "webui_set_frameless" :void
  (window size-t)
  (status :bool))

(defcfun "webui_set_transparent" :void
  (window size-t)
  (status :bool))

(defcfun "webui_set_custom_parameters" :void
  (window size-t)
  (params :string))

;;; --- File handler / root folder ---

(defcfun "webui_set_root_folder" :bool
  (window size-t)
  (path :string))

(defcfun "webui_set_default_root_folder" :bool
  (path :string))

(defcfun "webui_set_file_handler" :void
  (window size-t)
  (handler :pointer))

(defcfun "webui_set_file_handler_window" :void
  (window size-t)
  (handler :pointer))

(defcfun "webui_set_browser_folder" :void
  (path :string))

;;; --- Window properties ---

(defcfun "webui_is_shown" :bool
  (window size-t))

(defcfun "webui_set_timeout" :void
  (second size-t))

(defcfun "webui_set_icon" :void
  (window size-t)
  (icon :string)
  (icon-type :string))

;;; --- Encode / decode / memory ---

(defun webui-encode (str)
  "Base64-encode STR. Frees the C buffer returned by webui_encode."
  (let ((ptr (foreign-funcall "webui_encode" :string str :pointer)))
    (unless (null-pointer-p ptr)
      (unwind-protect (foreign-string-to-lisp ptr)
        (webui-free ptr)))))

(defun webui-decode (str)
  "Decode a Base64-encoded STR. Frees the C buffer returned by webui_decode."
  (let ((ptr (foreign-funcall "webui_decode" :string str :pointer)))
    (unless (null-pointer-p ptr)
      (unwind-protect (foreign-string-to-lisp ptr)
        (webui-free ptr)))))

(defcfun "webui_free" :void
  (ptr :pointer))

(defcfun "webui_malloc" :pointer
  (size size-t))

(defcfun "webui_memcpy" :void
  (dest :pointer)
  (src :pointer)
  (count size-t))

;;; --- Raw data ---

(defcfun "webui_send_raw" :void
  (window size-t)
  (func :string)
  (raw :pointer)
  (size size-t))

(defcfun "webui_send_raw_client" :void
  (event :pointer)
  (func :string)
  (raw :pointer)
  (size size-t))

;;; --- Window settings ---

(defcfun "webui_set_hide" :void
  (window size-t)
  (status :bool))

(defcfun "webui_set_size" :void
  (window size-t)
  (width :unsigned-int)
  (height :unsigned-int))

(defcfun "webui_set_position" :void
  (window size-t)
  (x :unsigned-int)
  (y :unsigned-int))

(defcfun "webui_set_profile" :void
  (window size-t)
  (name :string)
  (path :string))

(defcfun "webui_set_proxy" :void
  (window size-t)
  (proxy-server :string))

(defcfun "webui_get_url" :string
  (window size-t))

(defcfun "webui_set_public" :void
  (window size-t)
  (status :bool))

;;; --- Navigation ---

(defcfun "webui_navigate" :void
  (window size-t)
  (url :string))

(defcfun "webui_navigate_client" :void
  (event :pointer)
  (url :string))

;;; --- Cleanup ---

(defcfun "webui_clean" :void)

(defcfun "webui_delete_all_profiles" :void)

(defcfun "webui_delete_profile" :void
  (window size-t))

;;; --- Process IDs ---

(defcfun "webui_get_parent_process_id" size-t
  (window size-t))

(defcfun "webui_get_child_process_id" size-t
  (window size-t))

;;; --- Networking ---

(defcfun "webui_set_port" :bool
  (window size-t)
  (port size-t))

(defcfun "webui_get_port" size-t
  (window size-t))

(defcfun "webui_get_free_port" size-t)

;;; --- Config / TLS / runtime / logging ---

(defcfun "webui_set_config" :void
  (option :int)
  (status :bool))

(defcfun "webui_set_tls_certificate" :bool
  (cert-pem :string)
  (private-key-pem :string))

(defcfun "webui_set_runtime" :void
  (window size-t)
  (runtime size-t))

(defcfun "webui_get_mime_type" :string
  (file :string))

;;; --- JavaScript ---

(defcfun "webui_run" :void
  (window size-t)
  (script :string))

(defcfun "webui_run_client" :void
  (event :pointer)
  (script :string))

(defun webui-script (window script &key (timeout 0) (max-len 1024))
  "Run JavaScript and get the response back.
timeout is in seconds; 0 means wait forever (matches webui default)."
  (with-foreign-pointer-as-string (result max-len)
    (unless (foreign-funcall "webui_script"
                             size-t window
                             :string script
                             size-t timeout
                             :pointer result
                             size-t max-len
                             :bool)
      (error "Failed to run script: ~a" script))))

(defun webui-script-client (event script &key (timeout 0) (max-len 1024))
  "Run JavaScript on a single client and get the response back.
timeout is in seconds; 0 means wait forever."
  (with-foreign-pointer-as-string (result max-len)
    (unless (foreign-funcall "webui_script_client"
                             :pointer event
                             :string script
                             size-t timeout
                             :pointer result
                             size-t max-len
                             :bool)
      (error "Failed to run script: ~a" script))))

(defcfun "webui_get_count" size-t
  (event :pointer))

(defcfun "webui_get_int_at" :int64
  (event :pointer)
  (index size-t))

(defcfun "webui_get_int" :int64
  (event :pointer))

(defcfun "webui_get_float_at" :double
  (event :pointer)
  (index size-t))

(defcfun "webui_get_float" :double
  (event :pointer))

(defcfun "webui_get_string_at" :string
  (event :pointer)
  (index size-t))

(defcfun "webui_get_string" :string
  (event :pointer))

(defcfun "webui_get_bool_at" :bool
  (event :pointer)
  (index size-t))

(defcfun "webui_get_bool" :bool
  (event :pointer))

(defcfun "webui_get_size_at" size-t
  (event :pointer)
  (index size-t))

(defcfun "webui_get_size" size-t
  (event :pointer))

(defcfun "webui_return_int" :void
  (event :pointer)
  (n :int64))

(defcfun "webui_return_float" :void
  (event :pointer)
  (f :double))

(defcfun "webui_return_string" :void
  (event :pointer)
  (str :string))

(defcfun "webui_return_bool" :void
  (event :pointer)
  (bool :bool))

;;; --- Error handling ---

(defcfun "webui_get_last_error_number" size-t)

(defcfun "webui_get_last_error_message" :string)

;;; --- Wrapper interface ---

(defvar *interface-callbacks* (make-hash-table))

(defcallback %interface-trampoline :void ((window size-t)
                                          (event-number size-t)
                                          (element :pointer)
                                          (bind-id size-t)
                                          (client-id size-t))
  (let ((fn (gethash bind-id *interface-callbacks*)))
    (when fn (funcall fn window event-number element bind-id client-id))))

(defun webui-interface-bind (window element func)
  (let ((id (foreign-funcall "webui_interface_bind"
                             size-t window
                             :string element
                             :pointer (callback %interface-trampoline)
                             size-t)))
    (setf (gethash id *interface-callbacks*) func)
    id))

(defcfun "webui_interface_get_context" :pointer
  (window size-t)
  (event-number size-t))

(defcfun "webui_interface_set_response" :void
  (window size-t)
  (event-number size-t)
  (response :string))

(defcfun "webui_interface_is_app_running" :bool)

(defcfun "webui_interface_get_window_id" size-t
  (window size-t))

(defcfun "webui_interface_get_string_at" :string
  (window size-t)
  (event-number size-t)
  (index size-t))

(defcfun "webui_interface_get_int_at" :int64
  (window size-t)
  (event-number size-t)
  (index size-t))

(defcfun "webui_interface_get_float_at" :double
  (window size-t)
  (event-number size-t)
  (index size-t))

(defcfun "webui_interface_get_bool_at" :bool
  (window size-t)
  (event-number size-t)
  (index size-t))

(defcfun "webui_interface_get_size_at" size-t
  (window size-t)
  (event-number size-t)
  (index size-t))

(defcfun "webui_interface_show_client" :bool
  (window size-t)
  (event-number size-t)
  (content :string))

(defcfun "webui_interface_close_client" :void
  (window size-t)
  (event-number size-t))

(defcfun "webui_interface_send_raw_client" :void
  (window size-t)
  (event-number size-t)
  (func :string)
  (raw :pointer)
  (size size-t))

(defcfun "webui_interface_navigate_client" :void
  (window size-t)
  (event-number size-t)
  (url :string))

(defcfun "webui_interface_run_client" :void
  (window size-t)
  (event-number size-t)
  (script :string))

(defun webui-interface-script-client (window event-number script &key (timeout 0) (max-len 1024))
  "Run JavaScript on a single client via the interface API and get the response back.
timeout is in seconds; 0 means wait forever."
  (with-foreign-pointer-as-string (result max-len)
    (unless (foreign-funcall "webui_interface_script_client"
                             size-t window
                             size-t event-number
                             :string script
                             size-t timeout
                             :pointer result
                             size-t max-len
                             :bool)
      (error "Failed to run script: ~a" script))))

(defcfun "webui_interface_set_response_file_handler" :void
  (window size-t)
  (response :pointer)
  (length :int))
