;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :hemlock.command)

;;; Tree-sitter integration.
;;;
;;; Threading model:
;;;   Command thread — owns all Lisp data structures (buffer, line-plist, marks)
;;;     On every buffer modification: marks buffer dirty via *ts-text-change-hook*
;;;     In ts-ensure-colors-for-windows (called from redisplay-loop):
;;;       Phase 1 — dispatches parse jobs for dirty buffers (snapshots text, sends to actor)
;;;       Phase 2 — applies completed parse results from actor back to line-plist
;;;
;;;   Actor thread (pinned sento) — owns all C tree-sitter objects
;;;     Receives :parse messages, calls ts-worker-parse, stores results in pending-highlights
;;;     Receives :cleanup messages, frees C resources via ts-free-state-resources
;;;
;;; No C tree-sitter calls happen on the command thread.
;;; ts-store-colors-on-lines always runs on the command thread.
;;; The actor thread writes only to ts-buffer-state slots it owns
;;; (pending-highlights, pending-tick, parsing-p).

(defvar *ts-language* nil)
(defvar *ts-query* nil)
(defvar *ts-initialized* nil)
(defvar *ts-init-failed* nil)

(defvar *ts-highlights-path*
  (merge-pathnames "src/tree-sitter-cl/highlights.scm"
                   hemlock-system:*hemlock-base-directory*))

;;; Per-buffer parse state.  parser-ptr and tree-ptr are owned exclusively
;;; by the actor thread and must never be touched from the command thread.
(defstruct ts-buffer-state
  (parser-ptr nil)       ; C parser — actor thread only
  (tree-ptr   nil)       ; C tree  — actor thread only
  (tick       -1)
  (parsing-p  nil)
  (alive-p    t)
  (pending-highlights nil)
  (pending-tick       nil))

(defvar *ts-buffer-states* (make-hash-table :test #'eq))
(defvar *ts-last-error* nil)

;;; Sento actor infrastructure.
(defvar *ts-actor-system* nil "Sento actor system for tree-sitter background work.")
(defvar *ts-actor*        nil "Pinned sento actor — ALL C tree-sitter calls run here.")

;;; Buffers modified since their last parse dispatch.
;;; Written and read exclusively on the command thread.
(defvar *ts-dirty-buffers* nil)

;;; Self-pipe: actor thread writes a byte here when a parse completes,
;;; waking up sb-sys:serve-event so the main loop picks up pending highlights
;;; without waiting for the next keypress.
(defvar *ts-wakeup-read-fd*  nil)
(defvar *ts-wakeup-write-fd* nil)
(defvar *ts-wakeup-handler*  nil)

(defun ts-setup-wakeup-pipe ()
  "Create a self-pipe and register the read end with sb-sys:add-fd-handler."
  (when *ts-wakeup-handler*
    (ignore-errors (sb-sys:remove-fd-handler *ts-wakeup-handler*))
    (setf *ts-wakeup-handler* nil))
  (when *ts-wakeup-read-fd*
    (ignore-errors (sb-posix:close *ts-wakeup-read-fd*))
    (ignore-errors (sb-posix:close *ts-wakeup-write-fd*))
    (setf *ts-wakeup-read-fd* nil *ts-wakeup-write-fd* nil))
  (multiple-value-bind (rfd wfd) (sb-posix:pipe)
    (setf *ts-wakeup-read-fd* rfd
          *ts-wakeup-write-fd* wfd)
    (setf *ts-wakeup-handler*
          (sb-sys:add-fd-handler
           rfd :input
           (let ((drain-buf (make-array 64 :element-type '(unsigned-byte 8))))
             (lambda (fd)
               (declare (ignore fd))
               ;; Drain all pending bytes; content doesn't matter.
               (ignore-errors
                 (sb-sys:with-pinned-objects (drain-buf)
                   (sb-unix:unix-read *ts-wakeup-read-fd*
                                      (sb-sys:vector-sap drain-buf) 64)))))))))

(defvar *ts-wakeup-buf* (make-array 1 :element-type '(unsigned-byte 8) :initial-element 0)
  "Reusable byte buffer for wakeup pipe writes.")

(defun ts-wake-up-main-thread ()
  "Write a byte to the wakeup pipe from the actor thread, unblocking serve-event."
  (when *ts-wakeup-write-fd*
    (ignore-errors
      (sb-sys:with-pinned-objects (*ts-wakeup-buf*)
        (sb-unix:unix-write *ts-wakeup-write-fd*
                            (sb-sys:vector-sap *ts-wakeup-buf*) 0 1)))))


(defun ts-capture-name-to-font (name)
  (cond
   ((or (string= name "keyword")
        (string= name "keyword.function"))
    5)
   ((or (string= name "function")
        (string= name "function.definition"))
    6)
   ((string= name "function.call")
    4)
   ((or (string= name "string")
        (string= name "character"))
    2)
   ((or (string= name "number")
        (string= name "constant")
        (string= name "constant.builtin"))
    3)
   ((or (string= name "comment")
        (string= name "comment.block"))
    1)
   ((or (string= name "variable")
        (string= name "variable.parameter"))
    1)
   ((string= name "operator")
    6)
   ((or (string= name "string.escape")
        (string= name "string.special"))
    2)
   ((string= name "type")
    4)
   (t nil)))

(defun ts-actor-receive (msg)
  (case (car msg)
    (:parse
     (destructuring-bind (_ state tick text) msg
       (declare (ignore _))
       (ts-worker-parse state tick text)))
    (:cleanup
     (destructuring-bind (_ state) msg
       (declare (ignore _))
       (ts-free-state-resources state)))
    (t
     (setf *ts-last-error*
           (format nil "ts-actor: unknown message ~S" (car msg))))))

(defun ts-try-init ()
  (when *ts-init-failed* (return-from ts-try-init nil))
  (when *ts-initialized* (return-from ts-try-init t))
  (handler-case
      (progn
        (unless (tree-sitter/ffi:tree-sitter-available-p)
          (setf *ts-init-failed* t)
          (return-from ts-try-init nil))
        (tree-sitter/ffi:ensure-ts-wrapper-loaded)
        (unless tree-sitter/ffi::*ts-wrapper-loaded*
          (setf *ts-init-failed* t)
          (return-from ts-try-init nil))
        (setf *ts-language*
              (tree-sitter/language:load-language-from-system "commonlisp"))
        (let ((source (with-open-file (s *ts-highlights-path* :direction :input)
                        (let ((buf (make-string (file-length s))))
                          (read-sequence buf s)
                          buf))))
          (setf *ts-query*
                (tree-sitter/query:query-compile *ts-language* source)))
        ;; Create actor system and pinned actor BEFORE marking initialized.
        (setf *ts-actor-system*
              (make-actor-system '(:scheduler (:resolution 100 :max-size 10))))
        (setf *ts-actor*
              (actor-of *ts-actor-system*
                        :name "hemlock-ts"
                        :dispatcher :pinned
                        :receive #'ts-actor-receive))
        ;; Self-pipe for waking up the main event loop on parse completion.
        (ts-setup-wakeup-pipe)
        ;; Only true when actor is fully ready.
        (setf *ts-initialized* t)
        ;; Wire modification hook so buffer changes trigger parse scheduling.
        (setf *ts-text-change-hook* #'ts-on-buffer-modified)
        ;; Kick off initial parses for any already-open Lisp buffers.
        (dolist (buf *buffer-list*)
          (when (ts-lisp-buffer-p buf)
            (pushnew buf *ts-dirty-buffers* :test #'eq)))
        t)
    (error (e)
      (setf *ts-last-error* e *ts-init-failed* t)
      nil)))

;;; Create state without any C calls — safe to call from command thread.
(defun ts-get-or-create-state (buffer)
  (or (gethash buffer *ts-buffer-states*)
      (let ((state (make-ts-buffer-state)))
        (setf (gethash buffer *ts-buffer-states*) state)
        state)))

(defun ts-buffer-text (buffer)
  (let ((parts nil)
        (line (mark-line (buffer-start-mark buffer))))
    (loop while line
          do (push (line-string line) parts)
          (setf line (line-next line)))
    (format nil "~{~A~^~%~}" (nreverse parts))))

(defun ts-build-line-offsets (text)
  (let* ((bytes (babel:string-to-octets text :encoding :utf-8))
         (total (length bytes))
         (offsets (make-array 1 :adjustable t :fill-pointer 0)))
    (vector-push-extend 0 offsets)
    (loop for i from 0 below total
          when (= (aref bytes i) (char-code #\Newline))
          do (vector-push-extend (1+ i) offsets))
    (values offsets total)))

(defun ts-byte-to-line-col (byte-pos offsets)
  (let ((lo 0)
        (hi (1- (length offsets))))
    (loop while (<= lo hi)
          for mid = (ash (+ lo hi) -1)
          do (cond
              ((> (aref offsets mid) byte-pos) (setf hi (1- mid)))
              ((and (< mid (1- (length offsets)))
                    (<= (aref offsets (1+ mid)) byte-pos))
               (setf lo (1+ mid)))
              (t (return-from ts-byte-to-line-col
                   (values mid (- byte-pos (aref offsets mid)))))))
    (values lo (- byte-pos (aref offsets lo)))))

;;; Called only from the actor thread.
(defun ts-compute-all-highlights (parser-ptr text)
  (multiple-value-bind (offsets _total-bytes)
      (ts-build-line-offsets text)
    (declare (ignore _total-bytes))
    (let* ((tree-ptr (tree-sitter/ffi:ts-parser-parse-string
                      parser-ptr
                      (cffi:null-pointer)
                      text))
           (result (make-hash-table)))
      (when (and tree-ptr (not (cffi:null-pointer-p tree-ptr)))
        (let ((node-buf (cffi:foreign-alloc '(:struct tree-sitter/ffi:ts-node-raw)))
              (cursor   (tree-sitter/ffi:ts-query-cursor-new))
              (query-ptr     (tree-sitter/types:ts-query-ptr *ts-query*))
              (capture-names (tree-sitter/types:ts-query-capture-names *ts-query*)))
          (unwind-protect
              (progn
                (tree-sitter/ffi:ts-tree-root-node tree-ptr node-buf)
                (tree-sitter/ffi:ts-query-cursor-exec cursor query-ptr node-buf)
                (cffi:with-foreign-objects ((match-ptr  '(:struct tree-sitter/ffi:ts-query-match))
                                            (cap-idx-ptr :uint32))
                  (loop while (tree-sitter/ffi:ts-query-cursor-next-capture
                                cursor match-ptr cap-idx-ptr)
                        do (let* ((captures-ptr
                                   (cffi:foreign-slot-value
                                    match-ptr '(:struct tree-sitter/ffi:ts-query-match)
                                    'tree-sitter/ffi::captures))
                                  (cap-idx  (cffi:mem-ref cap-idx-ptr :uint32))
                                  (cap-ptr  (cffi:mem-aptr captures-ptr
                                                            '(:struct tree-sitter/ffi:ts-query-capture)
                                                            cap-idx))
                                  (node-ptr (cffi:foreign-slot-pointer
                                             cap-ptr '(:struct tree-sitter/ffi:ts-query-capture)
                                             'tree-sitter/ffi::node))
                                  (index    (cffi:foreign-slot-value
                                             cap-ptr '(:struct tree-sitter/ffi:ts-query-capture)
                                             'tree-sitter/ffi::index))
                                  (name     (aref capture-names index))
                                  (font     (ts-capture-name-to-font name)))
                             (when font
                               (let ((sbyte (tree-sitter/ffi:ts-node-start-byte node-ptr))
                                     (ebyte (tree-sitter/ffi:ts-node-end-byte   node-ptr)))
                                 (multiple-value-bind (start-line start-col)
                                     (ts-byte-to-line-col sbyte offsets)
                                   (multiple-value-bind (end-line end-col)
                                       (ts-byte-to-line-col ebyte offsets)
                                     (cond
                                       ((= start-line end-line)
                                        (push (list start-col end-col font)
                                              (gethash start-line result)))
                                       (t
                                        (push (list start-col 10000 font)
                                              (gethash start-line result))
                                        (loop for mid from (1+ start-line) below end-line
                                              do (push (list 0 10000 font)
                                                       (gethash mid result)))
                                        (push (list 0 end-col font)
                                              (gethash end-line result))))))))))))
            (tree-sitter/ffi:ts-query-cursor-delete cursor)
            (cffi:foreign-free node-buf))))
      (values result tree-ptr))))

(defun ts-normalize-ranges (ranges line-len)
  (when (zerop line-len)
    (return-from ts-normalize-ranges nil))
  (let ((slots (make-array line-len :initial-element 0)))
    (dolist (r ranges)
      (let ((s (max 0 (min (first r) line-len)))
            (e (max 0 (min (second r) line-len)))
            (f (third r)))
        (loop for i from s below e
              do (setf (aref slots i) f))))
    (let ((result nil)
          (i 0))
      (loop while (< i line-len)
            for f = (aref slots i)
            do (if (plusp f)
                   (let ((start i))
                     (loop while (and (< i line-len) (= (aref slots i) f))
                           do (incf i))
                     (push (list start i f) result))
                 (incf i)))
      (nreverse result))))

(defun ts-store-colors-on-lines (buffer highlight-table)
  (let ((line (mark-line (buffer-start-mark buffer)))
        (line-num 0))
    (loop while line
          do (let* ((ranges     (gethash line-num highlight-table))
                    (new-colors (if ranges
                                    (ts-normalize-ranges ranges (line-length line))
                                  nil)))
               (unless (equal (getf (line-plist line) 'syntax-colors) new-colors)
                 (setf (getf (line-plist line) 'syntax-colors) new-colors)))
          (setf line (line-next line))
          (incf line-num))))

;;; Free C resources — MUST be called from the actor thread.
(defun ts-free-state-resources (state)
  (let ((tree (ts-buffer-state-tree-ptr state)))
    (when (and tree (not (cffi:null-pointer-p tree)))
      (tree-sitter/ffi:ts-tree-delete tree)
      (setf (ts-buffer-state-tree-ptr state) nil)))
  (let ((parser (ts-buffer-state-parser-ptr state)))
    (when (and parser (not (cffi:null-pointer-p parser)))
      (tree-sitter/ffi:ts-parser-delete parser)
      (setf (ts-buffer-state-parser-ptr state) nil))))

;;; Send a cleanup message to the actor so C resources are freed on the actor thread.
(defun ts-cleanup-buffer (buffer)
  (let ((state (gethash buffer *ts-buffer-states*)))
    (when state
      (setf (ts-buffer-state-alive-p state) nil)
      (remhash buffer *ts-buffer-states*)
      (when *ts-actor*
        (! *ts-actor* (list :cleanup state)))))
  (setf *ts-dirty-buffers* (delete buffer *ts-dirty-buffers* :test #'eq)))

;;; The actual parse job — runs exclusively on the actor thread.
(defun ts-worker-parse (state tick text)
  (unwind-protect
      (progn
        (when (null (ts-buffer-state-parser-ptr state))
          (let ((parser-ptr (tree-sitter/ffi:ts-parser-new)))
            (when (or (null parser-ptr) (cffi:null-pointer-p parser-ptr))
              (return-from ts-worker-parse nil))
            (tree-sitter/ffi:ts-parser-set-language
             parser-ptr
             (tree-sitter/types:ts-language-ptr *ts-language*))
            (setf (ts-buffer-state-parser-ptr state) parser-ptr)))
        (let ((old-tree (ts-buffer-state-tree-ptr state)))
          (setf (ts-buffer-state-tree-ptr state) nil)
          (handler-case
              (multiple-value-bind (highlights new-tree-ptr)
                  (ts-compute-all-highlights (ts-buffer-state-parser-ptr state) text)
                (when (ts-buffer-state-alive-p state)
                  (setf (ts-buffer-state-pending-highlights state) highlights
                        (ts-buffer-state-pending-tick        state) tick
                        (ts-buffer-state-tree-ptr            state) new-tree-ptr)
                  ;; Wake up the main thread so it applies highlights immediately.
                  (ts-wake-up-main-thread))
                (when (and (not (ts-buffer-state-alive-p state))
                           new-tree-ptr
                           (not (cffi:null-pointer-p new-tree-ptr)))
                  (tree-sitter/ffi:ts-tree-delete new-tree-ptr))
                (when (and old-tree (not (cffi:null-pointer-p old-tree)))
                  (tree-sitter/ffi:ts-tree-delete old-tree)))
            (error (e)
              (setf *ts-last-error* e)
              (when (and old-tree (not (cffi:null-pointer-p old-tree)))
                (tree-sitter/ffi:ts-tree-delete old-tree))
              (setf (ts-buffer-state-tree-ptr state) nil)))))
    (setf (ts-buffer-state-parsing-p state) nil)))

(defun ts-invalidate-window-dis-lines (window)
  (when (window-first-line window)
    (loop for dl = (cdr (window-first-line window)) then (cdr dl)
          until (or (null dl) (eq dl the-sentinel))
          do (setf (dis-line-old-chars (car dl)) :ts-stale)))
  ;; Force maybe-update-window-image to re-render this window.
  (setf (window-tick window) (1- (buffer-modified-tick (window-buffer window)))))

(defun ts-lisp-buffer-p (buffer)
  (let ((mode (buffer-major-mode buffer)))
    (and mode (string= mode "Lisp"))))

;;; Called from *ts-text-change-hook* (via invoke-modifying-buffer) on the
;;; command thread after every text modification.  Just marks the buffer dirty.
(defun ts-on-buffer-modified (buffer)
  (when (and *ts-initialized* (not *ts-init-failed*) (ts-lisp-buffer-p buffer))
    (pushnew buffer *ts-dirty-buffers* :test #'eq)))

;;; Snapshot text and dispatch a parse job to the actor.
;;; If a parse is already in-flight, re-queue the buffer so the modified
;;; text is parsed once the current job completes.  Command thread only.
(defun ts-dispatch-parse (buffer)
  (let ((state (ts-get-or-create-state buffer)))
    (if (ts-buffer-state-parsing-p state)
        (pushnew buffer *ts-dirty-buffers* :test #'eq)
        (progn
          (setf (ts-buffer-state-parsing-p state) t)
          (! *ts-actor* (list :parse state
                              (buffer-modified-tick buffer)
                              (ts-buffer-text buffer)))))))

;;; Called from redisplay-loop (twice: redisplay-loop and redisplay-windows-from-mark).
;;; Phase 1: dispatch parses for dirty buffers.
;;; Phase 2: apply completed parse results to window dis-lines.
(defun ts-ensure-colors-for-windows ()
  ;; Surface any error from a previous parse or init failure.
  (when *ts-last-error*
    (let ((e *ts-last-error*))
      (setf *ts-last-error* nil)
      (ignore-errors (hemlock::message "ts: ~A" e))))
  (when (and *ts-initialized* (not *ts-init-failed*))
    (handler-case
        (progn
          ;; Phase 1: dispatch dirty buffers for parsing.
          ;; Also catch Lisp buffers that became visible (or had Lisp mode
          ;; activated) without triggering a text modification, e.g. opening
          ;; a file then calling Lisp mode manually.
          (dolist (win *window-list*)
            (let ((buf (window-buffer win)))
              (when (and (ts-lisp-buffer-p buf)
                         (not (gethash buf *ts-buffer-states*)))
                (pushnew buf *ts-dirty-buffers* :test #'eq))))
          (let ((dirty *ts-dirty-buffers*))
            (setf *ts-dirty-buffers* nil)
            (dolist (buf dirty)
              (when (buffer-name buf)   ; still alive?
                (ts-dispatch-parse buf))))
          ;; Phase 2: apply completed parse results to windows.
          (dolist (win *window-list*)
            (let ((buf (window-buffer win)))
              (when (and buf (ts-lisp-buffer-p buf))
                (let ((state (gethash buf *ts-buffer-states*)))
                  (when (and state
                             (ts-buffer-state-pending-highlights state)
                             (not (ts-buffer-state-parsing-p state)))
                    (let ((highlights (ts-buffer-state-pending-highlights state))
                          (tick       (ts-buffer-state-pending-tick state)))
                      (setf (ts-buffer-state-pending-highlights state) nil)
                      (ts-store-colors-on-lines buf highlights)
                      (setf (ts-buffer-state-tick state) tick))
                    (ts-invalidate-window-dis-lines win)))))))
      (error (e) (setf *ts-last-error* e)))))

(eval-when (:load-toplevel :execute)
  (defvar hemlock::*after-editor-initializations-funs* nil)
  (push (lambda () (ts-try-init)) hemlock::*after-editor-initializations-funs*))
