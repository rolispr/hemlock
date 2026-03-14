;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;;
;;; This file implements session streams.
;;;
;;; A session stream is a bidirectional stream which uses remote
;;; function calls to interact with a Hemlock session buffer. That
;;; is: the code in this file is executed on the agent side.
;;;
;;;

(in-package :hemlock)


;;;; Ts-streams.

(defconstant session-stream-output-buffer-size 512)

(defclass session-stream (hi::trivial-gray-stream-mixin
                     hi::fundamental-character-output-stream
                     hi::fundamental-character-input-stream)
  ((wire
    :initarg  :wire
    :initform nil
    :accessor session-stream-wire)

   (session
    :initarg  :session
    :initform nil
    :accessor session-stream-session)

   (output-buffer
    :initarg  :output-buffer
    :initform (make-string session-stream-output-buffer-size)
    :accessor session-stream-output-buffer
    :type     simple-string)

   (output-buffer-index
    :initarg  :output-buffer-index
    :initform 0
    :accessor session-stream-output-buffer-index
    :type     fixnum)

   (char-pos
    :initarg  :char-pos
    :initform 0
    :accessor session-stream-char-pos
    :type     fixnum
    :documentation "The current output character position on the line, returned by the :CHARPOS method.")

   (line-length
    :initarg :line-length
    :initform 80
    :accessor session-stream-line-length
    :documentation "The current length of a line of output.  Returned by STREAM-LINE-LENGTH method.")

   (current-input
    :initarg :current-input
    :initform nil
    :accessor session-stream-current-input
    :type list
    :documentation "This is a list of strings and stream-commands whose order manifests the
                    input provided by remote procedure calls into the agent of
                    TS-STREAM-ACCEPT-INPUT.")

   (input-read-index
    :initarg :input-read-index
    :initform 0
    :accessor session-stream-input-read-index
    :type fixnum)))

(defun make-session-stream (wire session)
  (make-instance 'session-stream
                 :wire wire
                 :session session))


;;;; Conditions.

(define-condition unexpected-stream-command (error)
  ;; Context is a string to be plugged into the report text.
  ((context :reader unexpected-stream-command-context :initarg :context))
  (:report (lambda (condition stream)
             (format stream "~&Unexpected stream-command while ~A."
                     (unexpected-stream-command-context condition)))))



;;;; Editor remote calls into agent.

;;; TS-STREAM-ACCEPT-INPUT -- Internal Interface.
;;;
;;; The editor calls this remotely in the agent to indicate that the user has
;;; provided input.  Input is a string, symbol, or list.  If it is a list, the
;;; the CAR names the command, and the CDR is the arguments.
;;;
(defun session-stream-accept-input (remote input)
  (let ((stream (hemlock.wire:remote-object-value remote)))
    (setf (session-stream-current-input stream)
          (nconc (session-stream-current-input stream)
                 (list (etypecase input
                         (string
                          (let ((newline
                                 (position #\newline input :from-end t)))
                            (setf (session-stream-char-pos stream)
                                  (if newline
                                      (- (length input) newline 1)
                                      (length input)))
                            input))
                         #+NILGB
                         (cons
                          (ext:make-stream-command (car input)
                                                   (cdr input)))
                         #+NILGB
                         (symbol
                          (ext:make-stream-command input)))))))
  nil)

;;; TS-STREAM-SET-LINE-LENGTH -- Internal Interface.
;;;
;;; This function is called by the editor to indicate that the line-length for
;;; a TS stream should now be Length.
;;;
(defun session-stream-set-line-length (remote length)
  (let ((stream (hemlock.wire:remote-object-value remote)))
    (setf (session-stream-line-length stream) length)))



;;;; Stream methods.

;;; %TS-STREAM-LISTEN -- Internal.
;;;
;;; Determine if there is any input available.  If we don't think so, process
;;; all pending events, and look again.
;;;
(defun %session-stream-listen (stream)
  (flet ((check ()
           (loop
              (let* ((current (session-stream-current-input stream))
                     (first (first current)))
                (cond ((null current)
                       (return nil))
                      #+NILGB
                      ((ext:stream-command-p first)
                       (return t))
                      ((>= (session-stream-input-read-index stream)
                           (length (the simple-string first)))
                       (pop (session-stream-current-input stream))
                       (setf (session-stream-input-read-index stream) 0))
                      (t
                       (return t)))))))
    (or (check)
        #+(or)
        (progn
          (dispatch-events-no-hang)
          (check)))))

(defmethod hi::stream-listen ((stream session-stream))
  (%session-stream-listen stream))

;;; %TS-STREAM-IN -- Internal.
;;;
;;; The READ-CHAR stream method.
;;;
(defmethod hi::stream-read-char ((stream session-stream))
  (hi::stream-force-output stream)
  (wait-for-session-input stream)
  (let ((first (first (session-stream-current-input stream))))
    (etypecase first
      (string
       (prog1 (schar first (session-stream-input-read-index stream))
         (incf (session-stream-input-read-index stream))))
      #+NILGB
      (ext:stream-command
       (error 'unexpected-stream-command
              :context "in the READ-CHAR method")))))

(defmethod hi::stream-read-char-no-hang ((stream session-stream))
  (cond
    ((%session-stream-listen stream)
     (hi::stream-force-output stream)
     (let ((first (first (session-stream-current-input stream))))
       (etypecase first
         (string
          (prog1 (schar first (session-stream-input-read-index stream))
            (incf (session-stream-input-read-index stream))))
         #+NILGB
         (ext:stream-command
          (error 'unexpected-stream-command
                 :context "in the READ-CHAR method")))))
    (t
     ;; not :eof!
     nil)))

;;; %TS-STREAM-READ-LINE -- Internal.
;;;
;;; The READ-LINE stream method.  Note: here we take advantage of the fact that
;;; newlines will only appear at the end of strings.
;;;

#+(or)
(defmethod stream-read-line (stream)
  (macrolet
      ((next-str ()
         '(progn
           (wait-for-session-input stream)
           (let ((first (first (session-stream-current-input stream))))
             (etypecase first
               (string
                (prog1 (if (zerop (session-stream-input-read-index stream))
                           (pop (session-stream-current-input stream))
                           (subseq (pop (session-stream-current-input stream))
                                   (session-stream-input-read-index stream)))
                  (setf (session-stream-input-read-index stream) 0)))
               #+NILGB
               (ext:stream-command
                (error 'unexpected-stream-command
                       :context "in the READ-CHAR method")))))))
    (do ((result (next-str) (concatenate 'simple-string result (next-str))))
        ((char= (schar result (1- (length result))) #\newline)
         (values (subseq result 0 (1- (length result)))
                 nil))
      (declare (simple-string result)))))

;;; WAIT-FOR-TYPESCRIPT-INPUT -- Internal.
;;;
;;; Keep calling server until some input shows up.
;;;
(defun wait-for-session-input (stream)
  (unless (%session-stream-listen stream)
    (let ((wire (session-stream-wire stream))
          (ts (session-stream-session stream)))
      #+(or)
      (progn
        (hemlock.wire:remote wire (session-buffer-ask-for-input ts))
        (hemlock.wire:wire-force-output wire))
      (loop until (%session-stream-listen stream)
            do (dispatch-events)))))

;;; %TS-STREAM-FLSBUF --- internal.
;;;
;;; Flush the output buffer associated with stream.  This should only be used
;;; inside a without-interrupts and without-gcing.
;;;
(defun %session-stream-flsbuf (stream)
  (when (and (session-stream-wire stream)
             (session-stream-output-buffer stream)
             (not (zerop (session-stream-output-buffer-index stream))))
    (hemlock.wire:remote (session-stream-wire stream)
      (session-buffer-output-string
       (session-stream-session stream)
       (subseq (the simple-string (session-stream-output-buffer stream))
               0
               (session-stream-output-buffer-index stream))))
    (setf (session-stream-output-buffer-index stream) 0)))

;;; %TS-STREAM-OUT --- internal.
;;;
;;; Output a single character to stream.
;;;
(defmethod hi::stream-write-char ((stream session-stream) char)
  (declare (base-char char))
  (when (= (session-stream-output-buffer-index stream)
           session-stream-output-buffer-size)
    (%session-stream-flsbuf stream))
  (setf (schar (session-stream-output-buffer stream)
               (session-stream-output-buffer-index stream))
        char)
  (incf (session-stream-output-buffer-index stream))
  (incf (session-stream-char-pos stream))
  (when (= (char-code char)
           (char-code #\Newline))
    (%session-stream-flsbuf stream)
    (setf (session-stream-char-pos stream) 0)
    (hemlock.wire:wire-force-output (session-stream-wire stream)))
  char)

;;; %TS-STREAM-SOUT --- internal.
;;;
;;; Output a string to stream.
;;;
#+(or)
(defmethod hi::stream-write-string ((stream session-stream) string &optional (start 0) (end (length string)))
  ;; This can't be true generally: --GB
  #+NIL (declare (simple-string string))
  (declare (fixnum start end))
  (let ((wire (session-stream-wire stream))
        (newline (position #\Newline string :start start :end end :from-end t))
        (length (- end start)))
    (when wire
      (let ((index (session-stream-output-buffer-index stream)))
        (cond ((> (+ index length)
                  session-stream-output-buffer-size)
               (%session-stream-flsbuf stream)
               (hemlock.wire:remote wire
                                    (session-buffer-output-string (session-stream-session stream)
                                                             (subseq string start end)))
               (when newline
                 (hemlock.wire:wire-force-output wire)))
              (t
               (replace (the simple-string (session-stream-output-buffer stream))
                        string
                        :start1 index
                        :end1 (+ index length)
                        :start2 start
                        :end2 end)
               (incf (session-stream-output-buffer-index stream)
                     length)
               (when newline
                 (%session-stream-flsbuf stream)
                 (hemlock.wire:wire-force-output wire)))))
      (setf (session-stream-char-pos stream)
            (if newline
                (- end newline 1)
                (+ (session-stream-char-pos stream)
                   length))))))

;;; %TS-STREAM-UNREAD -- Internal.
;;;
;;; Unread a single character.
;;;
(defmethod hi::stream-unread-char ((stream session-stream) char)
  (let ((first (first (session-stream-current-input stream))))
    (cond ((and (stringp first)
                (> (session-stream-input-read-index stream) 0))
           (setf (schar first (decf (session-stream-input-read-index stream)))
                 char))
          (t
           (push (string char) (session-stream-current-input stream))
           (setf (session-stream-input-read-index stream) 0)))))

;;; %TS-STREAM-CLOSE --- internal.
;;;
;;; Can't do much, 'cause the wire is shared.
;;;
(defmethod close ((stream session-stream) &key abort)
  (unless abort
    (force-output stream))
  #+NILGB (lisp::set-closed-flame stream)       ;Hugh!? what is that? --GB
  )

;;; %TS-STREAM-CLEAR-INPUT -- Internal.
;;;
;;; Pass the request to the editor and clear any buffered input.
;;;
(defmethod hi::stream-clear-input ((stream session-stream))
  (when (session-stream-wire stream)
    (hemlock.wire:remote-value (session-stream-wire stream)
                               (session-buffer-clear-input (session-stream-session stream))))
  (setf (session-stream-current-input stream) nil
        (session-stream-input-read-index stream) 0))

(defmethod hi::stream-finish-output ((stream session-stream))
  (when (session-stream-wire stream)
    (%session-stream-flsbuf stream)
    ;; Note: for the return value to come back,
    ;; all pending RPCs must have completed.
    ;; Therefore, we know it has synced.
    (hemlock.wire:remote-value (session-stream-wire stream)
                               (session-buffer-finish-output (session-stream-session stream))))
  t)

(defmethod hi::stream-force-output ((stream session-stream))
  (hi::stream-finish-output stream)
  t)

(defmethod hi::stream-line-column ((stream session-stream))
  (session-stream-char-pos stream))

(defmethod hi::stream-line-length ((stream session-stream))
  (session-stream-line-length stream))

#+NILGB ;; -- hmm.
(defmethod interactive-stream-p ((stream session-stream))
  t)

(defmethod hi::stream-clear-output ((stream session-stream))
  (setf (session-stream-output-buffer-index stream) 0))

;;; %TS-STREAM-MISC -- Internal.
;;;
;;; The misc stream method.
;;;
#+NILGB
(defun %session-stream-misc (stream operation &optional arg1 arg2)
  (case operation
    (:get-command
     (wait-for-session-input stream)
     (etypecase (first (session-stream-current-input stream))
       (stream-command
        (setf (session-stream-input-read-index stream) 0)
        (pop (session-stream-current-input stream)))
       (string nil)))))

(defmethod hi::stream-write-sequence
    ((stream session-stream) (seq string) start end &key)
  (loop for i from start below end
        do (write-char (elt seq i) stream)))

(defmethod hi::stream-read-sequence
    ((stream session-stream) (seq string) start end &key)
  (loop for i from start below end
        do (setf (elt seq i) (read-char stream))))

;; $Log: session-stream.lisp,v $
;; Revision 1.1  2004-07-09 13:38:55  gbaumann
;; Initial revision
;;
;; Revision 1.3  2003/08/05 19:51:13  gilbert
;; initial agent lisp support, still not ready for prime time.
;;
;;
