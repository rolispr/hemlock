;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;; Undo system — Emacs-style nil-boundary format with self-insert consolidation,
;;; size limits, and undo-in-region.
;;;
;;; Undo list entry format:
;;;   nil                         — command boundary (group separator)
;;;   (point-position pos)        — restore point to pos
;;;   (insert-string pos text)    — undo of insertion: delete TEXT at POS
;;;   (delete-region pos text)    — undo of deletion: insert TEXT at POS
;;;
;;; Positions are (buffer line-no char-pos) triples.

(in-package :hemlock.command)

;; Unfortunately we need numeric buffer positions. (Hmm, maybe after
;; all RMS has a point?) Anyhow to graft this onto hemlock we define
;; two functions MARK-POSITION and POSTION-MARK to convert and back
;; and fro. Further these new kind of buffer positions are passed
;; around as (buffer line-number character-position) triples.

(defun mark-position (mark)
  (let ((line-no 0)
        (line (mark-line mark)))
    (do ()
        ((null (line-previous line)))
      (incf line-no)
      (setf line (line-previous line)))
    (list (line-buffer (mark-line mark))
          line-no (mark-charpos mark))))

(defun position-mark (buffer line-no char-pos)
  (let ((line (mark-line (buffer-start-mark buffer))))
    (assert line)
    (dotimes (i line-no)
      (let ((next (line-next line)))
        (unless next
          (editor-error "Corrupted undo position, line ~D column ~D"
                        line-no char-pos))
        (setf line next)))
    (mark line char-pos)))

(defun position< (pos1 pos2)
  "T if POS1 is strictly before POS2. Both are (buffer line-no char-pos) triples."
  (or (< (cadr pos1) (cadr pos2))
      (and (= (cadr pos1) (cadr pos2))
           (< (caddr pos1) (caddr pos2)))))

(defun position<= (pos1 pos2)
  (not (position< pos2 pos1)))

;;;; Insertion

(defun update-tag-line-number (mark)
  (let* ((line (mark-line mark))
         (buffer (line-buffer line)))
    (assert buffer)
    (setf (buffer-tag-line-number buffer)
          (min (buffer-tag-line-number buffer)
               (line-number line)))))

;;; Insert can call itself and other functions.  We only want to record
;;; the outermost call.
(defvar *insert-noted-p* nil)

;;; When T, insert-character should attempt to merge with the previous
;;; undo entry rather than pushing a new one (self-insert consolidation).
(defvar *consolidate-insert* nil)

(defun word-break-char-p (char)
  "T if CHAR starts a new word group for undo consolidation."
  (or (char= char #\Space) (char= char #\Newline) (char= char #\Tab)))

(defun insert-adjacent-p (prev-pos prev-text cur-pos)
  "T if CUR-POS is immediately after PREV-TEXT inserted at PREV-POS (single-line only)."
  (and (eq (car prev-pos) (car cur-pos))
       (= (cadr prev-pos) (cadr cur-pos))
       (not (find #\Newline prev-text))
       (= (+ (caddr prev-pos) (length prev-text)) (caddr cur-pos))))

(defmethod insert-character :around (mark character)
  (let ((buffer (line-buffer (mark-line mark))))
    (cond ((and buffer (buffer-undo-p buffer))
           (unless *insert-noted-p*
             (let* ((pos (mark-position mark))
                    (merged nil))
               ;; Try consolidation with previous self-insert entry.
               ;; The hook already pushed (point-position ...) + nil for this command.
               ;; If consolidating, remove those two entries and merge into existing
               ;; insert-string.
               (when *consolidate-insert*
                 (let* ((lst (buffer-undo-list buffer))
                        ;; lst = ((point-position ...) nil (insert-string prev...) ...)
                        (after-header (cddr lst)))
                   (when (and after-header
                              (consp (car after-header))
                              (eq (car (car after-header)) 'insert-string)
                              (not (word-break-char-p character))
                              (insert-adjacent-p (cadr (car after-header))
                                                 (caddr (car after-header))
                                                 pos))
                     ;; Remove the freshly pushed point-position + nil
                     (setf (buffer-undo-list buffer) after-header)
                     ;; Append char to existing entry's text
                     (setf (caddr (car after-header))
                           (concatenate 'string (caddr (car after-header))
                                        (string character)))
                     (setf merged t))))
               (unless merged
                 (push `(insert-string ,pos ,(string character))
                       (buffer-undo-list buffer)))
               (update-tag-line-number mark)))
           (let ((*insert-noted-p* t))
             (call-next-method)))
          (t
           (when buffer
             (update-tag-line-number mark))
           (call-next-method)))))

(defmethod insert-string :around (mark string &optional (start 0) (end (length string)))
  (let ((buffer (line-buffer (mark-line mark))))
    (cond ((and buffer (buffer-undo-p buffer))
           (unless *insert-noted-p*
             (push `(insert-string ,(mark-position mark)
                                   ,(subseq string start end))
                   (buffer-undo-list buffer))
             (update-tag-line-number mark))
           (let ((*insert-noted-p* t))
             (call-next-method)))
          (t
           (when buffer
             (update-tag-line-number mark))
           (call-next-method)))))

(defmethod insert-region :around (mark region)
  (let ((buffer (line-buffer (mark-line mark))))
    (cond ((and buffer (buffer-undo-p buffer))
           (unless *insert-noted-p*
             (push `(insert-string ,(mark-position mark)
                                   ,(region-to-string region))
                   (buffer-undo-list buffer))
             (update-tag-line-number mark))
           (let ((*insert-noted-p* t))
             (call-next-method)))
          (t
           (when buffer
             (update-tag-line-number mark))
           (call-next-method)))))

(defmethod ninsert-region :around (mark region)
  ;; the "n" refers to the region argument.
  (let ((buffer (line-buffer (mark-line mark))))
    (cond ((and buffer (buffer-undo-p buffer))
           (unless *insert-noted-p*
             (push `(insert-string ,(mark-position mark)
                                   ,(region-to-string region))
                   (buffer-undo-list buffer))
             (update-tag-line-number mark))
           (let ((*insert-noted-p* t))
             (call-next-method)))
          (t
           (when buffer
             (update-tag-line-number mark))
           (call-next-method)))))

;;;; Deletion

;; We make delete-characters and delete-region both call off to
;; delete-and-save-region which is the most general method and has the
;; benefit to return the deleted stuff.

(defmethod delete-characters :around (mark &optional (n 1))
  (let ((buffer (line-buffer (mark-line mark))))
    (cond ((and buffer (buffer-undo-p buffer))
           ;; For now delete-characters just calls delete-region in
           ;; any case.  code borrowed from htext4.lisp
           (let* ((line (mark-line mark))
                  (charpos (mark-charpos mark))
                  (length (line-length* line)))
             (declare (ignore length))
             (setf (mark-line *internal-temp-mark*) line
                   (mark-charpos *internal-temp-mark*) charpos)
             (let ((other-mark (character-offset *internal-temp-mark* n)))
               (cond
                 (other-mark
                  (if (< n 0)
                      (setf (region-start *internal-temp-region*) other-mark
                            (region-end *internal-temp-region*) mark)
                      (setf (region-start *internal-temp-region*) mark
                            (region-end *internal-temp-region*) other-mark))
                  (delete-and-save-region *internal-temp-region*)
                  t)
                 (t nil)))))
          (t
           (when buffer
             (update-tag-line-number mark))
           (call-next-method)))))


(defmethod delete-region :around (region)
  ;; Defensive: if a non-region is passed (e.g. due to a bug elsewhere),
  ;; skip silently rather than crashing the editor.
  (unless (regionp region)
    (return-from delete-region nil))
  (let* ((mark (region-start region))
         (buffer (line-buffer (mark-line mark))))
    (cond ((and buffer (buffer-undo-p buffer))
           (delete-and-save-region region))
          (t
           (when buffer
             (update-tag-line-number mark))
           (call-next-method)))))

(defmethod delete-and-save-region :around (region)
  (unless (regionp region)
    (return-from delete-and-save-region nil))
  (let* ((mark (region-start region))
         (buffer (line-buffer (mark-line mark))))
    (cond ((and buffer (buffer-undo-p buffer))
           (let ((pos (mark-position mark))
                 (matter (call-next-method)))
             (push `(delete-region ,pos ,(region-to-string matter))
                   (buffer-undo-list buffer))
             (update-tag-line-number mark)
             matter))
          (t
           (when buffer
             (update-tag-line-number mark))
           (call-next-method)))))

;;;; Size limiting

(defvar *undo-limit* 80000
  "Discard old undo data when buffer-undo-list exceeds this many characters stored.")

(defun count-undo-size (undo-list)
  "Sum of string lengths across all undo entries."
  (let ((n 0))
    (dolist (entry undo-list n)
      (when (consp entry)
        (case (car entry)
          ((insert-string delete-region)
           (incf n (length (caddr entry)))))))))

(defun truncate-undo-list! (undo-list)
  "Destructively truncate UNDO-LIST at the first nil boundary past *undo-limit*."
  (let ((size 0)
        (prev nil)
        (tail undo-list))
    (loop
      (when (null tail) (return))
      (let ((entry (car tail)))
        (when (and (null entry) (> size *undo-limit*))
          ;; At a nil boundary past the size limit — cut here
          (when prev (setf (cdr prev) nil))
          (return))
        (when (consp entry)
          (case (car entry)
            ((insert-string delete-region)
             (incf size (length (caddr entry))))))
        (setf prev tail
              tail (cdr tail))))))

;;;; Invoke hook

(defvar last-was-undo-p nil)
(defvar this-is-undo-p nil)
(defvar undoing-undo-list nil)

(defun undo-invoke-hook (command p)
  (declare (ignore p))
  (setf this-is-undo-p nil)
  (let* ((self-insert-p (string= (command-name command) "Self Insert"))
         (buffer (current-buffer)))
    ;; Signal to insert-character whether to try consolidation.
    (setf *consolidate-insert* self-insert-p)
    (when (and buffer (buffer-undo-p buffer))
      ;; Always push point-position; only push nil boundary for non-self-insert.
      ;; For self-insert, insert-character :around will handle removing these
      ;; entries if it successfully consolidates.
      (push nil (buffer-undo-list buffer))
      (push (list 'point-position (mark-position (current-point)))
            (buffer-undo-list buffer))
      ;; Trim if over size limit (only after pushing a fresh nil boundary).
      (unless self-insert-p
        (when (> (count-undo-size (buffer-undo-list buffer)) *undo-limit*)
          (truncate-undo-list! (buffer-undo-list buffer))))))
  nil)

(defparameter *invoke-hook* #'(lambda (command p)
                                (undo-invoke-hook command p)
                                (funcall (command-function command) p)
                                (setf last-was-undo-p this-is-undo-p))
  "This function is called by the command interpreter when it wants to invoke a
  command.  The arguments are the command to invoke and the prefix argument.
  The default value just calls the Command-Function with the prefix argument.")

;;;; Undo command

(defcommand "Undo" (p)
  "Undo the last editing operation.  With prefix argument, undo within the current region."
  "Undo the last editing operation.  With prefix argument, undo within the current region."
  (when p
    (undo-in-region-command nil)
    (return-from undo-command nil))
  (setf this-is-undo-p t)
  ;; Skip this command's own point-position + nil entries.
  (let* ((buffer (current-buffer))
         (undo-list (if last-was-undo-p
                        undoing-undo-list
                        (cddr (buffer-undo-list buffer))))
         (modifiedp nil)
         (steps 0))
    (block done
      ;; Skip any leading nil boundaries (commands that made no buffer changes).
      (loop while (and undo-list (null (car undo-list)))
            do (pop undo-list))
      (when (null undo-list)
        (message "No further undo information")
        (return-from done nil))
      (loop
        (when (> (incf steps) 10000)
          (message "Undo limit reached")
          (return-from done nil))
        (cond
          ;; End of undo history.
          ((null undo-list)
           (message "No further undo information")
           (return-from done nil))
          ;; Nil boundary — end of current undo group.
          ((null (car undo-list))
           (pop undo-list)
           (if modifiedp
               (progn (message "Undo!")
                      (return-from done nil))
               ;; Empty group (e.g., navigation command), keep scanning.
               ))
          ;; Process an entry.
          (t
           (let ((chunk (pop undo-list)))
             (when (and (consp chunk)
                        (consp (cadr chunk))
                        (eq (car (cadr chunk)) buffer))
               (handler-case
                   (case (car chunk)
                     (insert-string
                      (let* ((pos (cadr chunk))
                             (matter (caddr chunk))
                             (n (length matter))
                             (mark (apply #'position-mark pos)))
                        ;; Verify text matches before deleting.
                        (let ((check-mark (copy-mark mark :temporary)))
                          (dotimes (i n)
                            (let ((c (next-character check-mark)))
                              (unless (eql c (schar matter i))
                                (editor-error "Undo lost sync")))
                            (mark-after check-mark)))
                        (delete-characters mark n)
                        (setf modifiedp t)))
                     (delete-region
                      (let ((pos (cadr chunk))
                            (matter (caddr chunk)))
                        (let ((mark (apply #'position-mark pos)))
                          (insert-string mark matter)
                          (setf modifiedp t))))
                     (point-position
                      (move-mark (current-point)
                                 (apply #'position-mark (cadr chunk)))))
                 (error (c)
                   (message "Undo failed: ~A" c)
                   (return-from done nil)))))))))
    (setf undoing-undo-list undo-list)))

;;;; Undo in region

(defun make-selective-undo-list (start-pos end-pos undo-list)
  "Return a filtered copy of UNDO-LIST containing only entries whose position
   falls within [START-POS, END-POS] (inclusive).  Nil boundaries are preserved.
   START-POS and END-POS are (buffer line-no char-pos) triples."
  (let ((result nil))
    (dolist (entry undo-list)
      (cond
        ;; Always keep nil group boundaries.
        ((null entry)
         (push nil result))
        ;; Keep entries whose recorded position is within the region.
        ((and (consp entry)
              (consp (cadr entry))
              (let ((pos (cadr entry)))
                (and (position<= start-pos pos)
                     (position<= pos end-pos))))
         (push entry result))))
    (nreverse result)))

(defcommand "Undo In Region" (p)
  "Undo changes within the current region."
  "Undo changes within the current region."
  (declare (ignore p))
  (let* ((buffer (current-buffer))
         (region (current-region nil nil))
         (start (mark-position (region-start region)))
         (end   (mark-position (region-end   region)))
         (filtered (make-selective-undo-list start end
                                             (buffer-undo-list buffer))))
    (when (every #'null filtered)
      (editor-error "No undo information for region"))
    ;; Install filtered list as undoing-undo-list and run one undo step.
    (setf this-is-undo-p t
          last-was-undo-p t
          undoing-undo-list filtered)
    (undo-command nil)))


(defcommand "Undo Mode" (p)
  "Enable the recording of Undo information in the current buffer."
  "Enable the recording of Undo information in the current buffer."
  (let* ((buffer (current-buffer))
         (p (if p (plusp p) (not (buffer-undo-p buffer)))))
    (setf (buffer-undo-p buffer) p)
    (setf (buffer-undo-list buffer) nil)
    (if p
        (message "Undo enabled")
        (message "Undo disabled"))))
