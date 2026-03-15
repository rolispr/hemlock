;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; Completion source protocol and matching styles.
;;;
;;; A completion-source describes a set of candidates together with
;;; optional annotation, preview, and action callbacks.
;;;
;;; *completion-styles* is a list of matching functions applied in order;
;;; the first style that returns a non-empty result wins.
;;;

(in-package :hemlock)


;;;; Completion source

(defstruct completion-source
  "Describes a set of completion candidates and how to handle them.

  ITEMS-FN    — (lambda ()) → list of strings
  CATEGORY    — keyword identifying the kind of candidate, e.g.
                :symbol :buffer :file :keyword :command :generic
  ANNOTATE-FN — (lambda (string)) → string | nil
                  annotation shown to the right of the candidate
  PREVIEW-FN  — (lambda (string)) → void | nil
                  called while the user navigates, before confirming
  ACTION-FN   — (lambda (string)) → void
                  called with the selected candidate"
  items-fn
  (category :generic)
  annotate-fn
  preview-fn
  action-fn)


;;;; Completion styles
;;;
;;; Each style is a function: (pattern candidates) -> filtered-candidates | nil
;;; Returning nil means "I found nothing; try the next style."
;;; Returning an empty list means "I matched, but nothing qualifies."

(defvar *completion-styles* nil
  "Ordered list of completion matching style functions.
Each function takes (pattern candidates) and returns a filtered, possibly
reordered subset of candidates, or nil to fall through to the next style.")

(defun filter-completions (pattern candidates)
  "Apply all *completion-styles* and merge results, preserving order and
removing duplicates.  Returns all candidates when PATTERN is empty.
Returns nil when no style matches."
  (if (zerop (length pattern))
      candidates
      (let ((seen (make-hash-table :test #'equal))
            (result nil))
        (dolist (style *completion-styles*)
          (let ((matches (funcall style pattern candidates)))
            (dolist (m matches)
              (unless (gethash m seen)
                (setf (gethash m seen) t)
                (push m result)))))
        (nreverse result))))


;;;; Built-in styles

(defun completion-style-prefix (pattern candidates)
  "Match candidates that begin with PATTERN (case-insensitive)."
  (let ((p (string-downcase pattern)))
    (let ((matches (remove-if-not
                    (lambda (c)
                      (let ((s (string-downcase c)))
                        (and (>= (length s) (length p))
                             (string= s p :end1 (min (length p) (length s))))))
                    candidates)))
      (and matches matches))))

(defun completion-style-substring (pattern candidates)
  "Match candidates containing PATTERN as a substring (case-insensitive)."
  (let ((p (string-downcase pattern)))
    (let ((matches (remove-if-not
                    (lambda (c)
                      (search p (string-downcase c)))
                    candidates)))
      (and matches matches))))

(defun completion-style-fuzzy-chars (pattern candidates)
  "Match candidates where every character of PATTERN appears in order
in the candidate string (case-insensitive).  Candidates are sorted by
match quality: earlier and more-consecutive matches rank higher."
  (let ((p (string-downcase pattern)))
    (let ((scored
           (loop for c in candidates
                 for score = (completion-fuzzy-score p (string-downcase c))
                 when score collect (cons score c))))
      (when scored
        (mapcar #'cdr
                (sort scored #'> :key #'car))))))

(defun completion-fuzzy-score (pattern string)
  "Return a score >= 0 if PATTERN characters appear in STRING in order,
nil otherwise.  Higher scores mean tighter matches."
  (let ((si 0)
        (slen (length string))
        (score 0)
        (prev-match -2))
    (loop for pc across pattern
          do (loop for i from si below slen
                   when (char= pc (char string i))
                     do (incf score (if (= i (1+ prev-match)) 3 1))
                        (setf prev-match i
                              si (1+ i))
                        (return)
                   finally (return-from completion-fuzzy-score nil)))
    ;; Bonus for matching at the start
    (when (and (plusp (length pattern))
               (char= (char pattern 0) (char string 0)))
      (incf score 5))
    score))

(defun completion-style-orderless (pattern candidates)
  "Split PATTERN on spaces; each token must match somewhere in candidate."
  (let ((tokens (loop for start = 0 then (1+ end)
                      for end = (position #\space pattern :start start)
                      for tok = (subseq pattern start end)
                      unless (zerop (length tok)) collect (string-downcase tok)
                      while end)))
    (when tokens
      (remove-if-not
       (lambda (c)
         (let ((low (string-downcase c)))
           (every (lambda (tok) (search tok low)) tokens)))
       candidates))))


;;;; String table adapter

(defun string-table-candidates (tables)
  "Extract all entry strings from a list of string-tables."
  (find-all-completions "" tables))


(setf *completion-styles*
      (list #'completion-style-prefix
            #'completion-style-substring
            #'completion-style-fuzzy-chars
            #'completion-style-orderless))
