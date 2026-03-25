;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;;    Ranked symbol completion for Lisp buffers.
;;;

(in-package :hemlock)


;;;; Internal matching structures

(defstruct (fuzzy-matching (:conc-name   fuzzy-matching.)
                           (:predicate   fuzzy-matching-p)
                           (:constructor %make-fuzzy-matching))
  symbol
  package-name
  score
  package-chunks
  symbol-chunks)

(defun make-fuzzy-matching (symbol package-name score package-chunks symbol-chunks)
  (declare (inline %make-fuzzy-matching))
  (%make-fuzzy-matching :symbol symbol :package-name package-name :score score
                        :package-chunks package-chunks
                        :symbol-chunks symbol-chunks))

(defmacro with-struct ((conc-name &rest names) obj &body body)
  "Like with-slots but works only for structs."
  (flet ((reader (slot) (intern (concatenate 'string
                                             (symbol-name conc-name)
                                             (symbol-name slot))
                                (symbol-package conc-name))))
    (let ((tmp (gensym "OO-")))
      `(let ((,tmp ,obj))
         (symbol-macrolet
             ,(loop for name in names collect
                    (typecase name
                      (symbol `(,name (,(reader name) ,tmp)))
                      (cons `(,(first name) (,(reader (second name)) ,tmp)))
                      (t (error "Malformed syntax in WITH-STRUCT: ~A" name))))
           ,@body)))))


;;;; Timing

(declaim (ftype (function () (integer 0)) get-real-time-in-msecs))
(defun get-real-time-in-msecs ()
  (let ((units-per-msec (max 1 (floor internal-time-units-per-second 1000))))
    (values (floor (get-internal-real-time) units-per-msec))))

(defun %guess-sort-duration (length)
  (if (zerop length)
      0
      (let ((comparasions (* 3.8 (* length (log length 2)))))
        (* 1000 (* comparasions (expt 10 -7))))))


;;;; Completion output format helpers

(defun %fuzzy-extract-matching-info (fuzzy-matching user-input-string)
  (multiple-value-bind (_ user-package-name __ input-internal-p)
      (parse-completion-arguments user-input-string nil)
    (declare (ignore _ __))
    (with-struct (fuzzy-matching. score symbol package-name package-chunks symbol-chunks)
        fuzzy-matching
      (let (symbol-name real-package-name internal-p)
        (cond (symbol
               (setf symbol-name (symbol-name symbol))
               (setf internal-p input-internal-p)
               (setf real-package-name (cond ((keywordp symbol)      "")
                                             ((not user-package-name) nil)
                                             (t package-name))))
              (t
               (setf symbol-name "")
               (setf real-package-name package-name)
               (setf internal-p (if user-package-name input-internal-p nil))))
        (values symbol-name
                real-package-name
                (if user-package-name internal-p nil)
                (completion-output-symbol-converter user-input-string)
                (completion-output-package-converter user-input-string))))))

(defun fuzzy-format-matching (fuzzy-matching user-input-string)
  (multiple-value-bind (symbol-name package-name internal-p symbol-converter package-converter)
      (%fuzzy-extract-matching-info fuzzy-matching user-input-string)
    (setq symbol-name  (and symbol-name  (funcall symbol-converter symbol-name)))
    (setq package-name (and package-name (funcall package-converter package-name)))
    (let ((result (untokenize-symbol package-name internal-p symbol-name)))
      (values result (search symbol-name result)))))

(defun symbol-classification->string (flags)
  (format nil "~A~A~A~A~A~A~A~A"
          (if (or (member :boundp flags)
                  (member :constant flags)) "b" "-")
          (if (member :fboundp flags) "f" "-")
          (if (member :generic-function flags) "g" "-")
          (if (member :class flags) "c" "-")
          (if (member :typespec flags) "t" "-")
          (if (member :macro flags) "m" "-")
          (if (member :special-operator flags) "s" "-")
          (if (member :package flags) "p" "-")))

(defun fuzzy-convert-matching-for-emacs (fuzzy-matching user-input-string)
  (with-struct (fuzzy-matching. symbol score package-chunks symbol-chunks) fuzzy-matching
    (multiple-value-bind (name added-length)
        (fuzzy-format-matching fuzzy-matching user-input-string)
      (list name
            (format nil "~,2f" score)
            (append package-chunks
                    (mapcar (lambda (chunk)
                              (let ((offset (first chunk)) (string (second chunk)))
                                (list (+ added-length offset) string)))
                            symbol-chunks))
            (symbol-classification->string (classify-symbol symbol))))))


;;;; Fuzzy matching generation

(defun fuzzy-matching-greaterp (m1 m2)
  (declare (type fuzzy-matching m1 m2))
  (let ((score1 (fuzzy-matching.score m1))
        (score2 (fuzzy-matching.score m2)))
    (cond ((> score1 score2) t)
          ((< score1 score2) nil)
          (t (string< (symbol-name (fuzzy-matching.symbol m1))
                      (symbol-name (fuzzy-matching.symbol m2)))))))

(defun %make-duplicate-symbols-filter (fuzzy-package-matchings)
  (let ((packages (mapcar (lambda (m)
                            (find-package (fuzzy-matching.package-name m)))
                          (coerce fuzzy-package-matchings 'list))))
    (lambda (symbol)
      (not (member (symbol-package symbol) packages)))))

(defun fuzzy-find-matching-symbols
    (string package &key (filter #'identity) external-only time-limit-in-msec)
  "Return two values: a vector of fuzzy matchings for symbols in PACKAGE and
the remaining time limit.  FILTER excludes symbols for which it returns NIL.
TIME-LIMIT-IN-MSEC of NIL means no limit."
  (let ((time-limit-p (and time-limit-in-msec t))
        (time-limit (or time-limit-in-msec 0))
        (rtime-at-start (get-real-time-in-msecs))
        (package-name (package-name package))
        (count 0))
    (declare (type boolean time-limit-p))
    (declare (type integer time-limit rtime-at-start))
    (declare (type (integer 0 #.(1- most-positive-fixnum)) count))
    (flet ((recompute-remaining-time (old-remaining-time)
             (cond ((not time-limit-p)
                    (values nil nil))
                   ((> count 0)
                    (setf count (mod (1+ count) 128))
                    (values nil old-remaining-time))
                   (t (let* ((elapsed-time (- (get-real-time-in-msecs) rtime-at-start))
                             (remaining (- time-limit elapsed-time)))
                        (values (<= remaining 0) remaining)))))
           (perform-fuzzy-match (string symbol-name)
             (let* ((converter (completion-output-symbol-converter string))
                    (converted-symbol-name (funcall converter symbol-name)))
               (compute-highest-scoring-completion string converted-symbol-name))))
      (let ((completions (make-array 256 :adjustable t :fill-pointer 0))
            (rest-time-limit time-limit))
        (block loop
          (do-symbols* (symbol package)
            (multiple-value-bind (exhausted? remaining-time)
                (recompute-remaining-time rest-time-limit)
              (setf rest-time-limit remaining-time)
              (cond (exhausted? (return-from loop))
                    ((or (not external-only) (symbol-external-p symbol package))
                     (when (funcall filter symbol)
                       (if (string= "" string)
                           (vector-push-extend (make-fuzzy-matching symbol package-name
                                                                    0.0 '() '())
                                               completions)
                           (multiple-value-bind (match-result score)
                               (perform-fuzzy-match string (symbol-name symbol))
                             (when match-result
                               (vector-push-extend
                                (make-fuzzy-matching symbol package-name score
                                                     '() match-result)
                                completions))))))))))
        (values completions rest-time-limit)))))

(defun fuzzy-find-matching-packages (name &key time-limit-in-msec)
  "Return a vector of fuzzy matchings for packages similar to NAME, and
the remaining time limit."
  (let ((time-limit-p (and time-limit-in-msec t))
        (time-limit (or time-limit-in-msec 0))
        (rtime-at-start (get-real-time-in-msecs))
        (converter (completion-output-package-converter name))
        (completions (make-array 32 :adjustable t :fill-pointer 0)))
    (declare (type boolean time-limit-p))
    (declare (type integer time-limit rtime-at-start))
    (declare (type function converter))
    (if (and time-limit-p (<= time-limit 0))
        (values #() time-limit)
        (loop for package in (list-all-packages) do
              (loop with max-pkg-name = ""
                    with max-result   = nil
                    with max-score    = 0
                    for package-name in (package-names package)
                    for converted-name = (funcall converter package-name)
                    do
                    (multiple-value-bind (result score)
                        (compute-highest-scoring-completion name converted-name)
                      (when (and result (> score max-score))
                        (setf max-pkg-name package-name)
                        (setf max-result   result)
                        (setf max-score    score)))
                    finally
                    (when max-result
                      (vector-push-extend (make-fuzzy-matching nil max-pkg-name
                                                               max-score max-result '())
                                          completions)))
              finally
              (return
                (values completions
                        (and time-limit-p
                             (let ((elapsed-time (- (get-real-time-in-msecs) rtime-at-start)))
                               (- time-limit elapsed-time)))))))))

(defun fuzzy-generate-matchings (string default-package-name time-limit-in-msec)
  "Compute all fuzzy matchings for STRING in DEFAULT-PACKAGE-NAME.
Returns (values results-vector interrupted-p)."
  (multiple-value-bind (parsed-symbol-name parsed-package-name package internal-p)
      (parse-completion-arguments string default-package-name)
    (flet ((fix-up (matchings parent-package-matching)
             (let* ((p parent-package-matching)
                    (p.name   (fuzzy-matching.package-name p))
                    (p.score  (fuzzy-matching.score p))
                    (p.chunks (fuzzy-matching.package-chunks p)))
               (map-into matchings
                         (lambda (m)
                           (let ((m.score (fuzzy-matching.score m)))
                             (setf (fuzzy-matching.package-name m) p.name)
                             (setf (fuzzy-matching.package-chunks m) p.chunks)
                             (setf (fuzzy-matching.score m)
                                   (if (equal parsed-symbol-name "")
                                       (/ p.score 100)
                                       (+ p.score m.score)))
                             m))
                         matchings)))
           (find-symbols (designator package time-limit &optional filter)
             (fuzzy-find-matching-symbols designator package
                                          :time-limit-in-msec time-limit
                                          :external-only (not internal-p)
                                          :filter (or filter #'identity)))
           (find-packages (designator time-limit)
             (fuzzy-find-matching-packages designator :time-limit-in-msec time-limit)))
      (let ((time-limit time-limit-in-msec) (symbols) (packages) (results))
        (cond ((not parsed-package-name)
               (setf (values packages time-limit) (find-packages parsed-symbol-name time-limit))
               (setf (values symbols  time-limit) (find-symbols parsed-symbol-name package time-limit)))
              ((string= parsed-package-name "")
               (setf (values symbols time-limit) (find-symbols parsed-symbol-name package time-limit)))
              (t
               (multiple-value-bind (found-packages rest-time-limit)
                   (find-packages parsed-package-name time-limit-in-msec)
                 (setf found-packages (sort found-packages #'fuzzy-matching-greaterp))
                 (loop
                       for package-matching across found-packages
                       for package = (find-package (fuzzy-matching.package-name package-matching))
                       while (or (not time-limit) (> rest-time-limit 0)) do
                         (multiple-value-bind (matchings remaining-time)
                             (find-symbols parsed-symbol-name package rest-time-limit
                                           (%make-duplicate-symbols-filter
                                            (remove package-matching found-packages)))
                           (setf matchings (fix-up matchings package-matching))
                           (setf symbols   (concatenate 'vector symbols matchings))
                           (setf rest-time-limit remaining-time)
                           (let ((guessed-sort-duration (%guess-sort-duration (length symbols))))
                             (when (<= rest-time-limit guessed-sort-duration)
                               (decf rest-time-limit guessed-sort-duration)
                               (loop-finish))))
                       finally
                         (setf time-limit rest-time-limit)
                         (when (equal parsed-symbol-name "")
                           (setf packages found-packages))))))
        (setf results (concatenate 'vector symbols packages))
        (setf results (sort results #'fuzzy-matching-greaterp))
        (values results (and time-limit (<= time-limit 0)))))))

(defun fuzzy-completion-set (string default-package-name &key limit time-limit-in-msec)
  "Return two values: a vector of completion objects sorted by score, and a
flag indicating whether TIME-LIMIT-IN-MSEC was exhausted."
  (check-type limit (or null (integer 0 #.(1- most-positive-fixnum))))
  (check-type time-limit-in-msec (or null (integer 0 #.(1- most-positive-fixnum))))
  (multiple-value-bind (matchings interrupted-p)
      (fuzzy-generate-matchings string default-package-name time-limit-in-msec)
    (when (and limit (> limit 0) (< limit (length matchings)))
      (if (array-has-fill-pointer-p matchings)
          (setf (fill-pointer matchings) limit)
          (setf matchings (make-array limit :displaced-to matchings))))
    (map-into matchings (lambda (m) (fuzzy-convert-matching-for-emacs m string)) matchings)
    (values matchings interrupted-p)))

(defun fuzzy-completions (string default-package-name &key limit time-limit-in-msec)
  "Return (list matches interrupted-p) for symbol designator STRING.
Each match is (completed-string score chunks classification-string) where
chunks is a list of (offset substring) pairs marking matched characters."
  (let* ((no-time-limit-p (or (not time-limit-in-msec) (zerop time-limit-in-msec)))
         (time-limit (if no-time-limit-p nil time-limit-in-msec)))
    (multiple-value-bind (completion-set interrupted-p)
        (fuzzy-completion-set string default-package-name :limit limit
                              :time-limit-in-msec time-limit)
      (list (coerce completion-set 'list) interrupted-p))))


;;;; Classifier

(defun classify-symbol (symbol)
  "Return a list of keyword classifiers for SYMBOL."
  (check-type symbol symbol)
  (flet ((type-specifier-p (s)
           (or (documentation s 'type)
               (ignore-errors (sb-ext:valid-type-specifier-p s)))))
    (let (result)
      (when (boundp symbol)             (push (if (constantp symbol) :constant :boundp) result))
      (when (fboundp symbol)            (push :fboundp result))
      (when (type-specifier-p symbol)   (push :typespec result))
      (when (find-class symbol nil)     (push :class result))
      (when (macro-function symbol)     (push :macro result))
      (when (special-operator-p symbol) (push :special-operator result))
      (when (find-package symbol)       (push :package result))
      (when (and (fboundp symbol)
                 (typep (ignore-errors (fdefinition symbol)) 'generic-function))
        (push :generic-function result))
      result)))


;;;; Core fuzzy algorithm

(defparameter *fuzzy-recursion-soft-limit* 30
  "Soft limit for recursion depth in RECURSIVELY-COMPUTE-MOST-COMPLETIONS.")
(declaim (fixnum *fuzzy-recursion-soft-limit*))

(defparameter *fuzzy-completion-symbol-prefixes* "*+-%&?<"
  "Characters likely to appear at the beginning of a symbol.")
(defparameter *fuzzy-completion-symbol-suffixes* "*+->"
  "Characters likely to appear at the end of a symbol.")
(defparameter *fuzzy-completion-word-separators* "-/."
  "Characters that separate words within a symbol name.")

(defun compute-highest-scoring-completion (short full)
  "Return the highest-scoring chunk list and score for completing SHORT onto FULL."
  (let* ((scored-results
          (mapcar (lambda (result)
                    (cons (score-completion result short full) result))
                  (compute-most-completions short full)))
         (winner (first (sort scored-results #'> :key #'first))))
    (values (rest winner) (first winner))))

(defun compute-most-completions (short full)
  "Return all possible ways to complete FULL with the letters in SHORT."
  (let ((*all-chunks* nil))
    (declare (special *all-chunks*))
    (recursively-compute-most-completions short full 0 0 nil nil nil t)
    *all-chunks*))

(defun recursively-compute-most-completions
    (short full
     short-index initial-full-index
     chunks current-chunk current-chunk-pos
     recurse-p)
  (declare (fixnum short-index initial-full-index)
           (simple-string short full)
           (special *all-chunks*))
  (flet ((short-cur ()
           (if (= short-index (length short))
               nil
               (aref short short-index)))
         (add-to-chunk (char pos)
           (unless current-chunk
             (setf current-chunk-pos pos))
           (push char current-chunk))
         (collect-chunk ()
           (when current-chunk
             (push (list current-chunk-pos
                         (coerce (reverse current-chunk) 'string)) chunks)
             (setf current-chunk nil
                   current-chunk-pos nil))))
    (when current-chunk (collect-chunk))
    (do ((pos initial-full-index (1+ pos)))
        ((= pos (length full)))
      (let ((cur-char (aref full pos)))
        (if (and (short-cur)
                 (char= cur-char (short-cur)))
            (progn
              (when recurse-p
                (recursively-compute-most-completions
                 short full short-index (1+ pos)
                 chunks current-chunk current-chunk-pos
                 (not (> (length *all-chunks*)
                         *fuzzy-recursion-soft-limit*))))
              (incf short-index)
              (add-to-chunk cur-char pos))
            (collect-chunk))))
    (collect-chunk)
    (if (short-cur)
        nil
        (let ((rev-chunks (reverse chunks)))
          (push rev-chunks *all-chunks*)
          rev-chunks))))

(defun score-completion (completion short full)
  "Score the completion CHUNKS as a match from SHORT to FULL."
  (labels ((at-beginning-p (pos)
             (= pos 0))
           (after-prefix-p (pos)
             (and (= pos 1)
                  (find (aref full 0) *fuzzy-completion-symbol-prefixes*)))
           (word-separator-p (pos)
             (find (aref full pos) *fuzzy-completion-word-separators*))
           (after-word-separator-p (pos)
             (find (aref full (1- pos)) *fuzzy-completion-word-separators*))
           (at-end-p (pos)
             (= pos (1- (length full))))
           (before-suffix-p (pos)
             (and (= pos (- (length full) 2))
                  (find (aref full (1- (length full)))
                        *fuzzy-completion-symbol-suffixes*)))
           (score-or-percentage-of-previous (base-score pos chunk-pos)
             (if (zerop chunk-pos)
                 base-score
                 (max base-score
                      (+ (* (score-char (1- pos) (1- chunk-pos)) 0.85)
                         (expt 1.2 chunk-pos)))))
           (score-char (pos chunk-pos)
             (score-or-percentage-of-previous
              (cond ((at-beginning-p pos)         10)
                    ((after-prefix-p pos)         10)
                    ((word-separator-p pos)       1)
                    ((after-word-separator-p pos) 8)
                    ((at-end-p pos)               6)
                    ((before-suffix-p pos)        6)
                    (t                            1))
              pos chunk-pos))
           (score-chunk (chunk)
             (loop for chunk-pos below (length (second chunk))
                   for pos from (first chunk)
                   summing (score-char pos chunk-pos))))
    (let* ((chunk-scores (mapcar #'score-chunk completion))
           (length-score (/ 10.0 (1+ (- (length full) (length short))))))
      (values
       (+ (reduce #'+ chunk-scores) length-score)
       (list (mapcar #'list chunk-scores completion) length-score)))))


;;;; Shared string helpers

(defun tokenize-symbol (string)
  "Parse STRING as a symbol designator.
Returns (values symbol-name package-name internal-p)."
  (let ((package (let ((pos (position #\: string)))
                   (if pos (subseq string 0 pos) nil)))
        (symbol (let ((pos (position #\: string :from-end t)))
                  (if pos (subseq string (1+ pos)) string)))
        (internp (not (= (count #\: string) 1))))
    (values symbol package internp)))

(defun untokenize-symbol (package-name internal-p symbol-name)
  (cond ((not package-name) symbol-name)
        (internal-p         (cat package-name "::" symbol-name))
        (t                  (cat package-name ":" symbol-name))))

(defun cat (&rest strings)
  (with-output-to-string (out)
    (dolist (s strings)
      (etypecase s
        (string    (write-string s out))
        (character (write-char s out))))))

(defun guess-package (string)
  "Find the package named STRING, or NIL."
  (when string
    (or (find-package string)
        (parse-package string)
        (if (find #\! string)
            (guess-package (substitute #\- #\! string))))))

(defun parse-package (string)
  "Find the package named STRING, or NIL."
  (ignore-errors
    (find-package (canonical-case string))))

(defun package-names (package)
  "Return the name and all nicknames of PACKAGE in a fresh list."
  (cons (package-name package) (copy-list (package-nicknames package))))


;;;; Insert helper

(defun insert-symbol-completion (completion)
  "Insert COMPLETION into the current buffer, respecting case of existing prefix."
  (let ((point (current-point)))
    (when completion
      (if *last-completion-mark*
          (move-mark *last-completion-mark* point)
          (setq *last-completion-mark* (copy-mark point :temporary)))
      (let ((mark *last-completion-mark*))
        (reverse-find-attribute mark :completion-wordchar #'zerop)
        (let* ((region (region mark point))
               (string (region-to-string region)))
          (declare (simple-string string))
          (delete-region region)
          (let* ((first (position-if #'alpha-char-p string))
                 (next  (if first (position-if #'alpha-char-p string
                                               :start (1+ first)))))
            (insert-string point
                           (if (and first (upper-case-p (schar string first)))
                               (if (and next (upper-case-p (schar string next)))
                                   (string-upcase completion)
                                   (word-capitalize completion))
                               completion))))))))


;;;; Complete Symbol command

(defcommand "Complete Symbol" (p)
  "Complete the symbol prefix at point in the current Lisp image.
With prefix argument, always prompt even for a single match."
  ""
  (let* ((prefix (symbol-string-at-point))
         (pkg    (or (package-at-point) "cl"))
         (raw    (fuzzy-completions (string-downcase prefix) pkg))
         (matches (first raw)))
    (cond
      ((null matches)
       (message "No completions for: ~A" prefix))
      ((and (null (cdr matches)) (not p))
       (insert-symbol-completion (first (first matches))))
      (t
       (let ((result (prompt (find-package pkg)
                            :default-string (string-downcase prefix))))
         (when (and result (plusp (length result)))
           (insert-symbol-completion result)))))))
