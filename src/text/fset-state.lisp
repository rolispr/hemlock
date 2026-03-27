;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; Immutable buffer state using FSet functional collections.
;;; Pure functions, no side effects, no hemlock dependencies.

(in-package :hemlock.text)


;;;; Core structures.

(defstruct (buffer-state (:conc-name bs-))
  "Immutable snapshot of a buffer's complete state.
Lines is an FSet seq of strings (one per line).
Marks is an FSet map from mark-id to (line-idx charpos kind).
Meta is an FSet map of buffer properties.
Tick is monotonically increasing, bumped on every edit."
  (lines (fset:seq "") :type fset:seq)
  (marks (fset:empty-map) :type fset:map)
  (meta  (fset:empty-map) :type fset:map)
  (tick  0 :type fixnum))

(defstruct (buffer-snapshot (:conc-name snap-))
  "Lightweight read-only view for the display-actor.
Lines is shared (structural sharing) with the buffer-state."
  (name          "" :type string)
  (tick          0  :type fixnum)
  (lines         (fset:seq "") :type fset:seq)
  (line-count    1  :type fixnum)
  (point-line    0  :type fixnum)
  (point-charpos 0  :type fixnum)
  (start-line    0  :type fixnum)
  (start-charpos 0  :type fixnum)
  (modes         nil :type list)
  (modified-p    nil :type boolean)
  (active-region nil) ; nil or (start-line start-cp end-line end-cp)
  ;; Per-line syntax colors: FSet seq of (list-of-ranges-or-nil), parallel to lines.
  ;; Each range is (start-charpos end-charpos font-spec).
  (colors        nil)
  ;; Modeline string (pre-rendered).
  (modeline      "" :type string)
  ;; Window dimensions and position for rendering.
  (width         80 :type fixnum)
  (height        24 :type fixnum)
  ;; Terminal row where this window starts (0 = top of screen).
  (top-row       0  :type fixnum)
  ;; Number of text lines (excluding modeline).
  (text-height   23 :type fixnum)
  ;; Terminal row of the modeline.
  (modeline-row  23 :type fixnum))


;;;; Buffer state construction.

(defun make-empty-buffer-state (name)
  "Create a fresh buffer-state with one empty line."
  (make-buffer-state
   :lines (fset:seq "")
   :marks (fset:map (:point (list 0 0 :right-inserting))
                    (:display-start (list 0 0 :left-inserting)))
   :meta (fset:map (:name name)
                   (:writable t)
                   (:modified-tick 0)
                   (:unmodified-tick 0)
                   (:modes (list "Fundamental")))
   :tick 0))

(defun state->snapshot (state)
  "Return a buffer-snapshot from State.  Shares the lines seq via structural sharing."
  (let ((point (fset:@ (bs-marks state) :point))
        (ds    (fset:@ (bs-marks state) :display-start)))
    (make-buffer-snapshot
     :name (or (fset:@ (bs-meta state) :name) "")
     :tick (bs-tick state)
     :lines (bs-lines state)
     :line-count (fset:size (bs-lines state))
     :point-line (first point)
     :point-charpos (second point)
     :start-line (if ds (first ds) 0)
     :start-charpos (if ds (second ds) 0)
     :modes (or (fset:@ (bs-meta state) :modes) nil)
     :modified-p (let ((mt (fset:@ (bs-meta state) :modified-tick))
                       (ut (fset:@ (bs-meta state) :unmodified-tick)))
                   (and mt ut (/= mt ut))))))


;;;; Map helpers.

(defun map-transform-values (fn map)
  "Return a new FSet map with same keys but values transformed by FN.
FN receives (value) and returns a new value."
  (let ((result (fset:empty-map)))
    (fset:do-map (k v map)
      (setf result (fset:with result k (funcall fn v))))
    result))


;;;; Mark adjustment.

(defun adjust-marks-after-insert (marks line-idx charpos length)
  "After inserting LENGTH chars at (LINE-IDX, CHARPOS) on a single line,
return a new marks map with adjusted positions."
  (map-transform-values
   (lambda (pos)
     (destructuring-bind (ml mc kind) pos
                  (cond
                    ((/= ml line-idx) pos)
                    ((> mc charpos) (list ml (+ mc length) kind))
                    ((and (= mc charpos) (eq kind :right-inserting))
                     (list ml (+ mc length) kind))
                    (t pos))))
              marks))

(defun adjust-marks-after-delete (marks line-idx start-cp end-cp)
  "After deleting chars from START-CP to END-CP on LINE-IDX (single line),
return a new marks map with adjusted positions."
  (let ((deleted (- end-cp start-cp)))
    (map-transform-values
     (lambda (pos)
       (destructuring-bind (ml mc kind) pos
                    (cond
                      ((/= ml line-idx) pos)
                      ((<= mc start-cp) pos)
                      ((<= mc end-cp) (list ml start-cp kind))
                      (t (list ml (- mc deleted) kind)))))
                marks)))

(defun adjust-marks-after-line-split (marks line-idx charpos)
  "After splitting LINE-IDX at CHARPOS (newline insertion),
marks at/after charpos move to the new line (line-idx+1) with adjusted charpos.
All marks on lines > line-idx shift down by 1."
  (map-transform-values
   (lambda (pos)
     (destructuring-bind (ml mc kind) pos
                  (cond
                    ((< ml line-idx) pos)
                    ((> ml line-idx) (list (1+ ml) mc kind))
                    ;; Same line
                    ((> mc charpos) (list (1+ ml) (- mc charpos) kind))
                    ((and (= mc charpos) (eq kind :right-inserting))
                     (list (1+ ml) 0 kind))
                    (t pos))))
              marks))

(defun adjust-marks-after-line-join (marks line-idx line-length)
  "After joining LINE-IDX with LINE-IDX+1 (newline deletion),
marks on line-idx+1 move to line-idx with charpos offset by LINE-LENGTH.
All marks on lines > line-idx+1 shift up by 1."
  (map-transform-values
   (lambda (pos)
     (destructuring-bind (ml mc kind) pos
                  (cond
                    ((<= ml line-idx) pos)
                    ((= ml (1+ line-idx)) (list line-idx (+ mc line-length) kind))
                    (t (list (1- ml) mc kind)))))
              marks))

(defun adjust-marks-after-multiline-delete (marks start-line start-cp end-line end-cp)
  "After deleting from (START-LINE, START-CP) to (END-LINE, END-CP),
adjust all marks. Lines between start and end are removed."
  (let ((lines-removed (- end-line start-line)))
    (map-transform-values
     (lambda (pos)
       (destructuring-bind (ml mc kind) pos
                    (cond
                      ;; Before the deletion
                      ((< ml start-line) pos)
                      ;; On the start line, before start-cp
                      ((and (= ml start-line) (<= mc start-cp)) pos)
                      ;; On the start line, after start-cp
                      ((= ml start-line) (list ml start-cp kind))
                      ;; Between start and end (inclusive of end)
                      ((and (> ml start-line) (< ml end-line))
                       (list start-line start-cp kind))
                      ;; On the end line
                      ((= ml end-line)
                       (if (<= mc end-cp)
                           (list start-line start-cp kind)
                           (list start-line (+ start-cp (- mc end-cp)) kind)))
                      ;; After the deletion
                      (t (list (- ml lines-removed) mc kind)))))
                marks)))


;;;; Edit operations.

(defun bs-insert-char (state line-idx charpos char)
  "Insert a single character. Returns new buffer-state."
  (if (char= char #\Newline)
      (bs-insert-newline state line-idx charpos)
      (let* ((lines (bs-lines state))
             (old-line (fset:@ lines line-idx))
             (new-line (concatenate 'string
                         (subseq old-line 0 charpos)
                         (string char)
                         (subseq old-line charpos))))
        (make-buffer-state
         :lines (fset:with lines line-idx new-line)
         :marks (adjust-marks-after-insert (bs-marks state) line-idx charpos 1)
         :meta (bs-meta state)
         :tick (1+ (bs-tick state))))))

(defun bs-insert-newline (state line-idx charpos)
  "Split line at charpos. Returns new buffer-state."
  (let* ((lines (bs-lines state))
         (old-line (fset:@ lines line-idx))
         (before (subseq old-line 0 charpos))
         (after  (subseq old-line charpos))
         (prefix (fset:subseq lines 0 line-idx))
         (suffix (fset:subseq lines (1+ line-idx)))
         (new-lines (fset:concat
                     (fset:with-last prefix before)
                     (fset:with-first suffix after))))
    (make-buffer-state
     :lines new-lines
     :marks (adjust-marks-after-line-split (bs-marks state) line-idx charpos)
     :meta (bs-meta state)
     :tick (1+ (bs-tick state)))))

(defun bs-insert-string (state line-idx charpos string)
  "Insert a string (possibly multi-line). Returns new buffer-state."
  (let ((nl (position #\Newline string)))
    (if (null nl)
        ;; Single line --simple splice
        (let* ((lines (bs-lines state))
               (old-line (fset:@ lines line-idx))
               (new-line (concatenate 'string
                           (subseq old-line 0 charpos)
                           string
                           (subseq old-line charpos))))
          (make-buffer-state
           :lines (fset:with lines line-idx new-line)
           :marks (adjust-marks-after-insert (bs-marks state) line-idx charpos (length string))
           :meta (bs-meta state)
           :tick (1+ (bs-tick state))))
        ;; Multi-line --split string on newlines, splice into seq
        (let* ((fragments (split-on-newlines string))
               (lines (bs-lines state))
               (old-line (fset:@ lines line-idx))
               (first-frag (concatenate 'string
                             (subseq old-line 0 charpos)
                             (first fragments)))
               (last-frag (concatenate 'string
                            (car (last fragments))
                            (subseq old-line charpos)))
               (middle (butlast (rest fragments)))
               (new-line-list (append (list first-frag) middle (list last-frag)))
               (prefix (fset:subseq lines 0 line-idx))
               (suffix (fset:subseq lines (1+ line-idx)))
               (inserted (reduce (lambda (seq s) (fset:with-last seq s))
                                 new-line-list
                                 :initial-value (fset:empty-seq)))
               (new-lines (fset:concat prefix (fset:concat inserted suffix)))
               ;; Mark adjustment for multi-line insert
               (new-marks (bs-marks state)))
          ;; Adjust marks: lines after insertion point shift down
          (let ((lines-added (1- (length new-line-list))))
            (setf new-marks
                  (map-transform-values
                   (lambda (pos)
                     (destructuring-bind (ml mc kind) pos
                       (cond
                         ((< ml line-idx) pos)
                         ((and (= ml line-idx) (< mc charpos)) pos)
                         ((and (= ml line-idx) (>= mc charpos))
                          ;; Moves to last inserted line
                          (list (+ ml lines-added)
                                (+ (- mc charpos) (length (car (last fragments))))
                                kind))
                         (t (list (+ ml lines-added) mc kind)))))
                   new-marks)))
          (make-buffer-state
           :lines new-lines
           :marks new-marks
           :meta (bs-meta state)
           :tick (1+ (bs-tick state)))))))

(defun bs-delete-char (state line-idx charpos)
  "Delete one character at (LINE-IDX, CHARPOS). Returns new buffer-state.
If at end of line, joins with next line."
  (let* ((lines (bs-lines state))
         (line (fset:@ lines line-idx))
         (line-len (length line)))
    (if (< charpos line-len)
        ;; Delete within line
        (let ((new-line (concatenate 'string
                          (subseq line 0 charpos)
                          (subseq line (1+ charpos)))))
          (make-buffer-state
           :lines (fset:with lines line-idx new-line)
           :marks (adjust-marks-after-delete (bs-marks state) line-idx charpos (1+ charpos))
           :meta (bs-meta state)
           :tick (1+ (bs-tick state))))
        ;; At end of line --join with next
        (if (< (1+ line-idx) (fset:size lines))
            (let* ((next-line (fset:@ lines (1+ line-idx)))
                   (joined (concatenate 'string line next-line))
                   (prefix (fset:subseq lines 0 line-idx))
                   (suffix (fset:subseq lines (+ 2 line-idx)))
                   (new-lines (fset:concat
                               (fset:with-last prefix joined)
                               suffix)))
              (make-buffer-state
               :lines new-lines
               :marks (adjust-marks-after-line-join (bs-marks state) line-idx line-len)
               :meta (bs-meta state)
               :tick (1+ (bs-tick state))))
            ;; At end of last line --nothing to delete
            state))))

(defun bs-delete-region (state start-line start-cp end-line end-cp)
  "Delete from (START-LINE, START-CP) to (END-LINE, END-CP). Returns new buffer-state."
  (if (= start-line end-line)
      ;; Single line deletion
      (let* ((lines (bs-lines state))
             (line (fset:@ lines start-line))
             (new-line (concatenate 'string
                         (subseq line 0 start-cp)
                         (subseq line end-cp))))
        (make-buffer-state
         :lines (fset:with lines start-line new-line)
         :marks (adjust-marks-after-delete (bs-marks state) start-line start-cp end-cp)
         :meta (bs-meta state)
         :tick (1+ (bs-tick state))))
      ;; Multi-line deletion
      (let* ((lines (bs-lines state))
             (first-line (fset:@ lines start-line))
             (last-line (fset:@ lines end-line))
             (joined (concatenate 'string
                       (subseq first-line 0 start-cp)
                       (subseq last-line end-cp)))
             (prefix (fset:subseq lines 0 start-line))
             (suffix (fset:subseq lines (1+ end-line)))
             (new-lines (fset:concat
                         (fset:with-last prefix joined)
                         suffix)))
        (make-buffer-state
         :lines new-lines
         :marks (adjust-marks-after-multiline-delete
                 (bs-marks state) start-line start-cp end-line end-cp)
         :meta (bs-meta state)
         :tick (1+ (bs-tick state))))))

(defun bs-line-count (state)
  "Number of lines in the buffer."
  (fset:size (bs-lines state)))

(defun bs-line-string (state line-idx)
  "Get the string for line LINE-IDX."
  (fset:@ (bs-lines state) line-idx))

(defun bs-region-to-string (state start-line start-cp end-line end-cp)
  "Extract text from a region as a string."
  (if (= start-line end-line)
      (subseq (fset:@ (bs-lines state) start-line) start-cp end-cp)
      (with-output-to-string (s)
        (write-string (subseq (fset:@ (bs-lines state) start-line) start-cp) s)
        (write-char #\Newline s)
        (loop for i from (1+ start-line) below end-line
              do (write-string (fset:@ (bs-lines state) i) s)
                 (write-char #\Newline s))
        (write-string (subseq (fset:@ (bs-lines state) end-line) 0 end-cp) s))))

(defun bs-move-mark (state mark-id new-line new-charpos)
  "Move a mark to a new position. Returns new buffer-state (same tick)."
  (let* ((old-pos (fset:@ (bs-marks state) mark-id))
         (kind (if old-pos (third old-pos) :right-inserting)))
    (make-buffer-state
     :lines (bs-lines state)
     :marks (fset:with (bs-marks state) mark-id (list new-line new-charpos kind))
     :meta (bs-meta state)
     :tick (bs-tick state))))

(defun bs-set-meta (state key value)
  "Set a metadata key. Returns new buffer-state (same tick)."
  (make-buffer-state
   :lines (bs-lines state)
   :marks (bs-marks state)
   :meta (fset:with (bs-meta state) key value)
   :tick (bs-tick state)))


;;;; Utilities.

(defun split-on-newlines (string)
  "Split STRING on #\\Newline, returning a list of strings."
  (loop with start = 0
        for pos = (position #\Newline string :start start)
        collect (subseq string start (or pos (length string)))
        while pos
        do (setf start (1+ pos))))
