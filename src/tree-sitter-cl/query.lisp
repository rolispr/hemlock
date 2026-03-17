(in-package :tree-sitter/query)

;;;; Query Error Types

(deftype query-error-type ()
  '(member nil :syntax :node-type :field :capture :structure :language))

(defun error-type-from-code (code)
  "Convert tree-sitter error code to keyword."
  (case code
    (0 nil)
    (1 :syntax)
    (2 :node-type)
    (3 :field)
    (4 :capture)
    (5 :structure)
    (6 :language)
    (otherwise :unknown)))

;;;; Node Text Extraction

(defun node-text (node source &key source-bytes)
  "Extract text of NODE from SOURCE string using byte offsets.
When SOURCE-BYTES is provided, skip re-encoding SOURCE."
  (when source
    (let* ((start-byte (node:node-start-byte node))
           (end-byte (node:node-end-byte node))
           (bytes (or source-bytes
                      (babel:string-to-octets source :encoding :utf-8))))
      (when (and (<= 0 start-byte) (<= end-byte (length bytes)))
        (babel:octets-to-string bytes :start start-byte :end end-byte :encoding :utf-8)))))

;;;; Query Predicate Parsing

(defun parse-pattern-predicates (query-ptr pattern-index)
  "Parse predicates for a single pattern into structured form.
Returns list of predicates. Each predicate is (name . args) where args
are (:capture index) or (:string value)."
  (let ((steps (ffi:ts-query-predicates-for-pattern query-ptr pattern-index)))
    (when steps
      (let ((predicates nil)
            (current nil))
        (dolist (step steps)
          (let ((type (car step))
                (value-id (cdr step)))
            (case type
              (0 ; Done
               (when current
                 (push (nreverse current) predicates)
                 (setf current nil)))
              (1 ; Capture
               (push (list :capture value-id) current))
              (2 ; String
               (let ((str (ffi:ts-query-string-value-for-id query-ptr value-id)))
                 (push (list :string str) current))))))
        (nreverse predicates)))))

(defun parse-all-predicates (query-ptr pattern-count)
  "Parse predicates for all patterns. Returns hash: pattern-index -> predicates."
  (let ((table (make-hash-table)))
    (dotimes (i pattern-count)
      (let ((preds (parse-pattern-predicates query-ptr i)))
        (when preds
          (setf (gethash i table) preds))))
    table))

;;;; Query Predicate Evaluation

(defun evaluate-predicate (predicate captures source)
  "Evaluate a single predicate. CAPTURES is a hash: capture-index -> node.
Returns T if predicate passes."
  (let* ((name-step (first predicate))
         (name (second name-step))
         (args (rest predicate)))
    (cond
      ;; #match? @capture "regex"
      ((string= name "match?")
       (let* ((capture-idx (second (first args)))
              (regex-str (second (second args)))
              (node (gethash capture-idx captures)))
         (when node
           (let ((text (node-text node source)))
             (when text
               (not (null (ppcre:scan regex-str text))))))))
      ;; #not-match? @capture "regex"
      ((string= name "not-match?")
       (let* ((capture-idx (second (first args)))
              (regex-str (second (second args)))
              (node (gethash capture-idx captures)))
         (when node
           (let ((text (node-text node source)))
             (when text
               (null (ppcre:scan regex-str text)))))))
      ;; #eq? @capture "string" or #eq? @capture @capture2
      ((string= name "eq?")
       (let* ((arg1 (first args))
              (arg2 (second args))
              (text1 (if (eq (first arg1) :capture)
                         (let ((n (gethash (second arg1) captures)))
                           (when n (node-text n source)))
                         (second arg1)))
              (text2 (if (eq (first arg2) :capture)
                         (let ((n (gethash (second arg2) captures)))
                           (when n (node-text n source)))
                         (second arg2))))
         (and text1 text2 (string= text1 text2))))
      ;; #not-eq?
      ((string= name "not-eq?")
       (let* ((arg1 (first args))
              (arg2 (second args))
              (text1 (if (eq (first arg1) :capture)
                         (let ((n (gethash (second arg1) captures)))
                           (when n (node-text n source)))
                         (second arg1)))
              (text2 (if (eq (first arg2) :capture)
                         (let ((n (gethash (second arg2) captures)))
                           (when n (node-text n source)))
                         (second arg2))))
         (and text1 text2 (not (string= text1 text2)))))
      ;; #any-of? @capture "val1" "val2" ...
      ((string= name "any-of?")
       (let* ((capture-idx (second (first args)))
              (values (mapcar #'second (rest args)))
              (node (gethash capture-idx captures)))
         (when node
           (let ((text (node-text node source)))
             (when text
               (member text values :test #'string=))))))
      ;; #not-any-of?
      ((string= name "not-any-of?")
       (let* ((capture-idx (second (first args)))
              (values (mapcar #'second (rest args)))
              (node (gethash capture-idx captures)))
         (when node
           (let ((text (node-text node source)))
             (when text
               (not (member text values :test #'string=)))))))
      ;; Unknown predicate - pass through
      (t t))))

(defun evaluate-match-predicates (predicates pattern-index captures source)
  "Evaluate all predicates for a pattern. Returns T if all pass."
  (let ((preds (gethash pattern-index predicates)))
    (if (null preds)
        t  ; No predicates = always pass
        (every (lambda (pred) (evaluate-predicate pred captures source))
               preds))))

;;;; Query Compilation

(define-condition query-compile-error (error)
  ((source :initarg :source :reader query-compile-error-source)
   (offset :initarg :offset :reader query-compile-error-offset)
   (error-type :initarg :error-type :reader query-compile-error-type))
  (:report (lambda (c stream)
             (format stream "Query compile error (~A) at offset ~D in: ~A"
                     (query-compile-error-type c)
                     (query-compile-error-offset c)
                     (query-compile-error-source c)))))

(defun query-compile (language source)
  "Compile a query from SOURCE string for LANGUAGE.
   Returns a ts-query object or signals query-compile-error."
  (let ((lang-ptr (etypecase language
                    (types:ts-language (types:ts-language-ptr language))
                    (cffi:foreign-pointer language))))
    (multiple-value-bind (query-ptr error-offset error-type)
        (ffi:ts-query-new lang-ptr source)
      (if query-ptr
          ;; Build capture names vector
          (let* ((capture-count (ffi:ts-query-capture-count query-ptr))
                 (capture-names (make-array capture-count :element-type 'string)))
            (dotimes (i capture-count)
              (setf (aref capture-names i)
                    (or (ffi:ts-query-capture-name-for-id query-ptr i) "")))
            (let* ((pattern-count (ffi:ts-query-pattern-count query-ptr))
                   (predicates (parse-all-predicates query-ptr pattern-count)))
              (make-instance 'types:ts-query
                             :ptr query-ptr
                             :language language
                             :capture-names capture-names
                             :predicates predicates)))
          ;; Error
          (error 'query-compile-error
                 :source source
                 :offset error-offset
                 :error-type (error-type-from-code error-type))))))

(defun query-delete (query)
  "Explicitly delete a query (normally handled by GC)."
  (trivial-garbage:cancel-finalization query)
  (ffi:ts-query-delete (types:ts-query-ptr query)))

(defun query-pattern-count (query)
  "Get the number of patterns in a query."
  (ffi:ts-query-pattern-count (types:ts-query-ptr query)))

(defun query-capture-count (query)
  "Get the number of capture names in a query."
  (ffi:ts-query-capture-count (types:ts-query-ptr query)))

(defun get-capture-name-by-index (query index)
  "Get a capture name by index from a query."
  (aref (types:ts-query-capture-names query) index))

;;;; Query Cursor

(defmacro with-query-cursor ((cursor) &body body)
  "Execute BODY with a query cursor bound to CURSOR."
  `(let ((,cursor (ffi:ts-query-cursor-new)))
     (unwind-protect
          (progn ,@body)
       (ffi:ts-query-cursor-delete ,cursor))))

(defun query-exec (cursor query node)
  "Execute a query on a node using a cursor."
  (ffi:ts-query-cursor-exec cursor
                            (types:ts-query-ptr query)
                            (types:ts-node-buffer node)))

;;;; Query Match Iteration

(defun query-matches (query node)
  "Get all matches of a query on a node."
  (with-query-cursor (cursor)
    (query-exec cursor query node)
    (cffi:with-foreign-object (match-ptr '(:struct ffi:ts-query-match))
      (loop :while (ffi:ts-query-cursor-next-match cursor match-ptr)
            :collect (extract-match query node match-ptr)))))

(defun query-captures (query node &key source)
  "Get all captures of a query on a node as a flat list.
When SOURCE is provided, evaluate predicates to filter matches."
  (with-query-cursor (cursor)
    (query-exec cursor query node)
    (let ((predicates (types:ts-query-predicates query)))
      (cffi:with-foreign-objects ((match-ptr '(:struct ffi:ts-query-match))
                                  (capture-index :uint32))
        (if (or (null source) (zerop (hash-table-count predicates)))
            (loop :while (ffi:ts-query-cursor-next-capture cursor match-ptr capture-index)
                  :collect (extract-single-capture query node match-ptr
                                                   (cffi:mem-ref capture-index :uint32)))
            (let ((results nil))
              (loop :while (ffi:ts-query-cursor-next-capture cursor match-ptr capture-index)
                    :do (let* ((pattern-idx (cffi:foreign-slot-value
                                            match-ptr '(:struct ffi:ts-query-match) 'ffi::pattern-index))
                               (cap-count (cffi:foreign-slot-value
                                          match-ptr '(:struct ffi:ts-query-match) 'ffi::capture-count))
                               (captures-ptr (cffi:foreign-slot-value
                                             match-ptr '(:struct ffi:ts-query-match) 'ffi::captures)))
                          (let ((capture-map (make-hash-table)))
                            (dotimes (i cap-count)
                              (let* ((cap-ptr (cffi:mem-aptr captures-ptr
                                                            '(:struct ffi:ts-query-capture) i))
                                     (cap-idx (cffi:foreign-slot-value
                                              cap-ptr '(:struct ffi:ts-query-capture) 'ffi::index))
                                     (node-ptr (cffi:foreign-slot-pointer
                                               cap-ptr '(:struct ffi:ts-query-capture) 'ffi::node))
                                     (node-buffer (cffi:foreign-alloc '(:struct ffi:ts-node-raw))))
                                (cffi:foreign-funcall "memcpy"
                                                      :pointer node-buffer
                                                      :pointer node-ptr
                                                      :size (cffi:foreign-type-size
                                                             '(:struct ffi:ts-node-raw))
                                                      :pointer)
                                (setf (gethash cap-idx capture-map)
                                      (make-instance 'types:ts-node
                                                     :tree (types:ts-node-tree node)
                                                     :buffer node-buffer))))
                            (when (evaluate-match-predicates predicates pattern-idx
                                                             capture-map source)
                              (let ((idx (cffi:mem-ref capture-index :uint32)))
                                (push (extract-single-capture query node match-ptr idx)
                                      results))))))
              (nreverse results)))))))

(defun query-captures-in-range (query node start-byte end-byte &key source)
  "Get captures within a byte range. When SOURCE is provided, evaluate predicates."
  (with-query-cursor (cursor)
    (ffi:ts-query-cursor-set-byte-range cursor start-byte end-byte)
    (query-exec cursor query node)
    (let ((predicates (types:ts-query-predicates query)))
      (cffi:with-foreign-objects ((match-ptr '(:struct ffi:ts-query-match))
                                  (capture-index :uint32))
        (if (or (null source) (zerop (hash-table-count predicates)))
            ;; No source or no predicates: original behavior
            (loop :while (ffi:ts-query-cursor-next-capture cursor match-ptr capture-index)
                  :collect (extract-single-capture query node match-ptr
                                                   (cffi:mem-ref capture-index :uint32)))
            ;; With source and predicates: filter
            (let ((results nil))
              (loop :while (ffi:ts-query-cursor-next-capture cursor match-ptr capture-index)
                    :do (let* ((pattern-idx (cffi:foreign-slot-value
                                            match-ptr '(:struct ffi:ts-query-match) 'ffi::pattern-index))
                               (cap-count (cffi:foreign-slot-value
                                          match-ptr '(:struct ffi:ts-query-match) 'ffi::capture-count))
                               (captures-ptr (cffi:foreign-slot-value
                                             match-ptr '(:struct ffi:ts-query-match) 'ffi::captures)))
                          ;; Build capture map for this match
                          (let ((capture-map (make-hash-table)))
                            (dotimes (i cap-count)
                              (let* ((cap-ptr (cffi:mem-aptr captures-ptr
                                                            '(:struct ffi:ts-query-capture) i))
                                     (cap-idx (cffi:foreign-slot-value
                                              cap-ptr '(:struct ffi:ts-query-capture) 'ffi::index))
                                     (node-ptr (cffi:foreign-slot-pointer
                                               cap-ptr '(:struct ffi:ts-query-capture) 'ffi::node))
                                     (node-buffer (cffi:foreign-alloc '(:struct ffi:ts-node-raw))))
                                (cffi:foreign-funcall "memcpy"
                                                      :pointer node-buffer
                                                      :pointer node-ptr
                                                      :size (cffi:foreign-type-size
                                                             '(:struct ffi:ts-node-raw))
                                                      :pointer)
                                (setf (gethash cap-idx capture-map)
                                      (make-instance 'types:ts-node
                                                     :tree (types:ts-node-tree node)
                                                     :buffer node-buffer))))
                            ;; Evaluate predicates
                            (when (evaluate-match-predicates predicates pattern-idx
                                                             capture-map source)
                              (let ((idx (cffi:mem-ref capture-index :uint32)))
                                (push (extract-single-capture query node match-ptr idx)
                                      results))))))
              (nreverse results)))))))

;;;; Match/Capture Extraction

(defun extract-match (query node match-ptr)
  "Extract a match structure from foreign memory."
  (let* ((pattern-index (cffi:foreign-slot-value match-ptr '(:struct ffi:ts-query-match)
                                                 'ffi::pattern-index))
         (capture-count (cffi:foreign-slot-value match-ptr '(:struct ffi:ts-query-match)
                                                 'ffi::capture-count))
         (captures-ptr (cffi:foreign-slot-value match-ptr '(:struct ffi:ts-query-match)
                                                'ffi::captures))
         (captures (loop :for i :below capture-count
                         :collect (extract-capture-at query node captures-ptr i))))
    (types:make-query-match pattern-index captures)))

(defun extract-capture-at (query node captures-ptr index)
  "Extract a single capture from the captures array."
  (let* ((capture-ptr (cffi:mem-aptr captures-ptr '(:struct ffi:ts-query-capture) index))
         (capture-index (cffi:foreign-slot-value capture-ptr '(:struct ffi:ts-query-capture)
                                                 'ffi::index))
         (node-ptr (cffi:foreign-slot-pointer capture-ptr '(:struct ffi:ts-query-capture)
                                              'ffi::node))
         ;; Copy the node data
         (node-buffer (cffi:foreign-alloc '(:struct ffi:ts-node-raw))))
    (cffi:foreign-funcall "memcpy"
                          :pointer node-buffer
                          :pointer node-ptr
                          :size (cffi:foreign-type-size '(:struct ffi:ts-node-raw))
                          :pointer)
    (let ((captured-node (make-instance 'types:ts-node
                                        :tree (types:ts-node-tree node)
                                        :buffer node-buffer)))
      (types:make-query-capture captured-node
                                capture-index
                                (get-capture-name-by-index query capture-index)))))

(defun extract-single-capture (query node match-ptr capture-index)
  "Extract a single capture from a match at the given index."
  (let* ((captures-ptr (cffi:foreign-slot-value match-ptr '(:struct ffi:ts-query-match)
                                                'ffi::captures)))
    (extract-capture-at query node captures-ptr capture-index)))

;;;; Match/Capture Accessors

(defun match-pattern-index (match)
  "Get the pattern index of a match."
  (types:query-match-pattern-index match))

(defun match-captures (match)
  "Get the captures of a match."
  (types:query-match-captures match))

(defun capture-node (capture)
  "Get the node of a capture."
  (types:query-capture-node capture))

(defun capture-index (capture)
  "Get the index of a capture."
  (types:query-capture-index capture))

(defun capture-name (capture)
  "Get the name of a capture."
  (types:query-capture-name capture))
