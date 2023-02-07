(defun decode-line (LINE)
  (let* ((chunks (uiop:split-string
                  LINE :separator '(#\-)))
         (last-chunk (car (last chunks))))
    (destructuring-bind (id cs)
        (uiop:split-string last-chunk :separator '(#\[))
      (list (parse-integer id)
            (subseq cs 0 (1- (length cs)))
            (butlast chunks)))))

(defun input ()
  (loop for line in (uiop:read-file-lines "day04.input")
        collect (decode-line line)))

(defun comparator (PAIR1 PAIR2)
  (destructuring-bind (v1 . k1) PAIR1
    (destructuring-bind (v2 . k2) PAIR2
      (or (> v1 v2)
          (and (= v1 v2)
               (char< k1 k2))))))

(defun checksum (CHUNKS)
  (let ((m (make-hash-table)))
    (loop for chunk in CHUNKS
          do (loop for c across chunk
                   do (incf (gethash c m 0))))
    (let ((pairs (loop for k being each hash-key of m
                       collect (cons (gethash k m) k))))
      (map 'string #'cdr
           (subseq (sort pairs #'comparator) 0 5)))))

(defun valid-p (LINE)
  (string= (second LINE)
           (checksum (third LINE))))

(defun solution1 (LINES)
  (loop for line in LINES
        when (valid-p line)
          summing (first line)))

;; (solution1 (input))
;; => 185371

(defun make-decoder (SHIFT)
  (lambda (CHR)
    (code-char (+ 97 (mod (+ (- (char-code CHR) 97) SHIFT) 26)))))

(defun decypher (LINE)
  (loop with decoder = (make-decoder (first LINE))
        for word in (third LINE)
        collect (map 'string decoder word)))

(defun solution2 (LINES)
  (loop for line in LINES
        for message = (decypher line)
        when (member "northpole" message :test #'equal)
          do (return (first line))))

;; (solution2 (input))
;; => 984
