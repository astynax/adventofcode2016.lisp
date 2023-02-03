(defun decode-line (LINE)
  (let* ((chunks (uiop:split-string
                  LINE :separator '(#\-)))
         (last-chunk (car (last chunks))))
    (multiple-value-bind (id id-stop)
        (parse-integer last-chunk
                       :junk-allowed t)
      (let ((cs (subseq last-chunk
                        (1+ id-stop)
                        (1- (length last-chunk)))))
        (list id cs (butlast chunks))))))

(defun input ()
  (loop for line in (uiop:read-file-lines "day04.input")
        collect (decode-line line)))

(defun get-cs (CHUNKS)
  (let ((m (make-hash-table)))
    (loop for chunk in CHUNKS
          do (loop for c across chunk
                   do (incf (gethash c m 0))))
    (let ((freqs
            (sort (loop for k being each hash-key in m
                        collect (cons (gethash k m) k))
                  (lambda (pair1 pair2)
                    (destructuring-bind
                        ((v1 . k1) (v2 . k2))
                        (list pair1 pair2)
                      (or (> v1 v2)
                          (and (= v1 v2)
                               (char< k1 k2))))))))
      (loop with cs = (make-string 5)
            for i from 0 below 5
            for (_ . v) in freqs
            do (setf (char cs i) v)
            finally (return cs)))))

(defun valid-p (LINE)
  (string= (second LINE)
           (get-cs (third LINE))))

(defun solution1 (LINES)
  (loop for line in LINES
        for id = (first line)
        when (valid-p line)
          summing id))

;; (solution1 (input))
;; => 185371

(defun rot (N CHR)
  (code-char (+ 97 (mod (+ (- (char-code CHR) 97) N) 26))))

(defun decypher-line (LINE)
  (loop with id = (first LINE)
        with chunks = (third LINE)
        for chunk in chunks
        collect (loop with out = (make-string (length chunk))
                      for i from 0
                      for c across chunk
                      do (setf (char out i) (rot id c))
                      finally (return out))))

(defun solution2 (LINES)
  (loop for line in LINES
        when (and (valid-p line)
                  (member "northpole"
                          (decypher-line line)
                          :test #'equal))
          do (return (first line))))

;; (solution2 (input))
;; 984
