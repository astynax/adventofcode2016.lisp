(defun decode-line (LINE)
  (loop for chunk in (uiop:split-string LINE)
        unless (zerop (length chunk))
          collect (parse-integer chunk)))

(defun input ()
  (loop for line in (uiop:read-file-lines "day03.input")
        collect (decode-line line)))

(defun solution1 (TRIANGLES)
  (loop for (a b c) in TRIANGLES
        when (and (> (+ a b) c)
                  (> (+ a c) b)
                  (> (+ b c) a))
          count 1))

;; (solution1 (input))
;; => 917

(defun flatten-columns (TRIPLES)
  (loop for (a b c) in TRIPLES
        collect a into list-a
        collect b into list-b
        collect c into list-c
        finally (return (append list-a
                                list-b
                                list-c))))

(defun split-by-3 (A-LIST)
  (loop with cursor = A-LIST
        while cursor
        for chunk = (subseq cursor 0 3)
        for new-cursor = (subseq cursor 3)
        collect chunk
        do (setf cursor new-cursor)))

(defun solution2 (TRIPLES)
  (solution1
   (split-by-3
    (flatten-columns TRIPLES))))

;; (solution2 (input))
;; => 1649
