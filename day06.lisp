(defun input ()
  (loop for line in (uiop:read-file-lines "day06.input")
        collect line))

(defun solution (LINES TEST)
  (coerce
   (loop with width = (length (first LINES))
         for col from 0 below width
         for m = (make-hash-table)
         do (loop for line in LINES
                  for c = (char line col)
                  do (incf (gethash c m 0)))
         collect
         (car (first
               (sort (loop for k being each hash-key of m
                           collect (cons k (gethash k m)))
                     (lambda (p1 p2)
                       (funcall TEST (cdr p1) (cdr p2)))))))
   'string))

;; (solution (input) #'>)
;; => "asvcbhvg"

;; (solution (input) #'<)
;; => "odqnikqv"
