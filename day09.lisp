(load "parser.lisp")

(defparameter p/n-x-n
  (p/do p/num (p/skip (p/string "x")) p/num))

(defun string-break (STR POS)
  (cons (subseq STR 0 POS)
        (subseq STR POS)))

(defun chop (STR)
  (cond ((string= "" STR) nil)
        ((eql (uiop:first-char STR) #\()
         (let ((i (position #\) STR)))
           (cons (funcall p/n-x-n (subseq STR 1 i))
                 (subseq STR (1+ i)))))
        (t (let ((i (position #\( STR)))
             (if (null i)
                 (cons STR "")
                 (string-break STR i))))))

(defun input ()
  (string-trim '(#\NEWLINE) (uiop:read-file-string "day09.input")))

(defun explode (STR &optional (DECOMPRESS nil))
  (loop with acc = 0
        with cur = STR
        for chunk = (chop cur)
        while chunk
        for (c . rest) = chunk
        if (listp c)
          do (destructuring-bind (s n) c
               (unless DECOMPRESS
                 (setf cur (subseq rest s))
                 (incf acc (* s n)))
               (when DECOMPRESS
                 (destructuring-bind (prefix . tail)
                     (string-break rest s)
                   (incf acc (* n (explode prefix t)))
                   (setf cur tail))))
        else
          do (incf acc (length c))
             (setf cur rest)
        finally
           (return acc)))

(defun solution1 (STR)
  (explode STR))

;; (solution1 (input))
;; => 70186

(defun solution2 (STR)
  (explode STR t))

;; (solution2 (input))
;; => 10915059201
