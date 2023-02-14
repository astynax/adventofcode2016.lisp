(defmacro defparser (name args &body body)
  `(defun ,name ,args
     (lambda (source)
       ,@body)))

(defparser p/string (sample)
  (let* ((l (length sample))
         (prefix (ignore-errors (subseq source 0 l))))
    (cond ((equal prefix sample)
           (values prefix (subseq source l)))
          (t (error "Expected:~% ~s~%Actual:~% ~s" sample source)))))

(defparser p/string-of (pred)
  (loop for i from 0 below (length source)
        for c = (aref source i)
        unless (funcall pred c)
          do (return (values (subseq source 0 i)
                             (subseq source i)))
        finally
           (return (values source ""))))

(defparameter p/num
  (lambda (source)
    (multiple-value-bind (res len)
        (parse-integer source :junk-allowed t)
      (cond ((null res)
             (error "Expected:~% NUMBER~%Actual:~% ~s~%" source))
            (t (values res (subseq source len)))))))

(defparser p/skip (parser)
  (multiple-value-bind (_ rest) (funcall parser source)
    (declare (ignore _))
    (values 'skip rest)))

(defparser p/progn (parser &rest parsers)
  (loop with s = source
        with last-res = nil
        for p in (cons parser parsers)
        do (multiple-value-bind (res rest)
               (funcall p s)
             (setf last-res res)
             (setf s rest))
        finally (return (values last-res s))))

(defparser p/or (p1 p2)
  (handler-case (funcall p1 source)
    ;; TODO: join errors if were no matches
    (t () (funcall p2 source))))

(defparser p/pure (val)
  (values val source))

(defparser p/map (f parser)
  (multiple-value-bind (res rest)
      (funcall parser source)
    (values (funcall f res) rest)))

(defparser p/map2 (f p1 p2)
  (multiple-value-bind (r1 rest1) (funcall p1 source)
    (multiple-value-bind (r2 rest2) (funcall p2 rest1)
      (values (funcall f r1 r2) rest2))))

(defparser p/many (parser)
  (funcall (p/or (p/map2 #'cons parser (p/many parser))
                 (p/pure nil))
           source))

(defun p/sep-by-1 (separator parser)
  (p/map2 #'cons parser (p/many (p/progn separator parser))))

(defun p/sep-by (separator parser)
  (p/or (p/sep-by-1 separator parser)
        (p/pure nil)))

(defparser p/do (parser &rest parsers)
  (loop with src = source
        with result = nil
        for step in (cons parser parsers)
        do (multiple-value-bind (val rest)
               (funcall step src)
             (setf src rest)
             (unless (eql 'skip val)
               (push val result)))
        finally
           (return (values (nreverse result) src))))

#+f
(parse "123,456,789foo"
       (p/sep-by (p/string ",") p/num)
       (p/skip (p/string-of #'alpha-char-p)))
;; => ((123 456 789))
#+f
(parse "123,456" p/num (p/skip (p/string ",")) p/num)
;; => (123 456)
(defun parse (source &rest steps)
  (multiple-value-bind (rest result)
      (apply (apply #'p/do steps) source)
    (if (zerop (length rest))
        (reverse result)
        (error "Unhandled input:~% ~s~%" rest))))
