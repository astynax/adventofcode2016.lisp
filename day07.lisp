(defun decode-line (LINE)
  (let ((package (uiop:split-string
                  LINE :separator '(#\[))))
    (cons (cons 'out (first package))
          (loop for pair in (cdr package)
                for items = (uiop:split-string
                             pair :separator '(#\]))
                collect (cons 'in (first items))
                collect (cons 'out (second items))))))

(defun input ()
  (loop for line in (uiop:read-file-lines "day07.input")
        collect (decode-line line)))

(defun has-key-p (STR)
  (loop for i from 0 below (- (length STR) 3)
        for c1 = (char STR i)
        for c2 = (char STR (+ i 1))
        for c3 = (char STR (+ i 2))
        for c4 = (char STR (+ i 3))
        when (and (char= c1 c4)
                  (char= c2 c3)
                  (not (char= c1 c2)))
          do (return t)))

(defun support-tls-p (PACKAGE)
  (flet ((check (key)
           (loop for (k . s) in PACKAGE
                 when (and (eql k key)
                           (has-key-p s))
                   do (return t))))
    (and (check 'out) (not (check 'in)))))

(defun solution1 (LINES)
  (loop for line in LINES
        count (support-tls-p line)))

;; (solution1 (input))
;; => 118

(defun aba-keys (STR)
  (loop for i from 0 below (- (length STR) 2)
        for c1 = (char STR i)
        for c2 = (char STR (+ i 1))
        for c3 = (char STR (+ i 2))
        when (and (char= c1 c3)
                  (not (char= c1 c2)))
          collect (coerce (list c1 c2 c3) 'string)))

(defun all-aba-keys (PACKAGE KEY)
  (loop for (k . s) in PACKAGE
        when (eql k KEY)
          nconcing (aba-keys s)))

(defun support-ssl-p (PACKAGE)
  (loop with outs = (all-aba-keys PACKAGE 'out)
        with ins = (all-aba-keys PACKAGE 'in)
        for o in outs
        for i = (let ((c1 (char o 0))
                      (c2 (char o 1)))
                  (coerce (list c2 c1 c2) 'string))
        when (member i ins :test #'string=)
          do (return t)))

(defun solution2 (LINES)
  (loop for line in LINES
        count (support-ssl-p line)))

;; (solution2 (input))
;; => 260
