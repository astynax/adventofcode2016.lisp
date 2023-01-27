(defun chop-step (SRC)
  (unless (zerop (length SRC))
    (let ((turn (char SRC 0)))
      (multiple-value-bind (val len)
          (parse-integer SRC :start 1 :junk-allowed t)
        (cons (cons turn val)
              (when (and (> (length SRC) len)
                         (eql (char SRC len) #\,))
                (subseq SRC (+ 2 len))))))))

(defun input (&optional TEST-DATA)
  (loop with src = (or TEST-DATA
                       (uiop:read-file-line "day01.input"))
        while src
        for (pair . new-src) = (chop-step src)
        do (setf src new-src)
        collect pair))

(defun turn-right (DIR)
  (case DIR
    ((n) 'w)
    ((w) 's)
    ((s) 'e)
    (otherwise 'n)))

(defun turn-left (DIR)
  (case DIR
    ((n) 'e)
    ((e) 's)
    ((s) 'w)
    (otherwise 'n)))

(defun one-step (DIR X Y)
  (case DIR
    ((n) (cons x (1- y)))
    ((s) (cons x (1+ y)))
    ((e) (cons (1- x) y))
    (otherwise (cons (1+ x) y))))

(defun solution
    (INP &key
           (TEST #'(lambda (S X Y) (declare (ignore S X Y))nil))
           STATE
           (STATE-STEP #'(lambda (S X Y) (declare (ignore X Y)) S)))
  (let ((x 0)
        (y 0))
    (loop named main
          with s = STATE
          with dir = 'n
          for (turn . len) in INP
          do
             (setf dir (if (eql turn #\R)
                           (turn-right dir)
                           (turn-left dir)))
             (loop repeat len
                   for (new-x . new-y) = (one-step dir x y)
                   do (setf x new-x)
                      (setf y new-y)
                      (if (funcall TEST s x y)
                          (return-from main)
                          (setf s (funcall STATE-STEP s x y)))))
    (+ (abs x) (abs y))))
;; (solution (input))
;; => 239

(defun solution2 (INP)
  (solution INP
            :test #'(lambda (S X Y) (gethash (cons X Y) S))
            :state (make-hash-table :test #'equal)
            :state-step #'(lambda (S X Y) (prog1 S
                                       (setf (gethash (cons X Y) S) t)))))
;; (solution2 (input))
;; => 141
