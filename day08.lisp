(load "parser.lisp")

(defparameter p/rect
  (p/do
   (p/pure 'rect)
   (p/skip (p/string "rect "))
   p/num
   (p/skip (p/string "x"))
   p/num))

(defparameter p/rotate
  (p/do
   (p/skip (p/string "rotate "))
   (p/or (p/progn (p/string "column x=")
                  (p/pure 'rotate-column))
         (p/progn (p/string "row y=")
                  (p/pure 'rotate-row)))
   p/num
   (p/skip (p/string " by "))
   p/num))

(defun input ()
  (loop for line in (uiop:read-file-lines "day08.input")
        nconcing (parse line (p/or p/rect p/rotate))))

(defun make-display ()
  (make-array '(6 50) :initial-element nil))

(defun rotate-column-by-1 (ARR X)
  (loop with h = (1- (array-dimension ARR 0))
        with tmp = (aref ARR h X)
        for y from h above 0
        do (setf (aref ARR y X)
                 (aref ARR (1- y) X))
        finally
           (setf (aref ARR 0 X) tmp)))

(defun rotate-row-by-1 (ARR Y)
  (loop with w = (1- (array-dimension ARR 1))
        with tmp = (aref ARR Y w)
        for x from w above 0
        do (setf (aref ARR Y x)
                 (aref ARR Y (1- x)))
        finally
           (setf (aref ARR Y 0) tmp)))

(defgeneric perform (ACTION ARR ARG1 ARG2))

(defmethod perform ((ACTION (eql 'rect)) ARR W H)
  (loop for x from 0 below W
        do (loop for y from 0 below H
                 do (setf (aref ARR y x) t))))

(defmethod perform ((ACTION (eql'rotate-column)) ARR X N)
  (loop repeat N
        do (rotate-column-by-1 ARR X)))

(defmethod perform ((ACTION (eql'rotate-row)) ARR Y N)
  (loop repeat N
        do (rotate-row-by-1 ARR Y)))

(defun populate (ARR STEPS)
  (loop for (action a b) in STEPS
        do (perform action ARR a b)
        finally (return ARR)))

(defun solution1 (STEPS)
  (loop with arr = (populate (make-display) STEPS)
        for x from 0 below 50
        sum (loop for y from 0 below 6
                  count (aref arr y x))))

;; (solution1 (input))
;; => 110

(defun draw (ARR)
  (loop for y from 0 below 6
        do (loop for x from 0 below 50
                 do (princ (if (aref ARR y x) #\# #\SPACE)))
           (princ #\NEWLINE)))

(defun solution2 (STEPS)
  (draw (populate (make-display) STEPS)))

;; (solution2 (input))
;; ####   ## #  # ###  #  #  ##  ###  #    #   #  ##
;;    #    # #  # #  # # #  #  # #  # #    #   #   #
;;   #     # #### #  # ##   #    #  # #     # #    #
;;  #      # #  # ###  # #  #    ###  #      #     #
;; #    #  # #  # # #  # #  #  # #    #      #  #  #
;; ####  ##  #  # #  # #  #  ##  #    ####   #   ##
