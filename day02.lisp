(defvar test-input
  '("ULL"
    "RRDDD"
    "LURDL"
    "UUUUD"))

(defun input ()
  (uiop:read-file-lines "day02.input"))

(defun keypad ()
  (loop
    with keymap = (make-hash-table :test #'equal)
    for y from 0 to 2
    do (loop
         for x from 0 to 2
         do (setf (gethash (cons x y) keymap)
                  (format nil "~s"
                          (+ 1 x (* 3 y)))))
    finally (return keymap)))

(defun keypad2 ()
  (loop with keymap = (make-hash-table :test #'equal)
        for row in
        '("  1  "
          " 234 "
          "56789"
          " ABC "
          "  D  ")
        for y from 0
        do (loop for c across row
                 for x from 0
                 unless (eql c #\SPACE)
                   do (setf (gethash (cons x y) keymap)
                            (make-string 1 :initial-element c)))
        finally (return keymap)))

(defun move (KEYMAP STEP X Y)
  (let ((key (case STEP
               ((#\U) (cons X (1- Y)))
               ((#\D) (cons X (1+ Y)))
               ((#\L) (cons (1- X) Y))
               ((#\R) (cons (1+ X) Y))
               (otherwise (cons X Y)))))
    (if (gethash key KEYMAP)
        key
        (cons X Y))))

(defun solve (KEYMAP START INP)
  (apply
   #'concatenate
   'string
   (loop with (x . y) = START
         for steps in INP
         do (loop for step across steps
                  for (new-x . new-y)
                    = (move KEYMAP step x y)
                  do (setf x new-x
                           y new-y))
         collect (gethash (cons x y) KEYMAP))))

;; (solve (keypad) (cons 1 1) (input))
;; => "92435"

;; (solve (keypad2) (cons 0 2) (input))
;; => "C1A88"
