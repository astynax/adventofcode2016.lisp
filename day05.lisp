(ql:quickload :md5)

(defparameter input
  (string-trim '(#\NEWLINE)
               (uiop:read-file-string "day05.input")))

(defun md5-list (PREFIX INDEX)
  (coerce (md5:md5sum-string (format nil "~a~a" PREFIX INDEX))
          'list))

(defun good-hash-p (SUM)
  (and (zerop (first SUM))
       (zerop (second SUM))
       (< (third SUM) 16)))

(defun solution1 (PREFIX)
  (format
   nil "~{~X~}"
   (loop with counter = 0
         while (< counter 8)
         for i from 0
         for h = (md5-list PREFIX i)
         when (good-hash-p h)
           do (incf counter)
           and collect (third h))))

;; (solution1 input)
;; => "801B56A7"

(defun solution2 (PREFIX)
  (format
   nil "~{~X~}"
   (loop with o = (make-hash-table)
         while (< (hash-table-count o) 8)
         for i from 0
         for h = (md5-list PREFIX i)
         for k = (third h)
         when (and (good-hash-p h)
                   (< k 8)
                   (not (gethash k o)))
           do (setf (gethash k o)
                    (floor (/ (fourth h) 16)))
         finally
            (return
              (loop for k from 0 to 7
                    collect (gethash k o))))))

;; (solution2 input)
;; => "424A0197"
