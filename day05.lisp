(ql:quickload :md5)

(defparameter input "abbhdwsy")

(defun is-good (MD5)
  (and (= 0 (first MD5))
       (= 0 (second MD5))
       (> 16 (third MD5))))

(defun ->char (MD5)
  (format nil "~X" (third MD5)))

(defun md5-bytes (PREFIX INDEX)
  (coerce (md5:md5sum-string (format nil "~a~a" PREFIX INDEX))
          'list))

(defun solution1 (PREFIX)
  (format
   nil "~{~X~}"
   (loop with counter = 0
         while (< counter 8)
         for i from 0
         for h = (md5-bytes PREFIX i)
         when (is-good h)
           do (incf counter)
           and collect (third h))))

;; (solution1 input)
;; => "801B56A7"

(defun solution2 (PREFIX)
  (loop with o = (make-hash-table)
        while (< (hash-table-count o) 8)
        for i from 0
        for h = (md5-bytes PREFIX i)
        for b = (third h)
        when (and (is-good h)
                  (< b 8)
                  (not (gethash b o)))
          do (setf (gethash b o)
                   (floor (/ (fourth h) 16)))
        finally
           (return
             (format
              nil "~{~X~}"
              (loop for i from 0 to 7
                    collect (gethash i o))))))

(solution2 input)
;; => "424A0197"
