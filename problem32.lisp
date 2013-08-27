(defun problem32 ()
  (let ((products (loop for i from 1 to 2000 nconc
                        (loop for j from 1 to 2000 
                              for prod = (* i j)
                              if (pandigital-p (nconc (explode i) (explode j) (explode prod)))
                              collect prod))))
    (reduce #'+ (remove-duplicates products))))

(defun pandigital-p (nums &key (range '(1 . 9))) 
  (and (= (length nums) (cdr range))
       (equal (sort (copy-list nums) #'<) 
              (make-num-lst (car range) (cdr range)))))
