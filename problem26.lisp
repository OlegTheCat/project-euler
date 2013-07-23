(defun period-starts-from (n)
	(let ((twos (factorize-by n 2))
		   (fives (factorize-by n 5)))
	  (values (1+ (max twos fives)) (/ (/ n (expt 2 twos)) (expt 5 fives)))))

(defun factorize-by (n factor &key (initial-value 0))
  (if (= 0 (mod n factor))
	(factorize-by (/ n factor) factor :initial-value (1+ initial-value))
	(values initial-value n)))

(defun period-length (n)
  (multiple-value-bind 
	(pos val)
	(period-starts-from n)
	(if (= val 1)
	  0
	  (do ((j 9 (+ 9 (* j 10))))
		  ((= 0 (mod j val)) (length (explode j)))))))

(defun problem26 ()
  (let ((alist (loop for i from 1 to 999 collect (cons i (period-length i)))))
	(car (rassoc (reduce #'max (mapcar #'cdr alist)) alist))))
