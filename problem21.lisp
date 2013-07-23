(defun dotted-pair-equal-p (x y)
  (or
	(and
	  (= (car x) (car y))
	  (= (cdr x) (cdr y)))
	(and 
	  (= (cdr x) (car y))
	  (= (car x) (cdr y)))))

(defun sum-proper-divisors-of (n)
  (reduce #'+ (proper-divisors-of n)))

(defun amicable-pair-of (n)
  (let ((new (sum-proper-divisors-of n)))
	(and (not (= n new))
		 (= (sum-proper-divisors-of new) n)
		 (cons n new))))

(defun amicable-pairs-below (below)
  (do ((i 1 (1+ i))
	   (result nil))
	((> i below) result)
		  (awhen (amicable-pair-of i)
			(pushnew it result :test #'dotted-pair-equal-p)))) 


(defun problem21 ()
  (reduce #'+ (mapcar #'(lambda (x) (+ (car x) (cdr x))) (amicable-pairs-below 10000))))
