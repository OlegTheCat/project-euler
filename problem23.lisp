(defun abundants-below-eq (n)
  (loop for i from 1 to n if (> (sum-proper-divisors-of i) i) collect i))


#|(defun sum-of-abundant-below (n)
  (let ((abundants (abundant-numbers-below n)))
	(labels ((odd-abundants (even odd)
				  (loop for i in even append
						(loop for j in odd for sum = (+ i j) if (not (> sum n)) collect sum)))
			 (even-abundants ()
							 (let ((abundants-below-48 '(24 30 32 36 38 40 42 44)))
							   (if (< n 48)
								 (remove-if #'(lambda (x) (> x n)) abundants-below-48)
								 (append abundants-below-48
										 (loop for i from 48 to n by 2 collect i))))))
	  (append (odd-abundants (remove-if #'oddp abundants) (remove-if #'evenp abundants))
			  (even-abundants)))))
|#

(defun sums-of-two-abundants-below-eq (n)
  (funcall (alambda (lst)
					(and lst
						 (union (mapcan #'(lambda (x)
											(let ((sum (+ (car lst) x)))
											  (and (not (> sum n)) 
												   (list sum)))) 
										lst)
								(self (cdr lst))))) (abundants-below-eq n)))


(defun problem23 ()
	(- (reduce #'+ (make-num-lst 1 20161)) (reduce #'+ (sums-of-two-abundants-below-eq 20161))))
