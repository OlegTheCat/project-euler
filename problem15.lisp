(defun problem15()
	(do ((lst '(0) (let ((new (reduce 
								#'(lambda (x y) (cons (+ (car x) y) x)) (reverse (butlast lst)) :initial-value '(1))))
					 (cons (* 2 (car new)) new))))
	  ((> (length lst) 20) (car lst))))
