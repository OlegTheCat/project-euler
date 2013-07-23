(defun all-permutations-of (lst)
  (if lst
	(let ((perms (all-permutations-of (cdr lst))))
	  (append 
		(mapcar #'(lambda (x)
					(cons (car lst) x)) 
				perms) 
		(mapcan #'(lambda (x) 
					(loop for i from 1 to (length x) collect (insert-before x (car lst) i))) 
				perms)))
	(list nil)))

(defun insert-before (lst elem pos)
  (if (= pos 0)
	(cons elem lst)
	(cons (car lst) (insert-before (cdr lst) elem (1- pos)))))

(defun problem24 ()
  (nth 999999 (mapcar #'stick-num (p (make-num-lst 0 9)))))


(defun p (l)
  (if (null l)
	(list nil)	
	(mapcan #'(lambda (x)
				(mapcar #'(lambda (y)
							(cons x y))
						(p (remove x l :count 1)))) l)))
