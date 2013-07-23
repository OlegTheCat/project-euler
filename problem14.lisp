(defun collatz-seq-next (n)
  (if (evenp n)
	(/ n 2)
	(1+ (* 3 n))))

(defun collatz-seq-length (n)
  (if (> n 1)
	(+ 1 (collatz-seq-length (collatz-seq-next n)))
	1))

(setf (fdefinition 'collatz-seq-length) (memoize #'collatz-seq-length))

(defun problem14 ()
  (let ((alst (loop for i from 1 to 1000000 collect (cons i (collatz-seq-length i)))))
	(car (rassoc (apply #'max (mapcar #'cdr alst)) alst))))
