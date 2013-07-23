(defun problem12()
  (loop for i from 2 by 1 for num = (* (/ (+ 1 i) 2) i) do
		(when (> (length (divisors-of num)) 500)
		  (return-from problem12 num))))
