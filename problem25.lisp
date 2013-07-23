(defun problem25 ()
  (do ((i 1 (1+ i)))
	((= 1000 (length (explode (fibo i :initial-values '(1 . 1))))) i)))
