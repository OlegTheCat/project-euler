(defun gen-formula (a b)
  #'(lambda (n) (+ (* n n) (* a n) b)))

(defun problem27 ()
  (labels ((%gen-coeficients (max-abs-a max-abs-b)
							 (loop for i from (- max-abs-a) to max-abs-a append
								   (loop for j from (- max-abs-b) to max-abs-b collect (list i j)))))
	(do ((i 0 (1+ i)) ;; Not do*
		 (coefs (%gen-coeficients 999 999) (delete-if-not #'(lambda (coef-pair) 
																(prime-p 
																  (funcall 
																	(gen-formula 
																	  (first coef-pair) 
																	  (second coef-pair)) i))) coefs)))
	  ((single coefs) (reduce #'* (car coefs))))))
