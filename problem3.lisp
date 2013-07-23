(defun largest-prime-factor (n largest)
  (aif (find-divisor n (1- largest) #'1- #'(lambda (n s) (> s 1)))
        (if (prime-p it)
            it
            (largest-prime-factor n it))))

(defun problem3()
  (largest-prime-factor 600851475143 (floor (sqrt 600851475143))))
