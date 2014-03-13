(defun problem37 ()
  (let ((truncatable-primes nil))
    (loop for i from 1 while (< (length truncatable-primes) 11) if (is-truncatable-p i) do
          (push i truncatable-primes))
    (reduce #'+ truncatable-primes)))

(defun is-truncatable-p (num)
  (and (prime-p num)
       (let ((nums (explode num)))
         (and (cdr nums) ;; len 2 or more
              (every #'prime-p (maplist #'(lambda (x) (stick-num x)) (cdr nums)))
              (every #'prime-p (mapbutlast #'(lambda (x) (stick-num x)) (butlast nums)))))))
