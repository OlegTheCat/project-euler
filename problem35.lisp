(defun rotations (lst)
  (labels ((%rotate (lst) (append1 (cdr lst) (car lst)))
           (%perform (lst number)
                     (if (zerop number)
                       nil
                       (cons lst (%perform (%rotate lst) (1- number))))))

    (%perform lst (length lst))))

(defun circular-prime-p (prime)
  (let ((digits (explode prime)))
    (every #'prime-p (mapcar #'stick-num (rotations digits)))))

(defun problem35 ()
  (let ((primes (primes-below 1000000)))
    (count-if #'circular-prime-p primes)))

