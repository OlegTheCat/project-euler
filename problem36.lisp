(defun double-base-palindrome-p (num)
  (let ((numbers (explode num))
        (bits (bits num)))
    (and (palindrome-p numbers)
         (palindrome-p bits))))

(defun problem36 ()
  (loop for i from 0 to 999999 if (double-base-palindrome-p i) sum
        i))
