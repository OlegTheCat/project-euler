(defun problem4 ()
  (apply #'max (mapcar #'stick-num (remove-if-not #'palindrome-p 
                 (mapcar #'explode-num 
                    (loop for i from 999 downto 100 append
                      (loop for j from 999 downto 100 collect (* i j))))))))
