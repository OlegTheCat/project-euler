(defun problem29 ()
  (let ((combs (loop for i from 2 to 100 nconc
                     (loop for j from 2 to 100 collect
                           (expt i j)))))
    (length (remove-duplicates combs))))
