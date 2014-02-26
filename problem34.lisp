(defun problem34 ()
  (let ((curious-nums (loop for i from 10 to 2903040
                            if (= i (reduce #'+ (mapcar #'factorial (explode i))))
                            collect i)))
    (reduce #'+ curious-nums)))
