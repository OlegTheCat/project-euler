(defun problem67 ()
  (with-open-file (stream "data/problem67.txt" :if-does-not-exist nil)
	(car (max-path (read-triangle stream)))))
