(defun problem13 ()
  (with-open-file (stream "data/problem13.txt" :if-does-not-exist nil)
	(stick-num (subseq (explode-num (funcall (alambda (stream) 
													  (aif (read-line stream nil nil)
														   (+ (parse-integer it) (self stream)) 
														   0)) 
											 stream)) 0 10))))