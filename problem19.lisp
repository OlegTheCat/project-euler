(defun problem19 ()
  (loop for year from 1901 to 2000 sum
		(loop for month from 1 to 12 sum
			  (multiple-value-bind 
				(second minute hour date month year day-of-week dst-p tz) 
				(decode-universal-time (encode-universal-time 0 0 0 1 month year))
				(if (= day-of-week 6) 1 0)))))
