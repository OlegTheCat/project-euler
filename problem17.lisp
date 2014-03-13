(defparameter repr 
  '((1 . "one")
	(2 . "two")
	(3 . "three")
	(4 . "four")
	(5 . "five")
	(6 . "six")
	(7 . "seven")
	(8 . "eight")
	(9 . "nine")
	(10 . "ten")
	(11 . "eleven")
	(12 . "twelve")
	(13 . "thirteen")
	(14 . "fourteen")
	(15 . "fifteen")
	(16 . "sixteen")
	(17 . "seventeen")
	(18 . "eighteen")
	(19 . "nineteen")
	(20 . "twenty")
	(30 . "thirty")
	(40 . "forty")
	(50 . "fifty")
	(60 . "sixty")
	(70 . "seventy")
	(80 . "eighty")
	(90 . "ninety")))

(defun num-to-word (num-lst)
  (and num-lst
	(case (length num-lst)
		(1 (transform-digit num-lst))
		(2 (transform-decade num-lst))
		(3 (transform-hundred num-lst))
		(4 (list "one" "thousand")))))

(defun transform-hundred (num-lst)
  (cons 
	(car (transform-digit (list (car num-lst)))) 
	(cons 
	  "hundred" 
	  (aand (transform-decade (cdr num-lst)) 
			(cons "and" it)))))

(defun transform-decade (num-lst)
  (case (car num-lst)
	(0 (transform-digit (cdr num-lst)))
	(1 (list (cdr-assoc (stick-num num-lst) repr)))
	(otherwise (cons (cdr-assoc (* 10 (car num-lst)) repr) (transform-digit (cdr num-lst))))))


(defun transform-digit (num-lst)
  (and (not (= (car num-lst) 0))
			(list (cdr-assoc (car num-lst) repr))))

(defun problem17 ()
  (length (apply #'concatenate 'string (loop for i from 1 to 1000 append (num-to-word (explode i))))))
