(defun problem31 ()
  (count-coins 200 '(1 2 5 10 20 50 100 200)))

(defun count-coins (money coins)
  (cond
    ((= money 0) 1)
    ((or (< money 0) (null coins)) 0)
    (t (+ (count-coins (- money (car coins)) coins) (count-coins money (cdr coins))))))
