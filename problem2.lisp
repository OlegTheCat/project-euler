(defun sum-of-even-fibos (below &optional (start-from 1))
  (let ((num (fibo start-from)))
      (if (< num below)
           (+ (if (evenp num) num 0) (sum-of-even-fibos below (1+ start-from)))
           0)))

(defun problem2()
  (sum-of-even-fibos 4000000))
