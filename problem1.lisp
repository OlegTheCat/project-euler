(defun get-sum-of-3-or-5 (num-below &optional (sum 0))
  (if (< num-below 1)
      sum
      (get-sum-of-3-or-5 (1- num-below ) (if (or (= (mod num-below 3) 0) (= (mod num-below 5) 0)) (+ sum num-below) sum))))


(defun problem1 ()
  (get-sum-of-3-or-5 999))
