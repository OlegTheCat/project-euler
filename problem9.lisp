(defun triplet-p (a b c)
  (and (< a b) (< b c) (= (+ (* a a) (* b b)) (* c c))))


(defun problem9()
   (loop for i from 1 to 1000 do
         (loop for j from (1+ i) to 1000 for k = (- 1000 i j) do
                     (and (triplet-p i j k) (return-from problem9 (* i j k))))))
