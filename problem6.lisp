(defun problem6 ()
  (let ((lst (make-num-lst 1 100)))
       (- (expt (apply #'+ lst) 2) (apply #'+ (mapcar #'(lambda (x) (* x x)) lst)))))
       
