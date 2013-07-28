(defun problem30 ()
  (let ((nums (make-num-lst 1 9999999)))
    (reduce #'+ (delete-if-not #'(lambda (num)
                                   (let ((digits (explode num)))
                                     (and (cdr digits) ;; 2 or more
                                          (= num 
                                             (reduce #'+
                                                     (mapcar 
                                                       #'(lambda (d)
                                                           (expt d 5))
                                                       digits)))))) nums))))
