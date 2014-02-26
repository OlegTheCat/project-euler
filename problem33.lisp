(defun problem33 ()
  (let* ((fractions (loop for i from 10 to 99 nconc
                          (loop for j from 10 to 99
                                if (< (/ i j) 1) 
                                collect
                                (list i j))))
         (non-trivial (filter #'(lambda (f)
                                  (let* ((expanded-fraction (mapcar #'explode f))
                                         (common-numbers (apply #'intersection expanded-fraction)))
                                    (when (and (single common-numbers)
                                               (is-non-trivial-p expanded-fraction))
                                      (let* ((cancelled-fraction (cancel-fraction expanded-fraction common-numbers))) 
                                        (unless (some #'null cancelled-fraction)
                                          (let ((simplified-cancelled-fraction (simplify-fraction cancelled-fraction))
                                                (simplified-fraction (simplify-fraction f)))
                                            (when (equal simplified-cancelled-fraction simplified-fraction)
                                              simplified-fraction))))))) fractions)))
    (simplify-fraction
      (reduce #'(lambda (x acc) (list (* (first x) (first acc))
                                      (* (second x) (second acc)))) non-trivial :initial-value '(1 1)))))

(defun cancel-fraction (expanded-fraction common-numbers)
  (mapcar #'stick-num (mapcar #'(lambda (x) (set-exclusive-or x common-numbers)) expanded-fraction)))

(defun simplify-fraction (fraction)
  (let ((fraction-gcd (apply #'gcd fraction)))
    (mapcar #'(lambda (x) (/ x fraction-gcd)) fraction)))

(defun is-non-trivial-p (expanded-fraction)
  (notevery #'zerop (mapcar #'second expanded-fraction)))
