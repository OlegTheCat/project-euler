(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
        (multiple-value-bind 
              (result exists)
            (gethash args cache)
          (if exists
              result
              (setf (gethash args cache)
                    (apply fn args))))))) 


(defun map0-n (fn n)
	(mapa-b fn 0 n))

(defun map1-n (fn n)
	(mapa-b fn 1 n))

(defun mapa-b (fn a b &optional (step 1))
	(do ((i a (+ i step))
		(result nil))
	  ((> i b) (nreverse result))
	  (push (funcall fn i) result)))

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))


(defmacro aand (&rest args)
  (cond ((null args) t)
		((null (cdr args)) (car args))
		(t `(aif ,(car args) (aand ,@(cdr args))))))

(defmacro alambda (parms &body body)
  `(labels ((self ,parms ,@body))
           #'self))

(defmacro awhen (test-form &body body)
  `(aif ,test-form
		(progn ,@body)))

(defmacro awhile (expr &body body)
  `(do ((it ,expr ,expr))
	 ((not it))
	 ,@body))

(defmacro when-bind ((var expr) &body body)
  `(let ((,var ,expr))
	 (when ,var
	   ,@body)))

(defmacro acond (&rest clauses)
  (if (null clauses)
	nil
	(let ((cl1 (car clauses))
		  (sym (gensym)))
	  `(let ((,sym ,(car cl1)))
		 (if ,sym
		   (let ((it ,sym)) ,@(cdr cl1))
		   (acond ,@(cdr clauses)))))))

(defmacro when-bind* (binds &body body)
  (if (null binds)
	`(progn ,@body)
	`(let (,(car binds))
	   (if ,(caar binds)
		 (when-bind* ,(cdr binds) ,@body)))))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
						   `(,s (gensym)))
				 syms)
	 ,@body))


#|(defmacro do-tuples/o (parms source &body body)
  (if parms
	(let ((src (gensym)))
	  `(prog ((,src ,source))
			 (mapc #'(lambda ,parms ,@body)
				   ,@(map0-n #'(lambda (n)
								 `(nthcdr ,n ,src)) (1- (length parms))))))))

(defmacro do-tuples/c (parms source &body body)
  (if parms
	(with-gensyms (src rest bodfn)
				  (let ((len (length parms)))
					`(let ((,src ,source))
					   (when (nthcdr ,(1- len) ,src)
						 (labels ((,bodfn ,parms ,@body))
						   (do ((,rest ,src (cdr ,rest)))
							 ((not (nthcdr ,(1- len) ,rest))
							  ,@(mapcar #'(lambda (args)
												  `(,bodfn ,@args))
										(dt-args len rest src))
							  nil)
							 (,bodfn ,@(map1-n #'(lambda (n)
												   `(nth ,(1- n)
														 ,rest))
											   len))))))))))

(defun dt-args (len rest src)
	(map0-n #'(lambda (m)
					  (map1-n #'(lambda (n)
										(let ((x (+ m n)))
										  (if (>= x len)
											`(nth ,(- x len) ,src)
											`(nth ,(1- x) ,rest)))) len)) (- len 2)))

|#

(defun mkstr (&rest args)
	(with-output-to-string (s)
	  (dolist (a args) (princ a s))))

(defun symb (&rest args)
	(values (intern (apply #'mkstr args))))


(defun mappend (fn &rest lsts)
	(apply #'append (apply #'mapcar fn lsts)))

(defun mapcars (fn &rest lsts)
  (let ((result nil))
	(dolist (lst lsts)
	  (dolist (obj lst)
		(push (funcall fn obj) result)))
	(nreverse result)))

(defun rmapcar (fn &rest args)
  (if (some #'atom args)
	(apply fn args)
	(apply #'mapcar
		   #'(lambda (&rest args)
					 (apply #'rmapcar fn args))
		   args)))

(proclaim '(inline last1 single append1 conc1 mklist))

(defun last1 (lst)
  (car (last lst)))

(defun single (lst)
  (and (consp lst) (not (cdr lst))))

(defun append1 (lst obj)
  (append lst (list obj)))

(defun conc1 (lst obj)
  (nconc lst (list obj)))

(defun mklist (obj)
  (if (listp obj) obj (list obj)))

(defun longer (x y)
  (labels ((compare (x y)
                    (and (consp x)
                         (or (null y)
                             (compare (cdr x) (cdr y))))))
          (if (and (listp x) (listp y))
              (compare x y)
              (> (length x) (length y)))))

(defun filter (fn lst)
  (let ((acc nil))
       (dolist (x lst)
               (let ((val (funcall fn x)))
                    (if val (push val acc))))
       (nreverse acc)))

(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
                (let ((rest (nthcdr n source)))
                     (if (consp rest)
                         (rec rest (cons (subseq source 0 n) acc))
                         (nreverse (cons source acc))))))
          (if source (rec source nil) nil)))

(defun explode-symb (sym)
  (map 'list #'(lambda (c)
                       (intern (make-string 1 :initial-element c)))
       (symbol-name sym)))

(defun explode-num (n)
  (and (>= n 1)
      (append1 (explode-num (floor (/ n 10))) (mod n 10))))

(defun explode-str (str)
  (coerce str 'list))

(defun explode (x)
  (cond
	((numberp x) (explode-num x))
	((stringp x) (explode-str x))
	((symbolp x) (explode-symb x))
	(t (error "Unhandled type"))))

(defun stick-num (lst &key (error nil))
  (multiple-value-bind (i n) (parse-integer (apply #'mkstr lst) :junk-allowed (not error))
    (declare (ignore n))
    i))

(defun cdr-assoc (key alist)
  (cdr (assoc key alist)))

(defun palindrome-p (lst &key (test #'=))
  (or (null (cdr lst))
      (and (funcall test (car lst) (last1 lst))
          (palindrome-p (butlast (cdr lst))))))

(defun make-num-lst (a b &optional (step 1))
  (mapa-b #'identity a b step))

(defun fibo (n &key (initial-values '(1 . 2)))
  (case n
	(1 (car initial-values))
	(2 (cdr initial-values))
    (otherwise (+ (fibo (1- n)) (fibo (- n 2))))))

(setf (fdefinition 'fibo) (memoize #'fibo))

(defun find-divisor (n &optional (start-from 2) (inc #'1+) (cond-fun #'(lambda (n s) (<= (* s s) n))))
  (and (funcall cond-fun n start-from)
      (if (= 0 (mod n start-from))
          start-from
          (find-divisor n (funcall inc start-from) inc cond-fun))))

(defun prime-p (n)
  (and (> n 0) (not (find-divisor n))))

(defun nth-prime(n &optional (start-from 2))
  (if (< n 1)
      (1- start-from)
      (if (prime-p start-from)
          (nth-prime (1- n) (1+ start-from))
          (nth-prime n (1+ start-from)))))

(defun primes-below (n &optional (start-from 2) (lst nil))
  (if (< start-from n)
      (if (prime-p start-from)
          (primes-below n (1+ start-from) (cons start-from lst))
          (primes-below n (1+ start-from) lst))
      (reverse lst)))

(defun next-prime (start-from)
  (if (prime-p start-from)
	start-from
	(next-prime (1+ start-from))))

(defun sieve (lst)
  (and lst
      (cons (car lst) (sieve (delete-if #'(lambda (x) (= 0 (mod x (car lst)))) lst)))))

;Destructive
(defun add-factor (factor alst)
  (aif (assoc factor alst)
	   (progn (rplacd it (1+ (cdr it))) alst)
	   (acons factor 1 alst)))

(defun factorial (n)
  (reduce #'* (make-num-lst 2 n)))

(defun factorize (n &optional (lst nil) (start-from 2))
  (if (prime-p n)
	(add-factor n lst)
	(if (= 0 (mod n start-from))
	  (factorize (/ n start-from) (add-factor start-from lst) start-from)
	  (factorize n lst (next-prime (1+ start-from))))))

(defun proper-divisors-of (n)
	(cons 1 (sort (loop for i from 2 to (sqrt n) if (= 0 (mod n i)) append (if (= (* i i) n) (list i) (list i (/ n i)))) #'<)))

(defun divisors-of (n)
  (append1 (proper-divisors-of n) n))

(defun cumsum (lst)
  (let* ((res nil)
		 (sum (reduce #'(lambda (acc el) 
						  (push acc res)
						  (+ el acc)) lst)))
	(reverse (cons sum res))))

(defun k-sublists (set k)
  (cond ((minusp k) '())
		((zerop k) (list '()))
		((= k 1) (mapcar #'list set))
		(t (loop for elems on set
				 nconc (mapcar #'(lambda (set) (cons (car elems) set))
							   (k-sublists (cdr elems) (- k 1)))))))
