(defun read-triangle (stream)
  (aand (read-line stream nil nil)
	   (cons (mapcar #'parse-integer (cl-utilities:split-sequence #\Space it)) (read-triangle stream))))


;; Slow algorithm that works by tree traversing
#|(defun triangle2tree (triangle &optional (fmake-node #'(lambda (val &optional list-of-nodes)
														 (and val
															  (cons val list-of-nodes)))))
  (if (cdr triangle)
	(maplist #'(lambda (x y) (funcall fmake-node (car x) (subseq y 0 2))) (car triangle) (triangle2tree (cdr triangle)))
	(mapcar fmake-node (car triangle))))

(defun max-path (tree)
  (if (cdr tree)
	(+ (car tree) (apply #'max (mapcar #'max-path (cdr tree))))
	(car tree)))|#

(defun max-of-pairs (lst)
  (and (cdr lst)
	(cons (max (car lst) (cadr lst)) (max-of-pairs (cdr lst)))))

(defun max-path (triangle)
  (if (cdr triangle)
	(mapcar #'+ (car triangle) (max-of-pairs (max-path (cdr triangle))))
	(car triangle)))

(defun problem18 ()
  (with-open-file (stream "data/problem18.txt" :if-does-not-exist nil)
	(car (max-path (read-triangle stream)))))
