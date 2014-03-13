(defun read-names (stream)
	(cl-utilities:split-sequence #\, (remove-if #'(lambda (x) (char= x #\")) (read-line stream nil nil))))

(defun problem22 ()
  (with-open-file (stream "data/problem22.txt")
	(reduce #'+ (names-to-scores (read-names stream)))))

(defun alphabet-pos-of (ch)
  (- (char-int (char-upcase ch)) 64))

(defun names-to-scores (names)
  (let ((i 0))
	(mapcar #'(lambda (x)
				(incf i)
				(* i (reduce #'+ (mapcar #'alphabet-pos-of (explode x)))))
			(sort names #'string<))))
