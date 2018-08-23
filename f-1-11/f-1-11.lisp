;;Exercise 1.11, Page 42

(defun f-1-11 (n)
  (if (< n 3)
      n
      (+ (f-1-11 (- n 1))
	 (* 2 (f-1-11 (- n 2)))
	 (* 3 (f-1-11 (- n 3))))))

(defun f-1-11-iter (n)
  (defun iterate (fn-3 fn-2 fn-1 count)
    (if (> count n)
	fn-1
	(iterate fn-2 fn-1 (+ (* 3 fn-3) (* 2 fn-2) fn-1) (+ count 1))))
  (if (< n 3)
      n
      (iterate 0 1 2 3)))
