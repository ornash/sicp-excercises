;;Exercise 1.11, Page 42

(define (f-1-11 n)
  (if (< n 3)
      n
      (+ (f-1-11 (- n 1))
	 (* 2 (f-1-11 (- n 2)))
	 (* 3 (f-1-11 (- n 3))))))

(define (f-1-11-iter n)
  (define (iterate fn-3 fn-2 fn-1 count)
    (if (> count n)
	fn-1
	(iterate fn-2 fn-1 (+ (* 3 fn-3) (* 2 fn-2) fn-1) (+ count 1))))
  
  (if (< n 3)
      n
      (iterate 0 1 2 3)))

;;Test case, all must evaluate to true
(= 0 (f-1-11-iter 0) (f-1-11 0))
(= 1 (f-1-11-iter 1) (f-1-11 1))
(= 2 (f-1-11-iter 2) (f-1-11 2))
(= 4 (f-1-11-iter 3) (f-1-11 3))
(= 11 (f-1-11-iter 4) (f-1-11 4))

