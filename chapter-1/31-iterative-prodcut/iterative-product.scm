;; Exercise 1.31

;;;;Requires utilities.scm
;;;;TODO: Load utilities.scm automatically
(load "../../utilities/utilities.scm")

(define (identity x) x)

(define (inc x) (+ x 1))

(define (product-recursive term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-recursive term (next a) next b))))

(define (product-iterative term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial-recursive n)
  (if (= n 0)
      1
      (* n (factorial-recursive (minus-1 n)))))

(define (factorial-iterative n)
  (define (iter n result)
    (if (= n 0)
	result
	(iter (minus-1 n) (* result n))))

  (iter n 1))

(define (factorial n)
  ;;(factorial-recursive n)
  (product-recursive identity 1 inc n)
  )

(factorial 7)



