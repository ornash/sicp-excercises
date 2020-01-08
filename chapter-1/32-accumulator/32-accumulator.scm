;; Exercise 1.32

;;;;Requires utilities.scm
(load "../../utilities/utilities.scm")

(define (accumulate-recursive combiner null-value term a next b)
  (define (apply-accum x)
    (if (> x b)
	null-value
	(combiner (term x) (apply-accum (next x)))))

  (apply-accum a))

(define (sum-using-accumulate-recursive a b)
  (accumulate-recursive + 0 identity a inc b))

(define (product-using-accumulate-recursive a b)
  (accumulate-recursive * 1 identity a inc b))

(define (accumulate combiner null-value term a next b)
  (define (apply-accum x result)
    (if (> x b)
	result
	(apply-accum (next x) (combiner result (term x)))))

  (apply-accum a null-value))

(define (sum-using-accumulate a b)
  (accumulate + 0 identity a inc b))

(define (product-using-accumulate a b)
  (accumulate * 1 identity a inc b))

;; Exercise 1.33
;; Should be easy with an additional argument.

;; Exercise 1.34
;; MIT Scheme reports: The object 2 is not applicable.

