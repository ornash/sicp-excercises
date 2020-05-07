;; Exercise 2.73

;;;;Requires utilities.scm
(load "../../utilities/utilities.scm")

(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp) (if (same-variable? exp var) 1 0))
	(else ((get 'deriv (operator exp)) (operands exp)
	       var))))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

;;a. they dont have any operator.
;;b.

;;(define (get super-operator operator))
;;(define (put super-operator operator proc))

(define (deriv-sum exp var)
 (make-sum (deriv (addend exp) var)
	   (deriv (augend exp) var)))

(define (deriv-product exp var)
 (make-sum
  (make-product (multiplier exp)
		(deriv (multiplicand exp) var))
  (make-product (deriv (multiplier exp) var)
		(multiplicand exp))))

;;install
(put 'deriv '+ deriv-sum)
(put 'deriv '* deriv-product)

;;c.
(define (deriv-exponent exp var)
 (let ((new-base (base exp))
       (new-expo (exponent exp)))
   (make-product
    (make-product new-expo
		  (make-exponentiation new-base (minus-1 new-expo)))
    (deriv new-base var))))

(put 'deriv '** deriv-exponent)

;;d: change all puts
