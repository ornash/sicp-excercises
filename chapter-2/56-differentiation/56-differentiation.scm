;; Exercise 2.56, 2.57

;;;;Requires utilities.scm
(load "../../utilities/utilities.scm")

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum arg1 . args)
  (define (reduce-sum first second)
    (cond ((=number? first 0) second)
	  ((=number? second 0) first)
	  ((and (number? first) (number? second)) (+ first second))
	  (else (list '+ first second))))
  
  (define (make-sum-op first rest)
    (let ((second (car rest))
	  (remaining (cdr rest)))
      (cond ((eq? nil remaining) (reduce-sum first second))
	    (else (reduce-sum first (make-sum-op second remaining))))))
  (make-sum-op arg1 args))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (make-product arg1 . args)
  (define (reduce-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	  ((=number? m1 1) m2)
	  ((=number? m2 1) m1)
	  ((and (number? m1) (number? m2)) (* m1 m2))
	  (else (list '* m1 m2))))
  
  (define (make-product-op first rest)
    (let ((second (car rest))
	  (remaining (cdr rest)))
      (cond ((eq? nil remaining) (reduce-product first second))
	    (else (reduce-product first (make-product-op second remaining))))))
  (make-product-op arg1 args))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))


(define (make-exponentiation base exponent)
  (cond ((= exponent 0) 1)
	((= exponent 1) base)
	((or (= base 0) (= base 1)) base)
	(else (list '** base exponent))))
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))
(define (base ex) (cadr ex))
(define (exponent ex) (caddr ex))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product 
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product 
           (deriv (multiplier exp) var)
           (multiplicand exp))))

	((exponentiation? exp)
	 (let ((new-base (base exp))
	       (new-expo (exponent exp)))
	   (make-product
	    (make-product new-expo
			  (make-exponentiation new-base (minus-1 new-expo)))
	    (deriv new-base var))))
	
        (else (error "unknown expression 
                      type: DERIV" exp))))

