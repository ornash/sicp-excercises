;; Exercise 2.58.2

;;;;Requires utilities.scm
(load "../../utilities/utilities.scm")

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))


(define (first-term exp) (car exp))
(define (operator exp) (cadr exp))
(define (next-term exp) (caddr exp))
(define (is-next-term-last? exp) (null? (cdddr exp)))
(define (remaining-terms exp)
  (if (is-next-term-last? exp) ;;is next term the last term?
      (next-term exp) ;;return last term
      (cddr exp))) ;;return all of remaining expression

(define (make-sum first second)
  (my-display (list 'make-sum first second))
  (cond ((=number? first 0) second)
	((=number? second 0) first)
	((and (number? first) (number? second)) (+ first second))
	((and (sum? first) (sum? second)) (append first (list '+) second))
	((sum? first) (append first (list '+ second)))
	((sum? second) (append (list first '+) second))
	(else (list first '+ second))))

(define (sum? x)
  (and (pair? x) (eq? (operator x) '+)))
(define (addend s) (first-term s))
(define (augend s) (remaining-terms s))

(define (make-product m1 m2)
  (my-display (list 'make-product m1 m2))
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	((and (product? m1) (product? m2)) (append m1 (list '*) m2))
	((product? m1) (append m1 (list '* m2)))
	((product? m2) (append (list m1 '*) m2))
	(else (list m1 '* m2))))

(define (product? x)
  (and (pair? x) (eq? (operator x) '*)))
(define (multiplier p) (first-term p))
(define (multiplicand p) (remaining-terms p))

(define (deriv-op exp var)
  (my-display 'deriv-op)
  (my-display exp)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv-op (addend exp) var)
                   (deriv-op (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product 
           (multiplier exp)
           (deriv-op (multiplicand exp) var))
          (make-product 
           (deriv-op (multiplier exp) var)
           (multiplicand exp))))
	
        (else (error "unknown expression 
                      type: DERIV-OP" exp))))

(define (parenthesize-op exp)
  (my-display 'parenthesize-op)
  (my-display exp)

  (cond ((number? exp) exp)
        ((variable? exp) exp)
        ((sum? exp)
	 (my-display 'sum)
         (make-sum
	  (parenthesize-op (addend exp))
	  (parenthesize-op (augend exp))))

        ((product? exp)
	 (my-display 'product)
	 ;;parenthesize-op by returning a "list" of obtained product. We can do that because we know how make-product and make-sum are defined using "list".
	 ;;parenthesize-op can be modified when precedence rules for evolve to include other operations.
	 (let ((multiplier-exp (multiplier exp))
	       (multiplicand-exp (multiplicand exp))
	       (next-term-exp (next-term exp))
	       (remaining-terms-exp (remaining-terms exp)))
	   (cond ((not (equal? multiplicand-exp remaining-terms-exp)) (error "Unhandled usecase."))
		 ((equal? next-term-exp multiplicand-exp) ;;just parenthesize-op both arguments. e.g. 3 * (x + y + 2)
		  (make-product (parenthesize-op multiplier-exp)
				(parenthesize-op multiplicand-exp)))
		 (else (cond ((sum? multiplicand-exp) ;;e.g. 3 * x + y + 2 to (3 * x) + y + 2
			      (make-sum (make-product (parenthesize-op multiplier-exp)
						      (parenthesize-op (addend multiplicand-exp)))
					(parenthesize-op (augend multiplicand-exp))))
			     ((product? multiplicand-exp) ;;e.g. 3 * x * y + 2 to (3 * x) * y + 2
			      (make-product (make-product (parenthesize-op multiplier-exp)
							  (parenthesize-op (multiplier multiplicand-exp)))
					    (parenthesize-op (multiplicand multiplicand-exp))))
			     (else (error "Unknown expression. type: parenthesize-op -> product" exp)))))))

        (else (error "unknown expression
                      type: parenthesize-op" exp))))

(define (parenthesize exp)
  (let ((paren-exp (parenthesize-op exp)))
    (if (null? (cdr paren-exp))
	(car paren-exp)
	paren-exp)))

(define (deriv exp var)
  (deriv-op (parenthesize exp) var))


