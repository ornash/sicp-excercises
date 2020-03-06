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


(define (make-sum first second)
  (cond ((=number? first 0) second)
	((=number? second 0) first)
	((and (number? first) (number? second)) (+ first second))
	((and (pair? first) (pair? second)) (append first (list '+) second))
	((pair? first) (append first (list '+ second)))
	((pair? second) (append (list first '+) second))
	(else (list first '+ second))))

(define (sum? x)
  (and (pair? x)
       (eq? (cadr x) '+)
       (or
	(not (pair? (augend x))) ;;augend is the last term
	(sum? (augend x))))) ;;or augend itself is a sum i.e. all successors are sums
(define (addend s) (car s))
(define (augend s)
  (if (null? (cdddr s)) ;;is augend the last term?
      (caddr s) ;;return last term
      (cddr s)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	((and (pair? m1) (pair? m2)) (append m1 (list '*) m2))
	((pair? m1) (append m1 (list '* m2)))
	((pair? m2) (append (list m1 '*) m2))
	(else (list m1 '* m2))))

;; (define (product? x)
;;   (and (pair? x) (eq? (cadr x) '*)))
(define (product? x) ;;return true if the expression/list contains '*
  (cond ((eq? nil x) #f) ;;reached the end
	((eq? '* (car x)) #t) ;;is the symbol *
	(else (product? (cdr x))))) ;;check next terms
(define (multiplier p)
  (if (eq? '* (cadr p)) ;;check if the next term is '*
      (list (car p)) ;;return left term
      (append (list (car p)) (multiplier (cdr p))))) ;;check next terms, return cons'ed result
(define (multiplicand p)
  (if (eq? '* (cadr p)) ;;check if the next term is '*
      (if (null? (cdddr p)) ;;is multiplicand the last term?
	  (caddr p) ;;return last term
	  (cddr p))
      (multiplicand (cdr p))))  

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
	 (display 'sum)
	 (newline)
	 (display exp)
	 (newline)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
	 (display 'prod)
	 (newline)
	 (display exp)
	 (newline)
         (make-sum
          (make-product 
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product 
           (deriv (multiplier exp) var)
           (multiplicand exp))))
	
        (else (error "unknown expression 
                      type: DERIV" exp))))

;;may be incorrect. check http://community.schemewiki.org/?sicp-ex-2.58

