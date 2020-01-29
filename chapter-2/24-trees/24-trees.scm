;; Exercise 2.25

;;;;Requires utilities.scm
(load "../../utilities/utilities.scm")

(define ex1 (list 1 3 (list 5 7) 9))
(car (cdr (car (cdr (cdr ex1)))))

(define ex2 (list (list 7)))
(car (car ex2))

(define ex3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(car (cdr (cadr (cadr (cadr (cadr (cadr ex3)))))))

;; Exercise 2.26
(define x (list 1 2 3))

(define y (list 4 5 6))

(append x y)
(1 2 3 4 5 6)

(cons x y)
((1 2 3) 4 5 6)

(list x y)
((1 2 3) (4 5 6))

;;Exercise 2.27
(define x (list (list 1 2) (list 3 4)))

(define (reverse aList)
  (define (accum aList result)
    (if (null? aList)
	result
	(accum (cdr aList)
	       (cons (car aList) result))))
  (accum aList '()))

;;testing
(reverse x)

(define (deep-reverse tree)
  (define (accum tree result)
    (cond ((null? tree) result)
	  ((pair? (car tree))
	   (accum (cdr tree)
		  (cons (deep-reverse (car tree))
			result)))
	  (else (accum (cdr tree)
		       (cons (car tree) result)))))
  (accum tree '()))

;;testing
(deep-reverse x)

;;Exercise 2.28


