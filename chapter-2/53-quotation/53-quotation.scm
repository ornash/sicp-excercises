;; Exercise 2.53

;;;;Requires utilities.scm
(load "../../utilities/utilities.scm")

(define (memq item x)
  (cond ((null? x) false)
	((eq? item (car x)) x)
	(else (memq item (cdr x)))))

(list 'a 'b 'c)
;;(a b c)

(list (list 'george))
;;((george))

(cdr '((x1 x2) (y1 y2)))
;;((y1 y2))

(cadr '((x1 x2) (y1 y2)))
;;(y1 y2)

(pair? (car '(a short list)))
;;#f

(memq 'red '((red shoes) (blue socks)))
;;#f

(memq 'red '(red shoes blue socks))
;;(red shoes blue socks)

;;2.54
(define (my-equal? left right)
  (cond ((not (and (pair? left) (pair? right))) (eq? left right))
	((and (pair? left) (pair? right))
	 (if (my-equal? (car left) (car right))
	     (my-equal? (cdr left) (cdr right))
	     false))
	(else false)))
;;testing
;;(equal? '(this is a list) '(this (is a) list))
;;(equal? '(this is a list) '(this is a list))


;;2.55
;;'abc
;;''abc
;;'''abc

