;; Exercise 2.17

;;;;Requires utilities.scm
(load "../../utilities/utilities.scm")

(define (last-pair aList)
  (cond ((null? (cdr aList)) alist)
      ((null? (cddr aList)) (cdr aList))
      (else (last-pair (cdr aList)))))

;;testing
(last-pair (list 1))
(last-pair (list 1 2))
(last-pair (list 1 2 3))
(last-pair (list 23 72 149 34))


;;2.18
(define (reverse aList)
  (define (accum aList result)
    (if (null? aList)
	result
	(accum (cdr aList) (cons (car aList) result))))
  (accum aList '()))

;;testing
(reverse (list 1 4 9 16 25))
;;Note: Don't solve for the result data, solve for the order in which operations are to be applied to achieve that result.

