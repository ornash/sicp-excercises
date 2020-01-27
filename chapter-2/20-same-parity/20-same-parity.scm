;; Exercise 2.20

;;;;Requires utilities.scm
(load "../../utilities/utilities.scm")

(define (same-parity . alist)
  (define (accum parity-checker items)
    (cond ((null? items) '())
	  ((parity-checker (car items)) (cons (car items) (accum parity-checker (cdr items))))
	  (else (accum parity-checker (cdr items)))))
  
  (accum (if (even? (car alist))
	     even?
	     odd?)
	 alist))

;;testing
(same-parity 1 2 3 4 5 6)
(same-parity 2 3 4 5 5 6 7 8 10)
(same-parity 1)
(same-parity 2)


