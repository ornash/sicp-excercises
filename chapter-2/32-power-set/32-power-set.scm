;; Exercise 2.32

;;;;Requires utilities.scm
(load "../../utilities/utilities.scm")

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
	(append rest (map (lambda (element)
			    (cons (car s) element))
			  rest)))))

;;testing
(subsets (list 1 2 3))
;;(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
