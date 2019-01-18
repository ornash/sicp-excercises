;;Exercise 1.19

;;;;Requires utilities.scm
;;;;TODO: Load utilities.scm automatically
(load "../../utilities/utilities.scm")


(define (fibonacci-recursive n)
  (display "recursive")
  (cond ((<= n 1) n)
	(else (+
	       (fibonacci-recursive (minus-1 n))
	       (fibonacci-recursive (- n 2))
	       ))))


(define (fibonacci-iterative n)
  (display "iterative \n")
  (define (iterate a b i)
    (display (cons a b))
    (display "\n")
    (cond ((= i n) a)
	  (else (iterate (+ a b) a (plus-1 i)))
	  ))
  (cond ((<= n 1) n)
	(else (iterate 1 0 1))))


(define (fibonacci n)
  (fibonacci-iterative n))

;; Test cases
(= 0 (fibonacci 0))
(= 1 (fibonacci 1))
(= 1 (fibonacci 2))
(= 2 (fibonacci 3))
(= 3 (fibonacci 4))
(= 5 (fibonacci 5))
(= 8 (fibonacci 6))
(= (fibonacci 7) (+
		  (fibonacci 6)
		  (fibonacci 5)))

;; Test by comparison
(define (validate-fibonacci n)
  (cond ((<= n 1) (= n (fibonacci n)))
	(else (= (fibonacci n)
		 (+ (fibonacci (minus-1 n))
		    (fibonacci (- n 2))))
	      )))


(validate-fibonacci 40)
