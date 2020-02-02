;; Exercise 2.33

;;;;Requires utilities.scm
(load "../../utilities/utilities.scm")


(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (my-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (my-length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

;;testing
(length (list 1 2 3 4))
(my-length (list 1 2 3 4))

;;2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ (* higher-terms x) this-coeff))
	      0
	      coefficient-sequence))


(horner-eval 2 (list 1 3 0 5 0 1))


;;2.35
(define (count-leaves tree)
  (accumulate + 0 (map (lambda (sub-tree)
			 (if (not (pair? sub-tree))
			     1
			     (count-leaves sub-tree))) tree)))


;;2.36
(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
	    (accumulate-n op init (map cdr seqs)))))
;;testing
;;should evaluate to true
(equal? (list 22 26 30) (accumulate-n + 0 s))
