;;All necessary utility functions

;;Environemnt utils
;;Load this file immediately after starting sbcl.

(define (scheme-file-path name)
  (string-append "~/home/personal/sicp/" name "/" name ".scm"))
(define (load-scheme-file name)
  (load (scheme-file-path name)))
;;Alias for load-scheme-file
(define (my-load name)
  (load-scheme-file name))

;;Programming utils
(define nil '())

(define (minus-1 x)
  (- x 1))
(define (plus-1 x)
  (+ x 1))
(define (inc x) (plus-1 x))

;;Math utils
(define (identity x) x)

(define (square x)
  (* x x))
(define (even? x)
  (= (modulo x 2) 0))
(define (odd? x)
  (= (modulo x 2) 1))
(define (double num)
  (* num 2))
(define (halve num)
  (/ num 2))
(define (average n1 n2)
  (/ (+ n1 n2)
     2.0))
(define (prime? x)
  (define (test divisor)
    (cond ((> (* divisor divisor) x) true)
	  ((= 0 (remainder x divisor)) false)
	  (else (test (+ divisor 1)))))
  (test 2))

;;Enumeration utils
(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

;;(enumerate-interval 2 7)

;;To enumerate the leaves of a tree, we can use(1)
(define (enumerate-tree tree)
  (cond ((null? tree) '())
	((not (pair? tree)) (list tree))
	(else (append (enumerate-tree (car tree))
		      (enumerate-tree (cdr tree))))))

;;(enumerate-tree (list 1 (list 2 (list 3 4)) 5))

;;Collection utils
;;apply the accumulation operation op on the sequence using initial as the base term for empty sequence.
;;the operator is expected to be a binary operator. The first argument of operator will be an element and
;;the second argument will be result of accumulation on rest of the elements after current element.
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;;apply map on the seq using given proc and flatten the results to create a single list.
;;the assumption is that proc is a unary operator that will return a list, otherwise flatmap will return error.
;;if the proc returns a list of list only the first level list is flattened.
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

;;(map (lambda (x) (cons 100 x)) (enumerate-interval 1 10))
;;(flatmap (lambda (x) (cons 100 x)) (enumerate-interval 1 10))
;;error: Object passed as an argument to append, is not a list.

;;(map (lambda (x) x) (enumerate-interval 1 10))
;;(flatmap (lambda (x) x) (enumerate-interval 1 10))
;;error: Object passed as an argument to append, is not a list.

;;(map (lambda (x) (list 100 x)) (enumerate-interval 1 10))
;;(flatmap (lambda (x) (list 100 x)) (enumerate-interval 1 10))

;;(map (lambda (x) (list (list 100 1) x)) (enumerate-interval 1 10))
;;(flatmap (lambda (x) (list (list 100 1) x)) (enumerate-interval 1 10))

;;check predicate for each element in sequence and return a sequence with elements that satisfy the predicate.
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
	((predicate (car sequence))
	 (cons (car sequence)
	       (filter predicate (cdr sequence))))
	(else (filter predicate (cdr sequence)))))

