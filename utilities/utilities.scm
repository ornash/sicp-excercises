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

;;Collection utils
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
	((predicate (car sequence))
	 (cons (car sequence)
	       (filter predicate (cdr sequence))))
	(else (filter predicate (cdr sequence)))))

;;Enumeration utils
(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

;;(enumerate-interval 2 7)

To enumerate the leaves of a tree, we can use(1)

(define (enumerate-tree tree)
  (cond ((null? tree) '())
	((not (pair? tree)) (list tree))
	(else (append (enumerate-tree (car tree))
		      (enumerate-tree (cdr tree))))))

;;(enumerate-tree (list 1 (list 2 (list 3 4)) 5))

