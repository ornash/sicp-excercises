;; Exercise 2.38

;;;;Requires utilities.scm
(load "../../utilities/utilities.scm")

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest))
	      (cdr rest))))
  (iter initial sequence))


(fold-right / 1 (list 1 2 3))

(fold-left / 1 (list 1 2 3))

(fold-right list '() (list 1 2 3))

(fold-left list '() (list 1 2 3))
;;commutative

;;2.39
(define (reverse-fold-right sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))

;;testing
(reverse-fold-right (list 1 2 3))

(define (reverse-fold-left sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))

;;testing
(reverse-fold-left (list 1 2 3))
