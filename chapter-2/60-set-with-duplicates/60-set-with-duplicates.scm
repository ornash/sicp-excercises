;; Exercise 2.60

;;;;Requires utilities.scm
(load "../../utilities/utilities.scm")

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (intersection-set set1 set2)
  (append set1 set2))

(define (union-set set1 set2)
  (append set1 set2))

