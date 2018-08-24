;;;;Requires utilities.scm
(load "../../utilities/utilities.scm")

(define (sum-larger-pair-squares first second third)
  (define (smaller-of-pair a b)
    (if (< a b) a b))
  (define (smallest) (smaller-of-pair first (smaller-of-pair second third)))
  (define (sum-of-squares a b)
    (+ (square a)
       (square b)))
  (cond ((= first (smallest))
	 (sum-of-squares second third))
	((= second (smallest))
	 (sum-of-squares first third))
	(else (sum-of-squares first second))))

;;Test cases, all should evaluate to true
(= 13 (sum-larger-pair-squares 1 2 3)
   (sum-larger-pair-squares 1 3 2)
   (sum-larger-pair-squares 2 1 3)
   (sum-larger-pair-squares 2 3 1)
   (sum-larger-pair-squares 3 1 2)
   (sum-larger-pair-squares 3 2 1))

