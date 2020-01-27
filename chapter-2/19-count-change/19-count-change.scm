;; Exercise 2.19

;;;;Requires utilities.scm
(load "../../utilities/utilities.scm")

(define us-coins (list 50 25 10 5 1))

(define unordered-us-coins (list 10 25 50 1 5))

(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (no-more? aList)
  (null? aList))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (first-denomination coin-values)
  (car coin-values))


(define (cc amount coin-values)
  (cond ((= amount 0) (display "leaf") (newline) 1)
	((or (< amount 0) (no-more? coin-values)) (display "leaf") (newline) 0)
	(else
	 (+ (cc amount
		(except-first-denomination coin-values))
	    (cc (- amount
		   (first-denomination coin-values))
		coin-values)))))

(define count-change cc)

;; testing
(= 1 (count-change 1 us-coins))
(= 2 (count-change 5 us-coins))
(= 4 (count-change 10 us-coins))
(= 13 (count-change 25 us-coins))
(= 50 (count-change 50 us-coins))
(= 292 (count-change 100 us-coins) (count-change 100 unordered-us-coins))

(count-change 1 uk-coins)
(count-change 5 uk-coins)
(count-change 10 uk-coins)
(count-change 25 uk-coins)
(count-change 50 uk-coins)
(count-change 100 uk-coins)

;;What is the time complexity of this?
;;probably exponential. i.e. 2^n where n is the number of unique coins + 1.
;;This is because sum of binomial coefficients at level l is 2^l and sum of all sums of binomial coefficients at each level is 2^(l+1).
;;Above analysis is wrong. Correct answer: http://www.ysagade.nl/2015/04/12/sicp-change-growth/

