;;Exercise 1.17 and 1.18, Page 46

;;;;Requires utilities.scm
;;;;TODO: Load utilities.scm automatically
(load "../../utilities/utilities.scm")

(define (slow-mult left right)
  (if (= right 0)
      0
      (+ left (* left (- right 1)))))

;; Test case
(= 12 (slow-mult 3 4))

;; When you can't solve the problem, go to fundamental principles.
;; 7 * 3
;; = (1 + 6) * 3 ;; bacause 7 is odd
;; = (1 * 3) + (6 * 3)
;; = 3 + (3 * 6) ;; halve 6 and double 3 in above step because 6 is even
;; = 3 + ((1 + 2) * 6) ;; because 3 is odd
;; = 3 + (1 * 6) + (2 * 6) 
;; = 3 + (1 * 6) + (1 * 12) ;; halve 2 and double 6 in above step because 2 is even
;; = 3 + 6 + 12
;; = 21
(define (fast-mult left right)
  (define (do-mult l r)
    (cond
     ((= r 1) l)
     ((odd? r)
      (+ l (do-mult l (- r 1))))
     (else (do-mult (double l) (halve r)))))
  (do-mult left right))

;; Test cases
(= 24 (fast-mult 6 4))
(= 12 (fast-mult 3 4))
(= 12 (fast-mult 4 3))
(= 15 (fast-mult 3 5))
(= 21 (fast-mult 3 7))
(= 57 (fast-mult 3 19))

(define (fast-mult-iter left right)
  (define (do-mult-iter l r carry)
    (cond
     ((= r 1) (+ l carry))
     ((odd? r)
      (do-mult-iter l (- r 1) (+ l carry)))
     (else (do-mult-iter (double l) (halve r) carry))))
  (do-mult-iter left right 0))

;; Test cases
(= 24 (fast-mult-iter 6 4))
(= 12 (fast-mult-iter 3 4))
(= 12 (fast-mult-iter 4 3))
(= 15 (fast-mult-iter 3 5))
(= 21 (fast-mult-iter 3 7))
(= 57 (fast-mult-iter 3 19))

;; Test by comparison
(define (validate-fast-mult start end)
  (define (compare left right)
    (display (cons left right))
    (display "\n")
    (= (* left right) (fast-mult left right) (fast-mult-iter left right)))
  (cond ((> start end) #t)
	((not (compare start end)) #f)
	(else (validate-fast-mult (+ start 1) (- end 1)))))

(equal? #t (validate-fast-mult 1 1000))

