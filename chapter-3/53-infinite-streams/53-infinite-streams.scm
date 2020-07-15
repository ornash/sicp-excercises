;; Exercise 3.53

;;;;Requires utilities.scm
(load "../../utilities/utilities.scm")

;; 3.53: Stream of power of 2s.

;; 3.54
;; defined in utilities.scm
(define mul-streams mul-number-streams)

(define factorials 
  (cons-stream 1 (mul-streams integer-stream factorials)))

;; 3.55
(define (partial-sums s)
  (define itself
    (cons-stream (+ 0 (stream-car s))
		 (sum-number-streams (stream-cdr s) itself)))
  itself) 

;; from http://community.schemewiki.org/?sicp-ex-3.55
;; (define (partial-sums-3 s) 
;;   (define ps (sum-number-streams s (cons-stream 0 ps))) 
;;   ps) 
