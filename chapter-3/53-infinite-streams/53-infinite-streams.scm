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
;; test
;;  (display-infinite-stream (partial-sums integer-stream) 50)

;; from http://community.schemewiki.org/?sicp-ex-3.55
;; (define (partial-sums-3 s) 
;;   (define ps (sum-number-streams s (cons-stream 0 ps))) 
;;   ps) 

;; Above implementations self-refer therefore don't result in recalculation
;; i.e. (stream-ref (partial-sums integer-stream) 50) does 50 "+" operations


;; (define (partial-sums-2 s) 
;;   (sum-number-streams s (cons-stream 0 (partial-sums-2 s))))
;; This implementation does recalculations and is inefficient.
;; i.e. (stream-ref (partial-sums-2 integer-stream) 50) does 1326 "+" operations

;; 3.56
(define S (cons-stream 1 (merge (merge (scale-stream S 2) (scale-stream S 3)) (scale-stream S 5))))

;; 3.57
;; Count the number of "+" operations to understand it.
;; Same as 3.55 above, self-reference along with memo-proc is necessary for efficiency.

;; Self reference is good when you want to use the stream computed so far i.e. next element in stream depends on previous elements.

;; 3.58
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) 
           den 
           radix)))

;; (display-infinite-stream (expand 1 7 10) 10) ;; repeats itself after few iterations
;; (display-infinite-stream (expand 3 8 10) 10) ;; reaches stable state at 0

