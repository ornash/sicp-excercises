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

;; 3.59
;; 3.59.1
;;(define (integrate-series power-series-coefs)
;;  (stream-map (lambda (term-no coef) (* coef (/ 1 term-no))) integer-stream power-series-coefs))

(define (integrate-series power-series-coefs)
  (stream-map / power-series-coefs integer-stream))

;; 3.59.2
(define exp-series
  (cons-stream 
   1 (integrate-series exp-series)))

;;another way to get exp-series
;;(define exp-series 
;;   (stream-map / ones (cons-stream 1 factorials))) 

(define cosine-series 
  (cons-stream 1 (stream-map - (integrate-series sine-series))))

(define sine-series 
  (cons-stream 0 (integrate-series cosine-series)))

;;stream of x^y where y goes from 0 to infinity
(define (term-stream x)
  (stream-map expt (stream-of-constant x) (integers-starting-from 0)))

(define (my-sine x)
  (mul-number-streams (term-stream x) sine-series))

(define (my-cosine x)
  (mul-number-streams (term-stream x) cosine-series))

;; testing
;; find sin(22/7/4) i.e. sin(0.7857)
;; summing up values returned by the following results in a correct value of 0.707320181
;; (display-inf-stream 10 (mul-number-streams (term-stream 0.7857) sine-series))

;; you can sum up automatically using integral as follows and see how the results stabilize
;; (display-inf-stream 20 (integral 0 1 (mul-number-streams (term-stream 0.7857) sine-series)))


;; 3.60
;; This one is interesting. There is no one answer for this.
;; Until now, we have seen streams that expand to infinity in a linear list.
;; This problem involves tree of streams and therefore can be organized in different ways leading to different answers.
;; e.g. take two 5 element series, the multiplication has 25 terms, there are multiple ways to arrive at those 25 terms.
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (sum-number-streams (scale-stream (stream-car s1) (stream-cdr s2))
			    (mul-series (stream-cdr s1) s2))))

(define (sine-squared x) (mul-series (my-sine x) (my-sine x)))

(define (cosine-squared x) (mul-series (my-cosine x) (my-cosine x)))

;; testing sin^2 x + cos^2 x = 1
;; (display-inf-stream 20 (integral 0 1 (sum-number-streams (sine-squared 0.7857) (cosine-squared 0.7857) )))

;; 3.61
;; We can do this because the constant term of power series is 1
;; Just rearranging the terms makes a difficult problem/operation very simple. We transformed division into multiplication.
(define (invert-unit-series power-series)
  (define itself
    (cons-stream 1 (scale-stream -1 (mul-series (stream-cdr power-series) itself))))
  itself)

;; 3.62
;; Express division as multiplication of numerator and inverted-dinominator

;; first define generic invert-series
(define (invert-series power-series)
  (if (= 0 (stream-car power-series)) (error "First term of power-series must be non-zero"))
  (define itself 
    (cons-stream (stream-car power-series) (scale-stream -1 (mul-series (stream-cdr power-series) itself)))) 
  itself)

(define (div-series s1 s2)
  (mul-series s1 (invert-series s2)))

(define tangent-series 
  (div-series sine-series cosine-series))

(define (my-tangent x)
  (mul-number-streams (term-stream x) tangent-series))
