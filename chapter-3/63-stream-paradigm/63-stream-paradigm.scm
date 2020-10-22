;; Exercise 3.63

;;;;Requires utilities.scm
(load "../../utilities/utilities.scm")

;; 3.63
;; memo-proc avoids redundant operations

;; 3.64
(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 
     1.0 (stream-map
          (lambda (guess)
            (sqrt-improve guess x))
          guesses)))
  guesses)

(define (stream-limit-stream s1 tolerance)
  (define (limit previous rest-s1)
    (let ((current (stream-car rest-s1)))
      (if (< (abs (- previous current)) tolerance)
	  (cons-stream current the-empty-stream)
	  (cons-stream current
		       (limit current (stream-cdr rest-s1))))))
  (limit (stream-car s1) (stream-cdr s1)))

(define (sqrt-stream-upto-tolerance x tolerance)
  (stream-limit-stream (sqrt-stream x) tolerance))

;;testing
;;(display-finite-stream (sqrt-stream-upto-tolerance 3 0.01))
;;(display-finite-stream (sqrt-stream-upto-tolerance 103 0.01))

;; I misunderstood the question, sqrt value is expected not the stream
(define (stream-limit stream tolerance)
  (if (< (abs (- (stream-ref stream 1) (stream-ref stream 0))) tolerance)
      (stream-ref stream 1)
      (stream-limit (stream-cdr stream) tolerance)))


(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

;; 3.65
;; Notice how we get positive numbers even after using "-" for stream-map. This is because "-" gets applied
;; multiple times, every other term is positive because "-" gets applied twice
(define (pi-summands n)
  (cons-stream 
   (/ 1.0 n)
   (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream 
   (partial-sums (pi-summands 1)) 4))

(define (ln2-summands n)
  (cons-stream
   (/ 1 n) 
   (stream-map - (ln2-summands (+ n 1)))))

(define ln2-stream
  (partial-sums (ln2-summands 1)))

