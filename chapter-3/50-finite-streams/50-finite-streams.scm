;; Exercise 3.50

;;;;Requires utilities.scm
(load "../../utilities/utilities.scm")


(define (my-stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply my-stream-map 
	      (cons proc (map stream-cdr argstreams))))))
;; Note that you cannot use the following in the last call, you have to use apply.
;; (my-stream-map proc (map stream-cdr argstreams))
;; This is because compiler/interpreter cannot determine whether variable arguments
;; were passed or a list was passed as just 1 variable argument.

;; Exercise 3.51
(define (display-line x)
  (newline)
  (display x))

(define (show x)
  (display-line x)
  x)

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1)
                                  high))))

(define x (stream-map show (stream-enumerate-interval 0 10)))
;; Note that following calls produce different results between 1st and rest of the calls for each index.
;; (stream-ref x 5) ;; prints 1 to 5 and returns 5
;; (stream-ref x 7) ;; prints 6 to 7 and returns 7
;; (stream-ref x 0) ;; returns 0
;; (stream-ref x 2) ;; returns 2
;; (stream-ref x 8) ;; prints 8 and returns 8
;; (stream-ref x 9) ;; prints 9 and returns 9
;; (stream-ref x 8) ;; returns 8
;; This is because memo-proc will ensure expressions are evaluated only once, subsequent calls return memoized results.

;; Exercise 3.52
(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq 
  (stream-map 
   accum 
   (stream-enumerate-interval 1 20)))

(define y (stream-filter even? seq))

(define z 
  (stream-filter 
   (lambda (x) 
     (= (remainder x 5) 0)) seq))

;;(stream-ref y 7) ;; without memo-proc, first call would provide same response but subsequent calls wont.
;;(display-stream z) ;; without memo-proc, first call will start with a base value of whatever is the current value of sum instead of 0.
;;assignment and streams together produce unpredictable results.
;;in general if operations in stream or on it have side-effects results will be unpredictable.
