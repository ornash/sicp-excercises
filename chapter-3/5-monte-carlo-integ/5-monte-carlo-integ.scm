;; Exercise 3.5

;;;;Requires utilities.scm
(load "../../utilities/utilities.scm")

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) 
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) 
                 trials-passed))))
  (iter trials 0))


(define bound cons)
(define lower car)
(define upper cdr)
(define (area xbound ybound)
  (* (- (upper xbound) (lower xbound)) (- (upper ybound) (lower ybound))))
(define (random-in-bound abound)
  (random-in-range (lower abound) (upper abound)))

(define (estimate-integral-bounds predicate xbound ybound trials)
  (define (integ-experiment)
    (predicate (random-in-bound xbound) (random-in-bound ybound)))
  (define fraction (monte-carlo trials integ-experiment))
  (* (area xbound ybound) fraction))


(define (estimate-integral pred x1 x2 y1 y2 trials)
  (estimate-integral-bounds pred (bound x1 x2) (bound y1 y2) trials))

(define (unit-circle-pred x y)
  (>= 1 (+ (square x) (square y))))

(define (estimate-pi trials)
  (estimate-integral unit-circle-pred -1 1 -1 1 trials))

;;for verification, should return 4.
(define (estimate-area trials)
  (estimate-integral (lambda (x y)
		       (and (and (>= x -1) (<= x 1)) (and (>= y -1) (<= y 1)))) -1 1 -1 1 trials))

;; Exercise 3.6
(define rand 
  (let ((x random-init)) 
    (define (dispatch message) 
      (cond ((eq? message 'generate) 
	     (begin (set! x (rand-update x)) 
		    x)) 
	    ((eq? message 'reset) 
	     (lambda (new-value) (set! x new-value))))) 
    dispatch)) 

;;Skipping exercise 3.7 and 3.27.
