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

(define (estimate-pi)
  (estimate-integral point-check-pred -1 1 -1 1 30))

(define (point-check-pred ))
