;; Exercise 3.1

;;;;Requires utilities.scm
(load "../../utilities/utilities.scm")

(define (make-accumulator init-sum)
  (define accum-sum init-sum)
  (lambda (new-val)
	  (set! accum-sum (+ accum-sum new-val))
	  accum-sum))

(define A (make-accumulator 5))

;;test cases
(A 10)
;;15

(A 10)
;;25

(a 100)
;;125

;; Exercise 3.2


(define (make-monitored mocked-f)
  (define counter 0)
  (define (update-and-apply arg)
    (set! counter (inc counter))
    (mocked-f arg))
  
  (lambda (arg)
    (cond ((eq? arg 'how-many-calls?) counter)
	  ((eq? arg 'reset-count) (set! counter 0))
	  (else (update-and-apply arg)))))

(define s (make-monitored A))

;;(s 100)
;;225

;;(s 'how-many-calls?)
;;1

;;Skipping exercise 3.3
;;Skipping exercise 3.4
