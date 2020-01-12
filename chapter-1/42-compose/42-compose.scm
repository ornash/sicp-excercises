;; Exercise 1.42

;;;;Requires utilities.scm
(load "../../utilities/utilities.scm")

(define (compose f g)
  (lambda (x)
    (f (g x))))

((compose square inc) 6)

;;1.43
(define (repeated f n)
  (define (repeated-call call-count input)
    (if (= call-count 1)
	(f input)
	(f (repeated-call (- call-count 1) input))))
  (lambda (x)
    (repeated-call n x)))

((repeated square 3) 5)

;;1.44
(define (smooth f)
  (define (dx x)
    (* 0.0001 x))
  (lambda (x)
    (average
     (average (f x) (f (+ x (dx x))))
     (f (- x (dx x))))))

(define (n-fold-smooth f n)
  (lambda (x)
    ((repeated (smooth f) n) x)))


;;Skipping 1.45

;;1.46
(define (iterative-improve good-enough? improve)
  (define (iterate new-guess)
    (if (good-enough? new-guess)
	new-guess
	(iterate (improve new-guess))))
  (lambda (guess)
    (iterate guess)))

(define (tolerance) 0.0001)

(define (average-damp-using f)
  (lambda (value) (average value (f value))))

(define (sqrt x)
  (define (close-enough? x-sqrt)
    (< (abs (- x (square x-sqrt))) (tolerance)))
  (define (improve-guess x-guess)
    ((average-damp-using (lambda (y) (/ x y))) x-guess))
  ((iterative-improve close-enough? improve-guess) 1.0))

(sqrt 625)
