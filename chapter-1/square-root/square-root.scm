(define (square-root number)
  (define (my-abs non-abs)
    (if (< non-abs 0)
	(- non-abs)
	non-abs))
  
  (define (average a b)
    (/ (+ a b) 2.0))
  
  (define (improve-guess current-guess)
    (average current-guess (/ number current-guess)))

  (define (good-enough? guess)
    (> 0.001 (my-abs (- (/ number guess) guess))))
  
  (define (newtons-method guess)
    (if (good-enough? guess)
	guess
	(newtons-method (improve-guess guess))
	))
  
  (newtons-method 1.0))

;;Test cases.
(square-root 10000)
