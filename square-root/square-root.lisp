(defun square-root (number)
  (defun my-abs (non-abs)
    (if (< non-abs 0)
	(- non-abs)
	non-abs))
  
  (defun average (a b)
    (/ (+ a b) 2))
  
  (defun improve-guess (current-guess)
    (average current-guess (/ number current-guess)))

  (defun good-enough? (guess)
    (> 0.001 (my-abs (- (/ number guess) guess))))
  
  (defun newtons-method (guess)
    (if (good-enough? guess)
	guess
	(newtons-method (improve-guess guess))
	))
  
  (newtons-method 1))
