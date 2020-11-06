;; Exercise 3.73

;;;;Requires utilities.scm
(load "../../utilities/utilities.scm")

;; Exercise 3.73
(define (RC res cap dt)
  (define (volt-stream initial-v current-stream)
    (sum-number-streams (scale-stream res current-stream)
			(integral initial-v dt (scale-stream (/ 1 cap) current-stream))))

  (lambda (initial-v current-stream)
    (volt-stream initial-v current-stream)))

;; testing
;; (define RC1 (RC 5 1 0.5))
;; (display-inf-stream 10 (RC1 0 integer-stream))

;; Exercise 3.74
(define zero-crossings
  (stream-map sign-change-detector sense-data (cons-stream 0 sense-data)))

;; Skipping 3.75 to 3.76
;; Check http://community.schemewiki.org/?sicp-ex-3.75

;; Exercise 3.77
(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
	       (let ((integrand (force delayed-integrand)))
		 (if (stream-null? integrand)
		     the-empty-stream
		     (integral (delay (stream-cdr integrand))
			       (+ (* dt (stream-car integrand))
				  initial-value)
			       dt)))))

;; Exercise 3.78
(define (solve-2nd a b dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams (scale-stream dy a) (scale-stream y b)))
  y)

;; Exercise 3.79
(define(general-solve-2nd f y0 dy0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)


;; Skipping 3.80 to 3.82
;; Check http://community.schemewiki.org/?sicp-ex-3.80


