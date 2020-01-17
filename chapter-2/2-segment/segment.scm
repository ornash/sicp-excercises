;; Exercise 2.2

;;;;Requires utilities.scm
;;;;TODO: Load utilities.scm automatically
(load "../../utilities/utilities.scm")


(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (make-segment start-point end-point)
  (cons start-point end-point))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (midpoint-segment segment)
  (make-point
   (average
    (x-point (start-segment segment))
    (x-point (end-segment segment)))
   (average
    (y-point (start-segment segment))
    (y-point (end-segment segment)))
   ))

;; Exercise 2.3

(define (make-rectangle left-bottom right-top)
  (cons left-bottom right-top))

(define (left-bottom rectangle)
  (car rectangle))

(define (right-top rectangle)
  (cdr rectangle))

(define (length rectangle)
  (abs (- (x-point (right-top rectangle)) (x-point (left-bottom rectangle)))))

(define (breadth rectangle)
  (abs (- (y-point (right-top rectangle)) (y-point (left-bottom rectangle)))))

(define (perimeter rectangle)
  (+ (* 2 (length rectangle)) (* 2 (breadth rectangle))))

(define (area rectangle)
  (* (length rectangle) (breadth rectangle)))

