;; Exercise 2.7

;;;;Requires utilities.scm
(load "../../utilities/utilities.scm")

(define (make-interval a b) (cons a b))

(define (lower-bound interval) (car interval))

(define (upper-bound interval) (cdr interval))

;; Exercise 2.8

(define (add-interval ix iy)
  (make-interval (+ (lower-bound ix) (lower-bound iy))
		 (+ (upper-bound ix) (upper-bound iy))))

(define (sub-interval ix iy)
  (make-interval (- (lower-bound ix) (lower-bound iy))
		 (- (upper-bound ix) (upper-bound iy))))

;;test
;;(define right (make-interval 5.8 6.2))
;;(define left (make-interval 1.8 2.2))

;;(define result (sub-interval right left))

;;following add-interval application proves sub-interval implementation is correct.
;;(add-interval result left)

;; Exercise 2.9

;; Exercise 2.10
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (<= (* (lower-bound y) (upper-bound y)) 0)
      (error y "Error. interval spans zero.")
   (mul-interval x
		(make-interval (/ 1.0 (upper-bound y))
			       (/ 1.0 (lower-bound y))))))

;; Skipping 2.11 to 2.16
