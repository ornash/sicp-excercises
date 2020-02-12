;; Exercise 2.44

;;;;Requires utilities.scm
(load "../../utilities/utilities.scm")

(define (up-split painter n)
  (if (= n 0)
      painter
      (let (smaller (up-split painter (- n 1)))
	(below painter (beside smaller smaller)))))
;;We did not have to worry about frames or sizes or image graphics implementation
;;all we thought about was how to split and organize different painters to create desired pattern.
;;The implementation of image graphics, below, beside will take care of the rest.

(define (right-split painter n)
  (if (= n 0)
      painter
      (let (smaller (right-split painter (- n 1)))
	(beside painter (below smaller smaller)))))

;;2.45
(define (split orig-op split-op)
  (lambda (painter n)
    (if (= n 0)
	painter
	(let (smaller ((split orig-op split-op) painter (- n 1)))
	  (orig-op painter (split-op smaller smaller)))))

(define right-split (split beside below))
(define up-split (split below beside))

;;2.46
(define make-vect cons)
(define xcor-vect car)
(define ycor-vect cdr)
(define (add-vect vect1 vect2)
  (make-vect
   (+ (xcor-vect vect1) (xcor-vect vect2))
   (+ (ycor-vect vect1) (ycor-vect vect2))))
(define (sub-vect vect1 vect2)
  (make-vect
   (- (xcor-vect vect1) (xcor-vect vect2))
   (- (ycor-vect vect1) (ycor-vect vect2))))
(define (scale-vect s vect)
  (make-vect
   (* s (xcor-vect vect))
   (* s (ycor-vect vect))))

(define origin (make-vect 0 0))
(define v1 (make-vect 1 2))
(define v2 (make-vect 3 4))
(add-vect v1 v2)
(sub-vect v1 v2)
(sub-vect v2 v1)
(scale-vect 2 v2)

;;2.47
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (cadr frame))
(define (edge2-frame frame)
  (car (cddr frame)))
(define frame1 (make-frame origin v2 v1))
(origin-frame frame1)
(edge1-frame frame1)
(edge2-frame frame1)

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (cadr frame))
(define (edge2-frame frame)
  (cddr frame))
(define frame1 (make-frame origin v1 v2))
(origin-frame frame1)
(edge1-frame frame1)
(edge2-frame frame1)

;;2.48
;;(define make-segment cons)
;;Using list instead of cons works better because argument to make-segment are vectors which are themselves cons and cons(cons cons) results in a wierd combo.
(define make-segment list)
(define start-segment car)
(define end-segment cadr)

;;2.49
(define bl origin)
(define br (make-vect 1 0))
(define tl (make-vect 0 1))
(define tr (make-vect 1 1))

(list bl br)
;;a
(define segments-painter display)
(segments-painter (list
		   (make-segment bl br)
		   (make-segment br tr)
		   (make-segment tr tl)
		   (make-segment tl bl)))
(list
		   (make-segment bl br)
		   (make-segment br tr)
		   (make-segment tr tl)
		   (make-segment tl bl))
;;b
(segments-painter (list
		   (make-segment bl tr)
		   (make-segment tl br)))
;;c
(define bm (scale-vect 0.5 br))
(define lm (scale-vect 0.5 tl))
(define tm (add-vect bm tl))
(define rm (add-vect br lm))

(segments-painter (list
		   (make-segment bm lm)
		   (make-segment lm tm)
		   (make-segment tm rm)
		   (make-segment rm bm)))

;;2.50 http://community.schemewiki.org/?sicp-ex-2.50
;;2.51 http://community.schemewiki.org/?sicp-ex-2.51
;;2.52 http://community.schemewiki.org/?sicp-ex-2.52
