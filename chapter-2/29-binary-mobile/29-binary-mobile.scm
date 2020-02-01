;; Exercise 2.29

;;;;Requires utilities.scm
(load "../../utilities/utilities.scm")

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cadr mobile))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cadr branch))
(define (is-mobile structure)
  (pair? structure))
(define (is-weight structure)
  (display "is-weight ")
  (display structure)
  (newline)
  (not (is-mobile structure)))

(define (total-weight mobile)
  (define (weigh-branch branch)
    (if (is-weight (branch-structure branch))
	(branch-structure branch)
	(weigh-mobile (branch-structure branch))))
  (define (weigh-mobile mobile)
    (if (is-weight mobile)
	mobile
	(+ (weigh-branch (left-branch mobile))
	   (weigh-branch (right-branch mobile)))))
  (weigh-mobile mobile))

;;testing
(define one (make-mobile (make-branch 1 1) (make-branch 1 2)))
(define four (make-mobile (make-branch 1 4) (make-branch 1 5)))
(define three (make-mobile (make-branch 1 3) (make-branch 1 four)))
(define bat-mobile (make-mobile (make-branch 1 one) (make-branch 1 three)))

(total-weight bat-mobile)

;;
(define (left-mobile mobile)
  (branch-structure (left-branch mobile)))
(define (right-mobile mobile)
  (branch-structure (right-branch mobile)))
(define (left-length mobile)
  (branch-length (left-branch mobile)))
(define (right-length mobile)
  (branch-length (right-branch mobile)))

(define (return-true)
  (display "true")
  (newline)
  #t)

(define (torque-balanced mobile)
  (if (is-weight mobile)
      (return-true)
      (= (torque (left-branch mobile))
	 (torque (right-branch mobile)))))

(define (torque branch)
  (* (branch-length branch)
     (total-weight (branch-structure branch))))

(define (is-balanced mobile)
  (display mobile)
  (newline)
  (if (is-weight mobile)
      (return-true)
      (and (and (is-balanced (left-mobile mobile))
		(is-balanced (right-mobile mobile)))
	   (torque-balanced mobile))))

;;testing
(define one-balanced (make-mobile (make-branch 2 1) (make-branch 1 2)))
(define four-balanced (make-mobile (make-branch 5 4) (make-branch 4 5)))
(define three-balanaced (make-mobile (make-branch 9 3) (make-branch 3 four-balanced)))
(define balanced-bat-mobile (make-mobile (make-branch 4 one-balanced) (make-branch 1 three-balanaced)))

(is-balanced bat-mobile)
(is-balanced balanced-bat-mobile)

;;29.d
;;only four changes are needed, replace list with cons and cadr with cdr
;; (define (make-mobile left right)
;;   (cons left right))

;; (define (make-branch length structure)
;;   (cons length structure))

;; (define (left-branch mobile)
;;   (car mobile))
;; (define (right-branch mobile)
;;   (cdr mobile))
;; (define (branch-length branch)
;;   (car branch))
;; (define (branch-structure branch)
;;   (cdr branch))

