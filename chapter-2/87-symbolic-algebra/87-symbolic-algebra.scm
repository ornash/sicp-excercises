;; Exercise 2.87

;;;;Requires utilities.scm
(load "../../utilities/utilities.scm")

(define (zero-term? term)
  (=zero? (coeff term)))

(define (zero-terms? terms)
  (cond ((empty-termlist? terms) #t)
	((not (zero-term? (first-term terms))) #f)
	(else (zero-terms? (rest-terms terms)))))

(put '=zero? '(polynomial)
     (lambda (poly)
       (or (=zero? (variable poly))
	   (zero-terms? (term-list poly)))))

;; Skipping 2.88 to 2.97
