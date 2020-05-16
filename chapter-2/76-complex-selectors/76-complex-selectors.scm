;; Exercise 2.77

;;;;Requires utilities.scm
(load "../../utilities/utilities.scm")

;; The apply-generic method defined in section 2.4 is designed such that it can keep
;; making recursive calls based on the procedure that is found in the table by get.
;; The recursion stops only when the retrieved procedure itself is not defined in terms
;; of apply-generic. generic magnitude is defined using apply-generic, magnitude for
;; complex number is installed to use generic magnitude procedure, magnitude for rectangular
;; and polar forms are installed to use concrete procedures. Therefore when generic magnitude
;; is called on a complex number the stack is:
;; generic-magnitude -> apply-generic -> generic-magnitude -> apply-generic -> concrete-manitude.

;; 2.78
(define (attach-tag type-tag contents)
  (cond ((number? contents) contents)
	((symbol? contents) contents)
	(else (cons type-tag contents))))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
	((symbol? datum) 'scheme-symbol)
	((pair? datum) (car datum))
	(else (error "Bad tagged datum: 
              TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
	((symbol? datum) datum)
	((pair? datum) (cdr datum))
	(else (error "Bad tagged datum: 
              CONTENTS" datum))))

;; 2.79
;; while installing scheme-number package
(put 'equ? '(scheme-number scheme-number) eq?)

;; while installing rational package
(put 'equ? '(rational rational)
     (lambda (x y)
       (eq? (* (numer x) (denom y)) (* (numer y) (denom x)))))

;; while installing complex package
(put 'equ? '(complex complex)
     (lambda (c1 c2)
       (and (eq? (real-part c1) (real-part c2))
	    (eq? (imag-part c1) (imag-part c2)))))

(define (equ? x y) (apply-generic 'equ? x y))

;; 2.80
;; while installing scheme-number package
(put '=zero? '(scheme-number) (lambda (num) (eq? 0 num)))

;; while installing rational package
(put '=zero? '(rational)
     (lambda (rat)
       (eq? 0 (numer rat))))

;; while installing complex package
(put '=zero? '(complex)
     (lambda (com)
       (and (eq? 0 (real-part com))
	    (eq? 0 (imag-part com)))))

(define (=zero? x) (apply-generic '=zero? x)) 

