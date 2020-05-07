;; Exercise 2.75

;;;;Requires utilities.scm
(load "../../utilities/utilities.scm")

(define (make-from-mag-ang x y)
  (define (dispatch op)
    (cond ((eq? op 'magnitude) x)
          ((eq? op 'angle) y)
          ((eq? op 'real-part)
	   (* (magnitude z) (cos (angle z))))
          ((eq? op 'imag-part)
	   (* (magnitude z) (sin (angle z))))
          (else
           (error "Unknown op: 
            MAKE-FROM-REAL-IMAG" op))))
  dispatch)

;;2.76

;;Changes
;;dispatch on type: change existing code of types, change generic code, introduct unique names.
;;data directed: install new types, create new constructors, install new operations, sharing code is easy.
;;message passing: create new type, update types to introduce new operations.

;;often adding new types: message passing would be the best. only add a new type no other changes required. data directed also works, but there are costs related to size of table, indexing, adding new constructors etc.
;;often adding new operations: data directed would be the best. just install new operations. code can be shared by procedures.
