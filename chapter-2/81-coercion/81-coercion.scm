;; Exercise 2.81

;;;;Requires utilities.scm
(load "../../utilities/utilities.scm")

(define (no-method-error op type-tags)
  (error "No method for these types: " (list op type-tags)))
;; a. it will result in infinite recursion.
;; b. Yes, it is correct that something needs to be done. His solution is wrong.
;; c.
(define (apply-generic-pair op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if ((eq? type1 type2) (no-method-error op type-tags))
		    (let ((t1->t2 (get-coercion type1 type2))
			  (t2->t1 (get-coercion type2 type1)))
		      (cond 
		       (t1->t2 (apply-generic-pair op (t1->t2 a1) a2))
		       (t2->t1 (apply-generic-pair op a1 (t2->t1 a2)))
		       (else (no-method-error op type-tags))))))
              (no-method-error op type-tags))))))

;; Exercise 2.82

(define (get-priority type)
  (cond ((eq? type 'complex) 4)
	((eq? type 'real) 3)
	((eq? type 'rational) 2)
	(else 1)))

(define (get-higher-type left right)
  (if (>= (get-priority left) (get-priority right))
      left
      right))

(define (find-highest type-tags)
  (accumulate (lambda (left right)
		    (get-higher-type left right))
		  (car type-tags)
		  (cdr type-tags)))

(define (coerce-arg-to-type arg type)
  ((get-coercion (type-tag arg) type) (contents arg)))

(define (coerce-to type args-list)
  (map (lambda (arg) (coerce-arg-to-type arg type))
       args-list))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((coerced-args (coerce-to (find-highest type-tags) args)))
      (accumulate (lambda (left right)
		    (apply-generic-pair op left right))
		  (car coerced-args)
		  (cdr coerced-args)))))


;; Exercise 2.83
(define (integer->rational integer) (make-rational integer 1)) 
(define (rational->real rational) 
  (define (integer->floating-point integer) 
    (* integer 1.0))
 
  (make-real (/ (integer->floating-point (numer rational)) 
		(denom rational)))) 

(define (real->complex real) 
   (make-complex-from-real-imag real 0)) 

(put-coersion 'integer 'rational integer->rational) 
(put-coersion 'rational 'real rational->real) 
(put-coersion 'real 'complex real->complex) 

(define (get-higher-type type)
  (cond ((eq? type 'real) 'complex)
	((eq? type 'rational) 'real)
	((eq? type 'integer) 'rational)
	(else (error "No higher type for: " type))))

(define (raise number)
  ((get-coercion (type-tag number) (get-higher-type (type-tag number)))
   (contents number)))

;; Exercise 2.84

;; (define (get-priority type))

(put-priority 'integer 1)
(put-priority 'rational 2)
(put-priority 'real 3)
(put-priority 'complex 4)

(define (apply-generic-pair op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if ((eq? type1 type2) (no-method-error op type-tags))
		    (let ((priority1 (get-priority type1))
			  (priority2 (get-priority type2)))
		      (cond 
		       ((> priority2 priority1) (apply-generic-pair op (raise a1) a2))
		       ((> priority1 priority2) (apply-generic-pair op a1 (raise a2)))
		       (else (no-method-error op type-tags))))))
              (no-method-error op type-tags))))))

;; Skipping 2.85 and 2.86 should be similar.
