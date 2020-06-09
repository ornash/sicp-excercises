;; Exercise 3.33

;;;;Requires utilities.scm
(load "../../utilities/utilities.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Primitives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;adder constraint, note that all set values are happening on connectors.
(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) 
                (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) 
                          (get-value a2))
                       me)) ;; use me as informant
          ((and (has-value? a1) 
                (has-value? sum))
           (set-value! a2
                       (- (get-value sum) 
                          (get-value a1))
                       me)) ;; use me as informant
          ((and (has-value? a2) 
                (has-value? sum))
           (set-value! a1
                       (- (get-value sum) 
                          (get-value a2))
                       me)))) ;; use me as informant
  (define (process-forget-value)
    (forget-value! sum me) ;;forget value on sum connector if it was set by me
    (forget-value! a1 me) ;;forget value on a1 connector if it was set by me
    (forget-value! a2 me) ;;forget value on a2 connector if it was set by me
    (process-new-value)) ;;process-new-value because, a connector may have had a value that was not originally set by the adder
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else (error "Unknown request: 
                        ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)


;;multiplier constraint
(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) 
                    (= (get-value m1) 0))
               (and (has-value? m2) 
                    (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) 
                (has-value? m2))
           (set-value! product
                       (* (get-value m1) 
                          (get-value m2))
                       me))
          ((and (has-value? product) 
                (has-value? m1))
           (set-value! m2
                       (/ (get-value product) 
                          (get-value m1))
                       me))
          ((and (has-value? product) 
                (has-value? m2))
           (set-value! m1
                       (/ (get-value product) 
                          (get-value m2))
                       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request: 
                   MULTIPLIER" 
                  request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

;;constant constraint
(define (constant value connector)
  (define (me request)
    (error "Unknown request: CONSTANT" 
           request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (probe name connector)
  (define (print-probe value)
    (newline) (display "Probe: ")
    (display name) (display " = ")
    (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else (error "Unknown request: 
                        PROBE" request))))
  (connect connector me)
  me)

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (for-each-except exception 
                         procedure 
                         list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) 
           (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

(define (make-connector)
  (let ((value false) 
        (informant false) 
        (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except 
              setter
              inform-about-value
              constraints))
            ((not (= value newval))
             (error "Contradiction" 
                    (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant false)
                 (for-each-except 
                  retractor
                  inform-about-no-value
                  constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint 
                     constraints))
          (set! constraints
                (cons new-constraint 
                      constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant true false))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) 
             set-my-value)
            ((eq? request 'forget) 
             forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation: 
                          CONNECTOR"
                         request))))
    me))

(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'value))
(define (set-value! connector 
                    new-value 
                    informant)
  ((connector 'set-value!) 
   new-value 
   informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Combinations and Abstractions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define C (make-connector))
(define F (make-connector))

(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)

(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))


(celsius-fahrenheit-converter C F)

(set-value! C 25 'user)
(set-value! F 212 'user)
(forget-value! C 'user)
(set-value! F 212 'user)

;; Exercise 3.33
;; this is beautiful
(define (averager a b c)
  (let ((sum (make-connector))
        (half (make-connector)))
    (multiplier sum half c)
    (adder a b sum)
    (constant 0.5 half)
    'ok))

(define v1 (make-connector))
(define v2 (make-connector))
(define averg (make-connector))

(probe "value1" v1)
(probe "value2" v2)
(probe "averg" averg)

(set-value! v1 10 'user)
;;Probe: value1 = 10
(set-value! v2 20 'user)
;;Probe: value2 = 20

(averager v1 v2 averg)
;;Probe: averg = 15.

(set-value! v2 30 'user)
;;Contradiction (20 30)

(forget-value! v2 'user)
;;Probe: averg = ?
;;Probe: value2 = ?

(set-value! v2 30 'user)
;;Probe: averg = 20.
;;Probe: value2 = 30

(forget-value! v2 'user)
;;Probe: averg = ?
;;Probe: value2 = ?

(set-value! averg 20 'user)
;;Probe: value2 = 30.
;;Probe: averg = 20

;; Exercise 3.34
;; 1. Multiplier is defined to use two inputs i.e. m1,m2 or m1,product or m2,product. When both m1 and m2 are the same objects, they will forget value simultaneously so multiplier wont be able to function.

;; Exercise 3.35

(define (squarer a b) 
  (define (process-new-value) 
    (if (has-value? b) 
	(if (< (get-value b) 0) 
	    (error "square less than 0 -- SQUARER" (get-value b)) 
	    (set-value! a 
			(sqrt (get-value b)) 
			me)) 
	(if (has-value? a) 
	    (set-value! b 
			(square (get-value a)) 
			me)))) 
  (define (process-forget-value) 
    (forget-value! a me) 
    (forget-value! b me) 
    (process-new-value)) 
  (define (me request) 
    (cond ((eq? request 'I-have-a-value) 
	   (process-new-value)) 
	  ((eq? request 'I-lost-my-value) 
	   (process-forget-value)) 
	  (else 
	   (error "Unknown request -- SQUARER" request)))) 
  (connect a me) 
  (connect b me) 
  me) 


;; Exercise 3.37
(define (c+ x y) 
  (let ((z (make-connector))) 
    (adder x y z) 
    z)) 

(define (c- x y) 
  (let ((z (make-connector))) 
    (adder z y x) 
    z)) 

(define (c* x y) 
  (let ((z (make-connector))) 
    (multiplier x y z) 
    z)) 

(define (c/ x y) 
  (let ((z (make-connector))) 
    (multiplier z y x) 
    z)) 

(define (cv x) 
  (let ((z (make-connector))) 
    (constant x z) 
    z)) 

(define (celsius-fahrenheit-converter x) 
  (c+ (c* (c/ (cv 9) (cv 5)) 
	  x) 
      (cv 32))) 
