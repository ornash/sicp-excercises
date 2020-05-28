;; Exercise 3.28

;;;;Requires utilities.scm
(load "../../utilities/utilities.scm")

;; Primitives
(define (make-wire))
(define (get-signal awire))
(define (set-signal! awire new-val))
(define (add-action! awire lambda-action))

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (logical-and w1 w2)
  (cond ((and (= w1 1) (= w2 1)) 1)
        ((or (= w1 1) (= w1 0) (= w2 1) (= w2 0)) 0)
        (else (error "Invalid signal" w1 w2))))

(define (logical-or w1 w2)
  (cond ((and (= w1 0) (= w2 0)) 0)
        ((or (= w1 1) (= w1 0) (= w2 1) (= w2 0)) 1)
        (else (error "Invalid signal" w1 w2))))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value 
           (logical-not (get-signal input))))
      (after-delay 
       inverter-delay
       (lambda ()
         (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) 
                        (get-signal a2))))
      (after-delay 
       and-gate-delay
       (lambda ()
         (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) 
                        (get-signal a2))))
      (after-delay 
       or-gate-delay
       (lambda ()
         (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

;; Combinations
;; Abstractions
