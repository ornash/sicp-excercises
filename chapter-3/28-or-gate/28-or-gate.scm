;; Exercise 3.28

;;;;Requires utilities.scm
(load "../../utilities/utilities.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Primitives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin ((car procedures))
             (call-each (cdr procedures)))))

(define (make-wire)
  (let ((signal-value 0) 
        (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each 
                  action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures 
            (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) 
             signal-value)
            ((eq? m 'set-signal!) 
             set-my-signal!)
            ((eq? m 'add-action!) 
             accept-action-procedure!)
            (else (error "Unknown operation: 
                          WIRE" m))))
    dispatch))

(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire lambda-action)
  ((wire 'add-action!) lambda-action))

(define (after-delay delay lambda-action))

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

;;Exercise 2.28
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Combinations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Exercise 3.29
(define (compound-or-gate a1 a2 output) 
  (let ((nota1 (make-wire)) 
	(nota2 (make-wire)) 
	(andnota1a2 (make-wire))) 
    (inverter a1 nota1) 
    (inverter a2 nota2) 
    (and-gate nota1 nota2 andnota1a2) 
    (inverter andnota1a2 output))) 
;; delay = and-gate-delay + twice inverter-delay. twice because only one of a1 or a2 changes so 2 inverters get used.

;; Exercise 3.30
(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((c1 (make-wire)) 
        (c2 (make-wire))
        (s  (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (ripple-carry-adder a-list b-list s-list c) 
  (let ((c-list (map (lambda (x) (make-wire)) (cdr a-list))) 
	(c-0 (make-wire))) 
    (map full-adder 
	 a-list 
	 b-list 
	 (append c-list (list c-0)) 
	 s-list 
	 (cons c c-list)) 
    'ok)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Abstractions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
