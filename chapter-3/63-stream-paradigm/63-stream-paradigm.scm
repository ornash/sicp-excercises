;; Exercise 3.63

;;;;Requires utilities.scm
(load "../../utilities/utilities.scm")

;; 3.63
;; memo-proc avoids redundant operations

;; 3.64
(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 
     1.0 (stream-map
          (lambda (guess)
            (sqrt-improve guess x))
          guesses)))
  guesses)

(define (stream-limit-stream s1 tolerance)
  (define (limit previous rest-s1)
    (let ((current (stream-car rest-s1)))
      (if (< (abs (- previous current)) tolerance)
	  (cons-stream current the-empty-stream)
	  (cons-stream current
		       (limit current (stream-cdr rest-s1))))))
  (limit (stream-car s1) (stream-cdr s1)))

(define (sqrt-stream-upto-tolerance x tolerance)
  (stream-limit-stream (sqrt-stream x) tolerance))

;;testing
;;(display-finite-stream (sqrt-stream-upto-tolerance 3 0.01))
;;(display-finite-stream (sqrt-stream-upto-tolerance 103 0.01))

;; I misunderstood the question, sqrt value is expected not the stream
(define (stream-limit stream tolerance)
  (if (< (abs (- (stream-ref stream 1) (stream-ref stream 0))) tolerance)
      (stream-ref stream 1)
      (stream-limit (stream-cdr stream) tolerance)))


(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

;; 3.65
;; Notice how we get positive numbers even after using "-" for stream-map. This is because "-" gets applied
;; multiple times, every other term is positive because "-" gets applied twice
(define (pi-summands n)
  (cons-stream 
   (/ 1.0 n)
   (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream 
   (partial-sums (pi-summands 1)) 4))

(define (ln2-summands n)
  (cons-stream
   (/ 1 n) 
   (stream-map - (ln2-summands (+ n 1)))))

(define ln2-stream
  (partial-sums (ln2-summands 1)))

;; 3.66
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream 
       (stream-car s1)
       (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x)
                  (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

;; testing
(define (check-pair apair left right) (if (and (= left (car apair)) (= right (cadr apair))) #t #f))
(define (temp-check-pair apair) (check-pair apair 3 100))
(stream-length (inf-stream-limited-by-self-cond (pairs integer-stream integer-stream) temp-check-pair))

;; x(n) = summation [as n goes from n to 1] (2 + (2 * x(n+1)))
;; and x(1) = x(2) i.e. previous-count, if max(n) = 1 then first-count.
;; where first-count is defined as
;; if i == j, first-count = 0
;; if i < j, first-count = ((2 * (j - i)) - 1)
(define (preceding-pairs-count i j)
  (define (recurrent-relation n previous-count)
    (if (= 1 n)
	previous-count
	(recurrent-relation (- n 1) (+ 2 (* 2 previous-count)))))
  (cond ((> i j) (error "i cannot be greater than j."))
	((< i j) (recurrent-relation i (- (* 2 (- j i)) 1)))
	(else (recurrent-relation i 0))))

;; 3.67
(define (pairs-all s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x)
                  (list (stream-car s) x))
                (stream-cdr t))
    (pairs-all (stream-cdr s) t))))

;; 3.68
(define (pairs-louis s t)
  (interleave
   (stream-map
    (lambda (x) 
      (list (stream-car s) x))
    t)
   (pairs-louis (stream-cdr s)
          (stream-cdr t))))

;; 3.69
(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map (lambda (x)
                  (cons (stream-car s) x))
                (pairs (stream-cdr t) (stream-cdr u)))
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define (pythagorean-triples s t u)
  (stream-filter (lambda (tripl)
		   (= (square (caddr tripl))
		      (+ (square (car tripl)) (square (cadr tripl)))))
		 (triples s t u)))

;; 3.70
(define (merge-weighted weight pairs1 pairs2)
  (cond ((stream-null? pairs1) pairs2)
        ((stream-null? pairs2) pairs1)
        (else
         (let ((p1 (stream-car pairs1))
	       (p2 (stream-car pairs2))
	       (p1-weight (weight (stream-car pairs1)))
               (p2-weight (weight (stream-car pairs2))))
           (cond ((< p1-weight p2-weight)
                  (cons-stream
                   p1
                   (merge-weighted weight (stream-cdr pairs1) pairs2)))
                 ((> p1-weight p2-weight)
                  (cons-stream 
                   p2
                   (merge-weighted weight pairs1 (stream-cdr pairs2))))
                 (else
                  (cons-stream 
                   p1
                   (merge-weighted weight (stream-cdr pairs1) pairs2))))))))


(define (pairs-weighted weight s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted weight
		   (stream-map (lambda (x)
				 (list (stream-car s) x))
			       (stream-cdr t))
		   (pairs-weighted weight (stream-cdr s) (stream-cdr t)))))

(define (weight-sum pair)
  (+ (car pair) (cadr pair)))

(define (weight-sum-235 pair)
  (let ((two-i (* 2 (car pair)))
	(three-j (* 3 (cadr pair)))
	(five-ij (* 5 (* (car pair) (cadr pair)))))
    (+ two-i three-j five-ij)))

;; testing
;; (display-inf-stream 20 (pairs-weighted weight-sum integer-stream integer-stream))
;; (display-inf-stream 20 (pairs-weighted weight-sum-235 integer-stream integer-stream))

;; 3.71
(define (weight-sum-cubed pair)
  (+ (cube (car pair)) (cube (cadr pair))))


;; testing
;; (display-inf-stream 20 (pairs-weighted weight-sum-cubed integer-stream integer-stream))

(define (equi-weight-consecutive-pairs weight head tail)
  (cond ((stream-null? tail) the-empty-stream)
	((= (weight head) (weight (stream-car tail)))
	 (cons-stream (list head (stream-car tail) (weight head))
		      (equi-weight-consecutive-pairs weight (stream-car tail) (stream-cdr tail))))
	(else (equi-weight-consecutive-pairs weight (stream-car tail) (stream-cdr tail)))))

(define ramanujan-numbers
  (let ((weight-ordered-pairs (pairs-weighted weight-sum-cubed integer-stream integer-stream)))
    (equi-weight-consecutive-pairs weight-sum-cubed
				   (stream-car weight-ordered-pairs)
				   (stream-cdr weight-ordered-pairs))))

;; Note: Problems can be solved in completely new ways if you change the paradigm of they way you think about problem, process/computation and its solution.
;; e.g. think about how you would solve ramanujan-numbers problem without streams and then think about how streams simplify the solution.
;; We are delaying the computation until it is really required.
;; Richard Hamming also refers to this in his lectures. e.g. switch from analog to digital computing.
