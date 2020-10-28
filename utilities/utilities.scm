;;All necessary utility functions

;;Environemnt utils
;;Load this file immediately after starting sbcl.

(define (scheme-file-path name)
  (string-append "~/home/personal/sicp/" name "/" name ".scm"))
(define (load-scheme-file name)
  (load (scheme-file-path name)))
;;Alias for load-scheme-file
(define (my-load name)
  (load-scheme-file name))

;;display followed by newline
(define (my-display . arg)
  (display arg)
  (newline))

;;Programming utils
(define nil '())

(define (minus-1 x)
  (- x 1))
(define (plus-1 x)
  (+ x 1))
(define (inc x) (plus-1 x))

;;Math utils
(define (identity x) x)

(define (square x)
  (* x x))
(define (cube x)
  (* x x x))
(define (even? x)
  (= (modulo x 2) 0))
(define (odd? x)
  (= (modulo x 2) 1))
(define (double num)
  (* num 2))
(define (halve num)
  (/ num 2))
(define (average n1 n2)
  (/ (+ n1 n2)
     2.0))
(define (prime? x)
  (define (test divisor)
    (cond ((> (* divisor divisor) x) true)
	  ((= 0 (remainder x divisor)) false)
	  (else (test (+ divisor 1)))))
  (test 2))

(define (int-sum-at n) (/ (* n (+ n 1)) 2))

(define (ints-until n) (if (= n 0) (list 0) (cons n (ints-until (- n 1)))))

(define (sums-until n) (map int-sum-at (reverse (ints-until n))))

;;Enumeration utils
(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

;;(enumerate-interval 2 7)

;;To enumerate the leaves of a tree, we can use(1)
(define (enumerate-tree tree)
  (cond ((null? tree) '())
	((not (pair? tree)) (list tree))
	(else (append (enumerate-tree (car tree))
		      (enumerate-tree (cdr tree))))))

;;(enumerate-tree (list 1 (list 2 (list 3 4)) 5))

;;Collection utils
;;apply the accumulation operation op on the sequence using initial as the base term for empty sequence.
;;the operator is expected to be a binary operator. The first argument of operator will be an element and
;;the second argument will be result of accumulation on rest of the elements after current element.
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;;apply map on the seq using given proc and flatten the results to create a single list.
;;the assumption is that proc is a unary operator that will return a list, otherwise flatmap will return error.
;;if the proc returns a list of list only the first level list is flattened.
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

;;(map (lambda (x) (cons 100 x)) (enumerate-interval 1 10))
;;(flatmap (lambda (x) (cons 100 x)) (enumerate-interval 1 10))
;;error: Object passed as an argument to append, is not a list.

;;(map (lambda (x) x) (enumerate-interval 1 10))
;;(flatmap (lambda (x) x) (enumerate-interval 1 10))
;;error: Object passed as an argument to append, is not a list.

;;(map (lambda (x) (list 100 x)) (enumerate-interval 1 10))
;;(flatmap (lambda (x) (list 100 x)) (enumerate-interval 1 10))

;;(map (lambda (x) (list (list 100 1) x)) (enumerate-interval 1 10))
;;(flatmap (lambda (x) (list (list 100 1) x)) (enumerate-interval 1 10))

;;check predicate for each element in sequence and return a sequence with elements that satisfy the predicate.
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
	((predicate (car sequence))
	 (cons (car sequence)
	       (filter predicate (cdr sequence))))
	(else (filter predicate (cdr sequence)))))


;;Stream APIs
;; Although a good idea, you shouldn't do this. A traversed stream is automatically materialized by memo-proc.
;; (define (materialize-stream a-stream)
;;   (if (stream-null? a-stream)
;;       nil
;;       (cons (stream-car a-stream) (materialize-stream (stream-cdr a-stream)))))

;; (define (my-display-stream a-stream)
;;   (map my-display (materialize-stream a-stream)))

(define (display-finite-stream s)
  (stream-for-each my-display s))

(define (accumulate-finite-stream op initial s)
  (if (stream-null? s)
      initial
      (op (stream-car s)
          (accumulate op initial (stream-cdr s)))))

(define (sum-number-streams s1 s2)
  (stream-map + s1 s2))

(define (mul-number-streams s1 s2)
  (stream-map * s1 s2))

(define (scale-stream factor stream)
  (stream-map
   (lambda (x) (* x factor))
   stream))

(define (stream-of-constant constant)
  (define a-stream
    (cons-stream constant a-stream))
  a-stream)

(define ones (stream-of-constant 1))

(define (integers-starting-from n)
  (cons-stream 
   n (integers-starting-from (+ n 1))))

(define integer-stream (integers-starting-from 1))

;;(define integer-stream (cons-stream 1 (sum-number-streams ones integer-stream)))

(define fibs-stream (cons-stream 0 (cons-stream 1 (sum-number-streams (stream-cdr fibs-stream)
								      fibs-stream))))

;; Infinite stream terminated by limit condition on companion stream(finite or infinite)
;; If any of the inf-s or limit-s terminates, returned inf-stream will terminate.
(define (inf-stream-limited-by-limiter-and-cond inf-s limit-s limit-cond-lambda)
  (cond ((stream-null? (stream-car inf-s)) the-empty-stream)
	((stream-null? (stream-car limit-s)) the-empty-stream)
	((limit-cond-lambda (stream-car limit-s)) the-empty-stream)
	(else (cons-stream
	       (stream-car inf-s)
	       (inf-stream-limited-by-limiter-and-cond (stream-cdr inf-s) (stream-cdr limit-s) limit-cond-lambda)))))

;; Infinite stream terminated by limit condition on itself.
(define (inf-stream-limited-by-self-cond inf-s self-limit-cond-lambda)
  (inf-stream-limited-by-limiter-and-cond inf-s inf-s self-limit-cond-lambda))

;; Infinite stream terminated when companion stream terminates.
(define (inf-stream-limited-by-limiter inf-s limit-s)
  (inf-stream-limited-by-limiter-and-cond inf-s limit-s stream-null?))

;; Infinite stream terminated when after limit-index elements are explored.
(define (inf-stream-limited-by-index inf-s limit-index)
  (inf-stream-limited-by-limiter-and-cond inf-s integer-stream (lambda (index) (> index limit-index))))

(define (display-inf-stream limit inf-s)
  (stream-for-each my-display (inf-stream-limited-by-index inf-s limit)))

(define (display-inf-stream-pair limit inf-s1 inf-s2)
  (stream-for-each my-display (inf-stream-limited-by-index inf-s1 limit) (inf-stream-limited-by-index inf-s2 limit)))

(define (arithmetic-series-stream initial-value diff)
  (define itself
    (cons-stream initial-value (sum-number-streams (stream-of-constant diff) itself)))
  itself)
;;Note that although this is efficient and results in only n operations to arrive at nth term in series.
;;If an entire stream is not required and you need nth term directly, use arithmetic series formula to get nth term.

;;integer-stream can then be written as
;;(define integer-stream (arithmetic-series-stream 1 1))

(define (equal-interval-stream interval) (arithmetic-series-stream interval interval))
;;integer-stream can then be written as
;;(define integer-stream (equal-interval-stream 1))

(define (sample-inf-stream interval input-stream)
  (define (roll-forward count stream)
    (if (= count 0)
	stream
	(roll-forward (- count 1) (stream-cdr stream))))
  (cons-stream (stream-car input-stream)
	       (sample-inf-stream interval (roll-forward interval input-stream))))

(define (sine-stream input-stream)
  (stream-map sin input-stream))

(define (cosine-stream input-stream)
  (stream-map cos input-stream))

(define (unit-circle-hypotenuse input-stream)
  (stream-map (lambda (sine-val cosine-val)
		(sqrt (+ (square sine-val) (square cosine-val))))
	      (sine-stream input-stream)
	      (cosine-stream input-stream)))

(define (unit-circle-sine-cosine-ratio input-stream)
  (stream-map (lambda (sine-val cosine-val)
		(/ sine-val cosine-val))
	      (sine-stream input-stream)
	      (cosine-stream input-stream)))

(define (integral constant dx input-stream)
  (define area-stream (scale-stream dx input-stream))
  (define itself
    (cons-stream constant
		 (sum-number-streams area-stream itself)))
  itself)

(define (streams-as-pair-stream s1 s2)
  (stream-map (lambda (x y) (cons x y)) s1 s2))

;;(display-inf-stream 2000 (cosine-stream (equal-interval-stream 0.001)))
;;(display-inf-stream 2000 (scale-stream 1000 (cosine-stream (equal-interval-stream 0.001))))
;;filter values perfectly divisible by 10 in a dataset with 1000 points (1/0.001) to get 100 sample values
;;filter values perfectly divisible by 100 in a dataset with 1000 points (1/0.001) to get 10 sample values
;;(= 0 (remainder (floor 20.1234) 10))
;;(display-inf-stream 2000 (stream-filter (lambda (x) (= 0 (remainder (floor x) 10))) (scale-stream 1000 (cosine-stream (equal-interval-stream 0.001)))))
;;find corresponding sine and you are done.
