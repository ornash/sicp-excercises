;;All necessary utility functions

;;Environemnt utils
;;Load this file immediately after starting sbcl.

(define (scheme-file-path name)
  (string-append "/Users/rghumare/home/personal/sicp/" name "/" name ".scm"))
(define (load-scheme-file name)
  (load (scheme-file-path name)))
;;Alias for load-scheme-file
(define (my-load name)
  (load-scheme-file name))

;;Programming utils
(define (minus-1 x)
  (- x 1))
(define (plus-1 x)
  (+ x 1))

;;Math utils
(define (square x)
  (* x x))
(define (even? x)
  (= (modulo x 2) 0))
(define (odd? x)
  (= (modulo x 2) 1))

(define (fib-iter n)
  (define (iter a b count)
    (cond ((= n 0) 0)
          ((= n 1) 1)
          ((= count n) a)
          (else (iter (+ a b) a (+ count 1)))))
   (iter 1 0 1))

(fib-iter 8)
