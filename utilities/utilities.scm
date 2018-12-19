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
(define (double num)
  (* num 2))
(define (halve num)
  (/ num 2))



