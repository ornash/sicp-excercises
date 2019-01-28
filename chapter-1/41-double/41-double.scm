;; Exercise 1.31

;; Read http://community.schemewiki.org/?sicp-ex-1.41 to understand this better.

;;;;Requires utilities.scm
;;;;TODO: Load utilities.scm automatically
(load "../../utilities/utilities.scm")

(define (double-apply f)
  ;;  (display "(f (f x))\n")
  (pp f)
  (lambda (x)
    (f (f x))))

(define (inc x)
  ;;  (display (string-append (string x) "\n"))
  (pp x)
  (+ x 1))

((double-apply inc) 0)

(((double-apply double-apply) inc) 0)

((double-apply (double-apply inc)) 0)

(((double-apply (double-apply double-apply)) inc) 0)

((double-apply (double-apply (double-apply inc))) 0)


;;This helps better in understanding whats going on.
(define (triple-apply f)
  (display "(f (f (f x)))\n")
  (lambda (x)
    (f (f (f x)))))


((triple-apply inc) 0)

((triple-apply (triple-apply inc)) 0)

((triple-apply (triple-apply (triple-apply inc))) 0)

(((triple-apply triple-apply) inc) 0)

;; To understand, note that
;; (((triple-apply triple-apply) inc) 0) is equivalent to ((triple-apply (triple-apply (triple-apply inc))) 0)





