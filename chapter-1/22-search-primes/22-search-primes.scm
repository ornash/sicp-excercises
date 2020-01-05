;; Exercise 1.22

(load "../21-find-prime/21-find-prime.scm")
(load "../../utilities/utilities.scm")




(define (report-time elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-time (- (runtime) start-time))))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (next-odd n)
  (if (odd? n)
      (+ n 2)
      (+ n 1)))

(define (find-first-prime start end)
  (cond ((prime? start) start)
	((>= start end) 'nil)
	(else (find-first-prime (next-odd start) end))))

(define (search-three-primes start end)
  (find-first-prime (next-odd (find-first-prime
			       (next-odd (find-first-prime start end))
			       end))
		    end))

(define (search-for-primes start end)
  (let ((start-time (runtime))
	)
    (if (null? (search-three-primes start end))
	(display "Failed to find three primes in range.")
	(report-time (- (runtime) start-time)))
    ))


;;skipping 1.23 and 1.24

;;1.25
;;Check http://community.schemewiki.org/?sicp-ex-1.25 for answer.
;;Using fast-expt also gives correct results but its intermediate results are huge which slows down the whole operation.

;;1.26
;;The answer is that if you use square() the time complexity is the height of the recursion tree which is lg n and the tree itself is straight and doesn't have any branches.
;;However, if you use (*) and make 2 calls to expmod, the recursion tree grows exponentially, and the time complexity is equal to the total number of nodes in a full binary tree which is n.

;;skipping 1.27 and 1.28

