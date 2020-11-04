;; Exercise 3.73

;;;;Requires utilities.scm
(load "../../utilities/utilities.scm")

;; Exercise 3.73
(define (RC res cap dt)
  (define (volt-stream initial-v current-stream)
    (sum-number-streams (scale-stream res current-stream)
			(integral initial-v dt (scale-stream (/ 1 cap) current-stream))))

  (lambda (initial-v current-stream)
    (volt-stream initial-v current-stream)))

;; testing
;; (define RC1 (RC 5 1 0.5))
;; (display-inf-stream 10 (RC1 0 integer-stream))

;; Exercise 3.74
(define zero-crossings
  (stream-map sign-change-detector sense-data (cons-stream 0 sense-data)))

;; Skipping 3.75 to 3.82
;; Check http://community.schemewiki.org/?sicp-ex-3.75
