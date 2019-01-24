;; Exercise 1.30

;;;;Requires utilities.scm
;;;;TODO: Load utilities.scm automatically
(load "../../utilities/utilities.scm")


(define (cube x)
  (* x x x))

(define (inc n) (+ n 1))

(define (sum-recursive term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum-recursive term (next a) next b))))

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (sum term a next b) (sum-iter term a next b))

(define (sum-cubes a b)
  (sum cube a inc b))

(sum-cubes 2 3)

