;;;;Requires utilities.lisp

(defun slow-exp (b n)
 (cond ((= n 1) b)
	(t (* b (slow-exp b (- n 1))) )))

(defun slow-exp-iter (b n)
  (defun calc-exp (n result)
    (if (= n 0)
	(* result 1)
	(calc-exp (minus-1 n) (* b result))))
  (calc-exp n 1))

;;Fast exponentiation using successive squaring.
;;Following program generates a recursive process.
(defun fast-exp (b n)
  (cond ((= n 1) b)
	((even? n) (square (fast-exp b (/ n 2))))
	(t (* b (fast-exp b (- n 1))))))

;; Fast exponentiation using successive squaring.
;; Following program generates an iterative process.
;; incorrect implementation
(defun fast-exp-iter-attempt1 (b n)
  (defun calc-exp (count result)
    (cond ((= count 0) result)
	  ((= count 1) (* b result))
	  ((odd? count) (calc-exp (minus-1 count) (* b result)))
	  (t (calc-exp (/ count 2) (square result)))))
  
  (calc-exp n 1))

;; Fast exponentiation using successive squaring.
;; Following program generates an iterative process.
;; incorrect implementation
(defun fast-exp-iter-attemp2 (b n)
  (defun calc-exp (count max-count result)
    (cond ((> count max-count) result)
	  ((= count 0) (calc-exp (plus-1 count) max-count (* 1 result)))
	  ((odd? count) (calc-exp (plus-1 count) max-count (* b result)))
	  ((or (= count max-count) (<= (* count 2) max-count))
	   (calc-exp (* count 2) max-count (square result)))
	  (t (calc-exp 0 (- max-count count) (square result)))
	  ))
  
  (calc-exp 0 n 1))

;; Fast exponentiation using successive squaring.
;; Following program generates an iterative process.
;; correct implementation
;; see what happens for 2^6 and 2^7 to understand following implementation.
(defun fast-exp-iter (b n)
  (defun calc-exp (count max-count part-result result)
    (cond ((> count max-count) (* part-result result))
	  ((= count 0) (calc-exp (plus-1 count) max-count part-result (* 1 result)))
	  ((odd? count) (calc-exp (plus-1 count) max-count part-result (* b result)))
	  ;; handle even count that are powers of 2; i.e. reach as close to max-count as possible by successive doubling of count and successive squaring of result.
	  ((or (= count max-count) (<= (* count 2) max-count))
	   (calc-exp (* count 2) max-count part-result (square result)))
	  ;; handle remaining even count
	  (t (calc-exp 0 (- max-count count) (* part-result (square result)) 1))
	  ))
  
  (calc-exp 0 n 1 1))
