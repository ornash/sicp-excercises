;; Exercise 1.20 Check the number of remainder operations performanced in applicative vs normal
;; order evaluation.

;;;;Requires utilities.scm
;;;;TODO: Load utilities.scm automatically
(load "../../utilities/utilities.scm")

(define (string-meth name arg1 arg2)
  (string-append "(" name " " (string arg1) ", " (string arg2) ")"))

(define (print-method name arg1 arg2)
  (display (string-append (string-meth name arg1 arg2) "\n")))


(string-meth "remainder" 206 40)

(define (show-gcd-normal-eval a b a-string b-string)
  (display (string-append "evaluate: " b-string "\n"))
  (cond ((= b 0) a)
	(else
	 ;;(print-method "gcd" a-string b-string)
	 (show-gcd-normal-eval b (remainder a b) b-string (string-meth "remainder" a-string b-string)))
	))

(define (show-gcd-applicative-eval a b a-string b-string)
  (display (string-append "evaluate: " b-string "\n"))
  (cond ((= b 0) a)
	(else
	 ;;(print-method "gcd" a-string b-string)
	 (show-gcd-applicative-eval b (remainder a b) b (string-meth "remainder" a b)))
	))

(define (gcd-normal a b)
  (show-gcd-normal-eval a b (string a) (string b)))

(define (gcd-applicative a b)
  (show-gcd-applicative-eval a b (string a) (string b)))


