;; Exercise 2.21

;;;;Requires utilities.scm
(load "../../utilities/utilities.scm")

(define (square-list items)
  (if (null? items)
      nil
      (cons (* (car items) (car items)) (square-list (cdr items)))))

(define (square-list-map items)
  (map
   (lambda (x) (* x x))
   items))

;;testing
(square-list (list 1 2 3 4))

(square-list-map (list 1 2 3 4))

;; Exercise 2.22
;; Check the note for 2.18 in chapter-2/17-last-pair/. Reasoning is similar.
;; It has to do with the way "cons" has to be applied to generate a list.
;; i.e. it has to be applied as (cons 1 (cons 2 (cons 3 (cons 4 '()))))

;; Exercise 2.23
(define (for-each proc list) 
   (cond 
    ((null? list) #t) 
    (else (proc (car list)) 
          (for-each proc (cdr list))))) 
  
(for-each (lambda (x) (newline) (display x))
	  (list 57 321 88))





