;; Exercise 2.30

;;;;Requires utilities.scm
(load "../../utilities/utilities.scm")

(define (square-tree tree)
  (cond ((null? tree) '())
	((not (pair? tree)) (square tree))
	(else (cons (square-tree (car tree))
		    (square-tree (cdr tree))))))

(define (square-tree-map tree)
  (map (lambda (sub-tree)
	 (if (not (pair? sub-tree))
	     (square sub-tree)
	     (square-tree-map sub-tree)))
       tree))

;;testing
(define tree
  (list 1
	(list 2 (list 3 4) 5)
	(list 6 7)))
(square-tree tree)
(square-tree-map tree)
;;(1 (4 (9 16) 25) (36 49))

;;Exercise 2.31

(define (tree-map proc tree)
  (map (lambda (sub-tree)
	 (if (not (pair? sub-tree))
	     (proc sub-tree)
	     (tree-map proc sub-tree)))
       tree))

(define (square-tree-abstract tree) (tree-map square tree))
(square-tree-abstract tree)

