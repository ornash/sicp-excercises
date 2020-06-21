;; Section 3.4.2 - different possible orderings for the ordered events.

(load "../utilities/counting.scm")


;;Given a sequence e.g. (1 2 3 7 8 9) and a separator, returns true if:
;;1. elements to the left of separator on number line appear in the sequence in order.
;;and 2. elements to the right or equal to separator on number line appear in the sequence in order.
;;e.g. (is-valid-seq (1 2 7 3 8 9) 7) returns true because both (1 2 3) and (7 8 9) are ordered.
;;e.g. (is-valid-seq (7 8 1 9 2 3) 7) returns true because both (1 2 3) and (7 8 9) are ordered.
;;e.g. (is-valid-seq (6 7 1 8 2 9) 6) returns true because both (1 2) and (6 7 8 9) are ordered.
(define (is-valid-seq seq separator)
 (define (is-valid seq abc xyz separator)
  (cond ((null? seq) #t)
	((< (car seq) separator)
	 (if (and (pair? abc) (> (car abc) (car seq))) ;;return false if top of stack abc is greater.
	     #f
	     (is-valid (cdr seq)
		       (cons (car seq) abc) ;;put on stack abc
		       xyz
		       separator)))
	(else (if (and (pair? xyz) (> (car xyz) (car seq))) ;;return false if top of stack xyz is greater.
		  #f
		  (is-valid (cdr seq)
			    abc
			    (cons (car seq) xyz) ;; put on stack xyz
			    separator)))))
 
 (is-valid seq '() '() separator))


(define (display-valid-seqs list-of-lists separator)
  (for-each (lambda (list)
	      (if (is-valid-seq list separator)
		  (my-display list)
		  #f))
	    list-of-lists))
;;  (display-valid-seqs (permutations (list 1 2 3 7 8 9)) 7)
;;  (display-valid-seqs (permutations (list 1 2 6 7 8 9)) 6)

;; therefore the formula for count of different possible orderings for the events that are consistent with the
;; individual orderings for the two processes e.g. (a b),(w x y z) is:
;; (size(seq1) + size(seq2))! / (size(seq1)! * size(seq2)!)
;; i.e. first get all permutations by prepending seq1 to seq2 as if order doesn't matter,
;; then divide by all permutations of seq1 (thereby removing unordered ones because only one permutation is valid i.e. the one that is ordered e.g. (a b))
;; follwed by division by all permutations of seq2 (thereby removing unordered ones because only one permutation is valid i.e. the one that is ordered e.g. (w x y z))

;; this formula can be extended to ordereings of more than two processes.

