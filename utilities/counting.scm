;;;;Requires utilities.scm
(load "../utilities/utilities.scm")

(define map-then-flatten flatmap)

;; returns permutations as list-of-lists by permuting given element over prior permutations
(define (permute-element-over-permutations element list-of-lists)
  
  ;; returns list-of-lists by creating a new list by inserting element for each position in aList.
  (define (permute-element-over-list element alist)
    (define (internal prefix element-as-list suffix)
      (cond ((null? suffix) (list (append prefix element-as-list suffix))) ;; return as list-of-lists
	    (else
	     (cons ;; cons-up-cdr-down on suffix by moving element-as-list from left to right and creating new list for each instance.
	      (append prefix element-as-list suffix) ;; new list with prefix,element,suffix appended together.
	      (internal (append prefix (list (car suffix)))
			element-as-list
			(cdr suffix))))))

    (internal '() (list element) alist))

  ;; permute by mapping the element over each list separately and then append all resulting list of lists together.
  (map-then-flatten (lambda (alist)
		      (permute-element-over-list element alist))
		    list-of-lists))

;; returns permutations as list-of-lists given a list of elements
(define (permutations elements)
  (cond ((null? elements) (list elements))
	((= 1 (length elements)) (list elements))
	(else ;;cdr-down, first permute tail and then head.
	 (permute-element-over-permutations (car elements)
					    (permutations (cdr elements))))))

(define (display-permutations list-of-lists)
  (for-each (lambda (list) (my-display list))
	    list-of-lists))

