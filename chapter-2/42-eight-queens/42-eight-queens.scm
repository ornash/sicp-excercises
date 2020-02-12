;; Exercise 2.42

;;;;Requires utilities.scm
(load "../../utilities/utilities.scm")

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
	(list empty-board)
	(filter
	 (lambda (positions) (safe? k positions))
	 (flatmap
	  (lambda (rest-of-queens)
	    (map (lambda (new-row)
		   (adjoin-position new-row k rest-of-queens))
		 (enumerate-interval 1 board-size)))
	  (queen-cols (- k 1))))))
  (queen-cols board-size))


;;Check
;;http://community.schemewiki.org/?sicp-ex-2.42
;;http://community.schemewiki.org/?sicp-ex-2.43

