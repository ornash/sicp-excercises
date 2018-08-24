(define (count-change amount)
  (define (first-denomination kinds-of-coins)
    (cond ((= kinds-of-coins 1) 1)
	  ((= kinds-of-coins 2) 5)
	  ((= kinds-of-coins 3) 10)
	  ((= kinds-of-coins 4) 25)
	  (else (= kinds-of-coins 5) 50)))
  
  (define (cc amount kinds-of-coins)
    (cond
     ;;reached bottom of recursion tree, thus this is one way to count.
     ((= amount 0) 1) 
     ;;amount is too low so this is not a way to count or  no more coins are left
     ((or (< amount 0) (= kinds-of-coins 0)) 0)
     (else (+
	    ;;Don't use highest denomination try other coins; and dont include highest coin to avoid duplicate ways.
	    (cc amount (- kinds-of-coins 1))
	    ;;Use the highest denomination and countChange for leftover amount using all the coins.
	    (cc (- amount (first-denomination kinds-of-coins))
		kinds-of-coins)))))
  
  (cc amount 5))

;;Test cases, all should evaluate to true
(= 1 (count-change 1))
(= 2 (count-change 5))
(= 4 (count-change 10))
(= 13 (count-change 25))
(= 50 (count-change 50))
(= 292 (count-change 100))

