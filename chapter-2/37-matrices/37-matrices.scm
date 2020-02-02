;; Exercise 2.37

;;;;Requires utilities.scm
(load "../../utilities/utilities.scm")

(define (display-matrix m)
  (map (lambda (row) (display row) (newline) row) m))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

;;testing
(dot-product (list 1 2 3) (list 4 5 6))

;;from "../33-accumulate/33-accumulate.scm"
;;Returns a sequence by applying given op starting with init on column of each of the seqs.
;;All of the sequences in the seqs list must be of the same length.
;;length of the result is equal to length of the sequences in the seqs list.
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
	    (accumulate-n op init (map cdr seqs)))))


(define vector (list 1 1 1 1))
(define matrix (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))


(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))
;;testing
(matrix-*-vector matrix vector)
(display-matrix (matrix-*-vector matrix vector))


(define (transpose mat)
  (accumulate-n cons '() mat))
;;testing
(transpose matrix)
(display-matrix (transpose matrix))


(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))
;;testing
(define matrix2 (list (list 1 2) (list 1 2) (list 1 2) (list 1 2)))
(matrix-*-matrix matrix matrix2)
(display-matrix (matrix-*-matrix matrix matrix2))
