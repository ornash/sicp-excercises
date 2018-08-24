;;All necessary utility functions

;;Environemnt utils
;;Load this file immediately after starting sbcl.
(defun lisp-file-path (name)
  (concatenate 'string "~/home/personal/sicp/" name "/" name ".lisp"))
(defun load-lisp-file (name)
  (load (lisp-file-path name)))
;;Alias for load-lisp-file
(defun my-load (name)
  (load-lisp-file name))

;;Programming utils
(defun minus-1 (x)
  (- x 1))
(defun plus-1 (x)
  (+ x 1))

;;Math utils
(defun square (x)
  (* x x))
(square 3)
(defun even? (x)
  (= (rem x 2) 0))
(defun odd? (x)
  (= (rem x 2) 1))





