;; Exercise 2.74

;;;;Requires utilities.scm
(load "../../utilities/utilities.scm")

;;as json structure
;;(division-name : "a",
;;	       division-file : (ename : "",
;;				      (erecord: (eaddress: "",
;;							   esalary : ""))))

;;as list structure
;;("division-name",
;; (("ename1", ("eaddress", "esalary")), ("ename2", ("eaddress", "esalary"))))

;;(define (put 'division-op 'division-name proc))
;;(define (get 'division-op 'division-name))

;;(define (put division-name file-op 2-arg-proc)) ;;ename, file
;;(define (get division-name file-op))

;;(define (put division-name record-op 1-arg-proc)) ;;record
;;(define (get division-name record-op))

;;1.
(define (get-record ename division-file)
  ((get ((get 'division-op 'division-name) division-file) 'employee-record) ename))

;;2.
(define (get-salary ename division-file)
  (let ((div-name) (get 'division-op 'division-name))
    ((get div-name 'employee-salary) ((get div-name 'employee-record) ename division-file))))

;;3.
(define (find-employee-record ename division-file-list)
  (cond ((null? division-file-list) 'No-Record-Found)
	((null? (get-record ename (car division-file-list))) find-employee-record ename (cdr division-file-list))
	(else (get-record ename (car division-file-list)))))

;;4.
;;install procedures for all puts listed above.
