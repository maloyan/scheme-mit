;;
;; File
;;   general.scm
;;
;; Author
;;   Jerod Weinman
;;
;; Summary
;;   Provides a collection of generally useful routines
;;
;; Provides
;;   (index-of val lst)
;;   (any? lst)
;;   (r-s proc left)
;;   (iota n)
(module general lang/plt-pretty-big
	(provide index-of
                 any?
                 r-s
                 iota)

;;
;; Procedure
;;   index-of
;;
;; Purpose
;;   Find the index of a value in a list
;;
;; Parameters
;;   val, a value
;;   lst, a list
;;
;; Produces
;;   index, a value
;;
;; Preconditions
;;   [None]
;;
;; Postconditions
;;   if index is not #f, then (equal? (list-ref lst index) val) is #t
(define index-of
  (lambda (val lst)
    (let loop ((index 0)
	       (lst lst))
      (cond
       ((null? lst)
	#f)
       ((equal? (car lst) val)
	index)
       (else
	(loop (+ 1 index) (cdr lst)))))))


;;
;; Procedure
;;   any?
;;
;; Purpose
;;   Determine if any element of a list is non-false
;;
;; Parameters
;;   lst, a list
;;
;; Produces
;;   is-any, a boolean
;;
;; Preconditions
;;   [None]
;;
;; Postconditions
;;   Returns #t if (not (list-ref lst index)) is #f for at least one
;;   value of index 0<= index < (length lst).

(define any? 
  (lambda (lst)
    (and (not (null? lst))
         (or (car lst) (any? (cdr lst))))))

;; right-section, functor for fixing the right-argument to a binary procedure
(define r-s
  (lambda (proc arg2)
    (lambda (arg1)
      (proc arg1 arg2))))

;; (iota n), produces a list (0 ... n-1)
(define iota
  (lambda (n)
    (let loop ((so-far null)
	       (n n))
      (if (zero? n)
	  so-far
	  (loop (cons (- n 1) so-far)
		(- n 1))))))
) ; module
