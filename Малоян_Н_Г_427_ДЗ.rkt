#lang scheme/base
(require scheme/mpair)

(define (make-queue) (mcons (mlist) (mlist)))

(define (front-queue q)
  (cond ((not (empty-queue? q)) (mcar (mcar q)))))

(define (insert-queue! q e)
  (let ((new-pair (mcons e '())))
    (if (empty-queue? q)
        (begin (set-mcar! q new-pair) (set-mcdr! q new-pair) q)
        (begin (set-mcdr! (mcdr q) new-pair) (set-mcdr! q new-pair) q))))

(define (delete-queue! q)
  (cond ((not (empty-queue? q))
         (set-mcar! q (mcdr (mcar q)))
         (if (null? (mcdr q))
             q
             (set-mcdr! q null)))))

(define (queue? q) (and (not (null? q)) (mlist? (mcar q)) (mlist? (mcdr q))))

(define (empty-queue? q) (null? (mcar q)))
