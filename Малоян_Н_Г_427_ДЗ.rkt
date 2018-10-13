#lang scheme/base
(require scheme/mpair)

(define (make-queue) (mcons (mlist) (mlist)))

(define (front-queue q)
  (cond ((not (empty-queue? q)) (caar q))))

(define (insert-queue! q e)
  (let ((new-pair (mcons e '())))
    (if (empty-queue? q)
        (begin (set-mcar! q new-pair) (set-mcdr! q new-pair) q)
        (begin (set-mcdr! (mcdr q) new-pair) (set-mcdr! q new-pair) q))))

(define (delete-queue! q)
  (cond ((not (empty-queue? q))
         (set-mcar! q (mcdr (mcar q)))
         q)))

(define (queue? q) (and (mlist? (mcar q)) (mlist? (mcdr q))))

(define (empty-queue? q) (null? (mcar q)))