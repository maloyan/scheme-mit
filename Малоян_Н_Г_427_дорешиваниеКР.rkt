#lang scheme/base

(define (fun1 lst)
  (if (null? lst)
      null
      (reverse (caddr (foldl (λ (x y) (cond ((> x (car y)) (list x (+ 1 (cadr y)) (list (cadr y))))
                                            ((= x (car y)) (list (car y) (+ 1 (cadr y)) (if (null? (caddr y))
                                                                                            (list (cadr y))
                                                                                            (cons (cadr y) (caddr y) ))))
                                            (else (list (car y) (+ 1 (cadr y)) (caddr y)))))
                             (list (car lst) 0 '())
                             lst)))))

"fun1"
(fun1 '( -1 0 1 -1 0 1 -1)) ;=>(2 5)
(fun1 '())                  ;=>()
(fun1 '(1 1 1 1))           ;=>(0 1 2 3)
(fun1 '(1 2 3 4))           ;=>(3)
(fun1 '(4 3 2 1))           ;=>(0)
(fun1 '(1 2 4 3))           ;=>(2)
(fun1 '(-4 -3 -2 -1))       ;=>(3)
(fun1 '(-5))                ;=>(0)

(define (fun2a n)
  ;перебирает делитель, начиная с 2
  ;n - число
  ;m - делитель
  (define (find n m)
    (if (= m n)
        n
        (if (integer? (/ n m))
            m
            (find n (+ m 1)))))
  ; функция рекурсии
  (define (rec n)
    (if (= n 1)
        '()
        (let ((num (find n 2)))
          (cons num (rec (/ n num))))))
  ; функция удаления дубликатов
  (define (remove-duplicates lst)
    (foldl (λ (x y) (cons x (filter (lambda (z) (not (= x z))) y))) null lst))
  (remove-duplicates (rec n)))

"fun2a"
(fun2a 1)   ;=>()
(fun2a 2)   ;=>(2)
(fun2a 12)  ;=>(2 3)
(fun2a 13)  ;=>(13)
(fun2a 100) ;=>(2 5)

(define (fun2b n)
  ;n    - начальное число
  ;cur  - текущее число
  ;last - последний элемент в списке ответа
  ;ans  - ответ
  (define (helper n cur last ans)
    (if (= n 1)
        ans
        (if (integer? (/ n cur))
            (helper (/ n cur) 2
                    (if (> cur last)
                        cur
                        last)
                    (if (> cur last)
                        (cons cur ans)
                        ans))
            (helper n (+ cur 1) last ans))))
  (helper n 2 0 '()))

"fun2b"
(fun2b 1)   ;=>()
(fun2b 2)   ;=>(2)
(fun2b 12)  ;=>(2 3)
(fun2b 13)  ;=>(13)
(fun2b 100) ;=>(2 5)

(define (fun3 n)
  ;cur     - текущее число, для которого решается задача
  ;m       - n элемент последовательности фибоначчи
  ;k       - (n+1)элемент последовательности фибоначчи
  ;forward - флаг обхода (0 - вперед) (1 - назад)
  ;ans     - ответ
  (define (helper cur m k forward ans)
    (if (= forward 1)
        (if (> k cur)
            (helper cur (- k m) m 0 0)
            (helper cur k (+ m k) 1 0))
        (if (= m 0)
            ans
            (if (>= cur k)
                (helper (- cur k) (- k m) m 0 (+ (* 2 ans) 1))
                (helper cur  (- k m) m 0 (* 2 ans))))))
  (helper n 0 1 1 0))

"fun3"
(fun3 1)   ;=>1
(fun3 2)   ;=>2
(fun3 3)   ;=>4
(fun3 4)   ;=>5
(fun3 5)   ;=>8
(fun3 6)   ;=>9
(fun3 9)   ;=>17
(fun3 10)  ;=>18