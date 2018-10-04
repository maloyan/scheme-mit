; заготовка "Доктора". Март 2018
#lang scheme/base
; В учебных целях используется базовая версия Scheme

; основная функция, запускающая "Доктора"
; параметр name -- имя пациента
(define (visit-doctor name)
  (printf "Hello, ~a!\n" name)
  (print '(what seems to be the trouble?))
  (doctor-driver-loop name null)
)

; цикл диалога Доктора с пациентом
; параметр name -- имя пациента
; said - список предыдущих реплик
(define (doctor-driver-loop name said)
  (newline)
  (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
  (let ((user-response (read)))
      (cond ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
             (printf "Goodbye, ~a!\n" name)
             (print '(see you next week)))
            (else (print (reply user-response said)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
                  (doctor-driver-loop name (cons user-response said))))))

; генерация ответной реплики по user-response -- реплике от пользователя
; fix добавить case
(define (reply user-response said)
  (if (null? said)
      (if (fifty-fifty)
          (qualifier-answer user-response)
          (hedge))
      (let ((rand (random 3)))
        (cond ((= rand 0) (qualifier-answer user-response))
              ((= rand 1) (hedge)) ; 1й способ
              ((= rand 2) (history-answer said))))))  ; 2й способ

; возвращает #f с вероятностью 1/2 либо #t с вероятностью 1/2
(define (fifty-fifty)
  (= (random 2) 0)
)
			
; 1й способ генерации ответной реплики -- замена лица в реплике пользователя и приписывание к результату нового начала
(define (qualifier-answer user-response)
        (append (pick-random '((you seem to think that)
                               (you feel that)
                               (why do you believe that)
                               (why do you say that)
                               (are you sure that)
                               (how did you notice that)
                               (it is unusual that))
                )
                (change-person user-response)
        )
 )

; случайный выбор одного из элементов списка lst
(define (pick-random lst)
  (list-ref lst (random (length lst)))
)

; замена лица во фразе			
(define (change-person phrase)
        (many-replace '((am are)
                        (are am)
                        (i you)
                        (me you)
                        (mine yours)
                        (my your)
                        (myself yourself)
                        (you i)
                        (your my)
                        (yours mine)
                        (yourself myself))
                      phrase))
  
; осуществление всех замен в списке lst по ассоциативному списку replacement-pairs
(define (many-replace replacement-pairs lst)
  (map (lambda (x) (let ((pat-rep (assoc x replacement-pairs)))
                      (if pat-rep
                          (cadr pat-rep)
                          x 
                          ))) lst))
;(define (many-replace replacement-pairs lst)  
;  (define (helper replacement-pairs lst ans)
;        (cond ((null? lst) (reverse ans))
;              (else (let ((pat-rep (assoc (car lst) replacement-pairs))) ; Доктор ищет первый элемент списка в ассоциативном списке замен
;                      (if pat-rep
;                          (helper replacement-pairs (cdr lst) (cons (cadr pat-rep) ans)) ; если поиск был удачен, то в начало ответа Доктор пишет замену
;                          (helper replacement-pairs (cdr lst) (cons (car lst) ans))) ; иначе в начале ответа помещается начало списка без изменений
;                      )))) ; рекурсивно производятся замены в хвосте списка
;  (helper replacement-pairs lst null))

; 2й способ генерации ответной реплики -- случайный выбор одной из заготовленных фраз, не связанных с репликой пользователя
(define (hedge)
       (pick-random '((please go on)
                       (many people have the same sorts of feelings)
                       (many of my patients have told me the same thing)
                       (please continue)
                       (try to calm down)
                       (be patient)
                       (it is good that you decided to visit me))
         )
)

(define (history-answer lst)
  (append '(earlier you said that) (change-person (pick-random lst))))