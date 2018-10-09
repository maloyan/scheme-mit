; заготовка "Доктора". Март 2018
#lang racket
; В учебных целях используется базовая версия Scheme

(define (ask-patient-name)
  (begin
    (println '(next!))
    (println '(who are you?))
    (print '**)
    (car (read))))

; основная функция, запускающая "Доктора"
; параметр name -- имя пациента
(define (visit-doctor stopword amount)
  (if (= amount 0)
      (print '(I'm done for today))
      (let ((name (ask-patient-name)))
        (if (equal? name stopword)
            (print '(time to go home))
            (begin
              (printf "Hello, ~a!\n" name)
              (print '(what seems to be the trouble?))
              (doctor-driver-loop name null)
              (visit-doctor stopword (- amount 1)))))))

; цикл диалога Доктора с пациентом
; параметр name -- имя пациента
; said - список предыдущих реплик
; stopword - стопслово для окончания работы
(define (doctor-driver-loop name said)
  (newline)
  (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
  (let ((user-response (read)))
      (cond ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
             (printf "Goodbye, ~a!\n" name)
             (printf "see you next week\n"))
            (else (print (reply user-response said)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
                  (doctor-driver-loop name (cons user-response said))))))

; генерация ответной реплики по user-response -- реплике от пользователя
(define (reply-old user-response said)
  (if (check-phrase? keywords user-response)
      (reply-keyword keywords user-response)
      (if (null? said)
          (if (fifty-fifty)
              (qualifier-answer user-response)
              (hedge))
          (case (random 3)
            ([0] (qualifier-answer user-response))
            ([1] (hedge))
            ([2] (history-answer said))))))


; возвращает #f с вероятностью 1/2 либо #t с вероятностью 1/2
(define (fifty-fifty)
  (= (random 2) 0)
)
			
; 1й способ генерации ответной реплики -- замена лица в реплике пользователя и приписывание к результату нового начала
(define (qualifier-answer user-response said)
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
(define (hedge phrase said)
       (pick-random '((please go on)
                       (many people have the same sorts of feelings)
                       (many of my patients have told me the same thing)
                       (please continue)
                       (try to calm down)
                       (be patient)
                       (it is good that you decided to visit me))
         )
)

; 3й способ генерации ответной реплики -- добавление одного из предыдущих ответов к словам earlier you said that
(define (history-answer phrase said)
  (append '(earlier you said that) (change-person (pick-random said))))

;Ключевые слова и реплики к ним
; сначала идут group слов, в которой содержатся ключевые слова
(define keywords
  '(((depressed suicide exams university)
     ((when you feel depressed, go out for ice cream)
      (depression is a disease that can be treated)))
    ((mother father parents brother sister uncle aunt grandma grandpa)
     ((tell me more about your * , i want to know all about your *)
     (why do you feel that way about your * ?)))
    ((university scheme lections)
     ((your education is important)
      (how many time do you spend to learning?)))
    ((health sick healthy ill)
     ((since when do you feel ill?)
      (i can give you medicine)))
    ((tired lazy)
     ((get back to work, you sluggard fool!)
      (you certainly should take a vacation)))))

; функция, проверяющее включение слова в список
; word - слово
; lst - список
(define (word-in-list? word lst)
  (foldl (λ (x y) (if (equal? x word)
                      #t
                      y)) #f lst))

; проверка наличия ключевых слов во фразе
(define (check-phrase? phrase said)
  (define (keyword-in-phrase keyword)
    (foldl (λ (x y) (if (word-in-list? x phrase)
                        #t
                        y)) #f (car keyword)))
  (ormap keyword-in-phrase keywords))

; 4й способ генерации ответной реплики
; из группы ключевых слов мы находим матчи. Если матчей в одной группе несколько, то выбираем один из них. Если матчей нет, то #f
; Получается список из #f и ключевых слов, которые заматчились. Выбираем из этого списка на рандоме заматченные слова.
; Записываем это выбранное слово с репликами в final-group. Выбираем из этих реплик какую-либо на рандоме. Если надо, заменяем * на слово.
; keywords - ключевые слова с репликами
; phrase - фраза, написанная пользователем
(define (reply-keyword phrase said)
  (let ((final-group (pick-random (filter (lambda (x) (not (eq? x #f)))
                                          (map
                                           (λ(x) (let ((match (filter (λ (y) (word-in-list? y phrase)) (car x))))
                                                   (if (null? match)
                                                       #f
                                                       (list (pick-random match) (cadr x))))) keywords)))))
    (many-replace (list(list '* (car final-group))) (pick-random (cadr final-group)))))

(define (always-true phrase said) #t)
(define (check-null phrase said) (not (null? said)))

(define (possible-replies phrase said)
    (list
     (list always-true 1 qualifier-answer)
     (list always-true 1 hedge)
     (list check-null  1 history-answer)
     (list check-phrase? 1 reply-keyword)))


(define (weighted-random lst)
  (define (choise prob lst)
    (cond ((null? lst) #f)
          ((< prob (cadar lst)) (cons (caar lst) (cddar lst)))
          (else (choise (- prob (cadar lst)) (cdr lst)))))
  (let ((sum (foldl (lambda (x y) (+ y (cadr x))) 0 lst)))
    (choise (* sum (random)) lst)))
  
(define (reply phrase said)
  (let ((correct (filter (lambda (x) ((car x) phrase said))  (possible-replies phrase said))))
    ((cadr (weighted-random correct)) phrase said)))
