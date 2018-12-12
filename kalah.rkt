#lang racket
(require racket/gui/base)
(require racket/trace)

; Описание игровой ситуации
(define (make-kalah-game)
  (list
   'game
   kalah-successor-fun
   kalah-terminal?     
   kalah-win?
   kalah-start-state
   kalah-display-board))


; Создает список всех возможных ходов
(define (kalah-successor-fun state)
  
  ; В ячейке slot ставит значение val камней
  ; board - игровая доска вида '(6 6 6 6 6 6 0 6 6 6 6 6 6 0)
  ; slot  - ячейка
  ; val   - значение, которое мы хотим присвоить этой ячейке
  (define (board-set board slot val)
    (list-set board slot val))
  
  ; Функция захвата камней
  ; В игре захват камней происходит, если последний в ходе камень
  ; игрок кладёт в пустую лунку в своём ряду, а противоположная лунка
  ; в ряду противника не пуста, тогда он забирает все камни из
  ; противоположной лунки вместе со своим последним камнем 
  (define (board-capture player board slot)
    (if (or (and player       (not (<= 0 slot 5)))
            (and (not player) (not (<= 7 slot 12)))
            (> (list-ref board slot) 1))
        board
        (let* ((opp-slot (- 12 slot))
               (kalah-slot (if player 6 13))
               (new-stones (+ 1 (list-ref board opp-slot)))
               (new-total (+ new-stones (list-ref board kalah-slot))))
          (board-set (board-set (board-set board slot 0)
                                opp-slot
                                0)
                     kalah-slot
                     new-total))))
  
  ; Обычный ход с ячейки slot
  ; Обнуляем текущий slot, проверяем можно ли захватить камни
  ; Если нет, то раскидываем оставшиеся камни в ямы
  (define (play-slot player slot board)
    (let drop ((board (board-set board slot 0))
               (prev-slot slot)
               (slot (modulo (+ 1 slot) 14))
               (left (list-ref board slot)))
      (if (zero? left)
          (board-capture player board prev-slot)
          (drop (board-set board slot (+ 1 (list-ref board slot)))
                slot
                (modulo (+ 1 slot) 14)
                (- left 1)))))
  
  (let ((player (car state))
        (board (cdr state)))
    (let add-slot-play ((slot (if player 0 7))
                        (left 6))
      (cond ((zero? left) null)
            ((not (zero? (list-ref board slot)))
             (cons (cons slot
                         (cons (if (= (if player 6 13) (+ slot (list-ref board slot)))
                                   player
                                   (not player))
                               (play-slot player slot board)))
                   (add-slot-play (+ 1 slot) ( - left 1))))
            (else (add-slot-play (+ 1 slot) (- left 1)))))))

; Проверка на конец игры
(define (kalah-terminal? state)
  (define (plays? slot left)
    (cond ((zero? left) #f)
          ((not (zero? (list-ref (cdr state) slot))) #t)
          (else (plays? (+ 1 slot) (- left 1)))))
  (or (not (plays? 0 6))
      (not (plays? 7 6))
      (> (list-ref (cdr state) 6) 36)
      (> (list-ref (cdr state) 13) 36)))

; Выиграл ли игрок player при игровой ситуации state
(define (kalah-win? player state)
  (define (tally slot left)
    (if (zero? left)
        0
        (+ (list-ref (cdr state) slot)
           (tally (+ 1 slot) (- left 1)))))
  (let ((score1 (tally 0 7))
        (score2 (tally 7 7)))
    (or (and player (> score1 score2))
        (and (not player) (> score2 score1)))))

; Начальное состояние игрового поля
(define kalah-start-state
  (list #t 6 6 6 6 6 6 0 6 6 6 6 6 6 0))
(define (kalah-display-board) #t)

(define (r-s proc arg2)
  (lambda (arg1)
    (proc arg1 arg2)))

(define (game? value)
  (and (list? value)
       (= 6 (length value))
       (equal? (car value) 'game)
       (procedure? (cadr value))
       (procedure? (caddr value))
       (procedure? (cadddr value))
       (procedure? (list-ref value 5))))

(define game-check
  (lambda (source-proc op)
    (lambda (game)
      (if (not (game? game))
          (error (string-append source-proc ": Expected a game, given ") game)
          (op game)))))


; Игрок - компьютер, играющий с помощью минмакс с альфа бета отсечением
; Принимает текущее состояние игры и выдает номер хода
; game  -
; plies -
; evaluation-fun - функция, которая считает наши очки (кол-во камней в калахе)
(define (make-alpha-beta-player game plies evaluation-fun)
  (λ (state)
    (car (alpha-beta-max-value game state
                               -inf.0 +inf.0 
                               0 plies 
                               evaluation-fun))))

; Для вершины Max
; Псевдокод
; Max-Value (s, alpha, beta):
;   if terminal(s) return U(s)
;   v = -inf
;   for c in next-states(s):
;     v' = Min-value(c, alpha, beta)
;     if v' > v, v = v'
;     if v' >= beta, return v
;     if v' > alpha, return v'
;   return v
(define (alpha-beta-max-value game state alpha beta depth plies evaluation-fun)
  ; Проверка на то, достигли ли мы максимальной глубины или терминального хода
  (cond [(or (= depth plies) ((game-terminal? game) state))
         (cons null (evaluation-fun state))]
        [else (let loop ([successors ((game-successors-fun game) state)]
                         [argmax null]
                         [maxval null]
                         [loop-alpha alpha])
                
                (if (or (null? successors) (not loop-alpha))
                    ; Нет пар (ход . положение доски) или цикл закончился
                    (cons argmax maxval)
                    ; Вызываем Min
                    (let* ([action-value (alpha-beta-min-value 
                                          game 
                                          (cdar successors)
                                          loop-alpha
                                          beta
                                          depth
                                          plies
                                          evaluation-fun)]
                           [val (cdr action-value)])
                      (cond [(or (null? argmax) (> val maxval))
                             (loop (cdr successors)
                                   (caar successors)
                                   val
                                   (if (>= val beta)
                                       #f
                                       (max loop-alpha val)))]              
                            [else (loop (cdr successors) 
                                        argmax
                                        maxval 
                                        loop-alpha)]))))]))

    
; Для вершины Min
; Псевдокод
; Min-Value (s, alpha, beta):
;   if terminal(s) return U(s)
;   v = +inf
;   for c in next-states(s):
;     v' = Max-value(c, alpha, beta)
;     if v' < v, v = v'
;     if v' <= alpha, return v
;     if v' < beta, return v'
;   return v
(define (alpha-beta-min-value game state alpha beta depth plies evaluation-fun)
  (cond [(or (= depth plies) ((game-terminal? game) state))
         (cons null (evaluation-fun state))] 
        [else (let loop ([successors ((game-successors-fun game) state)]
                         [argmin null] 
                         [minval null]
                         [loop-beta beta])
		 
                (if (or (null? successors) (not loop-beta))
                    (cons argmin minval)
                    (let* ([action-value (alpha-beta-max-value 
                                          game 
                                          (cdar successors)
                                          alpha
                                          loop-beta
                                          (+ 1 depth)
                                          plies
                                          evaluation-fun)]
                           [val (cdr action-value)])
                      (cond  [(or (null? argmin) (< val minval)) 
                              (loop (cdr successors)
                                    (caar successors)
                                    val
                                    (if (<= val alpha)
                                        #f
                                        (min loop-beta val)))]
                             [else (loop (cdr successors)  
                                         argmin 
                                         minval 
                                         loop-beta)]))))]))

; Функция, считающая очки (камни в калахе)
(define (simple-mancala-eval player)
  (lambda (state)
    (let ((board (cdr state)))
      (list-ref board (if player 6 13)))))


(define game-successors-fun (game-check "game-successors-fun" cadr))
(define game-terminal?      (game-check "game-terminal?" (r-s list-ref 2)))
(define game-win?           (game-check "game-win?" (r-s list-ref 3)))
(define game-start-state    (game-check "game-start-state" (r-s list-ref 4)))

(define new-state
  (list #t 6 6 6 6 6 6 0 6 6 6 6 6 6 0))

(define (play-pvp state game)
  (if ((game-terminal? game) state)
      (begin
        (for ([x '(0 1 2 3 4 5)])
          (send (vector-ref botbut x) set-label (number->string (list-ref state (+ x 1))))
          (send (vector-ref botbut x) enable #f)
            
          (send (vector-ref topbut x) set-label (number->string (list-ref state (+ x 8))))
          (send (vector-ref topbut x) enable #f))
        (send klhb set-label (number->string (list-ref state 7)))
        (send klht set-label (number->string (list-ref state 14)))
        (cond
          (((game-win? game) #t state)
           (send msg set-label "Первый игрок победил!"))
          (((game-win? game) #f state)
           (send msg set-label "Второй игрок победил!"))
          (else
           (send msg set-label "Ничья!"))))

      (begin
        (for ([x '(0 1 2 3 4 5)])
          (send (vector-ref botbut x) set-label (number->string (list-ref state (+ x 1))))
          (send (vector-ref botbut x) enable (car state))
            
          (send (vector-ref topbut x) set-label (number->string (list-ref state (+ x 8))))
          (send (vector-ref topbut x) enable (not (car state))))
          
        (send klhb set-label (number->string (list-ref state 7)))
        (send klht set-label (number->string (list-ref state 14)))
        (if (car state)
            (send msg set-label "1-Игрок, Ваш ход")
            (send msg set-label "2-Игрок, Ваш ход")))))


(define (play-cvc game player1 player2)
  (let play ((state (game-start-state game)))
    (if ((game-terminal? game) state)
        (begin
          (for ([x '(0 1 2 3 4 5)])
            (send (vector-ref botbut x) set-label (number->string (list-ref state (+ x 1))))
            (send (vector-ref botbut x) enable #f)
            
            (send (vector-ref topbut x) set-label (number->string (list-ref state (+ x 8))))
            (send (vector-ref topbut x) enable #f))
          (send klhb set-label (number->string (list-ref state 7)))
          (send klht set-label (number->string (list-ref state 14)))
          (cond
            (((game-win? game) #t state)
             (send msg set-label "Первый игрок победил!"))
            (((game-win? game) #f state)
             (send msg set-label "Второй игрок победил!"))
            (else
             (send msg set-label "Ничья!"))))
          
        (let ((move (if (car state)
                        (player1 state)
                        (player2 state))))
          (begin
            (for ([x '(0 1 2 3 4 5)])
              (send (vector-ref botbut x) set-label (number->string (list-ref state (+ x 1))))
              (send (vector-ref botbut x) enable (car state))
            
              (send (vector-ref topbut x) set-label (number->string (list-ref state (+ x 8))))
              (send (vector-ref topbut x) enable (not (car state))))
          
            (send klhb set-label (number->string (list-ref state 7)))
            (send klht set-label (number->string (list-ref state 14)))
            (sleep 1)
            (play (cdr (assoc move ((game-successors-fun game) state)))))))))

; Инициализация игры
(define kalah (make-kalah-game))

; Игрок1 - ИИ
(define simple-mancala-best-player1
  (make-alpha-beta-player kalah 5 (simple-mancala-eval #t)))

; Игрок2 - ИИ
(define simple-mancala-best-player2
  (make-alpha-beta-player kalah 5 (simple-mancala-eval #f)))

; Шрифт
(define fnt (make-object font% 14 'system))
(define fnt1 (make-object font% 48 'system))

; Основное окно
(define frame (new frame% [label "Калах"]
                   [alignment '(center center)]
                   [width 1000]
                   [height 600]))

; Игровое поле
(define panel1 (new horizontal-panel% [parent frame]
                    [style '(border)]
                    [min-height 400]))

; Калах второго игрока
(define kalah1 (new panel% [parent panel1]
                    [style '(border)]
                    [min-width 300]))
; Игровая доска
(define board (new vertical-panel% [parent panel1]
                   [style '(border)]
                   [min-width 600]))
; Калах второго игрока
(define kalah2 (new panel% [parent panel1]
                    [style '(border)]
                    [min-width 300]))
; Кол-во камней у второго игрока
(define klht (new message% [parent kalah1]
                  [label "0"]
                  [font fnt1]
                  [auto-resize #t]))
; Кол-во камней у первого игрока
(define klhb (new message% [parent kalah2]
                  [label "0"]
                  [font fnt1]
                  [auto-resize #t]))
; Верхние ямы
(define top (new horizontal-panel% [parent board]
                 [style '(border)]
                 [alignment '(center center)]
                 [min-height 200]))
; Нижние ямы
(define bot (new horizontal-panel% [parent board]
                 [style '(border)]
                 [alignment '(center center)]
                 [min-height 200]))

(define topbut (make-vector 6))
(for ([i '(5 4 3 2 1 0)])
  (vector-set! topbut i (new button% [parent top]
                             [label (number->string 0)]
                             [font fnt]
                             [horiz-margin 8]
                             [callback (lambda (button event)
                                         (begin
                                           (set! new-state (cdr (assoc (+ i 7) ((game-successors-fun kalah) new-state))))
                                           (play-pvp new-state kalah)))])))

(define botbut (make-vector 6))
(for ([x '(0 1 2 3 4 5)])
  (vector-set! botbut x (new button% [parent bot]
                             [label (number->string 0)]
                             [font fnt]
                             [horiz-margin 8]
                             [callback (lambda (button event)
                                         (begin
                                           (set! new-state (cdr (assoc x ((game-successors-fun kalah) new-state))))
                                           (play-pvp new-state kalah)))])))
(define panel2 (new vertical-panel% [parent panel1]
                    [style '(border)]
                    [min-height 400]))

(define game-name (new message% [parent panel2]
                       [min-width 350]
                       [min-height 50]
                       [font fnt]
                       [auto-resize #t]
                       [label "Калах"]))
; Кнопка для новой игры PvP
(define pvp (new button% [parent panel2]
                 [label "Новая игра (PvP)"]
                 [vert-margin 8]
                 [horiz-margin 8]
                 [min-width 330]
                 [min-height 50]
                 [font fnt]
                 [callback (lambda (button event)
                             (begin
                               (set! new-state kalah-start-state)
                               (play-pvp (game-start-state kalah) kalah)))]))

; Кнопка для новой игры CvC
(define cvc (new button% [parent panel2]
                 [label "Новая игра (CvC)"]
                 [vert-margin 8]
                 [horiz-margin 8]
                 [min-width 330]
                 [min-height 50]
                 [font fnt]
                 [callback (lambda (button event)
                             (begin
                               (send msg set-label "Играют ИИ, ждите")
                               (play-cvc kalah simple-mancala-best-player1 simple-mancala-best-player2)))]))

; статичное сообщение
(define msg (new message% [parent panel2]
                 [min-width 350]
                 [min-height 50]
                 [font fnt]
                 [auto-resize #t]
                 [label "Выберите режим игры"]))

(send frame show #t)
