
#lang racket
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

; Функция для отображения игрового поля
(define (kalah-display-board state)
  (let ((board (cdr state))
        (player (car state)))
  (define (disp-num num)
    (cond ((< num 10) (display " ")))
    (display num))
  (define(disp-board-row op slot left)
    (cond ((not (zero? left))
           (display "(")
           (disp-num (list-ref board slot))
           (display ") ")
           (disp-board-row op (op slot 1) (- left 1)))))
  (define (disp-slot-row op slot left)
    (cond ((not (zero? left))
           (display "  ")
           (disp-num slot)
           (display " ")
           (disp-slot-row op (op slot 1) (- left 1)))))
  (begin
    ;; DISPLAY PLAYER B
    (display "(  )  12   11   10    9    8    7  (  ) Player 2")
    (newline)
    (display "---------------------------------------")
    (newline)

    (display "(  ) ")
    (disp-board-row - 12 6)
    (display "(  ) ")
    (newline)

    ;; DISPLAY kalah (MANCALI?)
    (display "(")
    (disp-num (list-ref board 13))
    (display ")")
      
    (display "                              ")

    (display " (")
    (disp-num (list-ref board 6))
    (display ")")

    (newline)

      
    ;; DISPLAY PLAYER A

    (display "(  ) ")
    (disp-board-row + 0 6)
    (display "(  ) ")
    (newline)

    (display "---------------------------------------")
    (newline)
    
    (display "(  )   0    1    2    3    4    5  (  ) Player 1")
    (newline))))

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


(define game-successors-fun (game-check "game-successors-fun" cadr))
(define game-terminal?      (game-check "game-terminal?" (r-s list-ref 2)))
(define game-win?           (game-check "game-win?" (r-s list-ref 3)))
(define game-start-state    (game-check "game-start-state" (r-s list-ref 4)))
(define game-display-fun    (game-check "game-display-fun" (r-s list-ref 5)))

(define game-play 
  (lambda (game player1 player2)
    (let play ((state (game-start-state game)))
      ((game-display-fun game) state)
      (if ((game-terminal? game) state)
          (cond
           (((game-win? game) #t state)
            (display "Player 1 Wins!") (newline))
           (((game-win? game) #f state)
            (display "Player 2 Wins!") (newline))
           (else
            (display "Draw!") (newline)))
          
          (let ((move (if (car state)
                      (begin (display "Player 1, Ваш ход\n") (player1 state))
                      (begin (display "Player 2, Ваш ход\n") (player2 state)))))
            (display "Player ") 
            (display (if (car state) "1" "2")) 
            (display " сыграл ")
            (display move)
            (newline)

            (play (cdr (assoc move ((game-successors-fun game) state)))))))))

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

; Функция считающая очки (камни в калахе)
(define (simple-mancala-eval player)
  (lambda (state)
    (let ((board (cdr state)))
      (list-ref board (if player 6 13)))))

; Инициализация игры
(define kalah (make-kalah-game))

; Игрок1 - человек
(define (user1 state)
  (let ((move (read))
        (board (cdr state)))
    (if (and (<= 0 move 5) (not (= 0 (list-ref board move))))
        move
        (begin
          (display "Выберите непустое поле от 0 до 5\n")
          (user1 board)))))

; Игрок2 - человек
(define (user2 state)
  (let ((move (read))
        (board (cdr state)))
    (if (and (<= 7 move 12) (not (= 0 (list-ref board move))))
        move
        (begin
          (display "Выберите непустое поле от 7 до 13\n")
          (user2 board)))))
; Игрок1 - ИИ
(define simple-mancala-best-player1
  (make-alpha-beta-player kalah 5 (simple-mancala-eval #t)))

; Игрок2 - ИИ
(define simple-mancala-best-player2
  (make-alpha-beta-player kalah 5 (simple-mancala-eval #f)))

;(game-play kalah simple-mancala-best-player1 simple-mancala-best-player2)
(game-play kalah user1 simple-mancala-best-player2)
