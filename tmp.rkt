#lang racket
(require racket/trace)
(define (make-game successors-fun terminal? win? start-state display-fun)
    (list 'game successors-fun terminal? win? start-state display-fun))

(define make-mancala-game
  (λ() (make-game
        mancala-successor-fun
        mancala-terminal?     
        mancala-win?
        mancala-start-state
        (λ(state) (mancala-display-board (cdr state))))))

(define mancala-successor-fun
  (letrec              
      ((board-set
        (λ(board slot val)
          (cond ((null? board) null)
                ((= slot 0) (cons val (cdr board)))
                (else (cons (car board) (board-set (cdr board) (- slot 1) val))))))

       (board-capture
        (λ(player board slot)
          (if (or (and player       (not (<= 0 slot 5)))
                  (and (not player) (not (<= 7 slot 12)))
                  (> (list-ref board slot) 1))
              board
              (let* ((opp-slot (- 12 slot))
                     (mancala-slot (if player 6 13))
                     (new-stones (+ 1 (list-ref board opp-slot)))
                     (new-total (+ new-stones (list-ref board mancala-slot))))
                (board-set (board-set (board-set board slot 0)
                                      opp-slot
                                      0)
                           mancala-slot
                           new-total)))))
       
       (play-slot 
        (λ(player slot board)
          (let drop ((board (board-set board slot 0))
                     (prev-slot slot)
                     (slot (modulo (+ 1 slot) 14))
                     (left (list-ref board slot)))
            (if (zero? left)
                (board-capture player board prev-slot)
                (drop (board-set board slot (+ 1 (list-ref board slot)))
                      slot
                      (modulo (+ 1 slot) 14)
                      (- left 1)))))))
  (λ(state)
    (let ((player (car state))
          (board (cdr state)))
      (let add-slot-play ((slot (if player 0 7))
                          (left 6))
        (cond ((zero? left) null)
              ((not (zero? (list-ref board slot)))
               (cons (cons slot
                           (cons (not player) (play-slot player slot board)))
                     (add-slot-play (+ 1 slot) ( - left 1))))
              (else (add-slot-play (+ 1 slot) (- left 1)))))))))

(define mancala-terminal?
  (λ(state)
    (letrec
        ((board (cdr state))
         (plays? (λ(slot left)
                   (cond ((zero? left) #f)
                         ((not (zero? (list-ref board slot))) #t)
                         (else (plays? (+ 1 slot) (- left 1)))))))
      (or (not (plays? 0 6))
          (not (plays? 7 6))))))

(define mancala-win?
  (λ(player state)
    (letrec ((board (cdr state))
             (tally (lambda (slot left)
                      (if (zero? left)
                          0
                          (+ (list-ref board slot)
                             (tally (+ 1 slot) (- left 1))))))
             (score1 (tally 0 7))
             (score2 (tally 7 7)))
      (or (and player (> score1 score2))
          (and (not player) (> score2 score1))))))

(define mancala-start-state
  (list #t 6 6 6 6 6 6 0 6 6 6 6 6 6 0))


(define mancala-display-board
  (λ(board)
    (letrec ((disp-num (λ (num)
                         (cond ((< num 10) (display " ")))
                         (display num)))
             (disp-board-row (λ(op slot left)
                               (cond ((not (zero? left))
                                      (display "(")
                                      (disp-num (list-ref board slot))
                                      (display ") ")
                                      (disp-board-row op (op slot 1) (- left 1))))))
             (disp-slot-row (λ(op slot left)
                              (cond ((not (zero? left))
                                     (display "  ")
                                     (disp-num slot)
                                     (display " ")
                                     (disp-slot-row op (op slot 1) (- left 1)))))))
      ;; DISPLAY PLAYER B
      (display "(  )  12   11   10    9    8    7  (  )")
      (newline)

      (display "(  ) ")
      (disp-board-row - 12 6)
      (display "(  ) ")
      (newline)

      ;; DISPLAY MANCALA (MANCALI?)
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
      
      (display "(  )   0    1    2    3    4    5  (  )")
      (newline))))

(define simple-mancala-eval
  (lambda (player)
    (lambda (state)
      (let ((board (cdr state)))
        (list-ref board (if player 6 13))))))

(define r-s
  (lambda (proc arg2)
    (lambda (arg1)
      (proc arg1 arg2))))

(define game?
  (lambda (value)
    (and (list? value)
	 (= 6 (length value))
	 (equal? (car value) 'game)
	 (procedure? (cadr value))
	 (procedure? (caddr value))
	 (procedure? (cadddr value))
	 (procedure? (list-ref value 5)))))

(define game-check
  (lambda (source-proc op)
    (lambda (game)
      (if (not (game? game))
	  (error (string-append source-proc ": Expected a game, given ") game)
	  (op game)))))

(define game-start-state (game-check "game-start-state" (r-s list-ref 4)))
(define game-terminal? (game-check "game-terminal?" (r-s list-ref 2)))
(define game-successors-fun (game-check "game-successors-fun" cadr))
(define game-display-fun (game-check "game-display-fun" (r-s list-ref 5)))
(define game-win?  (game-check "game-win?" (r-s list-ref 3)))

(define game-play 
  (lambda (game player1 player2)
    (let play ((state (game-start-state game))
               (player #t))
      ((game-display-fun game) state) ; Display the current state
      (if ((game-terminal? game) state) ; Is the game over?
          (cond
           (((game-win? game) #t state)
            (display "Player 1 Wins!") (newline))
           (((game-win? game) #f state)
            (display "Player 2 Wins!") (newline))
           (else
            (display "Draw!") (newline)))
          
          (let ((move (if player       ; Otherwise, take the next move
                      (player1 state)
                      (player2 state))))
            (display "Player ") 
            (display (if player "1" "2")) 
            (display " chooses ")
            (display move)
            (newline)

            (play (cdr (assoc move ((game-successors-fun game) state)))
                  (not player)))))))


(define mancala (make-mancala-game))
(define mancala-player1-eval (simple-mancala-eval #t))
(define mancala-player2-eval (simple-mancala-eval #f))

(define (user1 x) 
  (read))
(define (user2 x) 
  (read))
(game-play mancala user1  user2)