;;
;; File
;;   mancala.scm
;;
;; Author
;;   Jerod Weinman
;;
;; Summary
;;   Implementation of mancala game 
;;
;; Provides
;;   mancala-start-state
;;   (make-mancala-game)
;;   (mancala-successor-fun state)
;;   (mancala-terminal? state)
;;   (mancala-win? player state)
;;   (mancala-display-board board)

(module mancala lang/plt-pretty-big
	(provide mancala-start-state
                 make-mancala-game
                 mancala-successor-fun
                 mancala-terminal?
                 mancala-win?
                 mancala-display-board)
	(require "game.scm")


;;
;; Practica
;;    In mancala, the state is represented as a list, whose car is the
;;    player (a boolean representing whether it's player 1's turn), and
;;    whose cdr is the board.  The board is represented as a list of 14
;;    holes, as follows. Indices 0-5 are the holes for player 1 with
;;    index 6 as player 1's mancala. Indices 7-12 are player 2's holes
;;    and index 13 is player 2's mancala.
;;
;; (^^^^) (12) (11) (10) (09) (08) (07) (^^^^)
;; ( 13 )                               ( 06 )
;; (vvvv) (00) (01) (02) (03) (04) (05) (vvvv)




;;
;; Procedure
;;   make-mancala-game
;;
;; Purpose
;;   Create a game for Mancala
;;
;; Parameters
;;   [None]
;;
;; Produces
;;   game,a  game
;;
;; Preconditions
;;   [None]
;;
;; Postconditions
;;   [None]
(define make-mancala-game
  (lambda ()
    (make-game
     mancala-successor-fun
     mancala-terminal?     
     mancala-win?
     mancala-start-state
     (lambda (state) (mancala-display-board (cdr state))))))


(define mancala-start-state
  (list #t 4 4 4 4 4 4 0 4 4 4 4 4 4 0))

;;
;; Procedure
;;   mancala-successor-fun
;;
;; Purpose
;;   A function that creates action-state successors for a mancala game
;;
;; Parameters
;;   state, a state
;;
;; Produces
;;   successors, a list
;;
;; Preconditions
;;   [None]
;;
;; Postconditions
;;   successors is a list with entries whose cars are a moves (hole numbers) 
;;      and cdrs are resulting states
(define mancala-successor-fun
  (letrec              
      ((board-set 
      ;; Function to set a board slot to a particular value (produces new board)
        (lambda (board slot val)
          (cond 
           ((null? board)
            null)
           ((= slot 0)
            (cons val (cdr board)))
           (else
            (cons (car board)
                  (board-set (cdr board) (- slot 1) val))))))

      (board-capture
      ;; Function to capture stones from opposing cup when warranted
        (lambda (player board slot)
          (if (or (and player       (not (<= 0 slot 5))) ; Player 1: wrong slot
                  (and (not player) (not (<= 7 slot 12))); Player 2: wrong slot
                  (> (list-ref board slot) 1))           ; slot wasn't empty
              board    ; no capture, return the board
              ; Capture by adding opposing player's opposite slot's stones to 
              ; our slot and setting their slot to zero.
              (let* ((opp-slot (- 12 slot))
                     (mancala-slot (if player 6 13))
                     (new-stones (+ 1 (list-ref board opp-slot)))
                     (new-total (+ new-stones (list-ref board mancala-slot))))
                ;(display "capturing ")
                ;(display (list-ref board opp-slot))
                ;(display " from ")
                ;(display opp-slot)
                ;(newline)
                (board-set 
                 (board-set 
                  (board-set board slot 0) ; Empty player slot
                  opp-slot ; Empty opponent slot
                  0)
                 mancala-slot
                 new-total))))) ; Capture to player mancala

       (play-slot 
        ;; Function to play a particular slot on a board
        (lambda (player slot board)
          (let drop ((board (board-set board slot 0)) ; Zero-out selected slot
                     (prev-slot slot)                 ; Previous slot
                     (slot (modulo (+ 1 slot) 14))    ; Next slot to drop in
                     (left (list-ref board slot)))    ; How many beans left
            (if (zero? left) ; No beans left
                (board-capture player board prev-slot) ; Capture any beans
                (drop                           ; otherwise continue dropping
                 (board-set board slot (+ 1 (list-ref board slot))) ; increment
                 slot                                               ; prev-slot
                 (modulo (+ 1 slot) 14)                             ; next slot
                 (- left 1)))))))                                   ; remaining
  (lambda (state)
    (let ((player (car state))
          (board (cdr state)))
      (let add-slot-play ((slot (if player 0 7))
                          (left 6))
        (cond
         ((zero? left) ; No slots left to generate plays for
          null)
         ((not (zero? (list-ref board slot))) ; There are beads in the slot
          (cons (cons slot ; move
                      (cons (not player) ; next state: other player
                            (play-slot player slot board))) ; resulting board
                (add-slot-play (+ 1 slot) ( - left 1))))
         (else ; No beads left in slot to be played
          (add-slot-play (+ 1 slot) (- left 1)))))))))
        


;;
;; Procedure
;;   mancala-terminal?
;;
;; Purpose
;;   Determine whether a mancala state has no successors
;;
;; Parameters
;;   state, a state
;;
;; Produces
;;   terminal, a boolean
;;
;; Preconditions
;;   [None]
;;
;; Postconditions
;;   terminal is true if either player has no more plays
(define mancala-terminal?
  (lambda (state)
    (letrec
        ((board (cdr state))
         (plays? (lambda (slot left)
                   (cond
                    ((zero? left)                  #f)
                    ((not (zero? (list-ref board slot))) #t)
                    (else
                     (plays? (+ 1 slot) (- left 1)))))))
      (or (not (plays? 0 6))
          (not (plays? 7 6))))))


;;
;; Procedure
;;   mancala-win?
;;
;; Purpose
;;   Determine if a player won a mancala game
;;
;; Parameters
;;   player, a boolean
;;   state, a state
;;
;; Produces
;;   won, a boolean
;;
;; Preconditions
;;   (equal? (mancala-terminal? state) #t)
;;
;; Postconditions
;;   won is true if the particular player's total number of stones is strictly 
;;      greater than the other player's total
(define mancala-win?
  (lambda (player state)
    (letrec ((board (cdr state))
             (tally (lambda (slot left)
                      (if (zero? left)
                          0
                          (+ (list-ref board slot)
                             (tally (+ 1 slot) (- left 1))))))
             (score1 (tally 0 7))
             (score2 (tally 7 7)))
;      (display "score1 ") (display score1) (newline)
;      (display "score2 ") (display score2) (newline)
      (or (and player (> score1 score2))
          (and (not player) (> score2 score1))))))
      
             

;;
;; Procedure
;;   mancala-display-board
;;
;; Purpose
;;   Display a mancala board
;;
;; Parameters
;;   board, a list
;;
;; Produces
;;   [Nothing]
;;
;; Preconditions
;;   [None]
;;
;; Postconditions
;;   [None]
(define mancala-display-board
  (lambda (board)
    (letrec ((disp-num (lambda (num)
                         (if (< num 10)
                             (display " "))
                         (display num)))
             (disp-board-row (lambda (op slot left)
                               (cond
                                ((not (zero? left))
                                 (display "(")
                                 (disp-num (list-ref board slot))
                                 (display ") ")
                                 (disp-board-row op (op slot 1) (- left 1))))))
             (disp-slot-row (lambda (op slot left)
                              (cond
                               ((not (zero? left))
                                (display " ")
                                (disp-num slot)
                                (display "  ")
                                (disp-slot-row op (op slot 1) (- left 1))))))
             (disp-empty-row (lambda (left)
                               (cond
                                ((not (zero? left))
                                 (display "     ")
                                 (disp-empty-row  (- left 1)))))))

                           
                           
      ;; DISPLAY PLAYER B

      (display "(  ) ")
      (disp-board-row - 12 6)
      (display "(  ) ")
      (newline)

      (display "(  ) ")
      (disp-slot-row - 12 6)
      (display "(  ) ")
      (newline)

      ;; DISPLAY MANCALA (MANCALI?)
      (display "(")
      (disp-num (list-ref board 13))
      (display ")")
      
      (disp-empty-row 6)

      (display " (")
      (disp-num (list-ref board 6))
      (display ")")

      (newline)

      
      ;; DISPLAY PLAYER A

      (display "(  ) ")
      (disp-board-row + 0 6)
      (display "(  ) ")
      (newline)
      
      (display "(  ) ")
      (disp-slot-row + 0 6)
      (display "(  ) ")
      (newline))))
      
) ; module
