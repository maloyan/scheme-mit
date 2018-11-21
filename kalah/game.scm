;;
;; File
;;   game.scm
;;
;; Author
;;   Jerod Weinman
;;
;; Summary
;;   Routines for a generic game interface
;;
;; Provides
;;   (game? value)
;;   (make-game successors-fun terminal? win? start-state display-fun)
;;   (game-successors-fun game)
;;   (game-terminal? game)
;;   (game-win? game)
;;   (game-start-state game)
;;   (game-display-fun game)
;;   (game-play game player1 player2)

(module game lang/plt-pretty-big
	(provide game?
                 make-game
                 game-successors-fun
                 game-terminal?
                 game-win?
                 game-start-state
                 game-display-fun
                 game-play)
	(require "general.scm")

;;
;; Procedure
;;   game?
;;
;; Purpose
;;   Test whether a value is a game
;;
;; Parameters
;;   value, any value
;;
;; Produces
;;   is-game, a boolean
;;
;; Preconditions
;;   [No additional.]
;;
;; Postconditions
;;   is-game is #t if value is a game created by procedure make-game, and likely
;;   to be #f otherwise.
(define game?
  (lambda (value)
    (and (list? value)
	 (= 6 (length value))
	 (equal? (car value) 'game)
	 (procedure? (cadr value))
	 (procedure? (caddr value))
	 (procedure? (cadddr value))
	 (procedure? (list-ref value 5)))))
;;
;; Procedure
;;   make-game
;;
;; Purpose
;;   Create a game
;;
;; Parameters
;;   successors-fun, a procedure
;;   terminal?, a procedure
;;   win?, a procedure
;;   start-state, a value
;;   display-fun, a procedure
;;
;; Produces
;;   game, a game
;;
;; Preconditions
;;   successors-fun is a procedure that takes a state and boolean (indicating 
;;      play by player 1) and produces a list, where each entry is a pair
;;      whose car is an action and whose cdr is a resulting state
;;   terminal? is a procedure that takes a state and returns #f if the state 
;;      is not terminal
;;   win? is a procedure that takes a player (boolean) and a state for which
;;       (terminal? state) is true and produces a boolean if the named player 
;;       has won
;;   start-state may be passed to successors-fun and terminal?
;;   display-fun is a procedure that takes a state and displays a 
;;      graphical representation of it
;;
;; Postconditions
;;   game is a valid game type consisting of the constituent values provided
(define make-game
  (lambda (successors-fun terminal? win? start-state display-fun)
    (list 'game successors-fun terminal? win? start-state display-fun)))


(define game-check
  (lambda (source-proc op)
    (lambda (game)
      (if (not (game? game))
	  (error (string-append source-proc ": Expected a game, given ") game)
	  (op game)))))


;;
;; Procedure
;;   game-successors-fun
;;
;; Purpose
;;   Get the successors generator for a game
;;
;; Parameters
;;   game, a game
;;
;; Produces
;;   successors-fun, a procedure
;;
;; Preconditions
;;   [None]
;;
;; Postconditions
;;   successors-fun is a procedure that takes a state and boolean (indicating 
;;      play by player 1) and produces a list, where each entry is a pair
;;      whose car is an action and whose cdr is a resulting state
(define game-successors-fun (game-check "game-successors-fun" cadr))


;;
;; Procedure
;;   game-terminal?
;;
;; Purpose
;;   Get the terminal test procedure for a game
;;
;; Parameters
;;   game, a game
;;
;; Produces
;;   terminal?, a procedure
;;
;; Preconditions
;;   [None]
;;
;; Postconditions
;;   terminal? is a procedure that takes a state and returns #f if the state 
;;      is not terminal
(define game-terminal? (game-check "game-terminal?" (r-s list-ref 2)))



;;
;; Procedure
;;   game-win?
;;
;; Purpose
;;   Get a function that determines whether a player has won a game
;;
;; Parameters
;;   game, a game
;;
;; Produces
;;   win?, a procedure
;;
;; Preconditions
;;   [None]
;;
;; Postconditions
;;   win? is a procedure that takes a player (boolean) and a state for which
;;       (terminal? state) is true and produces a boolean if the named player 
;;       has won
(define game-win?  (game-check "game-win?" (r-s list-ref 3)))

;;
;; Procedure
;;   game-start-state
;;
;; Purpose
;;   Get the starting state for a game
;;
;; Parameters
;;   game, a game
;;
;; Produces
;;   start-state, a value
;;
;; Preconditions
;;   [None]
;;
;; Postconditions
;;   start-state may be passed to the procedures returned by 
;;       (game-successors-fun game) and (game-terminal? game)
(define game-start-state (game-check "game-start-state" (r-s list-ref 4)))

;;
;; Procedure
;;   game-display-fun
;;
;; Purpose
;;   Get the state display procedure
;;
;; Parameters
;;   game, a game
;;
;; Produces
;;   display-fun, a procedure
;;
;; Preconditions
;;   [None]
;;
;; Postconditions
;;   display-fun is a procedure that takes a state and displays a 
;;      graphical representation of it
(define game-display-fun (game-check "game-display-fun" (r-s list-ref 5)))

;;
;; Procedure
;;   game-play
;;
;; Purpose
;;   Execute a game
;;
;; Parameters
;;   game
;;   player1, a procedure
;;   player2, a procedure
;;
;; Produces
;;   [Nothing]
;;
;; Preconditions
;;   player1 and player2 are procedures that take states for the game and 
;;      produce valid actions for that state
;;
;; Postconditions
;;   [None]
;;
;; Practica
;;   Player 1 always plays first
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
) ; module
