(module mancala-player lang/plt-pretty-big
  (provide simple-mancala-eval
           simple-mancala-best-player1
           simple-mancala-best-player2)

  (require "mancala.scm" "game.scm" "alphabeta.scm")

  ;; 
  ;; Procedure
  ;;   simple-mancala-eval
  ;;
  ;; Purpose
  ;;   Create a procedure for evaluating a player's mancala state
  ;;
  ;; Parameters
  ;;   player, a boolean
  ;;
  ;; Produces
  ;;   eval-fun, a procedure
  ;;
  ;; Preconditions
  ;;   [None]
  ;;
  ;; Postconditions
  ;;   eval-fun takes a state and returns the number of stones in player's mancala
  (define simple-mancala-eval
    (lambda (player)
      (lambda (state)
        (let ((board (cdr state)))
          (list-ref board (if player 6 13))))))

  (define mancala (make-mancala-game)) 

  ;;Our players for the tournament       
  (define simple-mancala-best-player1
    (make-alpha-beta-player mancala 5 (simple-mancala-eval #t)))
  (define simple-mancala-best-player2
    (make-alpha-beta-player mancala 5 (simple-mancala-eval #f)))

  ) ; module
