;;
;; File
;;   cutoff-minimax.scm
;;
;; Author
;;   Jerod Weinman
;;
;; Summary
;;   Provides a collection of routines implementing a MiniMax adversarial search
;;   with a depth-cutoff
;;
;; Provides
;;   (make-cutoff-minimax-player game plies evaluation-fun)
;;   (cutoff-minimax-search game state plies utility)
;;   (cutoff-max-value game state utility)
;;   (cutoff-min-value game state utility)
(module cutoff-minimax lang/plt-pretty-big
	(provide make-cutoff-minimax-player
                 cutoff-minimax-search
                 cutoff-max-value
                 cutoff-min-value)
	(require "game.scm" "general.scm")

;;
;; Procedure
;;   make-cutoff-minimax-player
;;
;; Purpose
;;   Create a player using minimax search with cutoff
;;
;; Parameters
;;   game, a game
;;   plies, a number
;;   evaluation-fun, a procedure
;;
;; Produces
;;   play, a procedure
;;
;; Preconditions
;;   plies > 0
;;   evaluation-fun is a procedure that takes a state and produces a number
;;
;; Postconditions
;;   play is a procedure that takes a state and produces a legal action
;;   play conducts a minimax search to the depth given by plies before calling 
;;      evaluation-fun on the search fringe.
(define make-cutoff-minimax-player
  (lambda (game plies evaluation-fun)
    (lambda (state)
      (cutoff-minimax-search game state plies evaluation-fun))))


;;
;; Procedure
;;   cutoff-minimax-search
;;
;; Purpose
;;   Return an action using minimax search
;;
;; Parameters
;;   game, a game
;;   state, a value
;;   plies, a number
;;   evaluation-fun, a procedure
;;
;; Produces
;;   action, a value
;;
;; Preconditions
;;   evaluation-fun is a procedure that takes a state and produces a number
;;   plies > 0
;;
;; Postconditions
;;   action is a valid action from the state
;;   Conducts a minimax search to the depth given by plies before calling 
;;      evaluation-fun on the search fringe.
(define cutoff-minimax-search
  (lambda (game state plies evaluation-fun)
    (car (cutoff-max-value game state 0 plies evaluation-fun))))



;;
;; Procedure
;;   cutoff-max-value
;;
;; Purpose
;;   Generate the best action-value pair for a state
;;
;; Parameters
;;   game, a game
;;   state, a value
;;   depth, a number
;;   plies, a number
;;   evaluation-fun, a procedure
;;
;; Produces
;;   action-value, a pair
;;
;; Preconditions
;;   depth <= plies
;;   evaluation-fun is procedure that takes a state and produces a number
;;
;; Postconditions
;;   (car action-value) is a valid, optimal action from state
;;   (cdr action-value) is the value of the action
(define cutoff-max-value
  (lambda (game state depth plies evaluation-fun)

    (cond 
     [(or (= depth plies)                 ; Maximum depth reached
	  ((game-terminal? game) state))  ; We are given a terminal state

      ;; Since this state is terminal or cutoff , there is no action to take
      ;; We return the value as the evaluation of the state
      (cons null (evaluation-fun state))] ; action-value pair
     
     [else
      ;; In this case, we do not have a terminal state, therefore
      ;; successors ought not be empty
  
      ;; Loop to find find maximimum value and the action giving it
      
      (let loop ([successors ((game-successors-fun game) state)]
		 ; successors is a list of (action . state) pairs
		 [argmax null]  ; maximizing action
		 [maxval null]) ; maximal value

	(if (null? successors)   ; No more moves

	    (if (null? maxval)   ; No successors at all! Error
		(error "Game produced no successors from non-terminal state" 
		       state)
		(cons argmax maxval)) ; Return the best action value pair
            
	    (let* ([action-value (cutoff-min-value 
				  game 
				  (cdar successors) ;  successor state
				  depth             ; same depth (ply)
				  plies             ; same cutoff
				  evaluation-fun)]
		   [val (cdr action-value)]) ; value of the action-value pair
	      (cond 
	       [(or (null? argmax)     ; haven't calculated any values yet, or
		    (> val maxval))    ; this move's value is an improvement
		(loop (cdr successors)   ; remaining possibilities
		      (caar successors)  ; new argmax
		      val)]              ; new maxval
	       [else
		(loop (cdr successors)   ; remaining possibilities
		      argmax             ; same argmax
		      maxval)]))))])))   ; same maxval


;;
;; Procedure
;;   cutoff-min-value
;;
;; Purpose
;;   Generate the worst action-value pair for a state
;;
;; Parameters
;;   game, a game
;;   state, a value
;;   depth, a number
;;   plies, a number
;;   evaluation-fun, a procedure
;;
;; Produces
;;   action-value, a pair
;;
;; Preconditions
;;   depth <= plies
;;   evaluation-fun is procedure that takes a state and produces a number
;;
;; Postconditions
;;   (car action-value) is a valid, optimal action from the state
;;   (cdr action-value) is the value of the action

(define cutoff-min-value
  (lambda (game state depth plies evaluation-fun)
    
    (cond
     [(or (= depth plies)                 ; Maximum depth reached
	  ((game-terminal? game) state))  ; We are given a terminal state

      ;; Since this state is terminal, there is no action to take
      ;; We return the value as the utility of the state
      (cons null (evaluation-fun state))] ; action-value pair
     
     [else
      ;; In this case, we do not have a terminal state, therefore
      ;; successors ought not be empty
      
      ;; Loop to find find minimum value and the action giving it
      
      (let loop ([successors ((game-successors-fun game) state)]
			     ; successors is a list of (action . state) pairs
		 [argmin null]  ; minimizing action
		 [minval null]) ; minimal value
		 
	(if (null? successors)   ; No more moves

	    (if (null? minval)   ; No successors at all! Error
		(error "Game produced no successors from non-terminal state" 
		       state)
                (cons argmin minval)) ; Return the best action value pair

	    (let* ([action-value (cutoff-max-value 
				  game 
				  (cdar successors) ; next successor state
				  (+ 1 depth)       ; increment depth (next ply)
				  plies             ; same cutoff
				  evaluation-fun)]
		   [val (cdr action-value)]) ; value of the action-value pair
	      
	      (cond 
	       [(or (null? argmin)     ; haven't calculated any values yet, or
		    (< val minval))    ; this move's value is an improvement
		(loop (cdr successors)   ; remaining possibilities
		      (caar successors)  ; new argmin
		      val)]              ; new minval
	       [else
		(loop (cdr successors)   ; remaining possibilities
		      argmin             ; same argmin
		      minval)]))))])))   ; same minval
) ; module
