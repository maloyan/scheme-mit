;; Lab: Adversarial Search
;; CS-261
;; File
;;   alphabeta.scm
;;
;; Authors
;;     Completed the implementations of alpha-beta-max-value and 
;;      alpha-beta-min-value. Everything else was written by Jerod Weinman
;;
;; Summary
;;   Implementation of minimax search using alpha-beta pruning
;;
;; Provides
;;   (make-alpha-beta-player game plies evaluation-fun)
;;   (alpha-beta-search game state plies evaluation-fun)
;;   (alpha-beta-max-value game state alpha beta depth plies eval-fun)
;;   (alpha-beta-min-value game state alpha beta depth plies eval-fun)


(module alphabeta lang/plt-pretty-big
  (provide make-alpha-beta-player
           alpha-beta-search
           alpha-beta-max-value
           alpha-beta-min-value)
  (require "game.scm")


  ;; Set DO-DISPLAY to #t to turn on intermediate displays of values
  ;; while running a alpha-beta search
  (define DO-DISPLAY #f)

  (define dprintf
    (lambda (format . args)
      (when DO-DISPLAY
        (apply printf (cons format args)))))


  
  ;;
  ;; Procedure
  ;;   make-alpha-beta-player
  ;;
  ;; Purpose
  ;;   Create a player using alpha-beta minimax search
  ;;
  ;; Parameters
  ;;   game, a game
  ;;   player, a boolean
  ;;   evaluation-fun, a procedure
  ;;
  ;; Produces
  ;;   play, a procedure
  ;;
  ;; Preconditions
  ;;   evaluation-fun takes a state of game and produces a number
  ;;
  ;; Postconditions
  ;;   play is a procedure that takes a state and produces a legal
  ;;   action for player in game
  (define make-alpha-beta-player
    (lambda (game plies evaluation-fun)
      (lambda (state)
        (alpha-beta-search game state plies evaluation-fun))))

  ;;
  ;; Procedure
  ;;   alpha-beta-search
  ;;
  ;; Purpose
  ;;   Return an action using alpha-beta search
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
  ;;   plies >= 0
  ;;   evaluation-fun takes a state of game and produces a number
  ;;
  ;; Postconditions
  ;;   action is a valid action from the state
  (define alpha-beta-search
    (lambda (game state plies evaluation-fun)
      (car (alpha-beta-max-value game state
                                 -inf.0 +inf.0 
                                 0 plies 
                                 evaluation-fun))))


    
  ;;
  ;; Procedure
  ;;   alpha-beta-max-value
  ;;
  ;; Purpose
  ;;   Generate the best action-value pair for a state
  ;;
  ;; Parameters
  ;;   game, a game
  ;;   state, a value
  ;;   alpha, a number
  ;;   beta, a number
  ;;   depth, a number
  ;;   plies, a number
  ;;   evaluation-fun, a procedure
  ;;
  ;; Produces
  ;;   action-value, a pair
  ;;
  ;; Preconditions
  ;;   0 <= depth < plies
  ;;   evaluation-fun takes a state of game and produces a number
  ;;
  ;; Postconditions
  ;;   (car action-value) is a valid, optimal action from the state
  ;;      (cdr action-value) is the value of the action
  ;From Jerod's code
  (define alpha-beta-max-value
    (lambda (game state alpha beta depth plies evaluation-fun)

      (dprintf "MAX state ~a~n" state)

      (cond 
        [(or (= depth plies)                 ; Maximum depth reached
             ((game-terminal? game) state))  ; We are given a terminal state

         ;; Some intermediate display material
         (if DO-DISPLAY ((game-display-fun game) state))
         (dprintf "MAX utility ~a~n" (evaluation-fun state))

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
                    [maxval null] ; maximal value
                    [loop-alpha alpha]) ; need an alpha to loop

           (if (or (null? successors)   ; No more moves
                   (not loop-alpha))     ;cut off  

               (if (null? maxval)   ; No successors at all! Error
                   (error "Game produced no successors from non-terminal state"
                          state)
                   (cons argmax maxval)) ; Return the best action value pair
            
               (let* ([action-value (alpha-beta-min-value 
                                     game 
                                     (cdar successors) ;  successor state
                                     loop-alpha
                                     beta
                                     depth             ; same depth (ply)
                                     plies             ; same cutoff
                                     evaluation-fun)]
                      [val (cdr action-value)]);value of the action-value pair
                 (cond
                   [(or (null? argmax) ; haven't calculated any values yet, or
                        (> val maxval))  ; this move's value is an improvement
                    (loop (cdr successors)   ; remaining possibilities
                          (caar successors)  ; new argmax
                          val                ; new maxval
                          (if (>= val beta)
                              #f
                              (max loop-alpha val)))]              
                   [else
                    (loop (cdr successors)   ; remaining possibilities
                          argmax             ; same argmax
                          maxval             ; same maxval
                          loop-alpha)]))))])))   
    
  ;;
  ;; Procedure
  ;;   alpha-beta-min-value
  ;;
  ;; Purpose
  ;;   Generate the worst action-value pair for a state
  ;;
  ;; Parameters
  ;;   game, a game
  ;;   state, a value
  ;;   alpha, a number
  ;;   beta, a number
  ;;   depth, a number
  ;;   plies, a number
  ;;   evaluation-fun, a procedure
  ;;
  ;; Produces
  ;;   action-value, a pair
  ;;
  ;; Preconditions
  ;;   0 <= depth < plies
  ;;   evaluation-fun that takes a state of game and produces a number
  ;;
  ;; Postconditions
  ;;   (car action-value) is a valid, optimal action from the state
  ;;   (cdr action-value) is the value of the action
  (define alpha-beta-min-value
    (lambda (game state alpha beta depth plies evaluation-fun)

      (dprintf "MIN state ~a~n" state)
    
      (cond
        [(or (= depth plies)                 ; Maximum depth reached
             ((game-terminal? game) state))  ; We are given a terminal state

         ;; Some intermediate display material
         (if DO-DISPLAY ((game-display-fun game) state))
         (dprintf "MIN utility ~a~n" (evaluation-fun state))

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
                    [minval null]  ; minimal value
                    [loop-beta beta]) ;set up a looping beta
		 
           (if (or (null? successors)   ; No more moves
                   (not loop-beta))     ;cut off

               (if (null? minval)   ; No successors at all! Error
                   (error "Game produced no successors from non-terminal state" 
                          state)
                   (cons argmin minval)) ; Return the best action value pair

               (let* ([action-value (alpha-beta-max-value 
                                     game 
                                     (cdar successors) ;  successor state
                                     alpha
                                     loop-beta
                                     (+ 1 depth)  ; increment depth (next ply)
                                     plies             ; same cutoff
                                     evaluation-fun)]
                      [val (cdr action-value)]) ;value of the action-value pair 
	      
                 (cond 
                   [(or (null? argmin) ; haven't calculated any values yet, or
                        (< val minval))  ; this move's value is an improvement
                    (loop (cdr successors)   ; remaining possibilities
                          (caar successors)  ; new argmin
                          val                ; new minval
                          (if (<= val alpha)
                              #f
                              (min loop-beta val)))]
                   [else
                    (loop (cdr successors)   ; remaining possibilities
                          argmin             ; same argmin
                          minval             ; same minval
                          loop-beta)]))))])))

  ) ; module
