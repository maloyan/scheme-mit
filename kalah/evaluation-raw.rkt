(module evaluation lang/plt-pretty-big
  (provide best-mancala-eval1
           best-mancala-eval1-beta
           
           )
  
  (require "mancala.scm")
  (require "mancala-player.scm")
  (require "general.scm")
  (require "game.scm")
  (require "cutoff-minimax.scm"); add any other modules you may need

        
  (define best-mancala-eval1
    (lambda (player)
      (lambda (state)
        (let* ([mancala-val-cur ((simple-mancala-eval player) state)]
               [one (first-half state)]
               [two (second-half state)]
               [stones-val  ((stones-diff player) one two)]
              ; [stones-val-end ((stones-diff-end-game player) one two)]
               [hole-list ((hole-diff player) one two)]
               [hole-val-pre (car hole-list)]
               [hole-val-end (cadr hole-list)]
               [hole-val-adj (flip-sign (caddr hole-list))]
               )
          (if (< mancala-val-cur 16)
              (+ (* 2 mancala-val-cur) stones-val hole-val-pre hole-val-adj)
              (+ (* 3 mancala-val-cur) (* 1.5 stones-val) hole-val-end ))))))

  (define best-mancala-eval1-beta
    (lambda (player)
      (lambda (state)
        (let* ([mancala-val ((simple-mancala-eval player) state)]
               [one (first-half state)]
               [two (second-half state)]
               [stones-val  ((stones-diff player) one two)]
               [hole-list ((hole-diff player) one two)]
               [hole-val-pre (car hole-list)]
               [hole-val-end (cadr hole-list)]
               )
          (+ (* 2 mancala-val) stones-val hole-val-pre)))))
   


  ;;; Procedure:
  ;;;   hole-index
  ;;; Parameters:
  ;;;   state, list representing the state of the the game
  ;;; Purpose:
  ;;;  returns the list of indicies containing 0 stones


  (define hole-index
    (lambda (board)
      (let helper ([board board]
                   [index 0])
        (cond
          ((null? board)
           '())
          ((zero? (car board))
           (cons index
                 (helper (cdr board) (add1 index))))
          (else
           (helper
            (cdr board) (add1 index)))))))

  ;;; Procedure:
  ;;;   first-half
  ;;; Parameters:
  ;;;   state, list representing the state of the the game
  ;;; Purpose:
  ;;;   split state into the first half containing only elements from player 1
  (define first-half
    (lambda (state)
      (let ((board (cdr state)))
        (let helper ([board board]
                     [index 0])
          (cond
            ((= index 6) '())
            (else
             (cons (car board) (helper
                                (cdr board) (add1 index)))))))))

  ;;; Procedure:
  ;;;   second-half
  ;;; Parameters:
  ;;;   state, list representing the state of the the game
  ;;; Purpose:
  ;;;   split state into the second half containing only elements from player 2
  
  (define second-half
    (lambda (state)
      (let ((board (cdr state)))
        (let ([with-mancala ((nest cdr 7) board)])
          (reverse (cdr (reverse with-mancala)))))))
        
      
  


  ;;; Procedure:
  ;;;   nest
  ;;; Parameters:
  ;;;  f, a unary procedure
  ;;;  n, a positive integer
  ;;; Purpose:
  ;;;    Return a function obtained from composing f with itself
  ;;;      n times
  ;;; Produces:
  ;;;   fun, a unary function
  ;;; Preconditions:
  ;;;   [No additional]
  ;;; Postcondition:
  ;;;   Let f^n(x) be the function obtained from composing f with itself
  ;;;    n times
  ;;;     fun(x) = f^n(x)


  (define nest
    (lambda (f n)
      (cond
        ((not (integer? n))
         (error "n must be a positive integer"))
        ((negative? n)
         (error "n cannot be a negative number"))
        ((zero? n)
         (error "n must be a positive integer"))
        (else
         (nest-helper f f n)))))

  ;;; Procedure:
  ;;;   nest-helper
  ;;; Parameters:
  ;;;  f, a unary procedure
  ;;;  f-cum, a unary procedure
  ;;;  n, a non-negative integer
  ;;; Purpose:
  ;;;    A helper procedure for nest that calls itself recursively until n is
  ;;;     zero; It passes f-cum as the cummulative function resuling
  ;;;      from composing f  
  ;;; Produces:
  ;;;   fun, a unary function
  ;;; Preconditions:
  ;;;   [No additional]
  ;;; Postcondition:
  ;;;   Let f^n(x) be the function obtained from composing f
  ;;;   with itself n times
  ;;;     fun(x) = f^n(x)


  (define nest-helper
    (lambda (f f-cum n)
      (cond
        ((zero? (sub1 n)) f-cum)
        (else
         (nest-helper f (lambda (x)
                          (f (f-cum x))) (sub1 n))))))




  (define flip-sign
    (lambda (value)
      (- 0 value)))
           
  ;;; Procedure:
  ;;;   stones-diff
  ;;; Parameters:
  ;;;   player, boolean indicating who's turn it is
  ;;; Purpose:
  ;;;  Produces a function that takes the first half and the second half of the
  ;;;  board and returns the difference of the stones

  (define stones-diff
    (lambda (player)
      (lambda (one two)
        (let* ([diff (- (apply + one)
                        (apply + two))])
          (if player
              diff
              (flip-sign diff))))))

  (define stones-diff-end-game
    (lambda (player)
      (lambda (one two)
        (let* ([diff (- (apply + (map stone-end-game one))
                        (apply + (map stone-end-game two)))])
          (if player
              diff
              (flip-sign diff))))))


  
  (define hole-diff
    (lambda (player)
      (lambda (one two)
        (cond (player
              (let* ([one-hole-index (hole-index one)]
                     [two-hole-index (hole-index two)]
                     [one-hole-len (length one-hole-index)]
                     [one-util (if (> one-hole-len 3)
                                   (- 3 one-hole-len)
                                   one-hole-len)]
                     [one-adj-list (map (lambda(num)
                                     (list-ref one (- 5 num)))
                                   two-hole-index)])
              
              (list (* 2 one-util) (* 2 (flip-sign one-hole-len))
                    (maximal one-adj-list))))
              (else
               (let* ([one-hole-index (hole-index one)]
                      [two-hole-index (hole-index two)]
                      [two-hole-len (length two-hole-index)]
                      [two-util (if (> two-hole-len 3)
                                    (- 3 two-hole-len)
                                    two-hole-len)]
                      [two-adj-list (map (lambda(num)
                                      (list-ref two (- 5 num)))
                                    one-hole-index)])
               
                      (list (* 2 two-util) (* 2 (flip-sign two-hole-len))
                           (maximal two-adj-list))))))))

  (define maximal
    (lambda (lst)
      (cond
       ((null? lst) 0)
       (else
       (apply max lst)))))


      
      (define stone-end-game
        (lambda (num)
          (if
           (> num 6)
           (+ num 2)
           num)))
       

      ) ; module
    