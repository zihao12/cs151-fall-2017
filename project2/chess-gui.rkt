#lang typed/racket
(require typed/test-engine/racket-tests)

(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require "../include/cs151-universe.rkt")

(require "../project1/optional.rkt")
(require "../project1/loc.rkt")
(require "chess-graphics.rkt")
(require "chess-logic.rkt")
;; === === ===
;; external links

(define-struct ChessWorld
  ([siz : Integer]
   [gam : ChessGame]
   [sel : (Optional Integer)]
   [pro : (Optional Move)]))
   ;[dst : (Optional Integer)]
   ;[checkmate? : Boolean]
   ;[stalemate? : Boolean]
  

(: new-chess-world : Integer -> ChessWorld)
;; create a new chessworld given the size of the square
(define (new-chess-world s)
  (ChessWorld s (ChessGame starting-board '()) 'None 'None))

(: world-from-game : ChessGame Integer -> ChessWorld)
;; generate the chessworld from existing chess game
(define (world-from-game g s)
  (ChessWorld s g 'None 'None))


(: draw-chess-world : ChessWorld -> Image)
;; draw out the chessworld
(define (draw-chess-world w)
  (match w
    [(ChessWorld s (ChessGame b _) (Some n) 'None)
     (board->image+ s n b)]
    [(ChessWorld s (ChessGame b _) (Some n) (Some pro))
     (above (board->image+ s n b)
            (text "CHOOSE YOUR PROMOTETO TYPE: PRESS q,b,n OR r" 20 'blue))]
    [(ChessWorld s (ChessGame b _) 'None _)
     (board->image+ s -1 b)]))
  ;;geshiyouwenti  
    

(: mouse->boaref : Integer Integer Integer -> (Optional Integer))
;; turn the location of the mouse into boaref, given the sidelength of a square
;; return 'None if the mouth is not on board
(define (mouse->boaref x y s)
  (cond
    [(or (> x (* s 8)) (> y (* s 8))) 'None]
    [else (Some (+ (* (- 7 (quotient y s)) 8) (quotient x s) ))]))

(: handle-click : ChessWorld Integer Integer Mouse-Event -> ChessWorld)
;; 1st click, highlight the chosen square (if clicked outside the board, do not change the world)
;; 2nd click, if on the highlighted square, de-highlight it;
;; 2nd click, if on a valid destination, find out its corresponding move and apply it to ChessGame, and dehighlight the chosen one
;; 2nd click, if on an invalid destination, do not change anything in the world
(define (handle-click w x y e)
  (local {(define hit (mouse->boaref x y (ChessWorld-siz w)))}
    (match e
      ["button-down" 
       (match w
         [(ChessWorld s g 'None _)
          (match hit
            ['None w]
            [(Some n) (ChessWorld s g (Some n) 'None)])]
         [(ChessWorld s g (Some selected) _)
          (match hit
            ['None w]
            [(Some n)
             (if (= n selected) (ChessWorld s g 'None 'None)
                 (match
                     (filter
                      (λ ([mv : Move]) (and (loc=? (Move-src mv) (boaref->loc selected))
                                             (loc=? (Move-dst mv) (boaref->loc (val-of hit)))))
                      (moves-player g))
                   ['() w]
                   [(cons f '()) (ChessWorld s (apply-move g f) 'None 'None)]
                   [(cons f _)(ChessWorld s g hit (Some f))]))])])]
      [_ w])))

( : handle-key : ChessWorld String -> ChessWorld)
;; let user choose his promotion type through keyboard
(define (handle-key w str)
  (match w
    [(ChessWorld s g hit (Some (Move src dst mvd cap promote)))
     (match str
       ["q" (ChessWorld s (apply-move g (Move src dst mvd cap (Some 'Queen))) 'None 'None)]
       ["b" (ChessWorld s (apply-move g (Move src dst mvd cap (Some 'Bishop))) 'None 'None)]
       ["n" (ChessWorld s (apply-move g (Move src dst mvd cap (Some 'Knight))) 'None 'None)]
       ["r" (ChessWorld s (apply-move g (Move src dst mvd cap (Some 'Rook))) 'None 'None)]
       [_ w])]
    [_ w]))
                    
       









(big-bang (new-chess-world 50) : ChessWorld
         [to-draw draw-chess-world]
         [on-mouse handle-click]
          [on-key handle-key])
;          [stop-when quit?])
          






















(test)