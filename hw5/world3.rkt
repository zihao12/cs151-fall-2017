#lang typed/racket
(require typed/test-engine/racket-tests)
(require "../include/cs151-image.rkt")
(require "../include/cs151-core.rkt")
(require "../include/cs151-universe.rkt")


(: smaller-than-one? : (Listof Integer) -> Boolean)
(define (smaller-than-one? xs)
  (match xs
    ['() #f]
    [(cons f r)
     (or (< f 1) (smaller-than-one? r))]))
(check-expect (smaller-than-one? (list 1 2 3 )) #f)
(check-expect (smaller-than-one? (list 0 2 3 )) #t)


(define-struct World
  ([list-of-radius : (Listof Integer)]
   [x : Integer]
   [y : Integer]))

(define W1 (World (list 1) 128 128))

(: draw : World -> Image)
;;draw the world
(define (draw w)
  (match w
    [(World l x y)
     (match l
       ['()(square 256 "solid" "green")]
       [(cons f r)
        (overlay/xy
         (circle f "outline" "blue")
         (* -1 (- x f))
         (* -1 (- y f))
         (draw (World r x y)))])]))

(: move-or-create : World Integer Integer Mouse-Event -> World)
;; use mouse to move and add circles
(define (move-or-create w x y e)
  (match* (w e)
    [((World l x0 y0)"button-down" )
     (World (append l (list 2))x0 y0)]
    [((World l _ _) "move" )
     (World l x y)]
    [(_ _) w]))

(: expand-or-shrink : World String -> World)
;;expand the radius of all circles by "+"
;;shrink the radius of all circles by "-"
(define (expand-or-shrink w k)
  (match* (w k)
    [((World l x y)"+") (World (map add1 l) x y)]
    [((World l x y)"-") (World (map sub1 l) x y)]
    [(_ _) w]))
  
(: radius-too-small? : World -> Boolean)
;;see if the radius of circles are smaller than one
;;if so, stop the game.
(define (radius-too-small? w)
  (match w
    [(World l _ _)
    (smaller-than-one? l)]))

(: run : -> World)
(define (run) 
(big-bang (World (list 2)128 128): World
          [to-draw draw]
          [on-mouse move-or-create]
          [on-key expand-or-shrink]
          [stop-when radius-too-small?]))

(test)