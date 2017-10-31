#lang typed/racket
(require typed/test-engine/racket-tests)
(require "../include/cs151-image.rkt")
(require "../include/cs151-core.rkt")
(require "../include/cs151-universe.rkt")

(define-struct World
  ([play? : Boolean]
   [num-of-ticks : Integer]
   [elapsed-time : Integer]))

(: draw : World -> Image)
;; draw the world
(define (draw w)
  (match w
    [(World #f  _ _)
     (overlay (text "ARE YOU READY? CLICK TO GET STARTED!" 12 "red") (square 256 "solid" "green"))]
    [(World _ n t)
     (overlay
      (above (text (string-append "Number of ticks: " (number->string n)) 16 "blue")
             (text (string-append "Time elapsed: " (number->string t)) 16 "red"))
      (square 256 "solid" "green"))]))

(: click : World Integer Integer Mouse-Event -> World)
;; use mouse to advance the world
;;first click to get started
;;then click to compete
(define (click w x y e)
  (match* (w e)
    [((World #f n t) "button-down") (World #t n t)]
     [((World #t n t) "button-down")
      (if (= n 20) w (World #t (add1 n) t))]
    [(_ _) w]))

(: time : World -> World)
;;record the time of the whole process
(define (time w)
  (match w
    [(World p? n t)
     (if (or (not p?) (= n 20)) w
         (World p? n (add1 t)))]))

(: quit? : World -> Boolean)
;;see if the game is finished
(define (quit? w)
  (match w
    [(World _ n _) (= n 20)]))

(: run : -> World)
(define (run) 
(big-bang (World #f 0 0) : World
            [to-draw draw]
            [on-mouse click ]
            [on-tick time 1]
            [stop-when quit?]))
(test)