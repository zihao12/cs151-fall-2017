#lang typed/racket
(require typed/test-engine/racket-tests)
(require "../include/cs151-image.rkt")
(require "../include/cs151-core.rkt")
(require "../include/cs151-universe.rkt")

(define-struct World
  ([play? : Boolean]
   [num-of-alp-left : Integer]
   [time-elapsed : Integer]
   [last-alp-typed : String]))

(define alphabet (list "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q"
                       "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
                       "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q"
                       "r" "s" "t" "u" "v" "w" "x" "y" "z"))

(define W1 (World #f 52 0 ""))
(define W2 (World #t 50 2 "B"))
(define W3 (World #t 49 2 "C"))

(: draw : World -> Image)
;; draw the initial world
(define (draw w)
  (match w
    [(World #f  _ _ _) (overlay (text "ARE YOU READY?" 16 "red")(square 256 "solid" "green"))]
    [(World _ n t k) (overlay
        (above
         (text (string-append "Last-typed : " k) 16 "blue")
         (above
          (text (string-append "Number of Characters Left : " (number->string n)) 16 "red")
          (text (string-append "Time-elapsed : " (number->string t)) 16 "red")))
        (square 256 "solid" "green"))]))

(: advance-by-key : World String -> World)
;;get input from the keyboard and advance the game
;; " " to get started
(define (advance-by-key w k)
  (match w 
    [(World #f n t c) (if (string=? k " ") (World #t n t c) w)]
    [(World _ n t c)
     (if (= n 0) w
         (if (string=? k (list-ref alphabet (- 52 n)))
             (World #t (sub1 n) t k)
             (World #t 52 t k)))]))
       
(check-expect (advance-by-key W2 "C") W3)

(: time : World -> World )
;; record the elapsed time
(define (time w)
  (match w
    [(World #f _ _ _) w]
    [(World #t n t k)
     (if (= n 0) w (World #t n (add1 t) k))]))

(: quit? : World -> Boolean)
;;quit the game after it finished
(define (quit? w)
  (match w
    [(World _ 0 _ _) #t]
    [_ #f]))


(: run : -> World)
(define (run) 
(big-bang (World #f 52 0 "") : World
          [to-draw draw]
          [on-key advance-by-key]
          [on-tick time 1]
          [stop-when quit?]))
(test)
          