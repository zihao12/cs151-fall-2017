#lang typed/racket
(require typed/test-engine/racket-tests)
(require "../include/cs151-image.rkt")
(require "../include/cs151-core.rkt")
(require "../include/cs151-universe.rkt")

;; This game is called " Harry Potter: True or False"
;; You are given a set of questions and you should try answering them by typing "t" or "f"
;; For a question you answer correctly, you will get 2 point and you will move on to the next question
;; For a question you answer wrongly, you will lose 1 point and you will move on to the next question
;; You can also choose to skip that question by clicking the mouse, and you will ni get or lose any point for that question
;;Finally you will see your total score and time spent. Good luck and have fun!

(define-struct World
  ([No : Integer]
   [elapsed-time : Integer]
   [score : Integer]))



(define q1 "Fred and George were born on April Fool's Day.")
(define q2 "Remus Lupin's Patronus is a werewolf.")
(define q3 "Voldemort was 74 years old when he died in the Battle of Hogwarts.")
(define q4 "The third smell Hermione recognizes in the Amortentia potion is Ron's hair.")
(define question (list q1 q2 q3 q4))
(define answer (list "t" "f" "f" "t"))
(define x (length question))

(: draw : World -> Image)
;;draw the world that shows the number of questions remained, score, and time spent
(define (draw w)
  (match w
    [(World n t s)
     (overlay
      (above (text (list-ref question n) 30 "yellow")             
             (above (text (string-append "# of questions to go: " (number->string (- x n)))
                   56 "green")
             (above
              (text (string-append "Your score: " (number->string s))
                   56 "maroon")
              (text (string-append "Time spent:  " (number->string t))
                   56 "red"))))
      (square 1024  "solid" "purple"))]))

(: try : World String -> World)
;; see if the answer is correct and update the score and number of questions left
(define (try w k)
  (match* (w k)
    [((World n t s) k)
     ;(if (= (string->number k) (list-ref answer n))
     (if (string=? k (list-ref answer n))
         (World (add1 n) t (+ s 2))
         (World (add1 n) t (sub1 s)))]))

(: skip : World Integer Integer Mouse-Event -> World)
;; skip when the player clicks the mouse
(define (skip w x y e)
  (match w
    [(World n t s)
     (match e
       ["button-down" (World (add1 n) t s)]
       [_ w])]))

(: time : World -> World)
;;time the game
(define (time w)
  (match w
    [(World n t s)
     (if (= n x) w (World n (add1 t) s))]))

(: quit? : World -> Boolean)
;; see if the game is finished
(define (quit? w)
  (match w
    [(World n t s)
     (= n x)]))

(: run : -> World)
(define (run) 
(big-bang (World 0 0 0) : World
          [to-draw draw]
          [on-mouse skip]
          [on-key try]
          [on-tick time 1]
          [stop-when quit?]))
(test)