#lang typed/racket
(require typed/test-engine/racket-tests)
(require "../include/cs151-image.rkt")
(require "../include/cs151-core.rkt")
(require "../include/cs151-universe.rkt")

(define dogs
  (bitmap/file "4-dogs.jpg"))

(define-struct (Some A)
  ([value : A]))

(define-type (Optional A)
  (U 'None (Some A)))

(define-struct World
  ([running? : Boolean]
   [elapsed-tenths : Integer]
   [lap-time : (Optional Integer)]
   [quit? : Boolean]))

(: draw : World -> Image)
(define (draw w)
  (match w
    [(World r? e lap q?)
     (overlay
      (above (text (number->string e) 100 "gray")
             (match lap
               ['None (text "lap time" 48 "white")]
               [ (Some t) (text (string-append "lap: "
                                               (number->string t))
                                48 "white")]))
      (scale 2/3 dogs))]))


(: tick : World -> World )
(define (tick w)
  (match w
    [(World #f _ _ _) w]
    [(World #t e lap q?) (World #t (add1 e) lap q?)]))

( : key : World String -> World)
(define (key w k)
  (match w
    [(World r? e lap q?)
     (match k
       [ "\r" (World (not r?) e lap q?)]
       [ "q" (World r? e lap #t)]
       [ "r" (World #f 0 lap #f)]
       [ "l" (World r? e (Some e) q?)]
       [ _ w])]))
                      



            
(: run : Integer -> World)
(define (run start-time)
  (big-bang ( World #f start-time 'None #f) : World
            [to-draw draw]
            [name "Puppies!!"]
            [on-tick tick 1/10]
            [on-key key]
            [stop-when World-quit?]))























