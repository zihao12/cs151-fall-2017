#lang typed/racket


(require typed/test-engine/racket-tests)

(require "include/cs151-core.rkt")
(require "include/cs151-image.rkt")
(require "include/cs151-universe.rkt")

(define sky
  (bitmap/file "sunset-smaller.jpg"))

(define-struct (Some A)
  ([value : A]))

(define-type (Optional A)
  (U 'None (Some A)))

(define-struct World
  ([running? : Boolean]
   [elapsed-tenths : Integer]
   [lap-time : (Optional Integer)]
   [quit? : Boolean]))

(: show-tenths : Integer Byte Image-Color -> Image)
(define (show-tenths t font-size k)
  (text (string-append (number->string (quotient t 10))
                       "."
                       (number->string (remainder t 10)))
        font-size
        k))

(show-tenths 23 80 "gray")

(: draw : World -> Image)
(define (draw w)
  (match w
    [(World _ e lap _)
     (match lap
       ['None
        (overlay (show-tenths e 100 "gray")
                 sky)]
       [(Some t)
        (overlay
         (above (show-tenths e 100 "gray")
                (show-tenths t 48 "white"))
         sky)])]))

;(draw (World #t 10 'None #f))
;(draw (World #t 10 (Some 1) #f))

(: key : World String -> World)
; space " " to start/stop stopwatch
; "q" to quit
; "l" for lap time
(define (key w k)
  (match w
    [(World r? e lap q?)
     (match k
       [" " (World (not r?) e lap q?)]
       ["q" (World r? e lap #t)]
       ["l" (World r? e (Some e) q?)]
       [_ w])]))

(: tick : World -> World)
(define (tick w)
  (match w
    [(World r? e lap q?)
     (if r?
         (World r? (add1 e) lap q?)
         w)]))
            
;; tests would go here...

(: run : Integer -> World)
;; given start time in 1/10 seconds (typically 0)
(define (run start)
  (big-bang (World #f start 'None #f) : World
            [to-draw draw]
            [name "Lecture 1 Stopwatch!"]
            [on-key key]
            [on-tick tick 1/10]
            [stop-when World-quit?]))