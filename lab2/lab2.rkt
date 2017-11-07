#lang typed/racket
(require "../include/cs151-image.rkt")
(require "../include/cs151-core.rkt")

(provide alt-shaded-rows)

(: num-text (Integer -> Image))
  ;; produce an image of the given number
  (define (num-text n)
    (text (number->string n) 14 "black"))

(: row-of-squares(Integer Integer -> Image))
;; generate a given number of squares with given side length
(define (row-of-squares n s)
  (cond
   [(<= n 0) empty-image]
   [ else (beside(square s "outline" "black") (row-of-squares (- n 1) s)) ]))


(: shaded-row (Integer Integer Image-Color -> Image))
;;generated row of squares like row-of-squares do, but with given color
(define (shaded-row n s c)
  (cond
   [(<= n 0) empty-image]
   [else (beside (overlay (square s "outline" "black" )
                          (square s "solid" c))(shaded-row (- n 1) s c))]))

(: alt-shaded-row (Integer Integer Image-Color Image-Color -> Image))
;;generate a row of alternating shadowed squares, with c1 being the coloor of the even-numbered square, c2 the color of the odd-numbered square.
(define (alt-shaded-row  n s c1 c2)
  (cond
    [(<= n 0) empty-image]
    [(= 0 (remainder n 2)) (beside (alt-shaded-row (- n 1) s c1 c2) (overlay (square s "outline" "black") (square s "solid" c2)) ) ]
    [else (beside (alt-shaded-row (- n 1) s c1 c2)(overlay (square s "outline" "black") (square s "solid" c1)) ) ]))

(: alt-shaded-rows (Integer Integer Integer Image-Color Image-Color -> Image))
;;generate rows of alternating shadowed squares
(define (alt-shaded-rows  n m s c1 c2)
  ( cond
     [(or (<= m 0) (<= n 0) ) empty-image]
     ;; this condition is crucial for a loop
     [(= 0 (remainder m 2))
      (above
       (alt-shaded-row n s c1 c2) (alt-shaded-rows n (- m 1) s c1 c2)
        )]
     [else 
      (above
       (alt-shaded-row n s c2 c1) (alt-shaded-rows n (- m 1) s c1 c2)
       )]
     ))

(: num-alt-shaded-row (Integer Integer Integer Image-Color Image-Color -> Image))
;;generate a row of given number alternating shaded squares, with number on each square, starting from the given number
(define (num-alt-shaded-row x n s c1 c2)
  (cond
    [(<= n 0) empty-image ]
    [ (= 1 (remainder n 2))
      (beside (overlay (num-text x) (overlay (square s "outline" "black") (square s "solid" c1)))
              (num-alt-shaded-row (+ x 1) (- n 1) s c1 c2)
       )]
    [else
     (beside (overlay (num-text x) (overlay (square s "outline" "black") (square s "solid" c2)))
              (num-alt-shaded-row (+ x 1) (- n 1) s c1 c2)
       )
     ]
    ))

(: num-alt-shaded-rows (Integer Integer Integer Integer Image-Color Image-Color -> Image))
;;generate rows of given number alternating shaded squares, with number on each square, starting from the given number
(define (num-alt-shaded-rows x m n s c1 c2)
  (cond
    [(or (<= m 0) (<= n 0) ) empty-image]
    [(= 1 (remainder m 2))
      (above (num-alt-shaded-row x n s c2 c1)
             (num-alt-shaded-rows (+ x n) (- m 1) n s c1 c2)
       )
     ]
    [else
      (above (num-alt-shaded-row x n s c1 c2)
             (num-alt-shaded-rows (+ x n) (- m 1) n s c1 c2)
       )
     ]
    ))

;s(num-alt-shaded-rows 0 8 8 36 "ivory" "maroon")
 



