#lang typed/racket
(require "../include/cs151-image.rkt")
(require "../include/cs151-core.rkt")

;; ==== ==== ==== ====
;; external interface
(provide  alt-shaded-row
          alt-shaded-rows)

;; ==== ==== ==== ====
;; operations
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