#lang typed/racket
(require typed/test-engine/racket-tests)

(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")

(require "optional.rkt")
(require "loc.rkt")
(require "chess-logic.rkt")
(require "background.rkt")
;; ==== ==== ==== ====
;; external interface

;; (provide board->image ; : Board -> Image）

;♔♕♖♗♘♙♚♛♜♝♞♟
(define size-of-square 32)
(define size-of-board 256)
(define test-board (list (Some (Piece 'Pawn  'White)) 'None (Some (Piece  'Rook 'Black))
                         'None 'None 'None 'None 'None))
                         

  
(define background (alt-shaded-rows 8 8 56 "beige" "brown"))
;;'Pawn 'Bishop 'Knight 'Rook 'King 'Queen
(: square->image : Square -> Image)
;; turn piecetype to its corresponding image
(define (square->image s)
  (match s
    ['None empty-image]
    [_
     (match (val-of s)
       [ (Piece 'Pawn  'White) (text "♙" size-of-square "white")]
       [ (Piece  'Bishop 'White) (text "♗" size-of-square "white")]
       [ (Piece  'Knight 'White) (text "♘" size-of-square "white")]
       [ (Piece  'Rook 'White) (text "♖" size-of-square "white")]
       [ (Piece  'King 'White) (text "♔" size-of-square "white")]
       [ (Piece  'Queen'White) (text "♕" size-of-square "white")]
       [ (Piece  'Pawn 'Black) (text "♟" size-of-square "black")]
       [ (Piece  'Bishop 'Black) (text "♝" size-of-square "black")]
       [ (Piece  'Knight 'Black) (text "♞" size-of-square "black")]
       [ (Piece  'Rook 'Black) (text "♜" size-of-square "black")]
       [ (Piece  'King'Black) (text "♚" size-of-square "black")]
       [ (Piece  'Queen 'Black) (text "♛" size-of-square "black")])]))

;(: row-of-board : Board Integer Image-Color Image-Color -> Image)
;;; draw the k/8 th row of the board
;(define (row-of-board b k c1 c2)
;  (local
;    {(define i (+ k 8))}
;    (cond
;      [(= k i) empty-image]
;      [else (beside (overlay (square->image (list-ref b k))
;                     (square size-of-square "solid" c1))
;                    (row-of-board b (add1 k) c2 c1))])))
;; Stupid DrRacket said index too largs!!!!
                    
              
    
                           
;(: board->image : Board -> Image)
;;; draw a board with pieces
;(define (board->image b)
;  (match b
;    ['() background]
;    [ _ 