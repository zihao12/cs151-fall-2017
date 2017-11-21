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
   [sel : Integer] ; set it as -1 if not highlighted
   [dst : (Optional Integer)]
   [checkmate? : Boolean]
   [stalemate? : Boolean]))

(: new-chess-world : Integer -> ChessWorld)
;; create a new chessworld given the size of the square
(define (new-chess-world s)
  (ChessWorld s (ChessGame starting-board '()) -1 'None #f #f))

(: world-from-game : ChessGame Integer -> ChessWorld)
;; generate the chessworld from existing chess game
(define (world-from-game g s)
  (ChessWorld s g -1 'None #f #f))


(: draw-chess-world : ChessWorld -> Image)
;; draw out the chessworld
(define (draw-chess-world w)
  (match w
    [(ChessWorld s (ChessGame b _) sel _ _ _)
     (board->image+ s sel b)]))











;(big-bang (new-chess-game s) : World
;          [to-draw draw-chess-world]
;          [on-mouse handle-click]
;          [on-key handle-key]
;          [stop-when quit?])
          






















(test)