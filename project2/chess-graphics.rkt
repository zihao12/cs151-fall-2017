#lang typed/racket
(require typed/test-engine/racket-tests)

(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")

(require "../project1/optional.rkt")
(require "../project1/loc.rkt")
(require "chess-logic.rkt")
;; ==== ==== ==== ====
;; external interface


(provide board->image ; : Integer Board -> Image 
         board->image+ ; : Integer Integer Board -> Image
         )

(define size-of-square 32)
(define sidelength 50)
 
;;'Pawn 'Bishop 'Knight 'Rook 'King 'Queen
(: square->image : Square -> Image)
;; turn piecetype to its corresponding image
(define (square->image s)
  (match s
    ['None (text "" size-of-square "white")]
    [_
     (match (val-of s)
       [ (Piece 'Pawn  'White) (text "♙" size-of-square "black")]
       [ (Piece  'Bishop 'White) (text "♗" size-of-square "black")]
       [ (Piece  'Knight 'White) (text "♘" size-of-square "black")]
       [ (Piece  'Rook 'White) (text "♖" size-of-square "black")]
       [ (Piece  'King 'White) (text "♔" size-of-square "black")]
       [ (Piece  'Queen'White) (text "♕" size-of-square "black")]
       [ (Piece  'Pawn 'Black) (text "♟" size-of-square "black")]
       [ (Piece  'Bishop 'Black) (text "♝" size-of-square "black")]
       [ (Piece  'Knight 'Black) (text "♞" size-of-square "black")]
       [ (Piece  'Rook 'Black) (text "♜" size-of-square "black")]
       [ (Piece  'King'Black) (text "♚" size-of-square "black")]
       [ (Piece  'Queen 'Black) (text "♛" size-of-square "black")])]))

(: take : All(A) Integer (Listof A) -> (Listof A))
;; take the first n elements of a given list
(define (take n xs)
  (match* (xs n)
    [('() _) '()]
    [(_ 0) '()]
    [((cons first rest) n)
     (append (list first) (take (sub1 n) rest))]))

(: drop : All(A) Integer (Listof A) -> (Listof A))
;; drop the first n elements of a given list
(define (drop n xs)
  (match* (xs n)
    [('() _) '()]
    [(_ 0) xs]
    [((cons _ rest) n)
     (drop (sub1 n) rest)]))

(: row-of-image : Integer Integer Board Image-Color Image-Color -> Image)
;; take in board(considered list of squares) and a number n
;;produce the image of first n elements in a row
(define (row-of-image sidelength n b c1 c2)
  (match (take n b)
    ['() empty-image]
    [(cons f r)
     (beside
      (overlay (square->image  f) (square sidelength "solid" c1))
      (row-of-image sidelength (sub1 n) r c2 c1))]))

(: board->image0 : Integer Board Image-Color Image-Color -> Image)
;; draw a board with pieces
(define (board->image0 sidelength b c1 c2)
   (match b
    ['() empty-image]
    [_
     (above 
      (board->image0 sidelength (drop 8 b) c1 c2)
      (if (even? (quotient (length b) 8))
          (row-of-image sidelength 8 b c1 c2)
          (row-of-image sidelength 8 b c2 c1)))]))

(: board->image : Integer Board -> Image)
;; draw a board with pieces
(define (board->image sidelength b)
  (board->image0 sidelength b  "brown" "beige"))

;(board->image 50 starting-board)



(: row-of-image+ : Integer Integer Integer Board Image-Color Image-Color -> Image)
;; take in the sidelength, number of squares, and an index, and board(considered list of squares) 
;;produce the image of first n elements in a row
;; if the index is smaller than n, highlight the corresponding square
(define (row-of-image+ sidelength n sel b c1 c2)
  (match (take n b)
    ['() empty-image]
    [(cons f r)
     (beside
      (overlay (square->image  f)
               (if (zero? sel) (square sidelength "solid" 'red)
                       (square sidelength "solid" c1)))
      (row-of-image+ sidelength (sub1 n) (sub1 sel) r c2 c1))]))


(: board->image+ : Integer Integer Board -> Image)
;; draw a board with pieces
;; highlight a specific square
(define (board->image+ sidelength sel b)
   (match b
    ['() empty-image]
    [_
     (above 
      (board->image+ sidelength  (- sel 8) (drop 8 b))
      (if (even? (quotient (length b) 8))
          (row-of-image+ sidelength 8 sel b "brown" "beige")
          (row-of-image+ sidelength 8 sel b "beige" "brown")))]))










  