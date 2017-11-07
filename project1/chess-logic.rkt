#lang typed/racket

(require typed/test-engine/racket-tests)

(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")

(require "optional.rkt")
(require "loc.rkt")

;; ==== ==== ==== ====
;; external interface

(provide PieceType
         Player
         (struct-out Piece)
         Square
         Board
         (struct-out Move)
         PromoteTo
         (struct-out ChessGame)
         ;; starting-board ; : Board
         ;; new-game       ; : ChessGame
         ;; board-ref      ; : Board Loc -> Square
         ;; board-update   ; : Board Loc Square -> Board
         ;; in-check?      ; : ChessGame -> Boolean
         ;; legal-move?    ; : ChessGame Move -> Boolean
         ;; moves-piece    ; : ChessGame Loc -> (Listof Move)
         ;; moves-player   ; : ChessGame -> (Listof Move)
         ;; checkmate?     ; : ChessGame -> Boolean
         ;; stalemate?     ; : ChessGame -> Boolean
         ;; apply-move     ; : ChessGame Move -> ChessGame
         ;; strings->board ; : (Listof String) -> Board
         )

;; ==== ==== ==== ====
;; data definitions

(define-type PieceType
  (U 'Pawn 'Bishop 'Knight 'Rook 'King 'Queen))

(define-type Player
  (U 'Black 'White))

(define-struct Piece
  ([type  : PieceType]
   [color : Player]))

(define-type Square
  (Optional Piece))

(define-type Board
  (Listof Square))

(define-type PromoteTo
  (U 'Queen 'Rook 'Bishop 'Knight))

(define-struct Move
  ([src        : Loc]
   [dst        : Loc]
   [moved      : Piece]
   [captured   : (Optional Piece)]
   [promote-to : (Optional PromoteTo)]))

(define-struct ChessGame
  ([board : Board]
   [history : (Listof Move)]))

;;name the piece as pieceij
;;i = 1 for white and i = 2 for black
;; j = 1,2,3,4,5,6 for King,Queen,Bishop,Knight,Rook,Pawn respectively 
(define piece11 (Piece 'King 'White ))
(define piece12 (Piece 'Queen 'White ))
(define piece13 (Piece 'Bishop 'White ))
(define piece14 (Piece 'Knight 'White ))
(define piece15 (Piece 'Rook 'White ))
(define piece16 (Piece 'Pawn 'White ))
(define piece21 (Piece 'King 'Black ))
(define piece22 (Piece 'Queen 'Black ))
(define piece23 (Piece 'Bishop 'Black ))
(define piece24 (Piece 'Knight 'Black ))
(define piece25 (Piece 'Rook 'Black ))
(define piece26 (Piece 'Pawn 'Black ))

(define testboard1 (list (Some piece11) (Some piece12) (Some piece13) 'None (Some piece14)))
(define testboard2 (list (Some piece11) (Some piece15) (Some piece13) 'None (Some piece14)))
(: file->number : File -> Integer )
;;turn File to numbers
(define (file->number f)
  (match f
    ['A 1] ['B 2] ['C 3] ['D 4] ['E 5] ['F 6] ['G 7] ['H 8]))


  

(: loc->boaref : Loc -> Integer)
;;turn location to the index in board
(define (loc->boaref loc)
         (match loc
           [(Loc f r) (sub1 (+  (* (sub1 r) 8) (file->number f)))]))

(: starting-board : Board)
;;This value represents the standard starting layout of a chess board,
;;with white at bottom (ranks 1 and 2).
(define starting-board
  (append (list (Some piece15) (Some piece14) (Some piece13) (Some piece12)
                (Some piece11) (Some piece13) (Some piece14) (Some piece15))
          (build-list 8
                      (λ ([n : Integer]) (Some piece16)))
          (build-list 32
                      (λ ([n : Integer]) 'None))
          (build-list 8
                      (λ ([n : Integer]) (Some piece26)))
          (list (Some piece25) (Some piece24) (Some piece23) (Some piece22)
                (Some piece21) (Some piece23) (Some piece24) (Some piece25))))

(: new-game : ChessGame)
;;This value contains a starting board and empty move history.
(define new-game (ChessGame starting-board '()))
  

(: board-ref : Board Loc -> Square)
;;Given a board and a location on it,
;;this function returnsthe contents of the specified square in the board.
(define (board-ref b loc)
     (list-ref b (loc->boaref loc)))
(check-expect (board-ref starting-board (Loc 'A 1)) (Some piece15))                        
(check-expect (board-ref starting-board (Loc 'A 8)) (Some piece25))
(check-expect (board-ref starting-board (Loc 'E 1)) (Some piece11))
(check-expect (board-ref starting-board (Loc 'A 4)) 'None)
(check-expect (board-ref starting-board (Loc 'F 2)) (Some piece16))

(: boardlist-update : Board Integer Square -> Board)
;;This function returns an updated board
;;where the contents of the specified(by index of the board) square are replaced with the given value.
(define (boardlist-update b n s)
  (match b
    [(cons f r)
     (cond
       [(= n 0) (cons s r)]
       [(> n 0) (cons f (boardlist-update r (sub1 n) s))]
       [else b])]))
(check-expect (boardlist-update testboard1 1 (Some piece15)) testboard2)


(: board-update : Board Loc Square -> Board)
;;This function returns an updated board
;;where the contents of the specified square are replaced with the given value.
(define (board-update b loc s)
  (boardlist-update b (loc->boaref loc) s))

;(: in-check? : ChessGame -> Boolean)
;;;This function returns true, if and only if the player whose turn it is is in check,
;;;according to the rules of chess and the current position of pieces on the board.
;;;do not, at this time, need to take castling, en passant, or promotion into account.
;(define (in-check? 








(test)
