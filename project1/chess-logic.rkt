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

(: file->number : File -> Integer)
;;turn File to numbers
(define (file->number f)
  (match f
    ['A 1] ['B 2] ['C 3] ['D 4] ['E 5] ['F 6] ['G 7] ['H 8]))

(: number->file : Integer -> File)
;;turn number to file
(define (number->file n)
  (match n
    [1 'A] [2 'B] [3 'C] [4 'D] [5 'E] [6 'F] [7 'G] [8 'H] [_ (error "too large for file")]))

(: number->rank : Integer -> Rank)
;;turn number to rank 
(define (number->rank n)
  (match n
    [1 1] [2 2] [3 3] [4 4] [5 5] [6 6] [7 7] [8 8] [_ (error "too large for rank")]))

(: loc->boaref : Loc -> Integer)
;;turn location to the index in board
(define (loc->boaref loc)
         (match loc
           [(Loc f r) (sub1 (+  (* (sub1 r) 8) (file->number f)))]))

(: boaref->loc : Integer -> Loc)
;;change position if the boardlist to Loc
(define (boaref->loc n )
  (Loc (number->file (add1 (remainder n 8)))
       (number->rank (add1 (exact-floor (/ n 8))))))
(check-expect (boaref->loc  63) (Loc 'H 8))
(check-expect (boaref->loc  7) (Loc 'H 1))
(check-expect (boaref->loc  8) (Loc 'A 2))
(check-expect (boaref->loc  0) (Loc 'A 1))

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
;;this function returns the contents of the specified square in the board.
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
;(define kingtest1 (board-update starting-board (Loc 'D 4) (Some (Piece 'King 'White))))

;; ----------;;;---------------------------
;;functions that decide if a piece is at the edge
(: right-edge? : Loc -> Boolean)
;; take in a location to see if it is the right edge of the board
(define (right-edge? loc)
  (= 7 (remainder  (loc->boaref loc) 8)))

(: left-edge? : Loc -> Boolean)
;; take in a location to see if it is the right edge of the board
(define (left-edge? loc)
  (= 0 (remainder  (loc->boaref loc) 8)))

(: upper-edge? : Loc -> Boolean)
;; take in a location to see if it is the right edge of the board
(define (upper-edge? loc)
  (>= (loc->boaref loc) 56))

(: lower-edge? : Loc -> Boolean)
;; take in a location to see if it is the right edge of the board
(define (lower-edge? loc)
  (<= (loc->boaref loc) 7))

(: rightorup-edge? : Loc -> Boolean)
;; take in a location to see if it is the right or upper edge of the board
(define (rightorup-edge? loc)
  (or (right-edge? loc) (upper-edge? loc)))

(: rightorlow-edge? : Loc -> Boolean)
;; take in a location to see if it is the right or upper edge of the board
(define (rightorlow-edge? loc)
  (or (right-edge? loc) (lower-edge? loc)))

(: leftorup-edge? : Loc -> Boolean)
;; take in a location to see if it is the right or upper edge of the board
(define (leftorup-edge? loc)
  (or (left-edge? loc) (upper-edge? loc)))

(: leftorlow-edge? : Loc -> Boolean)
;; take in a location to see if it is the right or upper edge of the board
(define (leftorlow-edge? loc)
  (or (left-edge? loc) (lower-edge? loc)))

;;below are the "edges" for knight where it cannot land another move at that specific position 
;; orders: 1 for upper-center-to-right-corner and count clockwise

(: kight-edge1? : Loc -> Boolean)
;; see if it can land on  kight-edge1
(define (kight-edge1? loc)
  (or (upper-edge? loc) (rightorup-edge? (boaref->loc (+ (loc->boaref loc) 8)))))

(: kight-edge2? : Loc -> Boolean)
;; see if it can land on  kight-edge2
(define (kight-edge2? loc)
  (or (right-edge? loc) (rightorup-edge? (boaref->loc (+ (loc->boaref loc) 1)))))

(: kight-edge3? : Loc -> Boolean)
;; see if it can land on  kight-edge1
(define (kight-edge3? loc)
  (or (right-edge? loc) (rightorlow-edge? (boaref->loc (+ (loc->boaref loc) 1)))))

(: kight-edge4? : Loc -> Boolean)
;; see if it can land on  kight-edge1
(define (kight-edge4? loc)
  (or (lower-edge? loc) (rightorlow-edge? (boaref->loc (- (loc->boaref loc) 8)))))

(: kight-edge5? : Loc -> Boolean)
;; see if it can land on  kight-edge1
(define (kight-edge5? loc)
  (or (lower-edge? loc) (leftorlow-edge? (boaref->loc (- (loc->boaref loc) 8)))))

(: kight-edge6? : Loc -> Boolean)
;; see if it can land on  kight-edge1
(define (kight-edge6? loc)
  (or (left-edge? loc) (leftorlow-edge? (boaref->loc (- (loc->boaref loc) 1)))))

(: kight-edge7? : Loc -> Boolean)
;; see if it can land on  kight-edge1
(define (kight-edge7? loc)
  (or (left-edge? loc) (leftorup-edge? (boaref->loc (- (loc->boaref loc) 1)))))

(: kight-edge8? : Loc -> Boolean)
;; see if it can land on  kight-edge1
(define (kight-edge8? loc)
  (or (upper-edge? loc) (leftorup-edge? (boaref->loc (+ (loc->boaref loc) 8)))))
  
;; ----------;;;---------------------------

(: one-step-for-some :  ChessGame Loc Player PieceType Integer (Loc -> Boolean) -> (Listof Move))
;; for a given piece in a given direction (Integer) for some type (King Rook Bishop Queen) except knight and Pawn
;; in one step, return a list of a single move if there is one; else 'None
(define (one-step-for-some g loc color type dir some-edge?)
  (match g
    [(ChessGame b _)
      (if (some-edge? loc)
          '()
          (match (list-ref b (+ (loc->boaref loc) dir))
            ['None (list
                    (Move loc
                          (boaref->loc (+ (loc->boaref loc) dir))
                          (Piece type color)
                          'None
                          'None))]
            [ (Some s)
              (match (val-of (Some s))
                [(Piece type2 color2)
                 (if (symbol=? color2 color)
                     '()
                     (list (Move loc (boaref->loc (+ (loc->boaref loc) dir))
                           (Piece type color) (Some(Piece type2 color2)) 'None)))])]))]))

(: steps-for-some :  ChessGame Loc Player PieceType Integer (Loc -> Boolean) -> (Listof Move))
;;;; for a given piece in a given direction (Integer) for some type (King Rook Bishop Queen) except knight and Pawn
;; in one step, return a list of steps if there is one; else 'None
(define (steps-for-some g loc color type dir some-edge?)
  (match g
    [(ChessGame b _)
     (append
      (one-step-for-some g loc color type dir some-edge?)
      (match (one-step-for-some g loc color type dir some-edge?)
        [(cons (Move _ _ _ 'None _)_)
         (steps-for-some g
                      (boaref->loc (+ (loc->boaref loc) dir))
                      color type dir some-edge?)]
         [_ '()]))]))
    

;; ----------;;;---------------------------
;; King!!! 
(: moves-king : ChessGame Loc Player -> (Listof Move))
;;given a piece that has the piecetype King, decide the moves it can take
(define (moves-king g loc color)
  (append
   (one-step-for-some g loc color 'King 8 upper-edge?)
   (one-step-for-some g loc color 'King -8 lower-edge?)
   (one-step-for-some g loc color 'King -1 left-edge?)
   (one-step-for-some g loc color 'King 1 right-edge?)
   (one-step-for-some g loc color 'King 7
                                    leftorup-edge?)
   (one-step-for-some g loc color 'King 9
                                    rightorup-edge?)
   (one-step-for-some g loc color 'King 7
                                    rightorlow-edge?)
   (one-step-for-some g loc color 'King -9
                                    leftorlow-edge?)))
      
(define kingtest1 (board-update starting-board (Loc 'D 4) (Some (Piece 'King 'White))))
(define kingtest2 (board-update kingtest1 (Loc 'D 5) (Some (Piece 'Queen 'Black))))
(define kingtest3 (board-update kingtest1 (Loc 'D 5) (Some (Piece 'Queen 'White))))
          
;; ----------;;;---------------------------
;; Rook!
(: moves-rook : ChessGame Loc Player -> (Listof Move))
;;given a piece that has the piecetype Rook, decide the moves it can take
(define (moves-rook g loc color)
  (append
   (steps-for-some g loc color 'Rook 8 upper-edge?)
   (steps-for-some g loc color 'Rook -8 lower-edge?)
   (steps-for-some g loc color 'Rook 1 right-edge?)
   (steps-for-some g loc color 'Rook -1 left-edge?)))
(define rooktest1 (board-update starting-board (Loc 'D 4) (Some (Piece 'Rook 'White))))

;; ----------;;;---------------------------
;; bishop
(: moves-bishop : ChessGame Loc Player -> (Listof Move))
;;given a piece that has the piecetype Rook, decide the moves it can take
(define (moves-bishop g loc color)
  (append
   (steps-for-some g loc color 'Bishop 7 leftorup-edge?)
   (steps-for-some g loc color 'Bishop -7 leftorlow-edge?)
   (steps-for-some g loc color 'Bishop 9 rightorup-edge?)
   (steps-for-some g loc color 'Bishop -9 rightorlow-edge?)))
(define bishoptest1 (board-update starting-board (Loc 'D 4) (Some (Piece 'Bishop 'White))))

;; ----------;;;---------------------------
;; queen
(: moves-queen : ChessGame Loc Player -> (Listof Move))
;;given a piece that has the piecetype Rook, decide the moves it can take
(define (moves-queen g loc color)
  (append
   (steps-for-some g loc color 'Queen 8 upper-edge?)
   (steps-for-some g loc color 'Queen -8 lower-edge?)
   (steps-for-some g loc color 'Queen 1 right-edge?)
   (steps-for-some g loc color 'Queen -1 left-edge?)
   (steps-for-some g loc color 'Queen 7 leftorup-edge?)
   (steps-for-some g loc color 'Queen -7 leftorlow-edge?)
   (steps-for-some g loc color 'Queen 9 rightorup-edge?)
   (steps-for-some g loc color 'Queen -9 rightorlow-edge?)))
(define queentest1 (board-update starting-board (Loc 'D 4) (Some (Piece 'Queen 'White))))

;; ----------;;;---------------------------
;; queen
(: moves-knight : ChessGame Loc Player -> (Listof Move))
;;given a piece that has the piecetype Rook, decide the moves it can take
(define (moves-knight g loc color)
  (append
   (one-step-for-some g loc color 'Knight 17 kight-edge1?)
   (one-step-for-some g loc color 'Knight 10 kight-edge2?)
   (one-step-for-some g loc color 'Knight -6 kight-edge3?)
   (one-step-for-some g loc color 'Knight -15 kight-edge4?)
   (one-step-for-some g loc color 'Knight -17 kight-edge5?)
   (one-step-for-some g loc color 'Knight -10 kight-edge6?)
   (one-step-for-some g loc color 'Knight 6 kight-edge7?)
   (one-step-for-some g loc color 'Knight 15 kight-edge8?)))
(define knighttest1 (board-update starting-board (Loc 'D 4) (Some (Piece 'Knight 'White))))
(define knighttest2 (board-update starting-board (Loc 'D 5) (Some (Piece 'Knight 'White))))

(test)
