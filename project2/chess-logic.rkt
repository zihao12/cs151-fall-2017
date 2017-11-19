#lang typed/racket

(require typed/test-engine/racket-tests)

(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")

(require "../project1/optional.rkt")
(require "../project1/loc.rkt")

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
          starting-board ; : Board
          new-game       ; : ChessGame
          board-ref      ; : Board Loc -> Square
          board-update   ; : Board Loc Square -> Board
          in-check?      ; : ChessGame -> Boolean
          legal-move?    ; : ChessGame Move -> Boolean
          moves-piece    ; : ChessGame Loc -> (Listof Move)
          moves-player   ; : ChessGame -> (Listof Move)
          checkmate?     ; : ChessGame -> Boolean
          stalemate?     ; : ChessGame -> Boolean
          apply-move     ; : ChessGame Move -> ChessGame
          strings->board ; : (Listof String) -> Board
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

(define whitemove1 (list (Move (Loc 'F 2) (Loc 'F 3) (Piece 'Pawn 'White) 'None 'None)))
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
;; ----------;;;---------------------------
 ;;board-ref 
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

;; ----------;;;---------------------------
;;board-update
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

;; ----------;;;---------------------------
;;strings->board

(: strings->board : (Listof String) -> Board)
;;write down the board in strings
;;Each character is either a hyphen (-)(an empty space), or a letter(piece)
;;PRNBQK for Black pieces and prnbqk for white pieces; n for knight and k for king 
(define (strings->board strs)
  (cond
    [(and (= (length strs) 8)
          (andmap (λ ([s : String]) (= 8 (string-length s))) strs))
     (map
      (λ ([s : Char])
        (match s
          [#\P (Some (Piece 'Pawn 'Black))]
          [#\R (Some (Piece 'Rook 'Black))]
          [#\N (Some (Piece 'Knight 'Black))]
          [#\B (Some (Piece 'Bishop 'Black))]
          [#\Q (Some (Piece 'Queen 'Black))]
          [#\K (Some (Piece 'King 'Black))]
          [#\p (Some (Piece 'Pawn 'White))]
          [#\r (Some (Piece 'Rook 'White))]
          [#\n (Some (Piece 'Knight 'White))]
          [#\b (Some (Piece 'Bishop 'White))]
          [#\q (Some (Piece 'Queen 'White))]
          [#\k (Some (Piece 'King 'White))]
          [#\- 'None]))
       (string->list
        (foldl
         string-append
         ""
         strs)))]
    [else (error "wrong board")]))
       
(define teststrings
   (list "RKN--P--"
         "RKQNK---"
         "----PPP-"
         "RKN--P--"
         "prkq----"
         "pkqn----"
         "--knbk--"
         "-k-p-b-r"))
(check-expect (list-ref (strings->board teststrings) 0) 'None)
(check-expect (list-ref (strings->board teststrings) 1) (Some (Piece 'King 'White)))
(check-expect (list-ref (strings->board teststrings) 11) (Some (Piece 'Knight 'White)))
(check-expect (list-ref (strings->board teststrings) 42) 'None)
(check-expect (list-ref (strings->board teststrings) 34) (Some (Piece 'Knight 'Black)))

;; ----------;;;---------------------------
(: opposite-color : Player -> Player)
;; show the opposite color of  a given player
(define (opposite-color color)
  (if (symbol=? color 'White) 'Black 'White))
(check-expect (opposite-color 'Black) 'White)
(check-expect (opposite-color 'White) 'Black)

(: whose-turn : (Listof Move) -> Player)
;;see whose turn it is from history
(define (whose-turn moves)
  (match moves
    ['() 'White]
    [_
     (match (last moves)
       [(Move _ _ (Piece _ color) _ _)
        (if (symbol=? color 'White) 'Black 'White)])]))
(check-expect (whose-turn '()) 'White)
(check-expect (whose-turn (list (Move (Loc 'D 3) (Loc 'D 4) (Piece 'Pawn 'White)'None 'None)
                                (Move (Loc 'D 7) (Loc 'D 6) (Piece 'Pawn 'Black) 'None 'None))) 'White)
(check-expect (whose-turn (list (Move (Loc 'D 3) (Loc 'D 4) (Piece 'Pawn 'White)'None 'None)
                                )) 'Black)

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
;; take steps of various direction and length
(: one-step-for-some :  Board Loc Player PieceType Integer (Loc -> Boolean) -> (Listof Move))
;; for a given piece in a given direction (Integer) for some type (King Rook Bishop Queen) except knight and Pawn
;; in one step, return a list of a single move if there is one; else 'None
(define (one-step-for-some b loc color type dir some-edge?)
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
                           (Piece type color) (Some(Piece type2 color2)) 'None)))])])))

(: steps-for-some-problematic :  Board Loc Player PieceType Integer (Loc -> Boolean) -> (Listof Move))
;; PROBLEMATIC for a given piece in a given direction (Integer) for some type (King Rook Bishop Queen) except knight and Pawn
;; in one step, return a list of steps if there is one; else 'None
;; it is problematic because the moves may not start from its original position
(define (steps-for-some-problematic b loc color type dir some-edge?)       
         (append
          (one-step-for-some b loc color type dir some-edge?)
          (match (one-step-for-some b loc color type dir some-edge?)
            [(cons (Move _ _ _ 'None _)_)
             (steps-for-some b
                             (boaref->loc (+ (loc->boaref loc) dir))
                             color type dir some-edge?)]     
            [_ '()])))

(: steps-for-some :  Board Loc Player PieceType Integer (Loc -> Boolean) -> (Listof Move))
;;;; for a given piece in a given direction (Integer) for some type (King Rook Bishop Queen) except knight and Pawn
;; in one step, return a list of steps if there is one; else 'None
(define (steps-for-some b loc color type dir some-edge?)    
  (map
  (λ ([mv : Move]) (Move loc (Move-dst mv) (Move-moved mv) (Move-captured mv) (Move-promote-to mv)))
  (steps-for-some-problematic b loc color type dir some-edge?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for the following functions that output a list of moves
;; I performed the eyeball test: i define teh test board and print the answers out and see if they are correct as
;;it is too time-consuming to write all down
;; ----------;;;---------------------------
;; King!!! 
(: moves-king : Board Loc Player -> (Listof Move))
;;given a piece that has the piecetype King, decide the moves it can take
;; this is incomplete due to castling
(define (moves-king b loc color)
  (append
   (one-step-for-some b loc color 'King 8 upper-edge?)
   (one-step-for-some b loc color 'King -8 lower-edge?)
   (one-step-for-some b loc color 'King -1 left-edge?)
   (one-step-for-some b loc color 'King 1 right-edge?)
   (one-step-for-some b loc color 'King 7
                      leftorup-edge?)
   (one-step-for-some b loc color 'King 9
                      rightorup-edge?)
   (one-step-for-some b loc color 'King -7
                      rightorlow-edge?)
   (one-step-for-some b loc color 'King -9
                      leftorlow-edge?)))
 (define kingtest1 (board-update starting-board (Loc 'D 4) (Some (Piece 'King 'White))))
(define kingtest2 (board-update kingtest1 (Loc 'D 5) (Some (Piece 'Queen 'Black))))
(define kingtest3 (board-update kingtest1 (Loc 'D 5) (Some (Piece 'Queen 'White))))     
;; ----------;;;---------------------------
;; Rook!
(: moves-rook : Board Loc Player -> (Listof Move))
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
(: moves-bishop : Board Loc Player -> (Listof Move))
;;given a piece that has the piecetype Rook, decide the moves it can take
(define (moves-bishop b loc color)
  (append
   (steps-for-some b loc color 'Bishop 7 leftorup-edge?)
   (steps-for-some b loc color 'Bishop -7 leftorlow-edge?)
   (steps-for-some b loc color 'Bishop 9 rightorup-edge?)
   (steps-for-some b loc color 'Bishop -9 rightorlow-edge?)))
(define bishoptest1 (board-update starting-board (Loc 'D 4) (Some (Piece 'Bishop 'White))))

;; ----------;;;---------------------------
;; queen
(: moves-queen : Board Loc Player -> (Listof Move))
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
(define queentest2 (board-update starting-board (Loc 'E 8) (Some (Piece 'Queen 'Black))))
;; ----------;;;---------------------------
;; knight
(: moves-knight : Board Loc Player -> (Listof Move))
;;given a piece that has the piecetype Knight, decide the moves it can take
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
(check-expect (length (moves-knight knighttest1 (Loc 'D 4) 'White)) 6)
(check-expect (length (moves-knight knighttest2 (Loc 'D 4) 'White)) 6)
;; ----------;;;---------------------------
;; moves-pawn
(: moves-pawn00 : Board Loc Player -> (Listof Move))
;;given a piece that has the piecetype Knight, decide the moves it can take
(define (moves-pawn00 b loc color)
  (if (symbol=? color 'White)
       (if (< (loc->boaref loc)56)
           (local
             {(define mvup
             (match (list-ref b (+ (loc->boaref loc) 8))
             ['None (list (Move loc (boaref->loc (+ (loc->boaref loc) 8)) (Piece 'Pawn 'White) 'None 'None))]
             [_ '()]))}
             (append
              mvup
              (if (or (empty? mvup) (not (= (Loc-rank loc) 2)) )
                  '()
                  (match (list-ref b (+ (loc->boaref loc) 16))
                    ['None
                     (list (Move loc (boaref->loc (+ (loc->boaref loc) 16)) (Piece 'Pawn 'White) 'None 'None))]
                    [_ '()]))
              (if (left-edge? loc) '()
                  (match (list-ref b (+ (loc->boaref loc) 7))
                    [(Some (Piece type 'Black))
                     (list (Move loc (boaref->loc (+ (loc->boaref loc) 7))
                           (Piece 'Pawn 'White) (Some(Piece type 'Black)) 'None))]
                    [_ '()]))
              (if (right-edge? loc) '()
                  (match (list-ref b (+ (loc->boaref loc) 9))
                    [(Some (Piece type 'Black))
                     (list (Move loc (boaref->loc (+ (loc->boaref loc) 9))
                           (Piece 'Pawn 'White) (Some(Piece type 'Black)) 'None))]
                    [_ '()]))))
           '())
       (if (> (loc->boaref loc)7)
           (local
             {(define mvup
             (match (list-ref b (- (loc->boaref loc) 8))
             ['None (list (Move loc (boaref->loc (- (loc->boaref loc) 8)) (Piece 'Pawn 'Black) 'None 'None))]
             [_ '()]))}
             (append mvup
                     (if (or (empty? mvup) (not (= (Loc-rank loc) 7)) )
                     '()
                     (match (list-ref b (- (loc->boaref loc) 16))
                       ['None
                        (list (Move loc (boaref->loc (- (loc->boaref loc) 16)) (Piece 'Pawn 'Black) 'None 'None))]
                       [_ '()]))
                     (if (left-edge? loc) '()
                         (match (list-ref b (- (loc->boaref loc) 9))
                           [(Some (Piece type 'White))
                            (list (Move loc (boaref->loc (- (loc->boaref loc) 9))
                                  (Piece 'Pawn 'Black) (Some (Piece type 'White)) 'None))]
                           [_ '()]))
                     (if (right-edge? loc) '()
                         (match (list-ref b (- (loc->boaref loc) 7))
                           [(Some (Piece type 'White))
                            (list (Move loc (boaref->loc (- (loc->boaref loc) 7))
                                  (Piece 'Pawn 'Black) (Some (Piece type 'White)) 'None))]
                           [_ '()]))))
           '())))

(: moves-pawn : Board Loc Player -> (Listof Move))
;; add the rule of promotion to moves-pawn00
(define (moves-pawn b loc color)
  (foldl
   (λ ([mv : Move] [mvs : (Listof Move)])
     (match mv
       [(Move s (Loc _ r) (Piece _ color) _ 'None)
        (if (or (and (= r 8) (symbol=? color 'White)) (and (= r 1) (symbol=? color 'Black)))
            (append (list 
                     (Move s (Move-dst mv) (Move-moved mv) (Move-captured mv) (Some 'Queen))
                     (Move s (Move-dst mv) (Move-moved mv) (Move-captured mv) (Some 'Knight))
                     (Move s (Move-dst mv) (Move-moved mv) (Move-captured mv) (Some 'Rook))
                     (Move s (Move-dst mv) (Move-moved mv) (Move-captured mv) (Some 'Bishop))) mvs)
            (append (list mv) mvs))]))
   '()
   (moves-pawn00 b loc color)))
         
(define promotepawn1 (strings->board
                    (list "---P-B--"
                          "----p---"
                          "--------"
                          "--------"
                          "--------"
                          "--------"
                          "--------"
                          "--------")))         
(check-expect (length (moves-pawn promotepawn1 (Loc 'E 7) 'White)) 12)
;;the 12 results are too lengthy to paste here; i have checked them myself


(: en-passant :  ChessGame Loc -> (Listof Move))
;; implement the additional rule for en-passant
(define (en-passant g loc)
  (match g
    [(ChessGame b hist)
     (match hist
       ['() '()]
       [_ 
        (match (last hist)
          [(Move src0 dst0 moved0 _ _)
           (match moved0
             [(Piece 'Pawn 'Black)
              (if (= -16 (- (loc->boaref dst0)(loc->boaref src0)))
                  (if (or (= (loc->boaref dst0) (add1 (loc->boaref loc)))
                          (= (loc->boaref dst0) (sub1 (loc->boaref loc))))
                      (list (Move loc (boaref->loc (+ (loc->boaref dst0) 8)) (Piece 'Pawn 'White)
                                  (Some (Piece 'Pawn 'Black)) 'None))
                      '())'())]
             [(Piece 'Pawn 'White)
              (if (= 16 (- (loc->boaref dst0)(loc->boaref src0)))
                  (if (or (= (loc->boaref dst0) (add1 (loc->boaref loc)))
                          (= (loc->boaref dst0) (sub1 (loc->boaref loc))))
                      (list (Move loc (boaref->loc (+ (loc->boaref src0) 8)) (Piece 'Pawn 'Black)
                                  (Some (Piece 'Pawn 'White)) 'None))
                      '())'())]
             [_ '()])])])]))
          
(define en-passant1 (strings->board
                    (list "--------"
                          "--------"
                          "--------"
                          "--------"
                          "------Pp"
                          "--------"
                          "--------"
                          "--------")))
(define en-hist1 (list (Move (Loc 'H 2) (Loc 'H 4) (Piece 'Pawn 'White) 'None 'None)))
(check-expect (en-passant (ChessGame en-passant1 en-hist1) (Loc 'G 4))
              (list
               (Move
                (Loc 'G 4)
                (Loc 'H 3)
                (Piece 'Pawn 'Black)
                (Some (Piece 'Pawn 'White))
                'None)))

(define testpawn (strings->board
                    (list "--------"
                          "-P------"
                          "b-------"
                          "--------"
                          "--b-b---"
                          "--------"
                          "--------"
                          "--------")))

(: moves-piece-incomplete : Board Loc -> (Listof Move))
;;given a location on a given board, produce its possible moves (does not consider the case that player has to get out of check)
(define (moves-piece-incomplete b loc)
     (match (list-ref b (loc->boaref loc))
       ['None '()]
       [(Some (Piece type color))
        (match type
          ['King (moves-king b loc color)]
          ['Queen (moves-queen b loc color)]
          ['Rook (moves-rook b loc color)]
          ['Bishop (moves-bishop b loc color)]
          ['Knight (moves-knight b loc color)]
          ['Pawn (moves-pawn b loc color)]
          )]))
       
;; ----------;;;---------------------------

 ;; ----------;;;---------------------------
;;in-check?                   
(: army-position : Board Player ->  (Listof Integer))
;; identify the positions of the entire army of a given player in the form of board reference
(define (army-position b color)
  (match b
    ['() '()]
    [(cons first rest)
     (append 
      (match first
       ['None '()]
       [(Some (Piece _ color2))
         (if (symbol=? color2 color)
             (list (- 63 (length rest)))
             '())])
      (army-position rest color))]))
(check-expect (army-position testpawn 'White) (list 26 28 40))

(: moves-player-incomplete : Board Player -> (Listof Move))
;; show the moves of the player
;; incomplete because it does not take out the leaves that expose the king to check
;; more so considering the three additions
;;but still useful for in-check
(define (moves-player-incomplete b color)
  (foldr
   (inst append Move)
   '()
  (map
   (λ([n : Integer]) (moves-piece-incomplete b (boaref->loc n)))
   (army-position b color))
  ))

 
(: in-check0? : Board Player -> Boolean)
;prototype for in-check?
;Do not take castling, en passant, or promotion into account
;; a player is incheck if his opposite color can check him 
;;ex black is incheck if (in-check0? b 'White) is #t
(define (in-check0? b color)
     (if (symbol=?  color 'White)
         (ormap
          (λ ([mv : Move])
            (match (Move-captured mv)
              ['None #f]
              [(Some (Piece 'King 'Black)) #t]
              [_ #f]))
          (moves-player-incomplete b 'White))
         (ormap
          (λ ([mv : Move])
            (match (Move-captured mv)
              ['None #f]
              [(Some (Piece 'King 'White)) #t]
              [_ #f]))
          (moves-player-incomplete b 'Black))))

 (define checktest (board-update starting-board (Loc 'E 3) (Some (Piece 'King 'Black))))
 (define  inchecktest1 (strings->board
                        (list "--------"
                              "----K---"
                              "----p---"
                              "--------"
                              "--------"
                              "--------"
                              "----k---"
                              "----P---")))
(define  inchecktest2 (strings->board
                        (list "--------"
                              "----K---"
                              "----r---"
                              "--------"
                              "--------"
                              "--------"
                              "----k---"
                              "----P---")))
(check-expect (in-check0? inchecktest1 'White) #f)
(check-expect (in-check0? inchecktest2 'White) #t)

(: in-check? : ChessGame -> Boolean)
;; same as in-check0? except it takes in a ChessGame
(define (in-check? g)
  (match g
    [(ChessGame b hist) (in-check0? b ((compose opposite-color whose-turn) hist))]))
(check-expect (in-check? (ChessGame inchecktest1 whitemove1)) #f)
(check-expect (in-check? (ChessGame inchecktest2 whitemove1)) #t)

;; ----------;;;---------------------------
;; moves-piece complete
;; first, implement castling

(: empty-neighborhood? : Board Loc Integer -> Boolean)
;;see if the ajacent squares are empty
;;ex: (empty-neighborhood? b loc -1) return whether its right eighbo is empty
(define (empty-neighborhood? b loc n)
  (match (list-ref b (+ n (loc->boaref loc)))
    ['None #t]
    [_ #f]))


(: castling : ChessGame Loc -> (Listof Move))
;; add the possible moves to king due to castling
;; loc is the location of the king
(define (castling g loc)
  (match g
    [(ChessGame b hist)
     (if
      (ormap
       (λ ([mv : Move])
         (match (Move-moved mv)
           [(Piece 'King color) (symbol=? color (whose-turn hist))]            
           [_ #f])) hist)
      '()
      (append
       (if
         (not (ormap
                (λ ([mv : Move])
                  (or (loc=? (boaref->loc (- (loc->boaref loc) 4)) (Move-src mv))
                       (loc=? (boaref->loc (- (loc->boaref loc) 4)) (Move-dst mv)))) hist));;see if the rooks(if the loc is indeed an unmoved king)            
         (if (and (empty-neighborhood? b loc -1)
                  (empty-neighborhood? b loc -2)
                  (empty-neighborhood? b loc -3)
                  (not (in-check0? b (opposite-color (whose-turn hist)) ))
                  (not (in-check0? (board-update b (boaref->loc (- (loc->boaref loc) 1))
                                                 (Some (Piece 'King (whose-turn hist))))
                                   (opposite-color (whose-turn hist))))
                  (not (in-check0? (board-update b (boaref->loc (- (loc->boaref loc) 2))
                                                 (Some (Piece 'King (whose-turn hist))))
                                   (opposite-color (whose-turn hist)))))
              (list (Move loc (boaref->loc (- (loc->boaref loc) 2)) (Piece 'King (whose-turn hist)) 'None 'None))
              '()) '())
          (if
           (not (ormap
                 (λ ([mv : Move])
                   (or (loc=? (boaref->loc (+ (loc->boaref loc) 3)) (Move-src mv))
                        (loc=? (boaref->loc (+ (loc->boaref loc) 3)) (Move-dst mv)))) hist))
           (if (and (empty-neighborhood? b loc 1)
                    (empty-neighborhood? b loc 2)                 
                    (not (in-check0? b (opposite-color (whose-turn hist))))
                    (not (in-check0? (board-update b (boaref->loc (+ (loc->boaref loc) 1))
                                                   (Some (Piece 'King (whose-turn hist))))
                                     (opposite-color (whose-turn hist))))
                    (not (in-check0? (board-update b (boaref->loc (+ (loc->boaref loc) 2))
                                                   (Some (Piece 'King (whose-turn hist))))
                                     (opposite-color (whose-turn hist)))))                  
             (list (Move loc (boaref->loc (+ (loc->boaref loc) 2)) (Piece 'King (whose-turn hist)) 'None 'None))
             '()) '())))]))

(define  testcastling1 (strings->board
                        (list "--------"
                              "--------"
                              "--K-----"
                              "--------"
                              "--------"
                              "--------"
                              "--------"
                              "r---k--r")))                  
(check-expect  (length (castling (ChessGame testcastling1 '()) (Loc 'E 1))) 2)
(check-expect  (length (castling (ChessGame testcastling1
                                            (list
                                             (Move (Loc 'A 7) (Loc 'A 1) (Piece 'Rook 'White) 'None 'None)
                                             (Move (Loc 'B 7) (Loc 'B 6) (Piece 'King 'Black) 'None 'None)))
                                 (Loc 'E 1))) 1)
(define  testcastling2 (strings->board
                        (list "--------"
                              "--------"
                              "--K-----"
                              "--------"
                              "--------"
                              "--------"
                              "----R---"
                              "r---k--r")))
(check-expect (castling (ChessGame testcastling2 '()) (Loc 'E 1)) '())

(define  testcastling3 (strings->board
                        (list "--------"
                              "--------"
                              "--K-----"
                              "--------"
                              "--------"
                              "--------"
                              "---R----"
                              "r---k--r")))
(check-expect (length (castling (ChessGame testcastling3 '()) (Loc 'E 1))) 1)

(define  testcastling4 (strings->board
                        (list "--------"
                              "--------"
                              "--K-----"
                              "--------"
                              "--------"
                              "--------"
                              "--R-----"
                              "r---k--r")))
(check-expect (length (castling (ChessGame testcastling4 '()) (Loc 'E 1))) 1)

( : moves-piece : ChessGame Loc -> (Listof Move))
;;produce all possible moves of a given loc
(define (moves-piece g loc)
  (match g
    [(ChessGame b hist)
     (append
      (filter
       (λ ([vm : Move])
         (match vm
           [(Move src dst moved _ _)
            (not (in-check0? (board-update
                              (board-update b dst (Some moved))
                              src 'None)(opposite-color (whose-turn hist))))]))
       (moves-piece-incomplete b loc))
      (match (list-ref b (loc->boaref loc))
        [(Some (Piece 'Pawn _))
         (en-passant g loc)]
        [_ '()])
     (match (list-ref b (loc->boaref loc))
        [(Some (Piece 'King _))
         (castling g loc)]
        [_ '()]))]))
        
;; ----------;;;---------------------------
;;legal-move?
(: piece=? : Piece Piece -> Boolean)
;; see if two pieces are the same
(define (piece=? p1 p2)
  (match* (p1 p2)
          [((Piece type1 color1) (Piece type2 color2)) (and (symbol=? type1 type2) (symbol=? color1 color2))]))
(check-expect (piece=? (Piece 'Queen 'White) (Piece 'Queen 'White)) #t)
(check-expect (piece=? (Piece 'Queen 'Black) (Piece 'Queen 'White)) #f)
(check-expect (piece=? (Piece 'Rook 'White) (Piece 'Queen 'White)) #f)

(: move=? : Move Move -> Boolean)
;; see if two moves are the same
(define (move=? m1 m2)
  (match* (m1 m2)
    [((Move s1 d1 mved1 c1 pr1) (Move s2 d2 mved2 c2 pr2))
     (and
      (loc=? s1 s2)
      (loc=? d1 d2)
      (piece=? mved1 mved2)
      (match* (c1 c2)
        [('None 'None) #t]
        [('None _) #f]
        [(_ 'None) #f]
        [((Some p1) (Some p2)) (piece=? p1 p2)])
      (symbol=? (get-opt pr1 'None) (get-opt pr2 'None)))]))
(check-expect (move=? (Move (Loc 'C 3) (Loc 'B 4)(Piece 'Queen 'White)(Some (Piece 'Rook 'Black)) 'None)
                      (Move (Loc 'C 3) (Loc 'B 4)(Piece 'Queen 'White)(Some (Piece 'Rook 'Black)) 'None)) #t)
        (check-expect (move=? (Move (Loc 'C 2) (Loc 'B 4)(Piece 'Queen 'White)(Some (Piece 'Rook 'Black)) 'None)
                      (Move (Loc 'C 3) (Loc 'B 4)(Piece 'Queen 'White)(Some (Piece 'Rook 'Black)) 'None)) #f)
(check-expect (move=? (Move (Loc 'C 3) (Loc 'B 4)(Piece 'Queen 'White)(Some (Piece 'Rook 'Black)) 'None)
                      (Move (Loc 'C 3) (Loc 'B 4)(Piece 'Rook 'White)(Some (Piece 'Rook 'Black)) 'None)) #f)
        
(: legal-move? : ChessGame Move -> Boolean)
;; see if a move if legal given a game
(define (legal-move? g mv)
  (match mv
    [(Move src dst mvd cap pro)
     (ormap
      (λ ([vm : Move]) (move=? vm mv))
      (moves-piece g src))]))

;; ----------;;;---------------------------
;;moves-player
(: moves-player : ChessGame -> (Listof Move))
;; produce the moves of the player whose turn is to play
(define (moves-player g)
  (match g
    [(ChessGame b hist)
     (foldr
      (inst append Move)
      '()
      (map
       (λ ([n : Integer]) (moves-piece g (boaref->loc n)))
       (army-position b (whose-turn hist))))]))

;; ----------;;;---------------------------
;;checkmate?
(: checkmate? : ChessGame -> Boolean)
;;return true if the specified player is in check and cannot get our of it
(define (checkmate? g)
  (and (in-check? g)
       (empty? (moves-player g))))

(define rw (Move (Loc 'A 2) (Loc 'A 4) (Piece 'Pawn 'White) 'None 'None))
(define rb (Move (Loc 'A 7) (Loc 'A 5) (Piece 'Pawn 'White) 'None 'None))
(define wkm (Move (Loc 'E 1) (Loc 'F 1) (Piece 'King 'White) 'None 'None))
(define bkm (Move (Loc 'E 8) (Loc 'F 8) (Piece 'King 'Black) 'None 'None))

(define anastasiamate (strings->board
                     (list
                          "--------"
                          "----n-PK"
                          "--------"
                          "-------r"
                          "--------"
                          "--------"
                          "--------"
                          "--------")))
(check-expect (checkmate? (ChessGame anastasiamate (list rw))) #t)

(define anderssenmate (strings->board
                     (list
                          "------Kr"
                          "------p-"
                          "-----k--"
                          "--------"
                          "--------"
                          "--------"
                          "--------"
                          "--------")))
(check-expect (checkmate? (ChessGame anderssenmate (list rw bkm wkm))) #t)

(define arabianmate (strings->board
                     (list
                          "-------K"
                          "-------r"
                          "-----n--"
                          "--------"
                          "--------"
                          "--------"
                          "--------"
                          "--------")))
(check-expect (checkmate? (ChessGame arabianmate (list rw bkm wkm))) #t)

(define bank-rankmate (strings->board
                     (list
                          "---r--K-"
                          "-----PPP"
                          "--------"
                          "--------"
                          "--------"
                          "--------"
                          "--------"
                          "--------")))
(check-expect (checkmate? (ChessGame bank-rankmate (list rw bkm wkm))) #t)

(define bishopandknightmate (strings->board
                     (list
                          "-------K"
                          "--------"
                          "-----bkn"
                          "--------"
                          "--------"
                          "--------"
                          "--------"
                          "--------")))
(check-expect (checkmate? (ChessGame bishopandknightmate (list rw bkm wkm))) #t)

(define blackburnmate (strings->board
                     (list
                          "-----RK-"
                          "-------b"
                          "--------"
                          "------n-"
                          "--------"
                          "--------"
                          "-b------"
                          "--------")))
(check-expect (checkmate? (ChessGame blackburnmate (list rw bkm wkm))) #t)


(define blindswine (strings->board
                     (list
                          "-----RK-"
                          "------rr"
                          "--------"
                          "--------"
                          "--------"
                          "--------"
                          "--------"
                          "--------")))
(check-expect (checkmate? (ChessGame blindswine (list rw bkm wkm))) #t)

(define retimate (strings->board
                     (list
                          "-NBb----"
                          "-PK-----"
                          "--P-----"
                          "--------"
                          "--------"
                          "--------"
                          "--------"
                          "---r---k")))
(check-expect (checkmate? (ChessGame retimate (list rw bkm wkm))) #t)

;;the examples are from wikipedia article "Checkmate Pattern"

(: stalemate? : ChessGame -> Boolean)
;;return true if the specified player is in check and cannot get our of it
(define (stalemate? g)
  (and (not (in-check? g))
       (empty? (moves-player g))))


(define teststalemate1 (strings->board
                     (list
                          "-----K--"
                          "-----p--"
                          "-----k--"
                          "--------"
                          "--------"
                          "--------"
                          "--------"
                          "--------")))
(check-expect (stalemate? (ChessGame teststalemate1 (list rw bkm wkm))) #t)

(define teststalemate2 (strings->board
                     (list
                          "KB-----r"
                          "--------"
                          "-k------"
                          "--------"
                          "--------"
                          "--------"
                          "--------"
                          "--------")))
(check-expect (stalemate? (ChessGame teststalemate2 (list rw bkm wkm))) #t)

(define teststalemate3 (strings->board
                     (list
                          "--------"
                          "--------"
                          "--------"
                          "--------"
                          "--------"
                          "--k-----"
                          "-r------"
                          "K-------")))
(check-expect (stalemate? (ChessGame teststalemate3 (list rw bkm wkm))) #t)

(define teststalemate4 (strings->board
                     (list
                          "--------"
                          "--------"
                          "--------"
                          "------k-"
                          "--------"
                          "-q------"
                          "P-------"
                          "K-------")))
(check-expect (stalemate? (ChessGame teststalemate4 (list rw bkm wkm))) #t)

(define teststalemate5 (strings->board
                     (list
                          "K-------"
                          "p-------"
                          "k-------"
                          "--------"
                          "-----b--"
                          "--------"
                          "--------"
                          "--------")))
(check-expect (stalemate? (ChessGame teststalemate5 (list rw bkm wkm))) #t)


(: cupdate : Loc Square -> (Board -> Board))
;; a curried verision of board-update
(define (cupdate loc sq)
  (λ ([b : Board]) (board-update b loc sq)))


(: normal-apply : Board Move -> Board)
;; hide the implicit changes due to the three special moves
;; assume it is legal
(define (normal-apply b mv)
  (match mv
        [(Move src dst moved captured promote-to)
;          (board-update (board-update b dst (Some moved))
;                        src 'None)
          ((compose (cupdate dst (Some moved)) (cupdate src 'None)) b)]))
          

(: pro-apply : Board Move -> Board)
;; apply the implicit effects of promotion 
;; assume it is legal
(define (pro-apply b mv)
  (match mv
        [(Move src dst mvd cap pro)
           (if (symbol=? (get-opt pro 'None) 'None) b
            ((cupdate dst (Some (Piece (val-of pro) (Piece-color mvd)))) b))]))
           
(: pas-apply : Board Move -> Board)
;; apply the implicit effects of en-passent
(define (pas-apply b mv)
  (match mv
    [(Move src dst (Piece 'Pawn c1) (Some (Piece 'Pawn c2)) _)
       (match (list-ref b (loc->boaref dst))
         ['None ((cupdate
                 (boaref->loc (- (loc->boaref dst) (* (sgn (- (loc->boaref dst) (loc->boaref src))) 8))) 'None) b)]
         [_ b])]
    [_ b]))
       
         
(define testpasapply (strings->board
                     (list
                          "--------"
                          "--------"
                          "--------"
                          "--------"
                          "----Pp--"
                          "--------"
                          "--------"
                          "--------")))

(define pasmv (Move (Loc 'E 4) (Loc 'F 3) (Piece 'Pawn 'Black) (Some (Piece 'Pawn 'White)) 'None))
 ;;performed an eyeball test

(: cas-apply : Board Move -> Board)
;;apply the implicit effects of castling
(define (cas-apply b mv)
  (match mv
    [(Move src dst (Piece 'King color) 'None 'None)
     (cond
       [(= 2 (- (loc->boaref dst) (loc->boaref src)))
        ((compose
          (cupdate (boaref->loc (add1 (loc->boaref dst))) 'None)
          (cupdate (boaref->loc (sub1 (loc->boaref dst))) (Some (Piece 'Rook color)))) b)]
       [(= -2 (- (loc->boaref dst) (loc->boaref src)))
        ((compose
          (cupdate (boaref->loc (sub1 (loc->boaref dst))) 'None)
          (cupdate (boaref->loc (add1 (loc->boaref dst))) (Some (Piece 'Rook color)))) b)]
       [else b])]))

(: apply-move : ChessGame Move -> ChessGame)
;; apply the move to chessgame
(define (apply-move g mv)
  (if (legal-move? g mv)
      (match g
        [(ChessGame b hist)
         (ChessGame
          (cas-apply (pas-apply (pro-apply (normal-apply b mv) mv) mv) mv)
          (append hist (list mv)))])
      g))
       

























































(test)
