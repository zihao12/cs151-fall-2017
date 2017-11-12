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

;; ----------;;;---------------------------
;; King!!! 
(: moves-king : Board Loc Player -> (Listof Move))
;;given a piece that has the piecetype King, decide the moves it can take
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


;; ----------;;;---------------------------
;; moves-pawn
(: moves-pawn : Board Loc Player -> (Listof Move))
;;given a piece that has the piecetype Knight, decide the moves it can take
(define (moves-pawn b loc color)
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
             ['None (list (Move loc (boaref->loc (- (loc->boaref loc) 8)) (Piece 'Pawn 'White) 'None 'None))]
             [_ '()]))}
             (append mvup
                     (if (or (empty? mvup) (not (= (Loc-rank loc) 7)) )
                     '()
                     (match (list-ref b (- (loc->boaref loc) 16))
                       ['None
                        (list (Move loc (boaref->loc (- (loc->boaref loc) 16)) (Piece 'Pawn 'White) 'None 'None))]
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


(: whose-turn : (Listof Move) -> Player)
;;see whose turn it is from history
(define (whose-turn moves)
  (match moves
    ['() 'White]
    [_
     (match (last moves)
       [(Move _ _ (Piece _ color) _ _)
        (if (symbol=? color 'White) 'Black 'White)])]))

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

(: moves-player-incomplete : Board Player -> (Listof Move))
(define (moves-player-incomplete b color)
  (foldr
   (inst append Move)
   '()
  (map
   (λ([n : Integer]) (moves-piece-incomplete b (boaref->loc n)))
   (army-position b color))
  ))
;; ----------;;;---------------------------
;;in-check?       
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
(: in-check? : ChessGame -> Boolean)
;; same as in-check0? except it takes in a ChessGame
(define (in-check? g)
  (match g
    [(ChessGame b hist) (in-check0? b ((compose opposite-color whose-turn) hist))]))
;; ----------;;;---------------------------
;;legal-move?

(: legal-move0? : Board Move Player -> Boolean)
;; see if a move is legal and get you out of check
;; assume that the the consequence of the move is right: moved is right and captured is right if there is one
(define (legal-move0? b mv color)
  (match mv
    [(Move src dst moved captured promoto-to)
     (and
      (ormap
       (λ ([vm : Move])
         (loc=? (Move-dst vm) dst))
       (moves-player-incomplete b color))
      (not (in-check0? (board-update
                   (board-update b dst (Some moved))
                   src 'None)(opposite-color color) )))]))
       
           
(define checklegal (strings->board
                    (list "--------"
                          "--------"
                          "----P---"
                          "---k----"
                          "--------"
                          "--q-----"
                          "--------"
                          "--------")))

(check-expect (legal-move0? checklegal (Move (Loc 'C 3) (Loc 'B 4)(Piece 'Queen 'White)'None 'None)
                            'White) #f)
(check-expect (legal-move0? checklegal (Move (Loc 'C 3) (Loc 'A 4)(Piece 'Queen 'White)'None 'None)
                            'White) #f)
(check-expect (legal-move0? checklegal (Move (Loc 'C 3) (Loc 'B 4)(Piece 'Queen 'White)'None 'None)
                            'White) #f)
(check-expect (legal-move0? checklegal (Move (Loc 'D 5) (Loc 'C 4)(Piece 'King 'White)'None 'None)
                            'White) #t)
(check-expect (legal-move0? checklegal (Move (Loc 'D 5) (Loc 'E 5)(Piece 'King 'White)'None 'None)
                            'White) #t)
(define checklegal2 (strings->board
                    (list "--------"
                          "--------"
                          "----R---"
                          "--------"
                          "--------"
                          "--q-----"
                          "--k-----"
                          "--------")))
(check-expect (legal-move0? checklegal2 (Move (Loc 'C 3) (Loc 'C 8)(Piece 'Queen 'White)'None 'None)
                       'White) #t)
(: legal-move? : ChessGame Move -> Boolean)
;; same as legal-move0?
(define (legal-move? g mv)
  (match g
    [(ChessGame b hist) (legal-move0? b mv (whose-turn hist))]))

;; ----------;;;---------------------------
;;moves-piece
(: moves-piece0 : Board Loc Player -> (Listof Move))
;;Given a game and a particular piece (identified by its location),
;;give a list of moves that can legally be made with that piece. If none, return the empty list.
(define (moves-piece0 b loc color)
  (filter
   (λ ([vm : Move])
     (match vm
       [(Move src dst moved _ _)
        (not (in-check0? (board-update
                          (board-update b dst (Some moved))
                          src 'None)(opposite-color color)))]))
 ;  (λ ([vm : Move]) (legal-move0? b vm color)) 
   (moves-piece-incomplete b loc)))
;(moves-piece0 checklegal (Loc 'C 3) 'White)
;(moves-piece0 checklegal (Loc 'D 5) 'White)

(: moves-piece : ChessGame Loc -> (Listof Move))
;; same as moves-piece0, except intake ChessGame
(define (moves-piece g loc)
  (match g
    [(ChessGame b hist) (moves-piece0 b loc (whose-turn hist))]))


;; ----------;;;---------------------------
;;moves-player

(: moves-player0 : Board Player -> (Listof Move))
;;produce all possible legal moves a player can make
(define (moves-player0 b color)
  (filter
   (λ ([vm : Move])
     (match vm
       [(Move src dst moved _ _)
        (not (in-check0? (board-update
                          (board-update b dst (Some moved))
                          src 'None)(opposite-color color)))]))
   (moves-player-incomplete b color)))
(define testplayer1 (strings->board
                     (list
                          "--------"
                          "--------"
                          "--------"
                          "--------"
                          "-----P--"
                          "--------"
                          "---k----"
                          "--------")))
(: moves-player : ChessGame -> (Listof Move))
;;produce all possible legal moves a player can make
(define (moves-player g)
  (match g
    [(ChessGame b hist)
     (moves-player0 b (whose-turn hist))]))

;; ----------;;;---------------------------
;;checkmate?
(: checkmate0? : Board Player -> Boolean)
;;return true if the specified player is in check and cannot get our of it
(define (checkmate0? b color)
  (and
   (in-check0? b (opposite-color color))
   (empty? (moves-player0 b color))))

(define testcheckmate0 (strings->board
                     (list
                          "--------"
                          "--------"
                          "--------"
                          "--------"
                          "--------"
                          "--------"
                          "--------"
                          "--Q-Q---")))
(: checkmate? : ChessGame -> Boolean)
;;return true if the specified player is in check and cannot get our of it
(define (checkmate? g)
  (match g
    [(ChessGame b hist)
     (checkmate0? b (whose-turn hist))]))

;; ----------;;;---------------------------
;; stalemate?

(: stalemate0? : Board Player -> Boolean)
;; stalemate is when the player whose turn it is to move
;; is not in check but has no legal moves
(define (stalemate0? b color)
  (and 
   (not (in-check0? b (opposite-color color)))
   (empty? (moves-player0 b color))))
(define teststalemate0 (strings->board
                     (list
                          "--------"
                          "--------"
                          "--------"
                          "--------"
                          "--------"
                          "--q-----"
                          "-r------"
                          "K-------")))
(check-expect (stalemate0? teststalemate0 'Black) #t)

(: stalemate? : ChessGame -> Boolean)
;; stalemate is when the player whose turn it is to move
;; is not in check but has no legal moves
(define (stalemate? g)
  (match g
    [(ChessGame b hist)
     (stalemate0? b (whose-turn hist))]))

(: apply-move : ChessGame Move -> ChessGame)
;Make the specified move for the player whose turn it is,
;modifying the board accordingly. Update the history of moves.
;Raise an error if the desired move is not legal according to legal-move?.
(define (apply-move g mv)
  (if (legal-move? g mv)
      (match* (g mv)
        [((ChessGame b hist) (Move src dst moved captured promote-to))
         (ChessGame
          (board-update (board-update b dst (Some moved))
                        src 'None)
          (append hist (list mv)))])
      (error "illegal move!")))









(test)
