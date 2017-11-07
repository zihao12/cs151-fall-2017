#lang typed/racket
(require "../include/cs151-core.rkt")

;; ==== ==== ==== ====
;; external interface

(provide File
         Rank
         DigitFile
         (struct-out Loc)
         (struct-out DigitLoc)
         loc=?
         digitloc=?

         )

;; ==== ==== ==== ====
;; data definitions

(define-type File (U 'A 'B 'C 'D 'E 'F 'G 'H))
(define-type Rank (U 1 2 3 4 5 6 7 8))

(define-struct Loc
  ([file : File]
   [rank : Rank]))

;;;;;;;;;;;supplementary data definitions
(define-type DigitFile (U 1 2 3 4 5 6 7 8))
(define-struct DigitLoc
  ([dfile : DigitFile]
   [rank : Rank]))
;; ==== ==== ==== ====
;; operations

(: loc=? : Loc Loc -> Boolean)
(define (loc=? loc1 loc2)
  (match* (loc1 loc2)
    [((Loc f1 r1) (Loc f2 r2))
     (and (symbol=? f1 f2) (= r1 r2))]))

(: digitloc=? : DigitLoc DigitLoc -> Boolean)
(define (digitloc=? dl1 dl2)
  (match* (dl1 dl2)
    [((DigitLoc df1 r1) (DigitLoc df2 r2))
     (and (= df1 df2) (= r1 r2))]))
