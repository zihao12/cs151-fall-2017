#lang typed/racket
(require typed/test-engine/racket-tests)
(require racket/local)
(require "../include/cs151-core.rkt")


;;Problem 1
(define-type CardBrand
  (U 'AmEx 'WorkPermit 'DisasterCard 'Coverup))

(define-struct CreditCard
  ([type : CardBrand]
   [num  : Integer]))

(: drop-the-last-digit(Integer -> Integer))
;;leave out the last digit of a given number
( define (drop-the-last-digit N)
   (exact-floor (/ (abs N) 10)))
(check-expect (drop-the-last-digit 233) 23 )

(: get-the-first-digit (Integer -> Integer))
;;get the first digit of a given integer
(define (get-the-first-digit N)
  (cond
    [(< N 10) N]
    [else (get-the-first-digit (drop-the-last-digit N))]))
(check-expect (get-the-first-digit 65656) 6 )

(: brand-digit (CardBrand -> Integer))
;;takes in a CardBrand and returns  the first digit of a valid card number for a card issued by that brand
( define (brand-digit d)
   (match d
     ['AmEx 3]
     ['WorkPermit 4]
     ['DisasterCard 5]
     ['Coverup 6]))
(check-expect (brand-digit 'AmEx) 3)

(: brand-valid? (CreditCard -> Boolean))
;;takes in a creditcard and see if the number if valid
;;;;;;;;; do i need to consider the lengtho of the number
(define (brand-valid? c)
  (match c
    [ (CreditCard type num)
        (= (brand-digit type) (get-the-first-digit num))]))
(check-expect (brand-valid? (CreditCard 'Coverup 6666666666666666)) #t)

(: build-card (Integer -> CreditCard))
;; takes in an Integer (the card number) and returns a properly branded CreditCard
(define (build-card d)
  (cond
    [(= (get-the-first-digit d) 3) (CreditCard 'AmEx d)]
    [(= (get-the-first-digit d) 4) (CreditCard 'WorkPermit d)]
    [(= (get-the-first-digit d) 5) (CreditCard 'DisasterCard d)]
    [(= (get-the-first-digit d) 6) (CreditCard 'Coverup d)]
    [else (error "build-card: input not valid")]))

 (check-expect (build-card 6666666666666666)  (CreditCard 'Coverup 6666666666666666) )  
 (check-error (build-card 9666666666666666) "build-card: input not valid")

;;Problem 2
(define-type IntTree (U IntNode 'IEmpty))

(define-struct IntNode
  ([val   : Integer]
   [left  : IntTree]
   [right : IntTree]))

(define-type StringTree (U StringNode 'SEmpty))

(define-struct StringNode
  ([val   : String]
   [left  : StringTree]
   [right : StringTree]))

(: mirror (IntTree -> IntTree))
;;creates a mirror image of the given binary tree
(define (mirror t)
  (match t
    ['IEmpty 'IEmpty]
    [(IntNode v l r)
    (IntNode v r l)]))
(check-expect (mirror (IntNode 1 (IntNode 2 'IEmpty 'IEmpty) (IntNode 3 'IEmpty 'IEmpty)))
              (IntNode 1 (IntNode 3 'IEmpty 'IEmpty) (IntNode 2 'IEmpty 'IEmpty)))

(: int-tree->string-tree (IntTree -> StringTree))
;; turn a IntTree into a StringTree
(define (int-tree->string-tree  I)
  (match I
    ['IEmpty 'SEmpty]
    [ (IntNode v l r)
      (StringNode (number->string v)
                 (int-tree->string-tree  l)
                 (int-tree->string-tree  r))]))
(check-expect (int-tree->string-tree (IntNode 1 (IntNode 2 'IEmpty 'IEmpty) (IntNode 3 'IEmpty 'IEmpty)))
              (StringNode (number->string 1)
                          (StringNode (number->string 2) 'SEmpty 'SEmpty)
                          (StringNode (number->string 3) 'SEmpty 'SEmpty)))
                 
(: right-edge (StringTree -> String))
;;return the right-edge of a given StringTree
(define (right-edge st)
  (match st
    ['SEmpty ""]
    [ (StringNode v l r)
      (string-append v
                     (right-edge r))]))
(check-expect (right-edge(StringNode "x"
                          (StringNode "xx" 'SEmpty 'SEmpty)
                          (StringNode "yy" 'SEmpty
                                      (StringNode "zzz" 'SEmpty 'SEmpty)))) "xyyzzz")


                                   
       
                             




















(test)