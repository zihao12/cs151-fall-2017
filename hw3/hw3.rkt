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
;;Problem 3
(define-type 3Tree (U 3Node '3Empty))

(define-struct 3Node
  ([root : Integer]
   [lsub : 3Tree]
   [msub : 3Tree]
   [rsub : 3Tree]))

(define T (3Node 1
       (3Node 2
              (3Node 3 '3Empty '3Empty '3Empty)
              (3Node 4 '3Empty '3Empty '3Empty)
              '3Empty)
       (3Node 8
              '3Empty
              (3Node 7
                     (3Node 5 '3Empty '3Empty '3Empty)
                     '3Empty
                     (3Node 6 '3Empty '3Empty '3Empty))
              '3Empty)
       (3Node 9
              '3Empty
              '3Empty
              (3Node 0 '3Empty '3Empty '3Empty))))

(: num-nodes : 3Tree -> Integer)
;;count the number of nodes in a tree
(define (num-nodes t)
  (match t
    ['3Empty 0]
    [(3Node _ ls ms rs)
     (+ 1
        (num-nodes ls)
        (num-nodes ms)
        (num-nodes rs))]))
(check-expect (num-nodes T) 10)
       
(: sum-nodes : 3Tree -> Integer)
;;add the values of all the nodes in the given tree
(define (sum-nodes t)
  (match t
    ['3Empty 0]
    [(3Node r ls ms rs)
     (+ r
        (sum-nodes ls)
        (sum-nodes ms)
        (sum-nodes rs))]))
(check-expect (sum-nodes T) 45)
 
(: height : 3Tree -> Integer)
;;return the number of generations in a tree
(define (height t)
  (match t
    ['3Empty 0]
    [ (3Node _ ls ms rs)
      (+ 1
         (max (height ls) (height ms) (height rs)))]))
(check-expect (height T) 4)

(: contains? : 3Tree Integer -> Boolean)
;; see if the given tree contains a given integer
(define (contains? t n)
  (match t
    ['3Empty #f]
    [(3Node r ls ms rs)
     (or (= r n)
         (contains? ls n)
         (contains? ms n)
         (contains? rs n))]))
(check-expect (contains? T 4) #t)
(check-expect (contains? T 11) #f)

(: leftmost : 3Tree -> (U Integer 'None))
;;search for the leftmost element in a given tree
(define (leftmost t)
  (match t
    ['3Empty 'None]
    [(3Node r ls ms rs)
       (match ls
         ['3Empty r]
         [(3Node r ls ms rs)
          (leftmost ls)])]))
(check-expect (leftmost T) 3)

(: farthest-item : 3Tree -> (U Integer 'None))
;; find the element farthest away from the root
(define (farthest-item t)
  (match t
    ['3Empty 'None]
    [(3Node r ls ms rs)
     (cond
       [(and (= (sub1 (height t)) (height ls))
             (> (height ls) 0))
        (farthest-item ls)]
       [(and (= (sub1 (height t)) (height ms))
             (> (height ms) 0))
        (farthest-item ls)]
       [(and (= (sub1 (height t)) (height ls))
             (> (height rs) 0))
        (farthest-item rs)]
       [else r])]))
(check-expect (farthest-item T) 5)








(test)