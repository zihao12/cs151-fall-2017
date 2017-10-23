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
   (if (> (abs N) 10) (exact-floor (/ (abs N) 10)) N))
(check-expect (drop-the-last-digit 233) 23 )
(check-expect (drop-the-last-digit 2) 2 )

(: length-of-the-digit (Integer -> Integer))
;;return the length of a given digit
(define (length-of-the-digit N)
  (if (> (abs N) 9) (+ 1 (length-of-the-digit (drop-the-last-digit N))) 1))
(check-expect (length-of-the-digit 233) 3)      
(check-expect (length-of-the-digit 3) 1)      
      
      

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
      (and (= (length-of-the-digit num) 16)
        (= (brand-digit type) (get-the-first-digit num)))]))
(check-expect (brand-valid? (CreditCard 'Coverup 6666666666666666)) #t)
(check-expect (brand-valid? (CreditCard 'Coverup 666666666666666)) #f)

(: build-card (Integer -> CreditCard))
;; takes in an Integer (the card number) and returns a properly branded CreditCard
(define (build-card d)
  (cond
    [( = 16 (length-of-the-digit d))
     (cond
    [(= (get-the-first-digit d) 3) (CreditCard 'AmEx d)]
    [(= (get-the-first-digit d) 4) (CreditCard 'WorkPermit d)]
    [(= (get-the-first-digit d) 5) (CreditCard 'DisasterCard d)]
    [(= (get-the-first-digit d) 6) (CreditCard 'Coverup d)]
    [else (error "build-card: input not valid")])]
    [else (error "build-card: input not valid")]))
 (check-expect (build-card 6666666666666666)  (CreditCard 'Coverup 6666666666666666) )  
 (check-error (build-card 9666666666666666) "build-card: input not valid")
(check-error (build-card 666666666666666) "build-card: input not valid")

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;I noticed I have misunderstood the meaning of mapping. I have corrected them in this verison.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: mirror (IntTree -> IntTree))
;;creates a mirror image of the given binary tree
(define (mirror t)
  (match t
    ['IEmpty 'IEmpty]
    [(IntNode v l r)
    (IntNode v (mirror r) (mirror l))]))
(check-expect (mirror (IntNode 1 (IntNode 2 'IEmpty 'IEmpty) (IntNode 3 'IEmpty (IntNode 4 'IEmpty 'IEmpty))))
              (IntNode 1 (IntNode 3 (IntNode 4 'IEmpty 'IEmpty) 'IEmpty) (IntNode 2 'IEmpty 'IEmpty)))

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

(define T2 (3Node 2
       (3Node 3
              (3Node 4 '3Empty '3Empty '3Empty)
              (3Node 5 '3Empty '3Empty '3Empty)
              '3Empty)
       (3Node 9
              '3Empty
              (3Node 8
                     (3Node 6 '3Empty '3Empty '3Empty)
                     '3Empty
                     (3Node 7 '3Empty '3Empty '3Empty))
              '3Empty)
       (3Node 10
              '3Empty
              '3Empty
              (3Node 1 '3Empty '3Empty '3Empty))))

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
        (farthest-item ms)]
       [(and (= (sub1 (height t)) (height ls))
             (> (height rs) 0))
        (farthest-item rs)]
       [else r])]))
(check-expect (farthest-item T) 5)

(: increment : 3Tree -> 3Tree)
;;plus one for each leave
(define (increment t)
  (match t
    ['3Empty '3Empty]
    [(3Node r ls ms rs)
     (3Node (+ r 1) (increment ls) (increment ms) (increment rs))]))
(check-expect (increment T) T2)

;;Problem 4
(define-type Category (U 'Book 'Food 'Electronics))

(define-struct Product
  ([name  : String]
   [cat   : Category]
   [price : Integer]))

(define-type Products (U 'NoProducts ProductCons))
(define-struct ProductCons
  ([p    : Product]
   [rest : Products]))

(define P1 (Product "Harry Potter1" 'Book  20))
(define P2 (Product "Hamburger1" 'Food  15))
(define P3 (Product "Iphone X" 'Electronics  700))
(define P4 (Product "Harry Potter2" 'Book  20))
(define P5 (Product "Harry Potter3" 'Book  20))
(define P6 (Product "Hamburger2" 'Food  15))
(define P7 (Product "Hamburger3" 'Food  15))

(define Purchase1 (ProductCons P1 (ProductCons P2 (ProductCons P3 'NoProducts))))
(define Purchase2 (ProductCons P1 (ProductCons P4 (ProductCons P5 'NoProducts))))
(define Purchase3 (ProductCons P2 (ProductCons P6 (ProductCons P7 'NoProducts))))
(define Purchase4 (ProductCons P1 (ProductCons P2 'NoProducts)))
 (define SHIP1 (ProductCons P3 'NoProducts))           
(: order-total : Products -> Integer)
;; calculate the total bill given the list of products
(define (order-total ps)
  (match ps
    ['NoProducts 0]
    [(ProductCons p rest)
     (+ (Product-price p)
        (order-total rest))]))
(check-expect (order-total Purchase1) 735)

(: media-mail? : Products -> Boolean)
;; see if the products can be shiped by media mail
(define (media-mail? ps)
  (match ps
    ['NoProducts #t]
    [(ProductCons p rest)
     (and (symbol=? (Product-cat p) 'Book)
          (media-mail? rest))]))
(check-expect (media-mail? Purchase1) #f)
(check-expect (media-mail? Purchase2) #t)

(: perishable? : Products -> Boolean)
;;see if the products inlcude anything perishable
(define (perishable? ps)
  (match ps
    ['NoProducts #f]
    [(ProductCons p rest)
     (or (symbol=? (Product-cat p) 'Food)
          (perishable? rest))]))
(check-expect (perishable? Purchase2) #f)
(check-expect (perishable? Purchase3) #t)
(check-expect (perishable? Purchase1) #t)

(: product=? : Product Product -> Boolean)
;; see  if two Products have the same name, category, and price.
(define (product=? p1 p2)
  (match* (p1 p2)
    [((Product n1 c1 price1) (Product n2 c2 price2))
     (and (string=? n1 n1) (symbol=? c1 c2) (= price1 price2))]))

(check-expect (product=? P1 P1) #t)
(check-expect (product=? P1 P2) #f)

(: in-stock? : Product Products -> Boolean)
;; see if a given product is contained in the givem list of products
(define (in-stock? p ps)
  (match ps
    ['NoProducts #f]
    [(ProductCons p0 rest)
     (or (product=? p0 p) (in-stock? p rest))]))
(check-expect   (in-stock? P1 Purchase1)  #t)
(check-expect   (in-stock? P2 Purchase2)  #f)

(: can-ship? : Products Products -> Boolean)
;;see if the first list of products are contained in the second list
(define (can-ship? ps1 ps2)
  (match ps1
    ['NoProducts #t]
    [(ProductCons p rest)
      (and (in-stock? p ps2) (can-ship? rest ps2))]))
(check-expect (can-ship? Purchase4 Purchase1)#t)
(check-expect (can-ship? Purchase4 Purchase2)#f)

(: back-ordered : Products Products -> Products)
;; make a list a products in the first list that go missing in the second list
(define (back-ordered ps1 ps2)
  (match ps1
    ['NoProducts 'NoProducts]
    [(ProductCons p rest)
     (if (in-stock? p ps2) (back-ordered rest ps2) (ProductCons p (back-ordered rest ps2)))]))
(check-expect (back-ordered Purchase1 Purchase4)SHIP1)
(check-expect (back-ordered Purchase4 Purchase1)'NoProducts)

(test)
