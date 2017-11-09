#lang typed/racket
(require typed/test-engine/racket-tests)
(require "../include/cs151-core.rkt")

(define-struct (Pr A B)
  ([a : A]
   [b : B]))

(define-struct (Some A)
  ([value : A]))

(define-type (Optional A)
  (U 'None (Some A)))

(define-type (Compare A)
  (A A -> Boolean))

(define-type (BSTree A)
  (U 'E (Nd A)))

(define-struct (Nd A)
  ([root    : A]
   [count   : Integer]
   [lesser  : (BSTree A)]
   [greater : (BSTree A)]))

(define-struct (BST A)
  ([less-than : (Compare A)]
   [tree-data : (BSTree A)]))
  
(define bstree1 (Nd 20 2
                    (Nd 16 1
                        (Nd 12 1 'E 'E)
                        (Nd 17 3 'E 'E))
                    (Nd 22 1
                        (Nd 21 3 'E 'E)
                        (Nd 29 8 'E 'E))))

(define bstree2 (Nd 20 2
                    (Nd 16 1 (Nd 12 1 'E 'E) (Nd 17 3 'E
                                                 (Nd 19 1 'E 'E)))
                    (Nd 22 1 (Nd 21 3 'E 'E) (Nd 29 8 'E 'E))));;bstree1 add number 19
(define bstree3 (Nd 20 2
                    (Nd 16 1 (Nd 12 1 'E 'E) (Nd 17 4 'E 'E))
                    (Nd 22 1 (Nd 21 3 'E 'E) (Nd 29 8 'E 'E))));;bstree1 add number 17

(define bst1
  (BST < bstree1))

(: make-empty : All (A) (Compare A) -> (BST A))
;; produce an empty BST given an ordering function
(define (make-empty func)
  (BST func 'E))
(check-expect (BST-tree-data (make-empty string<?)) 'E)

(: singleton : All (A) (Compare A) A -> (BST A))
;; produce a singleton BST given an ordering function and one item
(define (singleton func root)
  (BST func (Nd root 1 'E 'E)))
(check-expect (BST-tree-data (singleton < 1)) (Nd 1 1 'E 'E))

(: intrees : All (A) A (Compare A) (BSTree A)-> (BSTree A))
;; insert item into bstree
(define (intrees x func tree)
               (match tree
                 ['E (Nd x 1 'E 'E)]
                 [(Nd root c l r)
                  (cond
                    [(func x root) (Nd root c (intrees x func l) r)]
                    [(func root x) (Nd root c l (intrees x func r))]
                    [else (Nd root (add1 c) l r)])]))
(check-expect (intrees 19 < bstree1) bstree2)
(check-expect (intrees 17 < bstree1) bstree3)
               
(: insert : All (A) A (BST A) -> (BST A))
;; insert item into tree
(define (insert x t)
  (match t
    [(BST func tree)
       (BST func (intrees x func tree))]))       
(check-expect (BST-tree-data (insert 19 bst1)) bstree2)
(check-expect (BST-tree-data (insert 17 bst1)) bstree3)


(: contains? : All (A) A (BST A) -> Boolean)
;; test if tree contains item
(define (contains? x t)
  (match t
    [(BST func tree)
     (local
       {(: treecontains? : A (BSTree A) -> Boolean)
        (define (treecontains? x tree)
          (match tree
            ['E #f]
            [(Nd root _ l r)
             (cond
               [(func x root) (treecontains? x l)]
               [(func root x) (treecontains? x r)]
               [else #t])]))}
       (treecontains? x tree))]))
(check-expect (contains? 17 bst1) #t)
(check-expect (contains? 3 bst1) #f)


(: item-count : All (A) A (BST A) -> Integer)
;; return the number of this item in tree
(define (item-count x t)
  (match t
    [(BST func tree)
     (local
       {(: tree-count : A (BSTree A) -> Integer)
        (define (tree-count x tree)
          (match tree
            ['E 0]
            [(Nd root c l r)
             (cond
               [(func x root)(tree-count x l)]
               [(func root x)(tree-count x r)]
               [else c])]))}
       (tree-count x tree))]))         
(check-expect   (item-count 17 bst1) 3)

(: num-nodes : All (A) (BST A) -> Integer)
;; count the number of _nodes_ in the tree
(define (num-nodes t)
  (match t
    [(BST func tree)
     (local
       {(: tree-nodes : (BSTree A) -> Integer)
        (define (tree-nodes tree)
          (match tree
            ['E 0]
            [(Nd _ _ l r)
             (+ 1 (tree-nodes l) (tree-nodes r))]))}
       (tree-nodes tree))]))             
(check-expect (num-nodes bst1) 7)

(: num-items : All (A) (BST A) -> Integer)
;; count the number of _items_ in the tree (>= the number of nodes)
(define (num-items t)
  (match t
    [(BST func tree)
     (local
       {(: tree-items : (BSTree A) -> Integer)
        (define (tree-items tree)
          (match tree
            ['E 0]
            [(Nd _ c l r)
             (+ c (tree-items l) (tree-items r))]))}
       (tree-items tree))]))
(check-expect (num-items bst1) 19)   

(: directions-to : All (A) A (BST A) -> (Listof (U A 'LEFT 'RIGHT)))
;; return the path to the item and record of left/right turns
(define (directions-to x t)
  (match t
    [(BST func 'E) '()]
    [(BST func (Nd root c l r))
     (cond
       [(func x root) (append
                       (list root 'LEFT)
                       (directions-to x (BST func l)))]
       [(func root x) (append
                       (list root 'RIGHT)
                       (directions-to x (BST func r)))]
       [else (list root)])]))

(check-expect (directions-to 12 bst1) (list 20  'LEFT 16 'LEFT 12))
(check-expect (directions-to 17 bst1) (list 20  'LEFT 16 'RIGHT 17))

(: from-list : All (A) (Compare A) (Listof A) -> (BST A))
;; build a BST from the items in the list
(define (from-list func as)
  (local
    {(: list->bstree : (Listof A) -> (BSTree A))
     (define (list->bstree as)
       (foldr
        (λ ([a : A] [atree : (BSTree A)]) (intrees a func atree))
        'E
        (reverse as)))}
    (BST func (list->bstree as))))
(check-expect (BST-tree-data (from-list < (list 20 22 16 20 29 21 29 29 29 29 29 29 29 17 12 17 17 21 21)))
     bstree1)                              
  
(: inorder : All (A) (BST A) -> (Listof A))
;; construct the inorder traversal of items in tree
;; items that appear k times in tree must appear k times in result
(define (inorder t)
  (match t
    [(BST func tree)
     (local
       {(: treeinorder : (BSTree A) -> (Listof A))
        (define (treeinorder tree)
          (match tree
            ['E '()]
            [(Nd root c l r)
             (append (treeinorder l)
                     (build-list c
                                 (λ ([n : Integer]) root))
                     (treeinorder r))]))}
       (treeinorder tree))]))           
(check-expect (inorder bst1) (list 12 16 17 17 17 20 20 21 21 21 22 29 29 29 29 29 29 29 29 ))

(: min-item : All (A) (BSTree A) -> (Optional (Pr A Integer)))
;; return the minimum item in the tree and its count
(define (min-item tree)
  (match tree
    ['None 'None]
    [(Nd root c 'E _) (Some(Pr root c))]
    [(Nd _ _ l _) (min-item l)])) 
(check-expect (min-item bstree1) (Some (Pr 12 1)))          
          
  
       
















(test)














