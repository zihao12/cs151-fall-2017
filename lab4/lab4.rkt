#lang typed/racket
(require "../include/cs151-image.rkt")
(require "../include/cs151-core.rkt")
(require typed/test-engine/racket-tests)

(define-type Bank
  (U 'Lake 'Left 'Right))

(define-struct Account
  ([bank   : Bank]
   [number : Integer]))

(define-struct Transaction
  ([from   : Account]
   [to     : Account]
   [amount : Integer]))

(define-type Ledger (U 'LNil LCons))
(define-struct LCons
  ([first : Transaction]
   [rest  : Ledger]))

(define ACCOUNT1 (Account 'Lake 1234))
(define ACCOUNT2 (Account 'Left 4321))
(define ACCOUNT3 (Account 'Right 5678))


(define TRANSACTION1 (Transaction ACCOUNT1 ACCOUNT3 1000))
(define TRANSACTION2 (Transaction ACCOUNT1 ACCOUNT2 1000))
(define TRANSACTION3 (Transaction ACCOUNT3 ACCOUNT2 1000))
(define TRANSACTION4 (Transaction ACCOUNT1 ACCOUNT2 2000))
(define TRANSACTION5 (Transaction ACCOUNT1 ACCOUNT2 -2000))

(define Ledger1 (LCons TRANSACTION1 (LCons TRANSACTION2 'LNil)))
(define Ledger2 (LCons TRANSACTION1 (LCons TRANSACTION1 (LCons TRANSACTION2 'LNil))))
(define Ledger3 (LCons TRANSACTION2 (LCons TRANSACTION4 'LNil)))
(define Ledger4 (LCons TRANSACTION1 (LCons TRANSACTION3 'LNil)))
(define Ledger5 (LCons TRANSACTION1 (LCons TRANSACTION5 'LNil)))
(define Ledger6 (LCons TRANSACTION4  'LNil))
(define Ledger7 (LCons TRANSACTION1 (LCons TRANSACTION2
                                           (LCons TRANSACTION4 (LCons TRANSACTION4 'LNil)))))
(define Ledger8 (LCons TRANSACTION4 (LCons TRANSACTION4 'LNil)))

(: money-string : Integer -> String)
;; given a number of cents, produce a string of the form "$12.34"
(define (money-string n)
  (local
    {(define a (number->string (* (abs n) 0.01)))}
  (cond
    [(< n 0) (string-append "(" "$" a ")")]
    [(> n 0) (string-append "$" a)]
    [else (string-append "$" "0.00")])))
(check-expect (money-string 0) "$0.00")
(check-expect (money-string -1) "($0.01)")
(check-expect (money-string 102) "$1.02")

(: same-bank? : Account Account -> Boolean)
;; return true if the accounts are at the same bank
(define (same-bank? a1 a2)
  (match* (a1 a2)
      [((Account b1 n1) (Account b2 n2 ))
       (symbol=? b1 b2)]))
      
(check-expect (same-bank? ACCOUNT1 ACCOUNT2) #f)
(check-expect (same-bank? ACCOUNT1 ACCOUNT1) #t)


(: account=? : Account Account -> Boolean)
;; equality test for accounts
(define (account=? a1 a2)
  (match* (a1 a2)
    [((Account b1 n1) (Account b2 n2 ))
     (and (symbol=? b1 b2) (= n1 n2))]))
(check-expect (account=?  ACCOUNT1 ACCOUNT2) #f)
(check-expect (account=?  ACCOUNT1 ACCOUNT1) #t)

(: from-bank? : Bank Transaction -> Boolean)
;; return true if the transaction originated at (is "from") the given bank
(define (from-bank? B t)
  (match t
    [(Transaction from _ _)
     (match from
       [(Account b _)
        (symbol=? b B)])]))
(check-expect (from-bank? 'Left TRANSACTION1) #f)
(check-expect (from-bank? 'Lake TRANSACTION1) #t)

(: to-bank? : Bank Transaction -> Boolean)
;; return true if the transaction originated at (is "to") the given bank
(define (to-bank? B t)
  (match t
    [(Transaction _ to _)
     (match to
       [(Account b _)
        (symbol=? b B)])]))
(check-expect (to-bank? 'Right TRANSACTION1) #t)
(check-expect (to-bank? 'Lake TRANSACTION2) #f)

(: trx-volume : Ledger -> Integer)
;; count the number of transactions on the ledger   
(define (trx-volume l)
  (match l
    ['LNil 0]
    [(LCons _ sub) (add1 (trx-volume sub))]))
(check-expect (trx-volume Ledger1) 2)

(: trx-total : Ledger -> Integer)
;; return the total amount of money (in cents) involved in all transactions
(define (trx-total l)
  (match l
    ['LNil 0]
    [(LCons first sub)
     (match first
       [(Transaction _ _ amount)
        (cond
          [(<= amount 0) (error "Transaction amount must be positive")]
          [else (+ amount (trx-total sub))])])]))
 (check-expect (trx-total Ledger1) 2000) 
(check-error (trx-total Ledger5) "Transaction amount must be positive")

(: trx-from-account : Account Ledger -> Ledger)
;; return all transactions that originate (are "from") the given account

(define (trx-from-account a l)
  (match l
    ['LNil 'LNil]
    [(LCons first sub)
      (match first
        [(Transaction a2 _ _)
         (if (account=? a2 a) (LCons first (trx-from-account a sub))
             (trx-from-account a sub))])]))
  
(check-expect (trx-from-account ACCOUNT1 Ledger1) Ledger1)
(check-expect (trx-from-account ACCOUNT3 Ledger1) 'LNil)

(: trx-from-bank : Bank Ledger -> Ledger)
;; return all transactions that originate (are "from") the given bank
(define (trx-from-bank B l)
  (match l
    ['LNil 'LNil]
    [(LCons first sub)
     ( if (from-bank? B first) (LCons first (trx-from-bank B sub))
          (trx-from-bank B sub))]))

(check-expect (trx-from-bank 'Lake Ledger1) Ledger1)          
(check-expect (trx-from-bank 'Right Ledger1) 'LNil)


(: ledger-net : Bank Ledger -> Integer)
;; Computes the net amount of money that flows in or out of the given bank on the whole ledger.
(define (ledger-net B l)
  (match l
    ['LNil 0]
    [(LCons first sub)
     (match first
       [(Transaction from to amount)
        (cond
          [(from-bank? B first) (- (ledger-net B sub) amount)]
          [(to-bank? B first) (+ (ledger-net B sub)  amount )]
          [else (ledger-net B sub)])])]))
    

(check-expect (ledger-net 'Lake Ledger1) -2000)
(check-expect (ledger-net 'Right Ledger3) 0)
(check-expect (ledger-net 'Right Ledger1) 1000)

(: at-or-above : Integer Ledger -> Ledger)
;; return all transactions whose amount is greater than or
;; equal to the given threshold
(define (at-or-above n l)
  (match l
    ['LNil 'LNil]
    [(LCons first sub)
     (match first
       [(Transaction _ _ amount)
        (cond
          [(< amount 0) (error "Transaction amnount must be positive")]
          [(>= amount n) (LCons first  (at-or-above n sub))]
          [else (at-or-above n sub)])])]))
                        
(check-expect (at-or-above 500 Ledger1) Ledger1)
(check-expect (at-or-above 1200 Ledger3) Ledger6)
(check-error (at-or-above 1000 Ledger5) "Transaction amnount must be positive")
(check-expect (at-or-above 20000 Ledger1) 'LNil)

       
(: largest : Ledger -> Ledger)
;; compute the ledger of zero or more of the transactions
;; whose transaction amount is equal to the largest transaction
;; amount on the ledger
(define (largest l)
  (match l
    ['LNil 'LNil]
    [(LCons first sub)
     (match (largest sub)
       ['LNil (LCons first 'LNil)]
       [(LCons first1 sub1)
          (if (>= (Transaction-amount first) (Transaction-amount first1))
              (LCons first (largest sub)) (largest sub))])]))
(check-expect (largest Ledger7) Ledger8)
    















{test}