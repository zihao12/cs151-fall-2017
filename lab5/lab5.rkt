#lang typed/racket
(require typed/test-engine/racket-tests)
(require "../include/cs151-image.rkt")
(require "../include/cs151-core.rkt")

(define-type Bank
  (U 'Lake 'Left 'Right))

(define-struct Account
  ([bank   : Bank]
   [number : Integer]))

(define-struct Trx
  ([from   : Account]
   [to     : Account]
   [amount : Integer]))

(define-type Ledger
  (Listof Trx))

(define ACCOUNT1 (Account 'Lake 1234))
(define ACCOUNT2 (Account 'Left 4321))
(define ACCOUNT3 (Account 'Right 5678))
(define ACCOUNT4 (Account 'Lake 5678))

(define TRANSACTION1 (Trx ACCOUNT1 ACCOUNT3 1000))
(define TRANSACTION2 (Trx ACCOUNT1 ACCOUNT2 1000))
(define TRANSACTION3 (Trx ACCOUNT3 ACCOUNT2 1000))
(define TRANSACTION4 (Trx ACCOUNT1 ACCOUNT2 2000))
(define TRANSACTION5 (Trx ACCOUNT1 ACCOUNT2 -2000))
(define TRANSACTION6 (Trx ACCOUNT1 ACCOUNT1 5000))
(define TRANSACTION7 (Trx ACCOUNT1 ACCOUNT2 5000))
(define TRANSACTION8 (Trx ACCOUNT1 ACCOUNT4 5000))
(define TRANSACTION9 (Trx ACCOUNT2 ACCOUNT1 5000))

(define Ledger1 (list TRANSACTION1 TRANSACTION2))
(define Ledger2 (list TRANSACTION1 TRANSACTION2 TRANSACTION3))
(define Ledger3 (list TRANSACTION1 TRANSACTION2 TRANSACTION5))
(define Ledger4 (list TRANSACTION1 TRANSACTION2 TRANSACTION6))
(define Ledger5 (list TRANSACTION1 TRANSACTION2 TRANSACTION4))
(define Ledger6 (list TRANSACTION1 TRANSACTION2 TRANSACTION3 TRANSACTION6 TRANSACTION7 TRANSACTION4))

(define Ledger7 (list TRANSACTION1 TRANSACTION2 TRANSACTION8
                      TRANSACTION6 TRANSACTION9 TRANSACTION5))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;helper functions
(: account=? : Account Account -> Boolean)
;; equality test for accounts
(define (account=? a1 a2)
  (match* (a1 a2)
    [((Account b1 n1) (Account b2 n2 ))
     (and (symbol=? b1 b2) (= n1 n2))]))
(check-expect (account=?  ACCOUNT1 ACCOUNT2) #f)
(check-expect (account=?  ACCOUNT1 ACCOUNT1) #t)

(: from-bank? : Bank Trx -> Boolean)
;; return true if the transaction originated at (is "from") the given bank
(define (from-bank? B t)
  (match t
    [(Trx from _ _)
     (match from
       [(Account b _)
        (symbol=? b B)])]))
(check-expect (from-bank? 'Left TRANSACTION1) #f)
(check-expect (from-bank? 'Lake TRANSACTION1) #t)

(: to-bank? : Bank Trx -> Boolean)
(define (to-bank? B t)
  (match t
    [(Trx _ to _)
     (match to
       [(Account b _)
        (symbol=? b B)])]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;lab problems
(: trx-total : Ledger -> Integer)
;; hint: fold
;; compute the total amount of money in all transactions
(define (trx-total l)
  (foldl
   (lambda ([t : Trx] [n : Integer])(+ (Trx-amount t) n))
   0 l))
(check-expect (trx-total Ledger1) 2000)

(: trx-from-account : Account Ledger -> Ledger)
;; hint: filter
;; gather all transactions from the given account
(define (trx-from-account a l)
  (filter
   (lambda ([t : Trx])(or (account=? (Trx-from t) a) (account=? (Trx-to t) a)))
   l))
(check-expect (trx-from-account ACCOUNT3 Ledger2) (list TRANSACTION1 TRANSACTION3))


(: trx-from-bank : Bank Ledger -> Ledger)
;; hint: filter
;; gather all transactions from the given bank
(define (trx-from-bank b l)
  (filter
   (lambda ([t : Trx])(from-bank? b t))
   l))
(check-expect (trx-from-bank 'Right Ledger2) (list TRANSACTION3))

(: count-from-account : Account Ledger -> Integer)
;; hint: fold
;; count all transactions that originate from account
(define (count-from-account a l)
  (trx-total (trx-from-account a l)))
(check-expect (count-from-account ACCOUNT1 Ledger2) 2000)

(: trx-to-bank : Bank Ledger -> Ledger)
;; helper function
;; symmetric to trx-from-bank
(define (trx-to-bank b l)
  (filter
   (lambda ([t : Trx])(to-bank? b t))
   l))

(: count-to-bank : Bank Ledger -> Integer)
;; hint: fold
;; count all transactions that flow into given bank
(define (count-to-bank b l)
  (trx-total (trx-to-bank b l)))
(check-expect (count-to-bank 'Right Ledger1) 1000) 

(: verify : Ledger -> Boolean)
;; hint: andmap (see above for examples)
;; verify no trx has nonpositive amount and no trx goes to/from same account
(define (verify l)
  (and 
  (andmap
    (lambda ([t : Trx])(positive? (Trx-amount t))) l)
  (andmap
    (lambda ([t : Trx])(not(account=? (Trx-from t) (Trx-to t)))) l)))
;;Why (andmap (and oper1 oper2)) fails?
(check-expect (verify Ledger1) #t)
(check-expect (verify Ledger3) #f)
(check-expect (verify Ledger4) #f)

(: ledger-net : Bank Ledger -> Integer)
;; hint: fold
;; determine the net amount flowing into (positive) or 
;; out of (negative) bank given the ledger
(define (ledger-net b l)
  (foldl
   (lambda ([t : Trx] [pre : Integer])
     (cond
       [(from-bank? b t) (- pre (Trx-amount t))]
       [(to-bank? b t) (+ pre (Trx-amount t))]
       [else pre]))
   0 l))
(check-expect (ledger-net 'Right Ledger2)0)
(check-expect (ledger-net 'Lake Ledger1) -2000)

(: at-or-above : Integer Ledger -> Ledger)
;; hint: filter
;; return all transactions whose amount is >= the given threshold
(define (at-or-above n l)
  (filter
   (lambda ([t : Trx]) (>= (Trx-amount t) n))
   l))
(check-expect (at-or-above 1500 Ledger5) (list TRANSACTION4))

(: largest : Ledger -> Ledger)
;; match and return empty list for empty argument, fold nonempty
;; compute the ledger of zero or more of trxs whose amount 
;; is equal to the largest amount on the ledger
(define (largest l)
  (match l
    ['() '()]
    [(cons f r)
     (foldl
      (lambda ([challenger : Trx] [candidate : Ledger])
        (cond
          [(> (Trx-amount (list-ref candidate 0)) (Trx-amount challenger))
             candidate]
          [(= (Trx-amount (list-ref candidate 0)) (Trx-amount challenger))
             (append candidate (list challenger))]
          [else (list challenger)]))
      (list f)
      r)]))

(check-expect (largest Ledger6) (list TRANSACTION6 TRANSACTION7))

;; ==== ==== ==== ====

;; a report consists of a bank,
;; all transactions that flow into the bank from an external source,
;; all transactions internal to the bank,
;; all transactions that flow out of the bank to an external bank,
;; all transactions that involve the bank and are not verifiable
;; (per verify above)
(define-struct Report
  ([bank     : Bank]
   [inflow   : Ledger]
   [internal : Ledger]
   [outflow  : Ledger]
   [errors   : Ledger]))
(define Report1 (Report 'Lake (list TRANSACTION9) (list TRANSACTION8 ) (list TRANSACTION1 TRANSACTION2 )
                        (list TRANSACTION6 TRANSACTION5)))

(: error? : Trx -> Boolean)
;; see if a transaction has an error
(define (error? t)
  (or (<= (Trx-amount t) 0)
      (account=? (Trx-from t) (Trx-to t))))

(: inflow-trx : Bank Ledger -> Ledger)
;; get the inflow transaction of a given bank
(define (inflow-trx b l)
  (filter
   (λ ([tx : Trx])(and (not (error? tx)) (to-bank? b tx) (not (from-bank? b tx))))
   l))
(check-expect (inflow-trx 'Lake Ledger6) '())

(: outflow-trx : Bank Ledger -> Ledger)
;; get the outflow transaction of a given bank
(define (outflow-trx b l)
  (filter
   (λ ([tx : Trx])(and (not (error? tx)) (from-bank? b tx) (not (to-bank? b tx))))
   l))
(check-expect (outflow-trx 'Lake Ledger6)
              (list TRANSACTION1 TRANSACTION2  TRANSACTION7 TRANSACTION4))

(: internal : Bank Ledger -> Ledger)
;; get the internal transaction of a given bank
(define (internal b l)
  (filter
   (λ ([tx : Trx])(and (not (error? tx)) (to-bank? b tx) (from-bank? b tx)))
   l))
(check-expect (internal 'Lake (list TRANSACTION8 TRANSACTION1))(list TRANSACTION8))

(: errors : Bank Ledger -> Ledger)
;;get all the invalid transactions of a bank
(define (errors b l)
  (filter error? l))
(check-expect (errors 'Lake Ledger3) (list TRANSACTION5))

(: build-report : Bank Ledger -> Report)
; you can write this as four calls to filter,
; or, for a challenge, as one fold with a rather involved custom operator
(define (build-report b l)
  (Report
   b
   (inflow-trx b l)
   (internal b l)
   (outflow-trx b l)
   (errors b l)))
(check-expect (build-report 'Lake Ledger7) Report1)

(test)                   