#lang typed/racket
(require typed/test-engine/racket-tests)
(require "../include/cs151-core.rkt")

(: mobile-broadband-cost (Integer -> Integer))
;;compute singular wireless charges given number of megabytes
(define (mobile-broadband-cost m)
  (cond
    [(<= m 300) 20]
    [(< 300 m 3072) 30]
    [else
     (+ 30
     (* 15
     (exact-ceiling (/ (- m 3072) 1024)))
     )]
    ))
(check-expect (mobile-broadband-cost 5000) 60)
(check-expect (mobile-broadband-cost 300) 20)
(check-expect (mobile-broadband-cost 3072) 30)


;; problem 2
(: contains-digit? (Integer Integer-> Boolean))
;;see if a given integer contains a given integer
(: define (contains-digit? N d)
   ( cond
      [(= 0 (remainder (- N (* d 10)) 10))
       #t]
      [else
       (contains-digit?
       (exact-ceiling (/ N 10))
        d)
       ]
))











  (test)