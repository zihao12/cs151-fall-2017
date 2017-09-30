#lang typed/racket
(require typed/test-engine/racket-tests)
(require "../include/cs151-core.rkt")
(test)

(: square (Real -> Real))
;;Helper function that calculats square of a number
(define (square x)
  (* x x))
(check-expect(square 5) 25)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: c->f (Exact-Rational -> Exact-Rational))
;;convert degrees Celsius to degrees Fahrenheit
(define(c->f cel)
  (+(* cel 9/5) 32))

(check-expect (c->f 10) 50 )
(check-expect (c->f 0) 32 )

(: f->c (Exact-Rational -> Exact-Rational))
;;convert degree Fahrenheit to degrees Celsius
(define (f->c fah)
  (/(- fah 32) 9/5))
(check-expect (f->c 50) 10 )
(check-expect (c->f 32) 0 )
;; check-expect seems not to be working

(: eval-quadratic (Real Real Real Real -> Real))
;;calculate the value of function ax^2+bx+c
(define(eval-quadratic a b c x)
                       (+(* a x x) (* b x) c))
  
(check-expect (eval-quadratic 1 2 1 -1) 0)
(check-expect (eval-quadratic 1 6 9 -3) 0)

(: distance (Real Real Real Real -> Real))
;;calculate the distance between two points in Euclidean space
(define(distance x1 y1 x2 y2)
  (sqrt (+ (square (- x1 x2)) (square (- y1 y2)))))
(check-expect (distance 0 0 3 4) 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Problem 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: low-battery? (Real -> Boolean))
;;to see if it is low-battery
(define (low-battery? x)
  (<= x 0.2))
(check-expect (low-battery? 0.5) #f )
(check-expect (low-battery? 0.1) #t )

(: chicago-zip? (Real -> Boolean))
;; to decide if the zip code belongs to chicago
(define (chicago-zip? n)
  (< 60599 n 60700))
(check-expect (chicago-zip? 60637) #t)
(check-expect (chicago-zip? 60599) #f)

(: circle-contains? (Real Real Real Real Real -> Boolean))
; to deicide if a point is within a given circle
(define (circle-contains? x0 y0 r x y)
  (< (distance x0 y0 x y) r))
(check-expect (circle-contains? 0 0 3 4 5) #f)
(check-expect (circle-contains? 0 0 6 4 3) #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Problem 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: vector-add-x (Real Real Real Real -> Real ))
;;giving the x components of vectors resulting from the addition of two vectors
(define (vector-add-x x1 y1 x2 y2)
  (+ x1 x2))
(check-expect (vector-add-x 1 2 3 4) 4)

(: vector-add-y (Real Real Real Real -> Real ))
;;giving the y components of vecors resulting from the addition of two vectors
(define (vector-add-y x1 y1 x2 y2)
  (+ y1 y2))
(check-expect (vector-add-x 1 2 3 4) 6)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Problem 4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: lemons (Integer Integer -> Integer))
;; decide how many lemons you need for a certain number of adults and kids
(define (lemons x y)
  (exact-ceiling (+ (* 1/3 x) (* 1/4 y))))
(check-expect (lemons 3 5) 3)

(: lemons-in-bags (Integer Integer -> Integer))
;; return the number of lemons that is also a multiple of five
(define (lemons-in-bags x y)
  (+ (lemons x y) (- 5 (remainder (lemons x y) 5))))
(check-expect (lemons-in-bags 3 5) 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Problem 5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: offing-line-of-sight/meters (Real -> Real))
(define R 6371000)
;;Calculate the length of the distance from your eye to the farthest distance
(define (offing-line-of-sight/meters  h  )
  (sqrt (- (square (+ h R)) (square R))))
(check-expect (offing-line-of-sight/meters  0 ) 0)
(check-within (offing-line-of-sight/meters  1e3 ) 112884.89712977552 1e-15)

(: offing-ground-distance/meters (Real -> Real))
;;Calculate the length of the distance from your eye to the farthest distance
(define (offing-ground-distance/meters  h  )
  (* R (acos (/ R (+ h R)))))
(check-expect (offing-ground-distance/meters  0 ) 0)
(check-within (offing-ground-distance/meters  1e3 ) 112873.08605922057 1e-15)