#lang typed/racket

(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")

;; --- John Hancock Center ---
;; 344 m tall
;; with antennas, 457 m (so antennas are 113 m)
;; JHC dimensions from http://en.wikipedia.org/wiki/John_Hancock_Center
;; In the following named constants, the scale is 1 meter per pixel.

(: jhc-mid-h Integer)
;; middle part height
(define jhc-mid-h 339)

(: jhc-mid-w Integer)
;; middle part width
(define jhc-mid-w 44)

(: jhc-tri-w Integer)
;; triangle width
(define jhc-tri-w 12)
  
(: jhc-top-h Integer)
;; topper height
(define jhc-top-h 5)
  
(: jhc-top-w Integer)
;; topper width
(define jhc-top-w 37)
  
(: jhc-ant-h Integer)
;; antenna height
(define jhc-ant-h 113)
  
(: jhc-middle Image)
(define jhc-middle
  (rectangle jhc-mid-w jhc-mid-h "solid" "black"))
  
(: jhc-rside Image)
(define jhc-rside
  (right-triangle jhc-tri-w jhc-mid-h "solid" "black"))
  
(: jhc-lside Image)
(define jhc-lside
  (flip-horizontal jhc-rside))
  
(: jhc-topper Image)
(define jhc-topper
  (rectangle jhc-top-w jhc-top-h "solid" "black"))
  
(: jhc-antenna Image)
(define jhc-antenna
  (rectangle 1 jhc-ant-h "outline" "gray"))


(define jhc  (above (beside/align "baseline" jhc-antenna (beside/align "baseline" jhc-topper jhc-antenna) ) (beside (beside jhc-lside jhc-middle) jhc-rside)))