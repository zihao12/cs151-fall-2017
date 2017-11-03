#lang typed/racket
(require typed/test-engine/racket-tests)

(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")

(require "optional.rkt")
(require "loc.rkt")
(require "chess-logic.rkt")
(require "lab2.rkt")
;; ==== ==== ==== ====
;; external interface

;; (provide board->image ; : Board -> Image）

♔♕♖♗♘♙♚♛♜♝♞♟

(define background (alt-shaded-rows 8 8 56 "beige" "brown"))
background
;(: board->image : Board -> Image)
;;; draw a board with pieces
;(define 
;         