#lang racket

(require "helpers.rkt")

; Ex 2.39

(define (reverse-fr sequence)
  (fold-right
   (lambda (x y) (append y (list x))) nil sequence))

(define (reverse-fl sequence)
  (fold-left
   (lambda (x y) (cons y x)) nil sequence))

(reverse-fr '(1 2 3 4))

(reverse-fl '(1 2 3 4))