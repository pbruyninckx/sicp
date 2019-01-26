#lang racket

; Ex 2.35

(require "helpers.rkt")

(define (count-leaves t)
  (accumulate
   +
   0
   (map
    (lambda (el)
      (cond ((pair? el) (count-leaves el))
            ((null? el) 0)
            (else 1)))
    t)))

(define x (cons (list 1 2) (list 3 4)))
; 8
(count-leaves (list x x '()))
