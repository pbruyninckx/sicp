#lang racket

(require "helpers.rkt")

; Ex 2.38

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(fold-right / 1 '(1 2 3))
;3/2

(fold-left / 1 '(1 2 3))
;1/6

(fold-right list nil '(1 2 3))
;'(1 (2 (3 ())))

(fold-left list nil '(1 2 3))
;'(((() 1) 2) 3)

;(= (op a b) (op b a))