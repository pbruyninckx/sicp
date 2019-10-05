#lang racket
; What would the lazy evaluator do?
(define count 0)
(define (id x) (set! count (+ count 1)) x)

(define w (id (id 10)))

;;; L-Eval input:
count

;;; L-eval value:
; I thought this would be 0, but when defining w, id gets executed once.
1

;;; L-eval input:
w

;;; L-eval value:
10

;;; L-eval input:
count


;;; L-eval value:
2