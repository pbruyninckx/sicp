#lang racket
; Ex 4.28

;;; L-eval input:
(define (double f) (lambda (x) (f (f x))))

;;; L-eval value:
ok

;;; L-eval input:
(define (inc x) (+ x 1))

;;; L-eval value:
ok

;;; L-eval input:
((double inc) 0)

;;; L-eval value:
2
; When using 'eval' instead of actual-value, inc will be a thunk, and not be recognised as being a function