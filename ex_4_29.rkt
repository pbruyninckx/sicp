#lang racket
;; Function that always makes quite a difference despite always returning 1:
(define (twice n) (+ n n))
(define (2-pow x n)
  (if (= n 0)
      1
      (2-pow (twice x) (- n 1))))


;;
(define count 0)
(define (id x) (set! count (+ count 1)) x)

(define (square x) (* x x))

;;; L-Eval input:
(square (id 10))

;;; L-Eval value:
;with memmoization:
100
;without memoization:
100

;;; L-Eval input:
count

;;; L-Eval value:
;with memmoization:
1
;without memoization:
2


(define (twice n) (+ n n))
(define (2-pow x n)
  (if (= n 0)
      1
      (2-pow (twice x) (- n 1))))
