#lang racket

(require "helpers.rkt")

; From ex 2.36
(define  (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

; ex 2.37

(define m
  '((1 2 3 4)
    (5 6 7 8)
    (9 10 11 12)))
(define n
  '((2 3 5 7)
    (11 13 17 19)
    (21 23 29 31)
    (37 41 43 47)))
(define v
  '(2 3 5 7))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))

;(51 119 187)
(matrix-*-vector m v)

(transpose m)

;    235    262    298    326
;    519    582    674    742
;    803    902   1050   1158
(matrix-*-matrix m n)