#lang racket

(define (reverse l)
  (define (helper l acc)
    (if (null? l)
        acc
        (helper (cdr l) (cons (car l) acc))))
  (helper l '()))
