#lang sicp
(#%require r5rs/init)

(define (fringe t)
  (define (helper t acc)
    (cond ((null? t) acc)
          ((pair? (car t)) (helper (cdr t) (helper (car t) acc)))
          (else (helper (cdr t) (cons (car t) acc)))))
  (reverse (helper t '())))