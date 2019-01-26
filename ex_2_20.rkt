#lang sicp
(#%require r5rs/init)

(define (same-parity p . l)
  (let ((keep? (if (even? p) even? odd?)))
    (define (helper l)
      (cond ((null? l) '())
            ((keep? (car l)) (cons (car l) (helper (cdr l))))
            (else (helper (cdr l)))))
    (cons p (helper l))))
            