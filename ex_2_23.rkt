#lang sicp
(#%require r5rs/init)

(define (for-each f l)
  (cond ((not (null? l))
         (f (car l))
         (for-each f (cdr l)))))