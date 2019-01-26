#lang sicp
(#%require r5rs/init)

(define (deep-reverse l)
  (define (iter l acc)
    (cond ((null? l) acc)
          ((pair? (car l)) (iter (cdr l) (cons (deep-reverse (car l)) acc)))
          (else (iter (cdr l) (cons (car l) acc)))))
  (iter l '()))