#lang sicp

(define (square x)
  (* x x))

(define (fast-expt b n)
  (cond ((= n 0) 
         1)
        ((even? n) 
         (square (fast-expt b (/ n 2))))
        (else 
         (* b (fast-expt b (- n 1))))))

(define (fast-it-expt b n)
  (define (helper a b n)
    (cond ((= n 0) a)
          ((even? n)
           (helper a (square b) (/ n 2)))
          (else
           (helper (* a b) b (dec n)))))
  (helper 1 b n))