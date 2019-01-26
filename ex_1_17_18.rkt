#lang sicp

(define (double x)
  (+ x x))
(define (halve x)
  (/ x 2))

; 1.17
(define (fast-mult a b)
  (cond ((= b 0) 
         0)
        ((even? b) 
         (fast-mult (double a) (halve b)))
        (else 
         (+ a (fast-mult a (dec b))))))

; 1.18
(define (fast-it-mult a b)
  (define (helper acc a b)
    (cond ((= b 0) acc)
          ((even? b)
           (helper acc (double a) (halve b)))
          (else
           (helper (+ acc a) a (dec b)))))
  (helper 0 a b))