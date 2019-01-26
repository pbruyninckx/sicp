#lang sicp
; Ex 2.6


(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))


;(add-1 zero)
;(add-1 (lambda (f) (lambda (x) x)))
;(lambda (f1) (lambda (x1) (f1 (((lambda (f) (lambda (x) x)) f1) x1))))
;(lambda (f1) (lambda (x1) (f1 ((lambda (x) x) x1))))
;(lambda (f) (lambda (x) (f x)))
(define one (lambda (f) (lambda (x) (f x))))

;(add-1 one)
;(add-1 (lambda (f1) (lambda (x1) (f1 x1))))
;(lambda (f) (lambda (x) (f (((lambda (f1) (lambda (x1) (f1 x1))) f) x))))
;(lambda (f) (lambda (x) (f ((lambda (x1) (f x1)) x))))
;(lambda (f) (lambda (x) (f (f x))))
(define two
  (lambda (f) (lambda (x) (f (f x)))))


(define (add a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

((zero inc) 0)
((one inc) 0)
((two inc) 0)
(((add two (add-1 two)) inc) 0)