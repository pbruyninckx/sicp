#lang racket

(define (square x)
  (* x x))

(define (larger-square a b c)
  (- (+ (square a) (square b) (square c))
     (square (min a b c))))


(define (inc x)
  (+ x 1))

(define (dec x)
  (- x 1))



(define (average x y)
  (/ (+ x y) 2))

(define (** x y)
  (cond
    ((= y 0) 1)
    ((= y 1) x)
    (else (* x (** x (dec y))))))
    

(define (sqrt x)
  (define (improve guess)
    (average guess (/ x guess)))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.01))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
      guess
      (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))


(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))
(define (k n) (* 5 n n))

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1 ) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))
