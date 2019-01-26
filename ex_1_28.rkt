#lang sicp
(#%require (only racket/base random))

; Predefined methods
(define (square x)
  (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (next test-divisor)
  (if (= test-divisor 2)
      3
      (+ test-divisor 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) 
         n)
        ((divides? test-divisor n) 
         test-divisor)
        (else (find-divisor 
               n 
               (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

; 1.28

; Adapted expmod
(define (expmod base exp m)
  (define (non-trivial-square n)
    (if (and
         (not (= 1 n))
         (not (= n (- m 1)))
         (= (remainder (square n) m) 1))
        0
        (square n)))
  (cond
    ((= base 0) 0)
    ((= exp 0) 1)
    ((even? exp)
     (remainder (non-trivial-square (expmod base (/ exp 2) m)) m))
    (else (remainder (* base (expmod base (dec exp) m)) m))))

; Miller Rabin test adapted from fermat-test
(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 2)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) 
         (fast-prime? n (- times 1)))
        (else false)))

; Test wether fast-prime and prime act the same

(define (test-primes n)
  (define (test-prime iter)
    (cond
      ((> iter n) (display "Works"))
      ((not (equal? (fast-prime? iter (max 20 iter)) (prime? iter)))
           (display iter))
      (else (test-prime (inc iter)))))
  (test-prime 3))