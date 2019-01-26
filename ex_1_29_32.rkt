#lang sicp

; ex 1.30
(define (sum term a next b)
  (define (sum-iter acc a)
    (if (> a b)
        acc
        (sum-iter (+ acc (term a)) (next a))))
  (sum-iter 0 a))


(define (cube x)
  (* x x x))
; ex 1.29

(define (integral-simpson f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (simpson-term k)
    (cond
      ((or (= k 0) (= k n)) (y k))
      ((odd? k) (* 4 (y k)))
      (else (* 2 (y k)))))
  (* (/ h 3.0) (sum simpson-term 0 inc n)))

; ex 1.31

(define (simple-factorial x)
  (if (= x 1)
      1
      (* x (simple-factorial (dec x)))))

(define (product term a next b)
  (define (product-iter a result)
    (if (> a b)
        result
    (product-iter (next a) (* result (term a)))))
  (product-iter a 1))

(define (prod-recursive term a next b)
  (if (> a b)
      1
      (* (term a) (prod-recursive term (next a) next b))))

(define (factorial x)
  (product identity 1 inc x))
(define (square x)
  (* x x))

;(exact->inexact (* 4 (product (lambda (x) (/ (* (* 2. x) (* 2. (inc x))) (square (inc (* 2. x))))) 1 inc 2000002)))

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (accumulate-rec combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate-rec combiner null-value term (next a) next b))))

; Ex 1.33
(define (filtered-accumulate combiner null-value term a next b filter)
  (define (iter a result)
    (if
      (> a b) result
      (iter (next a)
            (if (filter a)
                (combiner result (term a))
                result))))
  (iter a null-value))

; - copied from the book -
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) 
         n)
        ((divides? test-divisor n) 
         test-divisor)
        (else (find-divisor 
               n 
               (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (sum-squared-primes a b)
  (filtered-accumulate + 0 square a inc b prime?))

(define (prod-rel-primes n)
  (filtered-accumulate * 1 identity 1 inc (dec n) (lambda (x) (= (gcd x n) 1))))