#lang racket

(define (unless condition normal-value exceptional-value)
  (if conditional
      exceptional-value
      normal-value))

; Friendly division always errors with normal order evaluation
(define (friendly-div a b)
  (unless (= b 0)
    (/ a b)
    (error "Please try not to divide by 0")))

; Factorial always recurses indefinitely with normal order evaluation
(define (factorial n)
  (unless (= n 0)
    (* n (factorial (- n 1)))
    1))