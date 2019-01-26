#lang racket

(require "helpers.rkt")

(define (unique-pairs n)
  (flatmap (lambda (i)
         (map (lambda (j)
                (list i j))
              (enumerate-interval (inc i) n)))
       (enumerate-interval 1 (dec n))))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) 
        (cadr pair) 
        (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter 
        prime-sum?
        (unique-pairs n))))