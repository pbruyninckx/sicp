#lang racket


(provide
 square
 accumulate
 fold-left
 fold-right
 nil
 enumerate-interval
 flatmap
 prime?
 inc
 dec
 unique-pairs)

(define (inc x)
  (+ x 1))

(define (dec x)
  (- x 1))

(define (square x) (* x x))

(define (accumulate op initial sequence)
    (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define nil '())

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)

(define (enumerate-interval a b)
  (range a (inc b)))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

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

(define (unique-pairs n)
  (flatmap (lambda (i)
         (map (lambda (j)
                (list j i))
              (enumerate-interval (inc i) n)))
       (enumerate-interval 1 (dec n))))
