#lang racket

; Exercise 1.11

(define (f-rec n)
  (if (< n 3)
      n
      (+ (f-rec (- n 1)) (* 2 (f-rec (- n 2))) (* 3 (f-rec (- n 3))))))

(define (inc x)
  (+ x 1))

(define (dec x)
  (- x 1))

(define (f-it n)
  (define (f-helper f0 f1 f2 ind)
    (if (= ind n)
        f2
        (f-helper f1 f2 (+ f2 (* 2 f1) (* 3 f0)) (inc ind))))
  (if (< n 3)
      n
      (f-helper 0 1 2 2)))


; Exercise 1.12
(define (p row col)
  (if (or (= row 1) (= col 1) (= row col))
      1
      (+ (p (dec row) (dec col)) (p (dec row) col))))

; Compute change - from 1.2.2
(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) 
             (= kinds-of-coins 0)) 
         0)
        (else 
         (+ (cc amount (- kinds-of-coins 1))
            (cc (- amount (first-denomination 
                           kinds-of-coins))
                kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))