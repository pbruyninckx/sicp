#lang racket

; Code from the book
(define (rand)
  (random 0 1000000000))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials
                          cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1)
                 trials-passed))))
  (iter trials 0))

; Ex 3.05
(define (random-range low high)
  (+ low (* (random) (- high low))))
(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (experiment)
    (let ((x (random-range x1 x2))
          (y (random-range y1 y2)))
      (P x y)))
  (* (monte-carlo trials experiment)
     (* (- x2 x1) (- y2 y1))))
(define (in-unit-circle x y)
  (< (+ (* x x) (* y y)) 1.0))