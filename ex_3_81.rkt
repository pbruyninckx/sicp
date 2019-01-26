#lang sicp

(#%require "series.rkt")

(define (rand-update x)
  (modulo (* x 279470273) 4294967291))

(define random-init 42)

(define random-numbers
  (cons-stream random-init
               (stream-map rand-update
                           random-numbers)))

(define random-probs
  (scale-stream
   random-numbers
   (/ 1.0 4294967291)))

(define (rand reqs)
  (define (my-rand-update x req)
    (cond ((eq? (car req) 'generate)
           (cons true (rand-update (cdr x))))
          ((eq? (car req) 'reset)
           (cons false (cadr req)))))
  (define full-rand
    (cons-stream (cons false random-init)
                 (stream-map my-rand-update
                             full-rand
                             reqs)))
  (scale-stream
   (stream-map cdr
               (stream-filter car full-rand))
   (/ 1.0 4294967291)))

; (display-stream (rand (make-stream '((generate) (generate) (reset 10) (generate) (reset 10) (generate) (generate)))) 4)


; Ex 3.82
(define (estimate-integral P x1 x2 y1 y2)
  (let (( randx
    (stream-map
     (lambda (x) (+ x1 (* x (- x2 x1))))
     (rand (cons-stream '(reset 10) (repeat-val '(generate))))))
  (randy
    (stream-map
     (lambda (y) (+ y1 (* y (- y2 y1))))
     (rand (cons-stream '(reset 1234567) (repeat-val '(generate)))))))
  (define estimates
    (stream-map P randx randy))
  (stream-map
   /
   (partial-sums estimates)
   integers)))

(define (in-circle x y)
  (if (< (+ (* x x) (* y y)) 1)
      1.0
      0.0))