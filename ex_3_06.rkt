#lang racket

(define random-init 42)
(define (rand-update x)
  (modulo (* x 279470273) 4294967291))

(define rand
  (let ((x random-init))
    (lambda (m)
      (cond
        ((eq? m 'reset)
         (lambda (new-value)
           (set! x new-value)))
        ((eq? m 'generate)
         (begin
           (set! x (rand-update x))
           (/ x 4294967291.)))))))