#lang racket

(define f
  (let ((n 0))
    (define (count)
      (set! n (+ n 1)))
    (lambda (x)
      (let ((orig-n n))
        (begin
          (count)
          (if (= x orig-n)
              x
              0))))))
