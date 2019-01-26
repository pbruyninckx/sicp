#lang racket

(define (make-accumulator s)
  (lambda (inc)
    (begin
      (set! s (+ s inc))
      s)))