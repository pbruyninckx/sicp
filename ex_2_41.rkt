#lang racket

(require "helpers.rkt")

(define (sum l)
  (foldl + 0 l))

(define (sum-triplets n s)
  (map
   (lambda (l) (list (cadr l) (car l) (- s (sum l))))
   (filter
    (lambda (l)
      (let ((k (- s (sum l))))
        (and (> k (car l))
             (<= k n))))
    (unique-pairs n))))