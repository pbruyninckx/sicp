#lang racket

(define (make-monitored f)
  (let ((counter 0))
    (define (reset-count)
      (set! counter 0))
    (define (how-many-calls)
      counter)
    (define (call-function arg)
      (begin
        (set! counter (+ counter 1))
        (f arg)))
    (lambda (arg)
      (cond ((eq? arg 'how-many-calls?)
             (how-many-calls))
            ((eq? arg 'reset-count)
             (reset-count))
            (else
             (call-function arg))))))