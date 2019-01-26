#lang sicp

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))


(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ;retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))
(define (clear! cell)
  (set-car! cell false))

; Doesn't work as it needs atomic operations
(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))

; Ex 3.46: example
; A                B
; (test-and-set! c)        (test-and-set! c)
; (car cell)->false
;                          (car cell) -> false
;                          (set-car! cell true)
;                          false
; (set-car cell true)
; false
;
; -> both operations got false and manage to set the cell to true.