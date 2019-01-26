#lang sicp

; Exercise 3.47

; Semaphore from mutex

; Part 1
(define (make-semaphore size)
  (let ((mutex (make-mutex)))
    (define (inc-sem)
      (mutex 'acquire)
      (set! size (+ size 1))
      (mutex 'release))
    (define (dec-sem)
      (mutex 'acquire)
      (cond ((> size 0)
             (set! size (- size 1))
             (mutex 'release))
            (else
             (mutex 'release)
             (dec-sem)))) ;try again
             
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (dec-sem))
            ((eq? m 'release) (inc sem))))
    the-semaphore))


; Part 2
(define (make-semaphore size)
  (let ((cnt (list size)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! size)
                 (the-mutex 'acquire))) ;retry
            ((eq? m 'release) (increase! size))))
    the-mutex))
; This one is also assumed to be atomic
(define (increase! size)
  (set-car! size (+ 1 (car size))))
; Doesn't work as it needs atomic operations
(define (test-and-set! cell)
  (if (= 0 (car cell))
      true
      (begin (set-car! cell (- (car cell) 1))
             false)))