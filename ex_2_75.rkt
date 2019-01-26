#lang racket

; Copied from book
(define (make-from-real-imag x y)
  (define (square v)
    (* v v))
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
           (error "Unknown op: 
            MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (apply-generic op arg) (arg op))

; Ex 2.75
(define (make-from-mag-ang m a)
  (define (dispath op)
    (cond ((eq? op 'real-part)
           (* m (cos a)))
          ((eq? op 'imag-part)
           (* m (sin a)))
          ((eq? op 'magnitude) m)
          ((eq? op 'angla) a)
          (else
           (error "Unknown op:
            MAKE-FROM-MAGN-ANG" op))))
  dispath)



; Ex 2.76
; Choose data-directed programming when new methods are added more often than more types,
; Choose message passing when the methods are constantg and new types are added often.