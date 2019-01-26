#lang sicp

(#%require "series.rkt")

(define (integral integrand initial-value dt)
  (define int
    (cons-stream 
     initial-value
     (add-streams (scale-stream integrand dt)
                  int)))
  int)

; Ex 3.73

(define (RC R C dt)
  (define (f i v0)
    (add-streams
      (scale-stream i R)
      (integral (scale-stream i (/ 1.0 C)) v0 dt)))
  f)

; Ex 3.74
(define (sign-change-detector a b)
  (cond ((< a 0 b) -1)
        ((> a 0 b) 1)
        (else 0)))

(define sense-data (make-stream '(1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4)))

(define (make-zero-crossings
         input-stream last-value)
  (cons-stream
   (sign-change-detector 
    (stream-car input-stream) 
    last-value)
   (make-zero-crossings 
    (stream-cdr input-stream)
    (stream-car input-stream))))

(define zero-crossings 
  (make-zero-crossings sense-data 0))

(define zero-crossings2
  (stream-map sign-change-detector
              sense-data
              (cons-stream 0 sense-data)))

; Ex 3.75
(define (make-zero-crossings-smooth 
         input-stream last-value last-last-value)
  (let ((avpt1
         (/ (+ last-value last-last-value)
            2))
        (avpt2 
         (/ (+ (stream-car input-stream) 
               last-value) 
            2)))
    (cons-stream 
     (sign-change-detector avpt2 avpt1)
     (make-zero-crossings-smooth
      (stream-cdr input-stream) (stream-car input-stream) last-value))))

;  Ex 3.76
(define (smooth x)
  (scale-stream
   (add-streams x (stream-cdr x))
   .5))
(define (make-zero-crossings-smooth-modular input-stream)
  (let ((smooth-stream (smooth input-stream)))
    (stream-map sign-change-detector
                smooth-stream
                (cons-stream 0 smooth-stream))))