#lang sicp

(#%require "series.rkt")

(define (integral
         delayed-integrand initial-value dt)
  (define int
    (cons-stream
     initial-value
     (let ((integrand
            (force delayed-integrand)))
       (add-streams
        (scale-stream integrand dt)
        int))))
  int)
;
;(define (solve f y0 dt)
;  (let ((y nil) (dy nil))
;    (set! y (integral (delay dy) y0 dt))
;    (set! dy (stream-map f y))
;    y))

; Ex 3.77

;(define (integral
;         delayed-integrand initial-value dt)
;  (cons-stream 
;   initial-value
;   (let ((integrand (force delayed-integrand)))
;     (if (stream-null? integrand)
;         the-empty-stream
;         (integral 
;          (delay (stream-cdr integrand))
;          (+ (* dt (stream-car integrand))
;             initial-value)
;          dt)))))

(define (solve f y0 dt)
  (let ((y nil) (dy nil))
    (set! y (integral (delay dy) y0 dt))
    (set! dy (stream-map f y))
    y))

; Ex 3.78
(define (solve-2nd a b y0 dy0 dt)
  (let ((y nil) (dy nil) (ddy nil))
    (set! y (integral (delay dy) y0 dt))
    (set! dy (integral (delay ddy) dy0 dt))
    (set! ddy (add-streams  (scale-stream dy a)
                            (scale-stream y b)))
    y))

; Ex 3.79
(define (gen-solve-2nd f y0 dy0 dt)
  (let ((y nil) (dy nil) (ddy nil))
    (set! y (integral (delay dy) y0 dt))
    (set! dy (integral (delay ddy) dy0 dt))
    (set! ddy (stream-map f dy y))
    y))

; Ex 3.80
(define (RLC R L C dt)
  (lambda (vc0 il0)
    (let ((vc nil) (il nil) (dvc nil) (dil nil))
      (set! vc (integral (delay dvc) vc0 dt))
      (set! il (integral (delay dil) il0 dt))
      (set! dvc (scale-stream il (/ -1.0 C)))
      (set! dil (add-streams
                 (scale-stream il (/ (- R) L))
                 (scale-stream vc (/ 1 L))))
      (cons vc il))))