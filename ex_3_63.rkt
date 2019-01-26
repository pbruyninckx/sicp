#lang sicp

(#%require "series.rkt")

; Ex 3.63
; Without memoisation the performance will just be as horrible
; as the Louis Reasoner version

(define (average . args)
  (/ (apply + args) (length args)))(define (sqrt-improve guess x)
  (average  guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream
     1.0
     (stream-map
      (lambda (guess) (sqrt-improve guess x))
      guesses)))
  guesses)

; Ex 3.64
(define (stream-limit s  tolerance)
  (let ((s0 (stream-car s))
        (s1 (stream-car (stream-cdr s))))
    (if (< (abs (- s1 s0)) tolerance)
        s1
        (stream-limit (stream-cdr s) tolerance))))

(define (square x)
  (* x x))
; Ex 3.65

(define (pi-summands n)
  (cons-stream 
   (/ 1.0 n)
   (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream 
   (partial-sums (pi-summands 1)) 4))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream
     (- s2 (/ (square (- s2 s1))
              (+ s0 (* -2 s1) s2)))
     (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream
   s
   (make-tableau transform (transform s))))
(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(define (ln2-terms n)
  (cons-stream (/ 1.0 n)
               (scale-stream (ln2-terms (inc n)) -1)))
(define ln2-stream (partial-sums (ln2-terms 1)))

(display-stream ln2-stream)
(display-stream (euler-transform ln2-stream))
(display-stream (accelerated-sequence euler-transform ln2-stream))



(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream 
       (stream-car s1)
       (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) 
                  (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))


; 3.66
; (1,100) -> +- 2*100 pairs before it
; (100,100) -> +- 2 * (100^2 / 2) pairs before it
; (50,100) ->


; 3.67
(define (all-pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (interleave
     (stream-map (lambda (x) 
                   (list (stream-car s) x))
                 (stream-cdr t))
     (stream-map (lambda (x)
                   (list x (stream-car t)))
                 (stream-cdr s)))
    (all-pairs (stream-cdr s) (stream-cdr t)))))

; 3.68
;(define (louse-pairs s t)
;  (interleave
;   (stream-map
;    (lambda (x) 
;      (list (stream-car s) x))
;    t)
;   (louse-pairs (stream-cdr s)
;          (stream-cdr t))))
; This comment doesn't work because it always calls louse-pairs recursively.
; There is nothing that makes it 'lazy'


; Ex 3.69
(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map (lambda (p) (cons (stream-car s) p))
                (pairs t (stream-cdr u)))
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(display-stream (stream-filter
                 (lambda (t) (= (+ (square (car t)) (square (cadr t))) (square (caddr t))))
                 (triples integers integers integers))
                4)

; Ex 3.70
(define (merge-weighted s1 s2 w)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let* ((s1car (stream-car s1))
                (s2car (stream-car s2))
                (w1 (w s1car))
                (w2 (w s2car)))
           (if (< w1 w2)
               (cons-stream 
                s1car 
                (merge-weighted
                 (stream-cdr s1) 
                 s2
                 w))
               (cons-stream 
                s2car 
                (merge-weighted s1 
                                (stream-cdr s2)
                                w)))))))

(define (weighted-pairs s t weight)
  (merge-weighted
   (stream-map (lambda (x) 
                 (list (stream-car s) x))
               (stream-cdr t))
   (cons-stream
    (list (stream-car s) (stream-car t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight))
   weight))

(display-stream (weighted-pairs integers integers (lambda (x) (+ (car x) (cadr x)))))
(let* ((divisible? (lambda (x n) (= (remainder x n) 0)))
       (valid-int (lambda (x) (not (or (divisible? x 2) (divisible? x 3) (divisible? x 5)))))
       (valid-integers (stream-filter valid-int integers)))
  (display-stream (weighted-pairs valid-integers valid-integers
                                  (lambda (x) (+ (* 2 (car x))
                                                 (* 3 (cadr x))
                                                 (* 5 (car x) (cadr x)))))
                  10))

; Ex 3.71
(display-stream
 (let* ((cube (lambda (x) (* x x x)))
        (w (lambda (x) (apply + (map cube x))))
        (s (stream-map w (weighted-pairs integers integers w))))
   (stream-map
    car
    (stream-filter
     (lambda (x) (equal? (car x) (cdr x)))
     (stream-map cons s (stream-cdr s)))))
 5)


; Ex 3.72
;(display-stream
; (let* ((cube (lambda (x) (* x x x)))
;        (w (lambda (x) (apply + (map cube x))))
;        (s (stream-map (lambda (x) (cons (w x) x)) (weighted-pairs integers integers w))))
;   (stream-map (lambda (x)
;                 (cons (caar x) (map cdr x)))
;        (stream-filter
;     (lambda (x) (apply = (map car x)))
;     (stream-map list s (stream-cdr s) (stream-cdr (stream-cdr s))))))
; 5)

