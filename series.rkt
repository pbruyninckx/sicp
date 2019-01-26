#lang sicp

(#%provide
 stream-ref
 stream-take
 stream-for-each
 display-stream
 cons-stream
 make-stream
 stream-car
 stream-cdr
 stream-null?
 stream-map
 mul-streams
 add-streams
 div-streams
 mul-series
 div-series
 scale-stream
 partial-sums
 ones
 integers
 stream-filter
 repeat-val
 )

; Code needed

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-take s n)
  (if (= n 0)
      the-empty-stream
      (cons-stream (stream-car s)
                   (stream-take (stream-cdr s) (- n 1)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin 
        (proc (stream-car s))
        (stream-for-each proc 
                         (stream-cdr s)))))

(define (display-stream s . args)
  (let ((n
         (if (null? args)
             10
             (car args))))
    (stream-for-each display-line (stream-take s n))))

(define (display-line x)
  (newline)
  (display x))

(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

(define (stream-car stream)
  (car stream))
(define (stream-cdr stream)
  (force (cdr stream)))
(define (stream-filter pred stream)
  (cond ((stream-null? stream) 
         the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream 
          (stream-car stream)
          (stream-filter 
           pred
           (stream-cdr stream))))
        (else (stream-filter 
               pred 
               (stream-cdr stream)))))
(define stream-null? null?)

(define (make-stream l)
  (if (null? l)
      (repeat-val 0)
      (cons-stream (car l)
                   (make-stream (cdr l)))))

;(define (delay exp)
 ; (lambda () exp))

;(define (force delayed-obj)
;  (delayed-obj))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1)
                                  high))))


; Ex. 3.50

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc
                    (map stream-cdr
                         argstreams))))))


; Ex 3.51

(define (show x)
  (display-line x)
  x)

(define x
  (stream-map
   show
   (stream-enumerate-interval 0 10)))
; This will print 0 - evaluation of other stream elements is postponed

(stream-ref x 5)
; 0 1 2 3 4 5 - all intermediate values are printed

(stream-ref x 7)
; 0 1 2 3 4 5 6 7 - all intermediate values are printed
; alternative: 6 7 - with memoisation
; looks like memoization is implemented in standard delay


; Ex 3.52

(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq
  (stream-map
   accum
   (stream-enumerate-interval 1 20)))


(define y (stream-filter even? seq))

(define z
  (stream-filter
   (lambda (x)
     (= (remainder x 5) 0)) seq))


(display "ex 3.52")
(stream-ref y 7)
; returns: 110
;(display sum)
; 110

;(display-stream z)
; returns: 10 15 45 55 105 120 190 210
; sum: (more than 126)

; Without memoisation the sum would have been bigger,
; as accum would have been called more than once per value

;(display sum)


(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (repeat-val v)
  (define s
    (cons-stream v s))
  s)

(define ones
  (cons-stream 1 ones))

(define integers
  (cons-stream 1 (add-streams ones integers)))

; Ex 3.53
(define s (cons-stream 1 (add-streams s s)))
;(1 2 4 8 16 ...)

; Ex 3.54
(define (mul-streams s1 s2)
  (stream-map * s1 s2))
(define factorials
  (cons-stream 1 (mul-streams integers factorials)))

; Ex 3.55
(define (partial-sums s)
  (cons-stream (stream-car s) (add-streams (stream-cdr s) (partial-sums s))))

;--
(define (scale-stream s n)
  (stream-map (lambda (x) (* x n)) s))

; Ex 3.56
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      s2)))
                 ((> s1car s2car)
                  (cons-stream s2car
                               (merge s1
                                      (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))

(define S (cons-stream
           1
           (merge (scale-stream S 2)
                  (merge (scale-stream S 3)
                         (scale-stream S 5)))))

; Ex 3.57
;(define fibs 
;  (cons-stream 
;   0 (cons-stream
;      1 (add-streams 
;         (stream-cdr fibs) fibs))))
; with memo-proc: O(fib(n)) = n
; without: O(fib(n)) = fib(n)


; Ex 3.58
; digits of num/den in base radix

; Ex 3.59
(define (div-streams s1 s2)
  (stream-map / s1 s2))
(define (integrate-series s)
  (div-streams
   s
   integers))

(define exp-series
  (cons-stream
   1 (integrate-series exp-series)))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))
(define cosine-series
  (cons-stream 1
               (stream-map (lambda (x) (- x))
                           (integrate-series sine-series))))

; Ex 3.60
(define (mul-series s1 s2)
  (cons-stream
   (* (stream-car s1) (stream-car s2))
   (add-streams
    (add-streams
     (stream-map (lambda (x) (* x (stream-car s1))) (stream-cdr s2))
     (stream-map (lambda (x) (* x (stream-car s2))) (stream-cdr s1)))
    (cons-stream
     0
     (mul-series (stream-cdr s1) (stream-cdr s2))))))

(define sumsinsqcossq
    (add-streams
     (mul-series sine-series sine-series)
     (mul-series cosine-series cosine-series)))


; Ex 3.61
(define (invert-unit-series s)
  (define inverted-series
    (cons-stream
     1
     (stream-map (lambda (x) (- x))
                 (mul-series (stream-cdr s) inverted-series))))
  inverted-series)

; Ex 3.62

(define (div-series s1 s2)
  (let ((norm-s2
         (stream-map (lambda (x) (/ x (stream-car s2))) s2)))
    (stream-map (lambda (x) (/ x (stream-car s2)))
                (mul-series s1 (invert-unit-series norm-s2)))))