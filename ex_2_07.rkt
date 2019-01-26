#lang sicp
(#%require (only racket/base error))


(define (add-interval x y)
  (make-interval (+ (lower-bound x)
                    (lower-bound y))
                 (+ (upper-bound x)
                    (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) 
               (lower-bound y)))
        (p2 (* (lower-bound x) 
               (upper-bound y)))
        (p3 (* (upper-bound x) 
               (lower-bound y)))
        (p4 (* (upper-bound x) 
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (and (<= (lower-bound y) 0)
           (>= (upper-bound y) 0))
      (error "Division by interval spanning 0 is not defined")
      (mul-interval x 
                    (make-interval 
                     (/ 1.0 (upper-bound y)) 
                     (/ 1.0 (lower-bound y))))))

; Ex 2.7
(define (make-interval a b) (cons a b))
(define lower-bound car)
(define upper-bound cdr)

(define (display-interval a)
  (display "[")
  (display (lower-bound a))
  (display ", ")
  (display (upper-bound a))
  (display "]"))

; Ex 2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x)
                    (upper-bound y))
                 (- (upper-bound x)
                    (lower-bound y))))

; Ex 2.9
(define (width i)
  (/ (- (upper-bound i) (lower-bound i))
     2))
;(width (add-interval a b))
;(width (make-interval (+ (lower-bound a)
;                         (lower-bound b))
;                      (+ (upper-bound a)
;                         (upper-bound b))))
;(/ (- (+ (upper-bound a)
;         (upper-bound b))
;      (+ (lower-bound a)
;         (lower-bound b)))
;   2)
;(/ (+ (- (upper-bound a)
;         (lower-bound a))
;      (- (upper-bound b)
;         (lower-bound b)))
;   2)
;(+ (width a) (width b))
(define a (make-interval 1 3))
(define b (make-interval 5 7))
(define c (make-interval 11 13))
;(width (add-interval a b))
;(width (add-interval a c))
;(width (mul-interval a b))
;(width (mul-interval a c))

;Ex 2.10
; - changed above

;Ex 2.11
(define (neg? a)
  (<= (upper-bound a) 0))
(define (pos? a)
  (>= (lower-bound a) 0))
(define (zero? a)
  (and (< (lower-bound a) 0)
       (> (upper-bound a) 0)))
(define (mul-interval-fast a b)
  (cond ((and (pos? a) (pos? b))
         (make-interval (* (lower-bound a) (lower-bound b))
                        (* (upper-bound a) (upper-bound b))))
        ((and (pos? a) (zero? b))
         (make-interval (* (upper-bound a) (lower-bound b))
                        (* (upper-bound a) (upper-bound b))))
        ((and (pos? a) (neg? b))
         (make-interval (* (upper-bound a) (lower-bound b))
                        (* (lower-bound a) (upper-bound b))))
        ((and (zero? a) (pos? b))
         (make-interval (* (lower-bound a) (upper-bound b))
                        (* (upper-bound a) (upper-bound b))))
        ((and (zero? a) (zero? b))
         (mul-interval a b))
        ((and (zero? a) (neg? b))
         (make-interval (* (upper-bound a) (lower-bound b))
                        (* (lower-bound a) (lower-bound b))))
        ((and (neg? a) (pos? b))
         (make-interval (* (lower-bound a) (upper-bound b))
                        (* (upper-bound a) (lower-bound b))))
        ((and (neg? a) (zero? b))
         (make-interval (* (lower-bound a) (upper-bound b))
                        (* (lower-bound a) (lower-bound b))))
        ((and (neg? a) (neg? b))
         (make-interval (* (upper-bound a) (upper-bound b))
                        (* (lower-bound a) (lower-bound b))))))

(define neg-interval (make-interval -9 -5))
(define zero-interval (make-interval -3 2))
(define pos-interval (make-interval 7 11))

(define all-intervals (list neg-interval zero-interval pos-interval))

(define (test-fast-mul)
  (define (works a b)
    (display "PASS: ")
    (display-interval a)
    (display " - ")
    (display-interval b)
    (newline)
    )
  (define (fail a b)
    (display "FAIL: ")
    (display-interval a)
    (display " - ")
    (display-interval b)
    (newline)
    )  
  (define (test-single-fast-mul a b)
    (if (equal? (mul-interval-fast a b)
                (mul-interval a b))
        (works a b)
        (fail a b)))
  (define (inner-helper a blist)
    (cond ((null? blist) #t)
          (else
           (test-single-fast-mul a (car blist))
           (inner-helper a (cdr blist)))))
  (define (outer-helper alist)
    (cond ((not (null? alist))
            (inner-helper (car alist) all-intervals)
            (outer-helper (cdr alist)))))
  (outer-helper all-intervals))

;(test-fast-mul)

;Ex 2.12

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) 
        (upper-bound i)) 
     2))

;(define (width i)
;  (/ (- (upper-bound i) 
;        (lower-bound i)) 
;     2))

(define (make-center-percent c p)
  (let ((w (abs (* p c .01))))
    (make-center-width c w)))

(define (percent i)
  (* 100 (/ (width i) (center i))))

; Ex 2.13
;(* (* a (+- 1 (/ pa 100))) (* b (+- 1 (/ pb 100))))
;(* a b (+- 1 (/ (+ pa pb) 100)))

; Ex 2.14
(define (par1 r1 r2)
  (div-interval 
   (mul-interval r1 r2)
   (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval 
     one
     (add-interval 
      (div-interval one r1) 
      (div-interval one r2)))))

(define r1 (make-center-percent 5 2))
(define r2 (make-center-percent 7 3))
