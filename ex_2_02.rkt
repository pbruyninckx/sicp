#lang sicp

;Ex 2.2
(define (make-segment start-point end-point)
  (cons start-point end-point))
(define (start-segment s)
  (car s))
(define (end-segment s)
  (cdr s))


(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))

(define (average a b)
  (/ (+ a b) 2))

(define (mid-point segment)
  (make-point
   (average (x-point (start-segment segment)) (x-point (end-segment segment)))
   (average (y-point (start-segment segment)) (y-point (end-segment segment)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

; Exercise 2.3

; Constructors
; - simple one: using bottom-left and top right point (segments are alined with axes)
; - flexible: centre point, width, height, rotation

(define (make-rect-simple bottom-left top-right)
  (cons bottom-left top-right))
(define (width-simple r)
  (- (x-point (cdr r)) (x-point (car r))))
(define (height-simple r)
  (- (y-point (cdr r)) (y-point (car r))))
(define (area-simple r)
  (* (width-simple r) (height-simple r)))
(define (perimeter-simple r)
  (* 2 (+ (width-simple r) (height-simple r))))

(define (make-rect-flex centre width height rot)
  (cons centre (cons (cons width height) rot)))
(define (width-flex r)
  (car (car (cdr r))))
(define (height-flex r)
  (cdr (car (cdr r))))
(define (area-flex r)
  (* (width-flex r) (height-flex r)))
(define (perimeter-flex r)
  (* 2 (+ (width-flex r) (height-flex r))))