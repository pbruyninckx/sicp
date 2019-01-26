#lang sicp
; Ex 2.5

(define (pow a n)
  (if (= n 0)
      1
      (* a (pow a (dec n)))))

(define (cons a b)
  (* (pow 2 a) (pow 3 b)))

(define (inv-pow x base)
  (define (helper x acc)
    (if (> (remainder x base) 0)
        acc
        (helper (/ x base) (inc acc))))
  (helper x 0))

(define (car p)
  (inv-pow p 2))

(define (cdr p)
  (inv-pow p 3))