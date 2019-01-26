#lang sicp
; Ex 1.37

(define (cont-frac n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (dec i)
              (/ (n i) (+ (d i) result)))))
  (iter k 0))

(define (cont-frac-rec n d k)
  (define (helper i)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (helper (inc i))))))
  (helper 1))

; 1.38
(+ 2.0
   (cont-frac
    (lambda (x) 1.0)
    (lambda (x)
      (if (= (remainder x 3) 2)
          (* (/ (+ x 1) 3) 2)
          1))
    30))

; 1.39
(define (pow a n)
  (define (helper a n result)
    (cond ((= n 0) result)
          ((even? n) (helper (* a a) (/ n 2) result))
          (else (helper a (dec n) (* a result)))))
  (helper a n 1))

(define (tan-cf x k)
  (cont-frac
   (lambda (i) (if (= i 1) x (- (* x x))))
   (lambda (i) (- (* 2 i) 1))
   k))