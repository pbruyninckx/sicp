#lang sicp

(define (average a b)
  (/ (+ a b)
     2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (square x)
  (* x x))

(define tolerance 0.00001)

(define (fixed-point f guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try guess))

(define dx .00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x)
            ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g)
               guess))

(define (pow a n)
  (define (helper a n result)
    (cond ((= n 0) result)
          ((even? n) (helper (* a a) (/ n 2) result))
          (else (helper a (dec n) (* result a)))))
  (helper a n 1))

; Ex 1.40
(define (cubic a b c)
  (lambda (x)
    (+ (pow x 3)
       (* a (pow x 2))
       (* b x)
       c)))

; Ex 1.41
(define (double f)
  (lambda (x) (f (f x))))

;(((double (double double))inc) 5)
;(((double (lambda (x) (double (double x)))) inc) 5)
;(((double (lambda (x) (lambda (y) ((double x) ((double x) y))))) inc) 5)
;(((lambda (z) ((lambda (x) (lambda (y) ((double x) ((double x) y)))) ((lambda (x) (lambda (y) ((double x) ((double x) y)))) z))) inc) 5)
;(((lambda (x) (lambda (y) ((double x) ((double x) y)))) ((lambda (x) (lambda (y) ((double x) ((double x) y)))) inc)) 5)
;(((lambda (x) (lambda (y) ((double x) ((double x) y)))) (lambda (y) ((double inc) ((double inc) y)))) 5)
;(((lambda (x) (lambda (y) ((double x) ((double x) y)))) (lambda (y) ((lambda (z) (inc (inc z))) ((lambda (a) (inc (inc a))) y)))) 5)
;(((lambda (x) (lambda (y) ((double x) ((double x) y)))) (lambda (y) (inc (inc ((lambda (a) (inc (inc a))) y))))) 5)
;(((lambda (x) (lambda (y) ((double x) ((double x) y)))) (lambda (y) (inc (inc (inc (inc y)))))) 5)
;(((lambda (x) (lambda (y) ((double x) ((lambda (z) (x (x z))) y)))) (lambda (y)  (inc (inc (inc (inc y)))))) 5)
;(((lambda (x) (lambda (y) ((double x) (x (x y))))) (lambda (y)  (inc (inc (inc (inc y)))))) 5)
;(((lambda (x) (lambda (y) ((lambda (z) (x (x z))) (x (x y))))) (lambda (y) (inc (inc (inc (inc y)))))) 5)
;(((lambda (x) (lambda (y) (x (x (x (x y)))))) (lambda (y) (inc (inc (inc (inc y))) ))) 5)
;((lambda (y) ((lambda (d) (inc (inc (inc (inc d))))) ((lambda (c) (inc (inc (inc (inc c))))) ((lambda (b) (inc (inc (inc (inc b))))) ((lambda (a) (inc (inc (inc (inc a))))) y))))) 5)
;((lambda (y) ((lambda (d) (inc (inc (inc (inc d))))) ((lambda (c) (inc (inc (inc (inc c))))) ((lambda (b) (inc (inc (inc (inc b))))) (inc (inc (inc (inc y)))))))) 5)
;((lambda (y) ((lambda (d) (inc (inc (inc (inc d))))) ((lambda (c) (inc (inc (inc (inc c))))) (inc (inc (inc (inc (inc (inc (inc (inc y))))))))))) 5)
;((lambda (y) ((lambda (d) (inc (inc (inc (inc d))))) (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc y)))))))))))))) 5)
;((lambda (y) (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc y))))))))))))))))) 5)
;(inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 5))))))))))))))))
;(inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 6)))))))))))))))
;(inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 7))))))))))))))
;...
;21


; Ex 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

; Ex 1.43
(define (repeated f n)
  (if (= n 0)
      identity
      (compose f (repeated f (dec n)))))

; Ex 1.44
(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (smooth-n f n)
  ((repeated smooth n) f))

; Ex 1.45
(define (sqrt-n x n)
  (define (int-log2 n result)
    (if (< n 2)
        result
        (int-log2 (/ n 2) (inc result))))
  (let ((n-damp (int-log2 n 0)))
    (fixed-point ((repeated average-damp n-damp) (lambda (y) (/ x (pow y (dec n))))) 1.0)))

; Ex 1.46

(define (iterative-improve good-enough? improve)
  (lambda (guess)
    (if (good-enough? guess)
        guess
        ((iterative-improve good-enough? improve) (improve guess)))))

(define (it-sqrt x)
    (define (good-enough? guess)
      (< (abs (- (square guess) x)) tolerance))
    (define (improve guess)
      (average guess (/ x guess)))
    ((iterative-improve good-enough? improve) 1.0))

(define (fixed-point-it f guess)
  (define (good-enough? next-guess)
    (< (abs (- (f next-guess) next-guess)) tolerance))
  (define (improve guess)
    (f guess))
  ((iterative-improve good-enough? improve) guess))