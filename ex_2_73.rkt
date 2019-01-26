#lang racket
(require "getput.rkt")
(require "helpers.rkt")

(define variable? symbol?)
(define (same-variable? a b)
  (and
   (variable? a)
   (variable? b)
   (eq? a b)))

(define (=number? x num)
  (and (number? x)
       (= x num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) 
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) 
             (=number? m2 0)) 
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) 
         (* m1 m2))
        (else (list '* m1 m2))))

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        ((=number? b 1) 1)
        ((=number? b 0) 0)
        (else (list '** b e))))

; Ex 2.73
(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) 
           (if (same-variable? exp var) 
               1 
               0))
         (else ((get 'deriv (operator exp)) 
                (operands exp) 
                var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))



(put 'deriv '+
     (lambda (ops var)
       (make-sum (deriv (car ops) var)
                 (deriv (cadr ops) var))))

(put 'deriv '*
     (lambda (ops var)
       (make-sum
        (make-product
         (car ops) (deriv (cadr ops) var))
        (make-product
         (deriv (car ops) var)
         (cadr ops)))))

(put 'deriv '**
     (lambda (ops var)
       (make-product
        (make-product (cadr ops)
                      (make-exponentiation (car ops)
                                           (- (cadr ops) 1)))


; 2.73.4
; Swap 'deriv and 'op everywhere
        (deriv (car ops) var))))