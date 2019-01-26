#lang racket

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product 
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product 
           (deriv (multiplier exp) var)
           (multiplicand exp))))
        ((exponentation? exp)
         (make-product
           (make-product (exponent exp)
                       (make-exponentiation (base exp)
                                            (- (exponent exp) 1)))
           (deriv (base exp) var)))
        (else (error "unknown expression 
                      type: DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (=number? x num)
  (and (number? x)
       (= x num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) 
         (+ a1 a2))
        (else ((if (pair? a1)
                   append
                   cons)
               a1 (cons '+ (to-list a2))))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) 
             (=number? m2 0)) 
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) 
         (* m1 m2))
        ((or (sum? m1) (sum? m2)) (list m1 '* m2))
        (else (cons m1 (cons '* (to-list m2))))))

(define (to-list x)
  (if (pair? x)
      x
      (list x)))

(define (simplify-exp l)
  (if (and (pair? l)
           (empty? (cdr l)))
      (simplify-exp (car l))
      l))

(define (sublist-to el l)
  (define (helper l acc)
    (if (and (not (pair? (car l))) (eq? (car l) el))
        (reverse acc)
        (helper (cdr l) (cons (car l) acc))))
  (simplify-exp (helper l '())))
(define (sublist-after el l)
  (simplify-exp (cdr (memq el l))))



(define (sum? x)
  (and (pair? x)
       (memq '+ x)))
(define (addend s) (sublist-to '+ s))
(define (augend s) (sublist-after '+ s))

(define (product? x)
  (and (pair? x)
       (not (sum? x))
       (eq? (cadr x) '*)))
(define (multiplier s) (simplify-exp (car s)))
(define (multiplicand s) (simplify-exp (cddr s)))

(define (exponentation? x)
  (and (pair? x)
       (eq? (cadr x) '**)))
(define (base e)
  (car e))
(define (exponent e)
  (caddr e))
(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        ((=number? b 1) 1)
        ((=number? b 0) 0)
        (else (list b '** e))))


