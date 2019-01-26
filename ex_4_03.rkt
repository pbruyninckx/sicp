#lang sicp


;(define (eval exp env)
;  (cond ((self-evaluating? exp) exp)
;        ((variable? exp) (lookup-variable-value exp env))
;        ((get 'eval (car exp)) (((get 'eval (car exp))) (cdr exp) env))
;        ((application? (car exp))
;         (apply (eval (operator exp) env)
;                (list-of-values
;                 (operands exp)
;                 env)))
;        (else
;         (error "Unknown expression type: EVAL " exp))))


;(define (fib n)
;  (let fib-iter ((a 1) (b 0) (count n))
;    (if (= count 0)
;        b
;        (fib-iter (+ a b) 
;                  a 
;                  (- count 1)))))

(define (fib2 n)
  (let ((fib-iter (lambda (a b count)
                    (if (= count 0)
                        b
                        (fib-iter (+ a b)
                                  a
                                  (- count 1))))))
    (fib-iter 1 0 n)))