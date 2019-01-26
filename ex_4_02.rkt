#lang sicp

; Ex 4.2.1
; (define x 3) will be interpreted as a function call to the
; 'define' function, rather than using the special form
; to define a variable

; Ex 4.2.2
; Make the 'function calls first' approach work
; by changing the function call syntax to (call fname operands)

(define (application? exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))