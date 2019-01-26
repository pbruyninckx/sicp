#lang racket

; Ex 2.53
(list 'a 'b 'c)
;(a b c)
(list (list 'george))
;((george))
(cdr '((x1 x2) (y1 y2)))
; ((y1 y2))
(cadr '((x1 x2) (y1 y2)))
; (y1 y2)
(pair? (car '(a short list)))
; #f
(memq 'red '((red shoes) (blue socks)))
; #f
(memq 'red '(red shoes blue socks))
; (red shoes blue socks)



; Ex 2.54
(define (equal? l1 l2)
  (cond
    ((not (eq? (pair? l1) (pair? l2))) #f)
    ((pair? l1) (and (equal? (car l1) (car l2))
                     (equal? (cdr l1) (cdr l2))))
    (else (eq? l1 l2))))


