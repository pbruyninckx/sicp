#lang sicp
; Ex 3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))
(define v '(a b c d))
(define w (mystery v))
v
;{a}
w
;{d c b a}

; Ex 3.15 is trivial