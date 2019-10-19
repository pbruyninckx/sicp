#lang sicp

; Ex 4.30

; Part 1
(define (for-each proc items)
  (if (null? items)
      'done
      (begin (proc (car items))
             (for-each proc (cdr items)))))

(for-each (lambda (x) (newline) (display x))
          '(1 3 2 4))

; This works with the lazy evaluator because primitive procedures (like display)
; will always force evaluation of all their arguments

(define (p1 x)
  (set! x (cons x '(2)))
  x)

(define (p2 x)
  (define (p e) e x)
  (p (set! x (cons x '(2)))))

(p1 1)
; -> '(1 2)
(p2 1)
; -> '(1 2) / 1

; The result of the for-each function would still be the same,
; as the primitive procedures are forced anyway

; I think that in the lazy evaluator, execution should stay lazy.
; However I think it might be interesting to have a special form
; that allows to force execution (although this might be challenging)
; to actually implement.
