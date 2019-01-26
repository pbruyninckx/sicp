#lang racket
; Ex 2.61 - sets as ordered lists

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set 
                         (cdr set1)
                         (cdr set2))))
              ((< x1 x2) (intersection-set 
                          (cdr set1) 
                          set2))
              ((< x2 x1) (intersection-set 
                          set1 
                          (cdr set2)))))))

(define (adjoin-set x set)
  (define (helper rem-set smaller)
    (cond ((null? rem-set)
           (foldl cons (list x) smaller))
          ((eq? x (car rem-set)) set)
          ((> x (car rem-set))
           (helper (cdr rem-set) (cons (car rem-set) smaller)))
          (else ;add x
           (foldl cons rem-set (cons x smaller)))))
  (helper set '()))

;(adjoin-set 0 '())
;(adjoin-set 0 '(1 2 3))
;(adjoin-set 1 '(1 2 3))
;(adjoin-set 2 '(1 2 3))
;(adjoin-set 3 '(1 2 3))
;(adjoin-set 4 '(1 2 3))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((eq? (car set1) (car set2))
         (cons (car set1) (union-set (cdr set1) (cdr set2))))
        ((< (car set1) (car set2))
         (cons (car set1) (union-set (cdr set1) set2)))
        (else
         (cons (car set2) (union-set set1 (cdr set2))))))