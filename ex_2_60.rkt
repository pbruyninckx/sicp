#lang racket
; Ex 2.60


; Note:
; n - elements in set
; N - elements used for representing, N >= n)

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))
; O(N)

(define (adjoin-set x set)
  (cons x set))
; O(1)

(define (union-set set1 set2)
  (append set1 set2))
; O(N) - due to append


(define (intersection-set set1 set2)
  (if (or (empty? set1) (empty? set2))
      '()
      (foldl (lambda (el1 result)
               (if (element-of-set? el1 set2)
                   (adjoin-set el1 result)
                   result))
             '()
             set1)))
; O(N^2)


; Use when
; - adding elements is common
; - elements are likely to be different,
;   so N is not much larger than n.