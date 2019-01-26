#lang racket
; Ex 2.66
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))
(define (make-leaf x)
  (make-tree x '() '()))

(define kv cons)
(define key car)
(define value cdr)

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((= given-key (key (entry set-of-records)))
         (entry set-of-records))
        ((< given-key (key (entry set-of-records)))
         (lookup given-key (left-branch set-of-records)))
        (else
         (lookup given-key (right-branch set-of-records)))))


(define records
  (make-tree (kv 2 "twee")
           (make-leaf (kv 1 "een"))
           (make-leaf (kv 3 "drie"))))