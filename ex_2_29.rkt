#lang sicp
(#%require r5rs/init)

;Ex 2.29

; .1
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch m)
  (car m))

(define (right-branch m)
  (cadr m))

(define (branch-length b)
  (car b))

(define (branch-structure b)
  (cadr b))

; .2
(define (total-branch-weight b)
    (let ((s (branch-structure b)))
      (if (pair? s)
          (total-weight s)
          s)))

(define (total-weight m)
  (+ (total-branch-weight (left-branch m))
     (total-branch-weight (right-branch m))))

; .3
(define (balanced? m)
  (define (torque branch)
    (* (branch-length branch) (total-branch-weight branch)))
  (if (number? m)
      true
      (and (balanced? (branch-structure (left-branch m)))
           (balanced? (branch-structure (right-branch m)))
           (= (torque (left-branch m))
              (torque (right-branch m))))))

; Test stuff
(define m-mini
  (make-mobile
   (make-branch 2 2)
   (make-branch 4/3 3)))
(define m
  (make-mobile
   (make-branch 5 1)
   (make-branch 1 m-mini)))
(total-weight m)
(balanced? m)

; .4
;Only selectors need to be updated (cadr -> car)