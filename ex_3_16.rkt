#lang sicp

; Ex 3.16
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(count-pairs (cons (cons 'a 'b) (cons 'c 'd)))
;3
(let* ((p3 (cons 'b 'c))
       (p2 (cons p3 p3))
       (p1 (cons 'a p2)))
  (count-pairs p1))

(let* ((p3 (cons 'a 'b))
       (p2 (cons p3 p3))
       (p1 (cons p2 p2)))
  (count-pairs p1))

(let* ((p3 (cons 'c 'b))
       (p2 (cons 'b p3))
       (p1 (cons 'a p2)))
  (set-cdr! p3 p1)
  p1)