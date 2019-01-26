#lang sicp

; Ex 3.17
(define (count-pairs x)
  (let ((acc '()))
    (define (seen? p)
      (> (apply + (map (lambda (other) (if (eq? p other) 1 0)) acc)) 0))
    (define (see p)
      (set! acc (cons p acc)))
    (define (helper x)
      (cond ((not (pair? x)) 0)
            ((seen? x) 0)
            (else
             (begin
               (see x)
               (+ (helper (car x))
                  (helper (cdr x))
                  1)))))
    (helper x)))

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
  (count-pairs p1))