#lang sicp

; Ex 3.17
(define (has-cycle? x)
  (let ((acc '()))
    (define (seen? p)
      (> (apply + (map (lambda (other) (if (eq? p other) 1 0)) acc)) 0))
    (define (see p)
      (set! acc (cons p acc)))
    (define (helper x)
      (cond ((not (pair? x)) #f)
            ((seen? x) #t)
            (else
             (begin
               (see x)
               (helper (cdr x))))))
    (helper x)))

;3
(let* ((p3 (cons 'b 'c))
       (p2 (cons p3 p3))
       (p1 (cons 'a p2)))
  (has-cycle? p1))

(let* ((p3 (cons 'a 'b))
       (p2 (cons p3 p3))
       (p1 (cons p2 p2)))
  (has-cycle? p1))

(let* ((p3 (cons 'c 'b))
       (p2 (cons 'b p3))
       (p1 (cons 'a p2)))
  (set-cdr! p3 p1)
  (has-cycle? p1))

(let* ((p3 (cons 'c 'b))
       (p2 (cons 'b p3))
       (p1 (cons 'a p2)))
  (set-cdr! p3 p3)
  (has-cycle? p1))