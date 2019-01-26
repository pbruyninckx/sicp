#lang sicp

(define (has-cycle? x)
  (define (helper a b)
    (cond ((eq? a b) #t)
          ((not (pair? b)) #f)
          ((not (pair? (cdr b))) #f)
          (else (helper (cdr a) (cdr (cdr b))))))
  (if (not (pair? x))
      #f
      (helper x (cdr x))))

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