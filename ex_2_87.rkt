#lang racket

(require "getput.rkt")
(require "ex_2_93_req.rkt")

(define (empty-termlist? l)
  (apply-generic 'empty-termlist? l))
(define (first-term l)
  (apply-generic 'first-term l))
(define (rest-terms l)
  (apply-generic 'rest-terms l))
(define (adjoin-term term term-list)
  ((apply-generic 'adjoin-term term-list) term))

(define (add-terms L1 L2)
  (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1)
        (else
         (let ((t1 (first-term L1))
               (t2 (first-term L2)))
           (cond ((> (order t1) (order t2))
                  (adjoin-term
                   t1
                   (add-terms (rest-terms L1)
                              L2)))
                 ((< (order t1) (order t2))
                  (add-terms L2 L1))
                 (else
                  (adjoin-term
                   (make-term
                    (order t1)
                    (add (coeff t1)
                         (coeff t2)))
                   (add-terms
                    (rest-terms L1)
                    (rest-terms L2)))))))))

(define (neg-terms L)
  (cond ((empty-termlist? L) L)
        (else
         (let ((t (first-term L)))
           (adjoin-term
                   (make-term
                    (order t)
                    (neg (coeff t)))
                   (neg-terms
                    (rest-terms L)))))))

(define (sub-terms L1 L2)
  (add-terms L1 (neg-terms L2)))

(define (mul-terms L1 L2)
  (if (empty-termlist? L1)
      (if (member 'sparse (map type-tag (list L1 L2)))
          (make-sparse-empty-termlist)
          (make-dense-empty-termlist))
      (add-terms
       (mul-term-by-all-terms
        (first-term L1) L2)
       (mul-terms (rest-terms L1) L2))))

(define (mul-term-by-all-terms t1 L)
  (if (empty-termlist? L)
      ((get 'the-empty-termlist (type-tag L)))
      (let ((t2 (first-term L)))
        (adjoin-term
         (make-term
          (+ (order t1) (order t2))
          (mul (coeff t1) (coeff t2)))
         (mul-term-by-all-terms
          t1
          (rest-terms L))))))

(define (make-term order coeff)
  (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))



(define (install-sparse-polynomial-package)
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (tag (cdr term-list)))
  (define (empty-termlist? term-list)
    (null? term-list))
  (define (adjoin-term term-list)
    (lambda (term)
      (tag (if (zero? (coeff term))
               term-list
               (cons term term-list)))))

  (define (tag term-list)
    (attach-tag 'sparse term-list))
  (put 'first-term '(sparse) first-term)
  (put 'rest-terms '(sparse) rest-terms)
  (put 'empty-termlist? '(sparse) empty-termlist?)
  (put 'the-empty-termlist 'sparse
       (lambda () (tag (the-empty-termlist))))
  (put 'make 'sparse-termlist
       (lambda (terms)
         (tag terms)))
  (put 'adjoin-term '(sparse)
       adjoin-term)
  'done
  )
(install-sparse-polynomial-package)
(define (make-sparse-termlist . terms)
  ((get 'make 'sparse-termlist) terms))

(define (make-sparse-empty-termlist)
  ((get 'the-empty-termlist 'sparse)))
(define (make-dense-empty-termlist)
  ((get 'the-empty-termlist 'dense)))

(define (install-dense-polynomial-package)
  (define (the-empty-dense-termlist)
    '(-1))
  (define (empty-dense-termlist? term-list)
    (or (< (car term-list) 0)
        (foldl (lambda (e ret) (and ret (zero? e)))
               #t
               (cdr term-list))))
  (define (first-dense-term term-list)
    (if (zero? (cadr term-list))
        (first-dense-term (rest-dense-terms-simple term-list))
        (make-term (car term-list) (cadr term-list))))
    
    ;(make-term (car term-list) (cadr term-list)))
  (define (rest-dense-terms term-list)
    (if (zero? (cadr term-list))
        (rest-dense-terms (rest-dense-terms-simple term-list))
        (non-zero-dense-terms (rest-dense-terms-simple term-list))))
  (define (rest-dense-terms-simple term-list)
    (cons (- (car term-list) 1) (cddr term-list)))
  (define (non-zero-dense-terms term-list)
    (cond ((empty-dense-termlist? term-list) term-list)
          ((zero? (cadr term-list)) (non-zero-dense-terms (rest-dense-terms-simple term-list)))
          (else term-list)))
  (define (add-dense-coeff coeff term-list)
    (cons (+ (car term-list) 1) (cons coeff (cdr term-list))))
  (define (make-dense-termlist coeffs)
    (foldl add-dense-coeff (the-empty-dense-termlist) coeffs))
  (define (adjoin-term term-list)
    (define (adjoin-helper term term-list)
      (cond ((zero? (coeff term)) term-list)
            ((= (car term-list) (- (order term) 1))
             (add-dense-coeff (coeff term) term-list))
            (else (adjoin-helper term (add-dense-coeff 0 term-list)))))
    (lambda (term)
      (tag (adjoin-helper term term-list))))

  (define (tag term-list)
    (attach-tag 'dense term-list))
  (put 'first-term '(dense) first-dense-term)
  (put 'rest-terms '(dense)
       (lambda (term-list) (tag (rest-dense-terms term-list))))
  (put 'empty-termlist? '(dense) empty-dense-termlist?)
  (put 'the-empty-termlist 'dense
       (lambda () (tag (the-empty-dense-termlist))))
  (put 'make 'dense-termlist
       (lambda (coeffs)
         (tag (make-dense-termlist coeffs))))
  (put 'adjoin-term '(dense) adjoin-term)
  )
(install-dense-polynomial-package)

(define (make-dense-termlist . coeffs)
  ((get 'make 'dense-termlist) coeffs))




(define (install-polynomial-package)
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p)
    (car p))
  (define (term-list p)
    (cdr p))
  (define (variable? x)
    (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1)
         (variable? v2)
         (eq? v1 v2)))
  (define (first-var v1 v2)
  (if (symbol<? v1 v2)
      v1
      v2))
  (define (change-var poly v)
    (if (same-variable? (variable poly) v)
        poly
        (make-poly
         v
         (make-sparse-termlist (make-term 0 (tag poly))))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1)
                        (variable p2))
        (make-poly
         (variable p1)
         (mul-terms (term-list p1)
                    (term-list p2)))
        (let ((v (first-var (variable p1) (variable p2))))
          (mul-poly (change-var p1 v) (change-var p2 v)))))
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1)
                        (variable p2))
        (make-poly
         (variable p1)
         (div-terms (term-list p1)
                    (term-list p2)))
        (error "Polys not in same var: DIV-POLY"
               (list p1 p2))))  

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1)
                        (variable p2))
        (make-poly
         (variable p1)
         (add-terms (term-list p1)
                    (term-list p2)))
        (let ((v (first-var (variable p1) (variable p2))))
          (add-poly (change-var p1 v) (change-var p2 v)))))
  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (gcd-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: GCD-POLY"
               (list p1 p2))))
  (define (reduce-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (map (lambda (t) (make-poly (variable p1) t))
             (reduce-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: GCD-POLY"
               (list p1 p2))))  
        
  (define (add-poly-number p n)
    (make-poly
     (variable p)
     (add-terms (term-list p)
                (make-sparse-termlist (make-term 0 n)))))
  (define (mul-poly-number p n)
    (make-poly
     (variable p)
     (mul-terms (term-list p)
                (make-sparse-termlist (make-term 0 n)))))
    
  (define (zero-poly? p)
    (define (zero-terms t)
      (cond ((empty? t) #t)
            ((zero? (coeff (first-term t)))
                    (zero-terms (rest-terms t)))
            (else #f)))
    (zero-terms (term-list p)))
  (define (one-poly? p)
    (define (one-terms? t)
      (cond ((empty? t) #f)
            ((zero? (coeff (first-term t)))
             (one-terms? (rest-terms t)))
            ((= 0 (order (first-term t)))
             (one? (coeff (first-term t))))
            (else #f)))
    (one-terms? (term-list p)))
  (define (neg-poly p)
    (make-poly
     (variable p)
     (map (lambda (t)
            (make-term (order t) (neg (coeff t))))
          (term-list p))))
  (define (sub-poly p1 p2)
    (add-poly p1 (neg-poly p2)))
  (define (pp-poly p)
    (define (variable p) (car p))
    (define (helper terms sep)
      (cond ((empty-termlist? terms) (void))
            (else
             (let* ((c (coeff (first-term terms)))
                    (o (order (first-term terms)))
                    (use-brackets (not (eq? 'scheme-number (type-tag c)))))
               (cond ((zero? c) (void))
                     (else
                      (if sep
                          (display " + ")
                          (void))
                      (cond ((one? c)
                             (if (= 0 o)
                                 (display "1")
                                 (void)))
                            (else (if use-brackets (display "(") (void))
                                  (pp c)
                                  (if use-brackets (display ")") (void))))
                      (cond ((= o 0) (void))
                            ((= o 1) (display (variable p)))
                            (else
                             (display (variable p))
                             (display "^")
                             (display o)))
                      (helper (rest-terms terms) #t)))))))
    (helper (cdr p) #f))
  (put 'pp '(polynomial) pp-poly)
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (add-poly p1 p2))))
  (put 'add '(scheme-number polynomial)
       (lambda (n p)
              (tag (add-poly-number p n))))
  (put 'add '(polynomial scheme-number)
       (lambda (p n)
              (tag (add-poly-number p n))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (mul-poly p1 p2))))
  (put 'mul '(polynomial scheme-number)
       (lambda (p n)
         (if (= n 0) 0
         (tag (mul-poly-number p n)))))
  (put 'mul '(scheme-number polynomial )
       (lambda (n p )
         (if (= n 0) 0
         (tag (mul-poly-number p n)))))
  (put 'make 'polynomial
       (lambda (var terms)
         (tag (make-poly var terms))))
  (put 'zero? '(polynomial)
       zero-poly?)
  (put 'one? '(polynomial)
       one-poly?)
  (put 'neg '(polynomial)
       (lambda (p)
         (tag (neg-poly p))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (sub-poly p1 p2))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (div-poly p1 p2))))
  (put 'greatest-common-divisor '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (gcd-poly p1 p2))))
  (put 'reduce '(polynomial polynomial)
       (lambda (p1 p2)
         (map tag (reduce-poly p1 p2))))
       
  'done)
(install-polynomial-package)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

;(define p1 (make-polynomial 'x (make-sparse-termlist (make-term 2 1) (make-term 1 2) (make-term 0 1))))
;(define p2 (make-polynomial 'x (make-sparse-termlist (make-term 3 2) (make-term 1 3) (make-term 0 4))))
;(define P0 (make-polynomial 'x (make-dense-termlist 5 4 3 2 1)))

;"%(mul p1 p2)

; Ex 2.91 - division

(define (div-terms L1 L2)
  (if (empty-termlist? L1)
      (list (make-dense-empty-termlist) 
            (make-dense-empty-termlist))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (if (> (order t2) (order t1))
            (list (make-dense-empty-termlist) L1)
            (let ((new-c (div (coeff t1) 
                              (coeff t2)))
                  (new-o (- (order t1) 
                            (order t2))))
              (let ((rest-of-result
                     (div-terms (sub-terms L1
                                           (mul-terms (make-sparse-termlist (make-term new-o new-c))
                                                      L2))
                                L2)))
                (list (adjoin-term (make-term new-o new-c) (car rest-of-result))
                      (cadr rest-of-result))))))))
(define (remainder-terms L1 L2)
  (cadr (div-terms L1 L2)))
(define (pseudoremainder-terms L1 L2)
  (let ((c (coeff (first-term L2))))
    (cadr (div-terms
           (mul-terms
            L1
            (make-sparse-termlist
             (make-term 0
                        (expt c (+ 1 (order (first-term L1)) (- (order (first-term L2))))))))
           L2))))
; Ex 2.97

(define (reduce-terms n d)
  (let* ((this-gcd (gcd-terms n d))
         (f (+ 1
               (max (order (first-term n))
                    (order (first-term d)))
               (- (order (first-term this-gcd)))))
         (nf (mul-term-by-all-terms
              (make-term 0 f)
              n))
         (df (mul-term-by-all-terms
              (make-term 0 f)
              d))
         (n-large (car (div-terms nf this-gcd)))
         (d-large (car (div-terms df this-gcd))))
    (map (compose
          remove-common-factor
          (lambda (nd) (car (div-terms nd this-gcd)))
          (lambda (nd) (mul-term-by-all-terms (make-term 0 f) nd)))
         (list n d))))
; Ex 2.94

(define (remove-common-factor terms)
  (define (get-common-factor terms acc)
    (if (empty-termlist? terms)
        acc
        (get-common-factor (rest-terms terms) (gcd acc (coeff (first-term terms))))))
  (let ((c (get-common-factor terms 0)))
    (car (div-terms
          terms
          (make-sparse-termlist (make-term 0 c))))))

(define (gcd-terms a b)
  (if (empty-termlist? b)
      (remove-common-factor a)
      (gcd-terms b (pseudoremainder-terms a b))))

;(div (make-polynomial 'x (make-sparse-termlist (make-term 5 1) (make-term 0 -1)))
;     (make-polynomial 'x (make-dense-termlist -1 0 1)))


; Ex 2.92  -- polynomials in different variables
(pp (add (make-polynomial 'x (make-sparse-termlist
                          (make-term 5 (make-polynomial 'y (make-dense-termlist 1 1)))
                          (make-term 0 (make-polynomial 'y (make-dense-termlist 1 0 0 0 1)))))
     (make-polynomial 'x (make-sparse-termlist
                          (make-term 5 (make-polynomial 'y (make-dense-termlist 1 1)))
                          (make-term 0 (make-polynomial 'y (make-dense-termlist 1 0 0 0 1)))))))


; Simple poly x + y
(define x+y_ (make-polynomial 'x (make-dense-termlist
                                  (make-polynomial 'y (make-dense-termlist 0 1))
                                  (make-polynomial 'y (make-dense-termlist 1)))))
(define x+y (make-polynomial 'x (make-dense-termlist
                                 (make-polynomial 'y (make-dense-termlist 0 1))
                                 1)))

;(pp x+y)
;(newline)
;(pp (mul x+y x+y))
;(newline)
(define x
    (make-polynomial 'x (make-sparse-termlist (make-term 1 1))))
(define y (make-polynomial 'y (make-dense-termlist 0 1)))


(define pp1 (make-polynomial 'x '(sparse (2 1) (0 1))))
(define pp2 (make-polynomial 'x '(sparse (3 1) (0 1))))
(define rf (make-rational pp2 pp1))



(define ppp1 
  (make-polynomial 
   'x '(sparse (4 1) (3 -1) (2 -2) (1 2))))

(define ppp2 
  (make-polynomial 
   'x '(sparse (3 1) (1 -1))))

;(greatest-common-divisor ppp1 ppp2)


; Ex 2.95
(define P1 (make-polynomial 'x '(sparse (2 1) (1 -2) (0 1))))
(define P2 (make-polynomial 'x '(sparse (2 11) (0 7))))
(define P3 (make-polynomial 'x '(sparse (1 13) (0 5))))

(define Q1 (mul P1 P2))
(define Q2 (mul P1 P3))
(newline)
(greatest-common-divisor Q1 Q2)
(pp (greatest-common-divisor Q1 Q2))
(newline)




         ; (list n-large d-large))))


(reduce-terms (cddr Q1) (cddr Q2))
(reduce Q1 Q2)


(define p1 
  (make-polynomial 'x '(sparse (1 1) (0 1))))
(define p2 
  (make-polynomial 'x '(sparse (3 1) (0 -1))))
(define p3 
  (make-polynomial 'x '(sparse (1 1))))
(define p4 
  (make-polynomial 'x '(sparse (2 1) (0 -1))))
(define rf1 (make-rational p1 p2))
(define rf2 (make-rational p3 p4))
(rest-terms '(dense 2 1 0 -1))
(add rf1 rf2)