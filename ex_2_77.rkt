#lang racket

(provide
 square
 add
 mul
 neg
 div
 sub
 zero?
 one?
 equ?
 pp
 attach-tag
 type-tag
 contents
 install-scheme-number-package
 apply-generic)

(require "getput.rkt")

(define (square x)
  (* x x))

;(define (apply-generic op . args)
;  (let ((type-tags (map type-tag args)))
;    (let ((proc (get op type-tags)))
;      (if proc
;          (apply proc (map contents args))
;          (error
;            "No method for these types: 
;             APPLY-GENERIC"
;            (list op type-tags))))))

(define (real-part x)
  (apply-generic 'real-part x))
(define (imag-part x)
  (apply-generic 'imag-part x))
(define (magnitude x)
  (apply-generic 'magnitude x))
(define (angle x)
  (apply-generic 'angle x))
(define (numer x)
  (apply-generic 'numer x))
(define (denom x)
  (apply-generic 'denom x))

(define (attach-tag type-tag contents)
  (cond
    ((eq? type-tag 'scheme-number) contents)
    (else
     (cons type-tag contents))))

(define (type-tag datum)
  (cond
    ((number? datum)
     'scheme-number)
    ((pair? datum)
      (car datum))
     (else (error "Bad tagged datum: 
              TYPE-TAG" datum))))

(define (contents datum)
  (cond
    ((number? datum) datum)
    ((pair? datum)
      (cdr datum))
      (else (error "Bad tagged datum: 
              CONTENTS" datum))))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (zero? x) (apply-generic 'zero? x))
(define (one? x) (apply-generic 'one? x))
(define (raise x) (apply-generic 'raise x))
(define (project x) (apply-generic 'project x))
(define (neg x) (apply-generic 'neg x))
(define (pp x) (apply-generic 'pp x))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put 'zero? '(scheme-number)
       (lambda (x) (= x 0)))
  (put 'one? '(scheme-number)
       (lambda (x) (= x 1)))
  (put 'neg '(scheme-number)
       (lambda (x) (- x)))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'pp '(scheme-number)
       print)
  'done)
(install-scheme-number-package)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (equ? x y)
    (= (* (numer x) (denom y))
       (* (denom  x) (numer y))))
  (define (zero? x)
    (and (= 0 (numer x))
         (not (= 0 (denom x)))))
  (define (project-rational x)
    (inexact->exact (round (/ (numer x) (denom x)))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'numer '(rational)
       numer)
  (put 'denom '(rational)
       denom)
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational)
       (lambda (x y) (equ? x y)))
  (put 'zero? '(rational)
       (lambda (x) (zero? x)))
  (put 'project '(rational)
       project-rational)
  'done)

(install-rational-package)

(define (make-rational n d)
  ((get 'make 'rational) n d))
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) 
    (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) 
    (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)
(install-rectangular-package)
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)
(install-polar-package)
(define (install-complex-package)
  ;; imported procedures from rectangular 
  ;; and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 
          'rectangular) 
     x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) 
     r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag 
     (+ (real-part z1) (real-part z2))
     (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag 
     (- (real-part z1) (real-part z2))
     (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang 
     (* (magnitude z1) (magnitude z2))
     (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang 
     (/ (magnitude z1) (magnitude z2))
     (- (angle z1) (angle z2))))
  (define (equ? z1 z2)
    (and (= (real-part z1) (real-part z2))
         (= (imag-part z1) (imag-part z2))))
  (define (zero? z)
    (= (magnitude z) 0))
   (define (project-complex x)
    (let ((sc-rat (rationalize (inexact->exact (real-part x)) 1/100)))
      (make-rational (numerator sc-rat) (denominator sc-rat))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) 
         (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) 
         (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) 
         (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) 
         (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  (put 'equ? '(complex complex)
       (lambda (z1 z2) (equ? z1 z2)))
  (put 'zero? '(complex)
       (lambda (z) (zero? z)))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'project '(complex) project-complex)
  'done)

(install-complex-package)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

; Ex 2.77
;(magnitude '(complex rectangular 3 . 4))
;(apply-generic 'magnitude '(complex rectangular 3 . 4))
;((get 'magnitude '(complex)) '(rectangular 3 . 4))
;(magnitude '(rectangular 3 . 4))
;(apply-generic 'magnitude '(rectangular 3 . 4))
;((get 'magnitude '(rectangular)) '(3 . 4))
;(sqrt (+ (square 3) (square 4)))

; Exercises 2.78, 2.79, and 2.80 have been added in the code above



; Code from 2.5 coercion / conversion between types
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (and (= (length args) 2) (not (apply eq? type-tags)))
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 
                       (get-coercion type1
                                     type2))
                      (t2->t1 
                       (get-coercion type2 
                                     type1)))
                  (cond (t1->t2
                         (apply-generic 
                          op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic 
                          op a1 (t2->t1 a2)))
                        (else
                         ((error 
                          "No method for 
                           these types"
                          (list 
                           op 
                           type-tags)))))))
              (error 
               "No method for these types"
               (list op type-tags)))))))

  ; Ex 2.81
  ; 1. It still won't work
  ; 2. If the method is originally implemented for the
  ;    desired types, the method just works.
  ; 3. Added (not (apply eq? type-args))


(put-coercion 'scheme-number 'rational
              (lambda (x) (make-rational (contents x) 1))) 

; Ex 2.82 -- more than two arguments
; Code from 2.5 coercion / conversion between types
; This strategy will not work for a function (e.g. exp)
; where the exponent can only be a natural number, while
; the base can be anything (although it will work if the base
; happens to be a natural number).
(define (apply-generic-multi op . args)
  (let ((type-tags (map type-tag args)))
    (define (fail-method) (error 
               "No method for these types"
               (list op type-tags)))
    (define (get-coercion-methods to-type)
      (map (lambda (t) (if (eq? t to-type) identity (get-coercion t to-type))) type-tags))
    (define (map-methods methods vals)
      (map (lambda (f v) (f v)) methods vals))
    (define (all? l)
      (cond
        ((empty? l) #t)
        ((car l) (all? (cdr l)))
        (else #f)))
    (define (try-coercions rem-types)
      (if (empty? rem-types)
          (fail-method)
          (let
              ((coercion-methods (get-coercion-methods (car rem-types)))
               (proc (get op (build-list (length args) (const (car rem-types))))))
            (if (and (all? coercion-methods)
                     proc)
                (apply proc (map contents (map-methods coercion-methods args)))
                (try-coercions (cdr rem-types))))))
  
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (try-coercions type-tags)))))

; Ex 2.83
(define (install-raise-package)
  (define (raise-integer n)
    (make-rational n 1))
  (define (raise-rational x)
    (make-complex-from-real-imag (* 1.0 (/ (car x) (cdr x))) 0))
  (put 'raise '(scheme-number) raise-integer)
  (put 'raise '(rational) raise-rational)
  'done)
(install-raise-package)
(define tower
  '(scheme-number rational complex))

; Ex 2.84
;Find out which of two types is the highest
(define (highest-type t1 t2)
  (let ((rem1 (length (memv t1 tower)))
        (rem2 (length (memv t2 tower))))
    (if (> rem1 rem2)
        t2
        t1)))
(define (raise-to val type)
  (if (eq? (type-tag val) type)
      val
      (raise-to (raise val) type)))

(define (apply-generic-raise op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (and (= (length args) 2) (not (apply eq? type-tags)))
              (let ((high-type (highest-type (car type-tags) (cadr type-tags))))
                (let ((proc (get op (list high-type high-type))))
                  (if proc
                      (apply proc
                             (map contents
                                  (map (lambda (x) (raise-to x high-type))
                                  args)))
                      (error "No method for these types"
                             (list op type-tags)))))
              (error "No method for these types"
                     (list op type-tags)))))))


(define (drop x)
  (let ((proc (get 'project (list (type-tag x)))))
    (if (and
         proc
         (equ? x (raise (project x))))
        (drop (project x))
        x)))

(define apply-generic-drop
  (compose drop apply-generic))

; Ex 2.86
; Fancy complex numbers:
; We need to add type-information to the real/imag magn-angle
; part of the complex numbers
; This means relying on generic operations for
; any operation that involves complex numbers.