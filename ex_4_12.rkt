#lang sicp



(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))


(define (process-frame f-found f-absent do-recurse-frames var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (if do-recurse-frames
             (env-loop
              (enclosing-environment env))
             (f-absent var vars vals env)))
            ((eq? var (car vars))
             (f-found vars vals))
            (else (scan (cdr vars)
                        (cdr vals)))))
    (if (eq? env the-empty-environment)
        (f-absent var env)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (lookup-variable-value var env)
  (process-frame
   (lambda (vars vals) (car vals))
   (lambda (var frame)
     (error "Unbound variable " var))
   #true
   var
   env))

(define (set-variable-value! var val env)
  (process-frame
   (lambda (vars vals)
     (set-car! vals val))
   (lambda (var frame)
     (error "Unbound variable " var))
   #true
   var
   env))

(define (define-variable-value! var val env)
  (process-frame
   (lambda (vars vals)
     (set-car! vals val))
   (lambda (var frame)
     (add-binding-to-frame! var val frame))
   #false
   var
   env))

     

(define (make-unbound! var env)
  (process-frame
   (lambda (vars vals)
     (set-car! vals '())
     (set-car! vars '()))
   (lambda (var frame)
     (error "Variable not found in frame " var))
   #false
   var
   env))

