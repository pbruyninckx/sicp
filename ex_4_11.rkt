#lang sicp


(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())



(define (make-frame var-vals)
  (cons 'frame var-vals))
(define (frame-varvals frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (cons var val) (cdr frame))))
(define (get-var var-val) (car var-val))
(define (get-val var-val) (cdr var-val))


(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan var-vals)
      (cond ((null? var-vals)
             (env-loop
              (enclosing-environment env)))
            ((eq? var (get-var (car var-vals))
             (get-val (car var-vals))))
            (else (scan (cdr var-vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable " var)
        (let ((frame (first-frame env)))
          (scan (frame-varvals frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan var-vals)
      (cond ((null? var-vals)
             (env-loop
              (enclosing-environment env)))
            ((eq? var (get-var (car var-vals)))
             (set-cdr! (car var-vals) val))
            (else (scan (cdr var-vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET! " var)
        (let ((frame (first-frame env)))
          (scan (frame-varvals frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan var-vals)
      (cond ((null? var-vals)
             (add-binding-to-frame! var val frame))
            ((eq? var (get-var (car var-vals)))
             (set-cdr! (car var-vals) val))
            (else (scan (cdr var-vals)))))
    (scan (frame-varvals frame))))
