#lang racket

(define (make-account balance my-password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance 
                     (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          ((eq? m 'account) dispatch)
          (else (error "Unknown request: 
                 MAKE-ACCOUNT" m))))
  (define (get-account try-password)
    (if (eq? my-password try-password)
        dispatch
        (error "Incorrect password")))
  (lambda (pwd m) ((get-account pwd) m)))

(define (make-joint account pwd npwd)
  (let ((base-account (account pwd 'account)))
    (lambda (try-password m)
      (if (eq? try-password npwd)
          (base-account m)
          (error "Incorrect joint password")))))