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
  (define handle-password
    (let ((wrong-counts 0))
      (lambda ( p f-pass f-fail f-multifail)
        (if (eq? p my-password)
            (begin
              (set! wrong-counts 0)
              (f-pass))
            (begin
              (set! wrong-counts (+ wrong-counts 1))
              (if (>= wrong-counts 7)
                  (f-multifail)
                  (f-fail)))))))
  (define (call-the-cops)
    (error "Calling the COPS"))
  (define (dispatch try-password m)
    (handle-password
     try-password
     (lambda ()
       (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request: 
                 MAKE-ACCOUNT" m))))
     (lambda () (error "Incorrect password"))
     call-the-cops))
  dispatch)