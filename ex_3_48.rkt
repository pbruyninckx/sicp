#lang sicp

; Exercise 3.48
; A deadlock can occur when e.g.
; Process A has a lock, and process B has another lock,
; and they both want to acquire the resource that's locked
; by the other one.
; This can not happen when all processes lock all objects
; in the same order.

; make-account adapted to have a number
(define last-account-nb -1)

(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin 
          (set! balance (- balance amount))
          balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (get-next-account-nb)
    (set! last-account-nb (+ 1 last-account-nb))
    last-account-nb)
  (define account-number (get-next-account-nb))
  (let ((balance-serializer 
         (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) 
             balance-serializer)
            ((eq? m 'number) account-number)
            (else (error "Unknown request: 
                          MAKE-ACCOUNT"
                         m))))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((if ((< (account1 'number) (account2 'number))
         (serializer2 (serializer1 exchange))
         (serializer1 (serializer2 exchange))
     account1
     account2)))))