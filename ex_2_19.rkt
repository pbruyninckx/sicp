#lang sicp
(#%require r5rs/init)

(define us-coins (list 50 25 10 5 1))

(define uk-coins (list 200 100 50 20 10 5 2 1))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define no-more? null?)
(define except-first-denomination cdr)
(define first-denomination car)

(cc 100 us-coins)

