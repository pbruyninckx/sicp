#lang racket

; Exercise 3.43

; If they are run sequentially, then nothing odd can happen.
; I suppose this was the trivial case.

; If we would use the simple serialisation (i.e. per account only),
; then all actions operating on single accounts would be correct.
; However, when exchanging several accounts things could go wrong as follows:

; A 10 - B 20 - C 30
; (exchange C A) and (exchange C B)
; diff C A: 20
;                    diff C B: 10
; withdraw C 20-> C : 10
; withdraw C 10 -> C: 0
; deposit A 20 -> A: 30
; deposit B 10 -> B: 30
; Final result:
; A 30 - B 30 - C 0
; No money was lost or created

; If the individual accounts wouldn't be serialised,
; then there would be the ordinary option
; of having two immediate set! actions
; setting! values computed from out-dated variables