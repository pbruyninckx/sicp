#lang sicp

; Exercise 3.44

; This look sperfectly fine to me.
; It doesn't matter whether other withdraw/deposit operations
; are happening in between.
; The only thing that might go wrong is when from-account
; doesn't have at least amount, which is explicitly
; assumed to be not the case.