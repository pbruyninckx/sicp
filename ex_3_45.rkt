#lang sicp

; Exercise 3.45

; It tries to use the same serialiser twice.
; First: get serialiser.
; Inside the method the same serialiser is used for the
; withdraw/deposit method, and it will never get access to it.
