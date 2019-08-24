#lang racket
; Ex 4.26
; unless has been added to eval-applay as a special form

; Example of places where higher-order function can not be used
;(map and '(#f #f #t #t) '(#f #t #f #t))