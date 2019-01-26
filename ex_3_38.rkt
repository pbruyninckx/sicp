#lang sicp

; Ex 3.38
; There are six possible orders, of which two and two are identical,
; leaving 4 possible orders:

; (+ 10 -20) halve -> 45
; +10 halve -20    -> 35
; -20 halve +10    -> 50
; halve (-20 +10)  -> 40

; Some other values
; start halve
;               read balanve for +10
;                              read belance for  -20
; set result of halve: 50
;                              set belance for -20: 80
;               set balance for +10: 110
; ...
