#lang racket
; ex 4.15


(define (run-forever)
  (run-forever))

(define (try p)
  (if (halts? p p)
      (run-forever)
      'halted))

(try try)
; assume (halts? try) -> true
; -> will run forever, hence try won't halt

; assume (halts? try) -> false
; -> will return 'halted, the oppostire of (halts? try)