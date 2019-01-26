#lang racket

(require "helpers.rkt")

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) 
           (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position 
                    new-row 
                    k 
                    rest-of-queens))
                 (enumerate-interval 
                  1 
                  board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (queens-slow board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) 
           (safe? k positions))
         (flatmap
          (lambda (new-row)
            (map (lambda (rest-of-queens)
                   (adjoin-position 
                    new-row k rest-of-queens))
                 (queen-cols (- k 1))))
          (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

(define empty-board '())


(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))

(define (safe? k positions)
  (let ((new-pos (car positions)))
    (define (safe-subpos? diff rem-positions)
      (if (empty? rem-positions)
          true
          (if
           (or
            (= new-pos (car rem-positions))
            (= new-pos (- (car rem-positions) diff))
            (= new-pos (+ (car rem-positions) diff)))
           false
           (safe-subpos? (inc diff) (cdr rem-positions)))))
    (if (empty? positions)
        #t
        (safe-subpos? 1 (cdr positions)))))

; Ex 2.43
; It will be slow since (queen-cols) will be called board-size times on each recursion.
; Hence the time needed will be T * board-size^board-size