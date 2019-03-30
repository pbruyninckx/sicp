#lang sicp

; Factorial
((lambda (n)
   ((lambda (fact) (fact fact n))
    (lambda (ft k)
      (if (= k 1)
          1
          (* k (ft ft (- k 1)))))))
 10)

; Fibonacci
((lambda (n)
   ((lambda (fib) (fib fib n))
    (lambda (ft k)
      (if (< k 3)
          1
          (+ (ft ft (- k 2)) (ft ft (- k 1)))))))
 6)

; Even/odd
(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0)
         true
         (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0)
         false
         (ev? ev? od? (- n 1))))))

(map f '(0 1 2 3 4))