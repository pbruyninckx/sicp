#lang sicp

;a <- bq + aq + ap
;b <- bp + aq
;
;a2 <- (bp+aq)q + (bq+aq+ap)q + (bq+aq+ap)p
;   <- bpq + aqq + bqq + aqq + apq + bpq + apq + app
;   <- 2*bpq + 2*aqq + bqq + 2*apq + app
;   <- b*(2bq + qq) + a*(2qq + 2pq + pp)
;   <- b*(2qp + qq) + a*(2qp + qq) + a *(pp + qq)
;
;
;
;b2 <- (bp+aq)p + (bq+aq+ap)q
;   <- b*(pp + qq) + a*(2qp + qq)
;
;
;=>
;p' = pp + qq
;q' = 2qp + qq

(define (slow-fib n)
  (if (< n 2)
      n
      (+ (fib (dec n)) (fib (dec (dec n))))))

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) 
         b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))
                   (+ (* 2 p q) (* q q))
                   (/ count 2)))
        (else 
         (fib-iter (+ (* b q) 
                      (* a q) 
                      (* a p))
                   (+ (* b p) 
                      (* a q))
                   p
                   q
                   (- count 1)))))