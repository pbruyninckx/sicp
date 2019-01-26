#lang sicp
(#%require r5rs/init)

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest
                (map
                 (lambda (l) (cons (car s) l))
                 rest)))))

; Subets explanations:
; Recursive solution:
; Base case: If the list is empty, the list of subsets a list contaiing the empty list
; Otherwise:
; - List of subsets of the list apart from the first element
; - That list of subsets with the first element added (at the front)