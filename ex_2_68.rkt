#lang racket

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) 
                (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch 
                (car bits) 
                current-branch)))
          (if (leaf? next-branch)
              (cons 
               (symbol-leaf next-branch)
               (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) 
                        next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: 
               CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) 
         (cons x set))
        (else 
         (cons (car set)
               (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set 
         (make-leaf (car pair)    ; symbol
                    (cadr pair))  ; frequency
         (make-leaf-set (cdr pairs))))))


; Ex 2.67
(define sample-tree
  (make-code-tree 
   (make-leaf 'A 4)
   (make-code-tree
    (make-leaf 'B 2)
    (make-code-tree 
     (make-leaf 'D 1)
     (make-leaf 'C 1)))))

(define sample-message 
  '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(decode sample-message sample-tree)


; Ex 2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append 
       (encode-symbol (car message) 
                      tree)
       (encode (cdr message) tree))))
(define (encode-symbol symbol tree)
  (cond ((null? tree) (error "Empty tree"))
        ((leaf? tree)
         (if (eq? (symbol-leaf tree) symbol)
             '()
             (error "Symbol not found")))
        ((member symbol (symbols (left-branch tree)))
         (cons 0 (encode-symbol symbol (left-branch tree))))
        (else
         (cons 1 (encode-symbol symbol (right-branch tree))))))


; Ex 2.69

(define (generate-huffman-tree pairs)
  (successive-merge 
   (make-leaf-set pairs)))

(define (successive-merge leaves)
  (if (null? (cdr leaves))
      (car leaves)
      (successive-merge
       (adjoin-set
        (make-code-tree
         (car leaves)
         (cadr leaves))
        (cddr leaves)))))

(define pairs '((A 4) (B 2) (C 1) (D 1)))
(define leaves (make-leaf-set pairs))
(define my-tree (generate-huffman-tree pairs))
(decode (encode '(A D A B B C A) my-tree) my-tree)


; Ex 2.70
(define rock-pairs '((A 2) (BOOM 1) (GET 2) (JOB 2)
                           (NA 16) (SHA 3) (YIP 9) (WAH 1)))

(define rock-tree (generate-huffman-tree rock-pairs))

(define rock-msg
  '(GET A JOB
    SHA NA NA NA NA NA NA NA NA
    GET A JOB
    SHA NA NA NA NA NA NA NA NA
    WAH YIP YIP YIP YIP
    YIP YIP YIP YIP YIP
    SHA BOOM))

; Bits required with huffman encoding: 84
(length (encode rock-msg rock-tree))

; Bits required with fixed-length encoding: 108
(* 3 (length rock-msg))


; Ex 2.71
; 1 bit for most frequent symbol
; N-1 bits for least frequent symbol
(define skew-tree (generate-huffman-tree
                   '((A 1) (B 2) (C 4) (D 8) (E 16))))
(length (encode '(A) skew-tree))
(length (encode '(E) skew-tree))


; Ex 2.72
Probably O(n log(n))
; N: searching for symbol
; log(N) average depth of tree
