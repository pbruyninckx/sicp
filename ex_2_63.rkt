#lang racket

; Ex 2.63
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))
(define (make-leaf x)
  (make-tree x '() '()))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? 
          x 
          (left-branch set)))
        ((> x (entry set))
         (element-of-set? 
          x 
          (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree 
          (entry set)
          (adjoin-set x (left-branch set))
          (right-branch set)))
        ((> x (entry set))
         (make-tree
          (entry set)
          (left-branch set)
          (adjoin-set x (right-branch set))))))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append 
       (tree->list-1 
        (left-branch tree))
       (cons (entry tree)
             (tree->list-1 
              (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list 
         (left-branch tree)
         (cons (entry tree)
               (copy-to-list 
                (right-branch tree)
                result-list)))))
  (copy-to-list tree '()))

(define tree->list tree->list-2)

(define tree1
  (make-tree 7
             (make-tree 3 (make-tree 1 '() '()) (make-tree 5 '() '()))
             (make-tree 9 '() (make-tree 11 '() '()))))

(define tree2
  (make-tree 3
             (make-tree 1 '() '())
             (make-tree
              7
              (make-tree 5 '() '())
              (make-tree 9 '() (make-tree 11 '() '())))))

(define tree3
  (make-tree
   5
   (make-tree
    3
    (make-leaf 1)
    '())
   (make-tree
    9
    (make-leaf 7)
    (make-leaf 11))))

(define (list->tree elements)
  (car (partial-tree 
        elements (length elements))))

; Ex 2.46
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size 
             (quotient (- n 1) 2)))
        (let ((left-result 
               (partial-tree 
                elts left-size)))
          (let ((left-tree 
                 (car left-result))
                (non-left-elts 
                 (cdr left-result))
                (right-size 
                 (- n (+ left-size 1))))
            (let ((this-entry 
                   (car non-left-elts))
                  (right-result 
                   (partial-tree 
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree 
                     (car right-result))
                    (remaining-elts 
                     (cdr right-result)))
                (cons (make-tree this-entry 
                                 left-tree 
                                 right-tree)
                      remaining-elts))))))))
; How list->tree works
; It assumes the input list is sorted, so all that is needed is:
; - split list as
;   * first half of elements
;   * centre element
;   * remaining half of elements
; - Construct a tree (recursively) for first half and second half of elements
; - combine two trees with centre element into a new tree
;
; Given that in scheme the creation of a sublist of the first n elements
; is a fairly expensive operation, the method does not create this sub-list,
; but instead opts for a function that mentions how many elements of the
; function we like to process, and returns the yet unprocessed elements,
; so there are no un-needed list administration operations.
;
; Order of growth is O(n)

; Ex 2.56

(define (union-list l1 l2)
  (define (helper l1 l2 acc)
    (cond ((empty? l2)
           (foldl cons acc l1))
          ((empty? l1)
           (foldl cons acc l2))
          ((= (car l1) (car l2))
           (helper (cdr l1) (cdr l2) (cons (car l1) acc)))
          ((< (car l1) (car l2))
           (helper (cdr l1) l2 (cons (car l1) acc)))
          (else
           (helper l1 (cdr l2) (cons (car l2) acc)))))
  (reverse (helper l1 l2 '())))

(define (intersection-list l1 l2)
  (define (helper l1 l2 acc)
    (cond ((empty? l2)
           acc)
          ((empty? l1)
           acc)
          ((= (car l1) (car l2))
           (helper (cdr l1) (cdr l2) (cons (car l1) acc)))
          ((< (car l1) (car l2))
           (helper (cdr l1) l2 acc))
          (else
           (helper l1 (cdr l2) acc))))
  (reverse (helper l1 l2 '())))

(define (union-set t1 t2)
  (let ((l1 (tree->list t1))
        (l2 (tree->list t2)))
    (list->tree (union-list l1 l2))))

(define (intersection-set t1 t2)
  (list->tree (intersection-list
               (tree->list t1)
               (tree->list t2))))
  
