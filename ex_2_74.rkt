#lang racket
(require "getput.rkt")

(define db1 (cons 'db1 '()))
(define get-tag car)
(define get-data cdr)

(define (install-package-db1)
  (define (add-info-db1 empl address salary)
    (set! db1 (cons 'db1
                    (cons
               (list empl address salary)
               (get-data db1)))))

  (define (get-record rem name)
    (cond ((empty? rem) false)
          ((eq? name (caar rem))
           (car rem))
          (else (get-record (cdr rem) name))))
  (put 'add-record 'db1 add-info-db1)
  (put 'get-record 'db1
       (lambda (name)
         (get-record (get-data db1) name))))
(install-package-db1)


(define db2 (cons 'db2 '()))

(define (install-package-db2)
  (define (entry tree) (car tree))
  (define (left-branch tree) (cadr tree))
  (define (right-branch tree) (caddr tree))
  (define (make-tree entry left right)
    (list entry left right))
  (define (make-leaf x)
    (make-tree x '() '()))

  (define kv cons)
  (define key car)
  (define value cdr)

  (define (lookup given-key set-of-records)
    (cond ((null? set-of-records) false)
          ((string=? given-key (key (entry set-of-records)))
           (entry set-of-records))
          ((string<? given-key (key (entry set-of-records)))
           (lookup given-key (left-branch set-of-records)))
          (else
           (lookup given-key (right-branch set-of-records)))))

  (define (make-record name address salary)
    (list name salary address))
  
  (define (insert-into-tree tree record)
    (cond ((empty? tree) (make-leaf record))
          ((string<? (key record) (key (entry tree)))
           (make-tree (entry tree) (insert-into-tree (left-branch tree) record) (right-branch tree)))
          (else
           (make-tree (entry tree) (left-branch tree) (insert-into-tree (right-branch tree) record)))))
  (define (add-record name address salary)
    (set! db2 (cons 'db2
                    (insert-into-tree (get-data db2) (make-record name address salary)))))
  (define (get-record name)
    (let ((record (lookup name (get-data db2))))
      (if record
          (list (car record) (caddr record) (cadr record))
          false)))
    
  (put 'add-record 'db2 add-record)
  (put 'get-record 'db2 get-record))

(install-package-db2)

(define add-info-db1 (get 'add-record 'db1))
(add-info-db1 "a" "AHA" 1)
(add-info-db1 "c" "CDC" 3)
(add-info-db1 "b" "BEB" 4)

(define add-info-db2 (get 'add-record 'db2))
(add-info-db2 "bb" "BEBE" 40)
(add-info-db2 "cc" "CFCF" 50)
(add-info-db2 "aa" "AJAJ" 20)

(define (get-record db name)
  ((get 'get-record db) name))

(define (get-salary db name)
  (caddr ((get 'get-record db) name)))

(define (find-employee-record dbs name)
  (cond ((empty? dbs) false)
        (else
         (let ((rec ((get 'get-record (car dbs)) name)))
           (if rec
               rec
               (find-employee-record (cdr dbs) name))))))