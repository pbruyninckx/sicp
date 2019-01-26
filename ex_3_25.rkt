#lang sicp

;(define (lookup key table)
;  (let ((record (assoc key (cdr table))))
;    (if record
;        (cdr record)
;        false)))



(define (make-table)
  (define (assoc key records)
    (cond ((null? records) false)
          ((eq? key (caar records))
           (car records))
          (else (assoc key (cdr records)))))

  (let ((local-table (list '*table*)))
    (define (lookup keys table)
      (if (null? keys)
          (cdr table)
          (let ((subtable
                 (assoc (car keys) (cdr table))))
            (if subtable
                (lookup (cdr keys) subtable)
                false))))

    (define (insert! keys value table)
      (if (null? keys)
          (set-cdr! table value)
          (let ((subtable
                 (assoc (car keys) (cdr table))))
            (if subtable
                (insert! (cdr keys) value subtable)
                (let ((new-subtable (cons (car keys) '())))
                  (insert! (cdr keys) value new-subtable)
                  (set-cdr!
                   table
                   (cons new-subtable (cdr table)))))))

      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) (lambda (keys) (lookup (cons '*table* keys) local-table)))
            ((eq? m 'insert-proc!) (lambda (keys value) (insert! (cons '*table* keys) value local-table)))
            (else (error "Unknown operation: TABLE " m))))
    dispatch))

