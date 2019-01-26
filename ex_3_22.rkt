#lang sicp

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (set-front-ptr! item)
      (set! front-ptr item))
    (define (set-rear-ptr! item)
      (set! rear-ptr item))

    (define (dispatch m)
      (cond ((eq? m 'front-ptr) front-ptr)
            ((eq? m 'rear-ptr) rear-ptr)
            ((eq? m 'set-front-ptr!) set-front-ptr!)
            ((eq? m 'set-rear-ptr!) set-rear-ptr!)))
    dispatch))

(define (front-ptr q) (q 'front-ptr))
(define (rear-ptr q) (q 'rear-ptr))
(define (set-front-ptr! q item)
  ((q 'set-front-ptr!) item))
(define (set-rear-ptr! q item)
  ((q 'set-rear-ptr!) item))


(define (empty-queue? q)
  (null? (front-ptr q)))

(define (front-queue q)
  (if (empty-queue? q)
      (error "FRONT called with an empty queue" q)
      (car (front-ptr q))))

(define (insert-queue! q item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? q)
           (set-front-ptr! q new-pair)
           (set-rear-ptr! q new-pair)
           q)
          (else (set-cdr! (rear-ptr q)
                          new-pair)
                (set-rear-ptr! q new-pair)
                q))))

(define (delete-queue! q)
  (cond ((empty-queue? q)
         (error "DELETE! called with 
                 an empty queue" q))
        (else (set-front-ptr!
               q
               (cdr (front-ptr q)))
              q)))

(define (print-queue q)
  (display (front-ptr q)))