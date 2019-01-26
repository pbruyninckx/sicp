#lang sicp

(#%require sicp-pict)


; Ex 2.44

;(define (up-split painter n)
;  (if (= n 0)
;      painter
;      (let ((up (up-split painter (dec n))))
;        (below painter
;               (beside up up)))))
;
;(define (right-split painter n)
;  (if (= n 0)
;      painter
;      (let ((right (right-split painter (dec n))))
;        (beside painter
;                (below right right)))))


(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (dec n)))
            (right (right-split painter (dec n)))
            (corner (corner-split painter (dec n))))
        (below
         (beside painter
                 (below right right))
         (beside (beside up up)
                 corner)))))


; Ex 2.45

(define (split op1 op2)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split op1 op2) painter (dec n))))
          (op1 painter
               (op2 smaller smaller))))))

(define right-split (split beside below))
(define up-split (split below beside))

(paint (up-split einstein 3))
(paint (right-split einstein 3))
(paint (corner-split einstein 3))


; Ex 2.46
(define (in-unit x)
  (max (min x .99) 0))
;(define (make-vect x y)
;  (cons (in-unit x) (in-unit y)))
;(define xcor-vect car)
;(define ycor-vect cdr)
(define xcor-vect vector-xcor)
(define ycor-vect vector-ycor)
(define (op-vect op a b)
  (make-vect
   (op (xcor-vect a) (xcor-vect b))
   (op (ycor-vect a) (ycor-vect b))))
(define (add-vect a b) (op-vect + a b))
(define (sub-vect a b) (op-vect - a b))
(define (scale-vect s v)
  (make-vect
   (* s (xcor-vect v))
   (* s (ycor-vect v))))


; Ex 2.47
;(define (make-frame origin edge1 edge2)
;  (list origin edge1 edge2))
;(define origin-frame car)
;(define edge1-frame cadr)
;(define edge2-frame caddr)

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define origin-frame car)
(define edge1-frame cadr)
(define edge2-frame cddr)

; Ex 2.48
(define (make-segment start-segment end-segment)
  (cons start-segment end-segment))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))

; Ex 2.49
(define (outline-painter)
  (let ((bl (make-vect 0 0))
        (br (make-vect 0 1))
        (tr (make-vect 1 1))
        (tl (make-vect 1 0)))
  (segments->painter
   (list (make-segment bl br)
         (make-segment br tr)
         (make-segment tr tl)
         (make-segment tl bl)))))
(paint (outline-painter))

(define (X-painter)
  (let ((bl (make-vect 0 0))
        (br (make-vect 0 1))
        (tr (make-vect 1 1))
        (tl (make-vect 1 0)))
  (segments->painter
   (list (make-segment bl tr)
         (make-segment tl br)))))
(paint (X-painter))

(define (diamon-painter)
  (let ((top (make-vect .5 1))
        (left (make-vect 0 .5))
        (bottom (make-vect .5 0))
        (right (make-vect 1 .5)))
    (segments->painter
     (list (make-segment top left)
           (make-segment left bottom)
           (make-segment bottom right)
           (make-segment right top)))))
(paint (diamon-painter))

(define (wave-painter)
  (let ((p1 (make-vect 0 .7))
        (p2 (make-vect .45 .69))
        (p3 (make-vect .4 .85))
        (p4 (make-vect .45 1))
        (p5 (make-vect .55 1))
        (p6 (make-vect .6 .85))
        (p7 (make-vect .55 .69))
        (p8 (make-vect 1 .3))
        (p9 (make-vect 0 .6))
        (p10 (make-vect .4 .6))
        (p11 (make-vect .2 0))
        (p12 (make-vect .3 0))
        (p13 (make-vect .5 .35))
        (p14 (make-vect .7 0))
        (p15 (make-vect .8 0))
        (p16 (make-vect .6 .6))
        (p17 (make-vect 1 .2)))
    (segments->painter
     (list (make-segment p1 p2)
           (make-segment p2 p3)
           (make-segment p3 p4)
           (make-segment p5 p6)
           (make-segment p6 p7)
           (make-segment p7 p8)
           (make-segment p9 p10)
           (make-segment p10 p11)
           (make-segment p12 p13)
           (make-segment p13 p14)
           (make-segment p15 p16)
           (make-segment p16 p17)))))
(paint (wave-painter))

; Ex 2.50
(define flip-horiz
  (transform-painter
   (make-vect 1.0 0.0)
   (make-vect 0.0 0.0)
   (make-vect 1.0 1.0)))
(paint (flip-horiz einstein))

(define rotate-180
  (transform-painter
   (make-vect 1.0 1.0)
   (make-vect 0.0 1.0)
   (make-vect 1.0 0.0)))
(paint (rotate-180 einstein))

(define rotate-270
  (transform-painter
   (make-vect 0.0 1.0)
   (make-vect 0.0 0.0)
   (make-vect 1.0 1.0)))
(paint (rotate-270 einstein))

(define (rotate-90 p)
  (rotate-270
    (rotate-180 p)))

; Ex 2.51
(define (my-below p-up p-low)
  (let ((up-painter
         (transform-painter
          (make-vect 0.0 0.5)
          (make-vect 1.0 0.5)
          (make-vect 0.0 1.0)))
        (low-painter
         (transform-painter
          (make-vect 0.0 0.0)
          (make-vect 1.0 0.0)
          (make-vect 0.0 0.5))))
    (lambda (frame)
      ((up-painter p-up) frame)
      ((low-painter p-low) frame))))
(paint (my-below einstein diagonal-shading))

(define (my-below2 p-up p-low)
  (rotate-90
   (beside
    (rotate-270 p-low)
    (rotate-270 p-up))))
(paint (my-below2 einstein diagonal-shading))

