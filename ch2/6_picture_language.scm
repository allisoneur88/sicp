(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect
    (+ (xcor-vect v1) (xcor-vect v2))
    (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect
    (- (xcor-vect v1) (xcor-vect v2))
    (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect v factor)
  (make-vect
    (* factor (xcor-vect v))
    (* factor (ycor-vect v))))

(define vect1 (make-vect 3 3))
(define vect2 (make-vect 1 1))

(add-vect vect1 vect2)
(sub-vect vect1 vect2)
(scale-vect vect1 10)

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
      (origin-frame frame)
      (add-vect
       (scale-vect (xcor-vect v) (edge1-frame frame))
       (scale-vect (ycor-vect v) (edge2-frame frame))))))

(define (make-frame-list origin edge1 edge2)
  (list origin edge1 edge2))
(define frame (make-frame-list (make-vect 1 1) (make-vect 3 1) (make-vect 1 2)))

(define (origin-list frame)
  (car frame))
(define (edge1-list frame)
  (cadr frame))
(define (edge2-list frame)
  (caddr frame))
(origin-list frame)
(edge1-list frame)
(edge2-list frame)

(define (make-frame-cons origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define frame-cons (make-frame-cons (make-vect 1 1) (make-vect 3 1) (make-vect 1 2)))

(define (origin-cons frame)
  (car frame))
(define (edge1-cons frame)
  (cadr frame))
(define (edge2-cons frame)
  (cddr frame))

(origin-cons frame-cons)
(edge1-cons frame-cons)
(edge2-cons frame-cons)

(define a ''abracadabra)
(define b (quote (quote abracadab)))

(display a)
(display b)
