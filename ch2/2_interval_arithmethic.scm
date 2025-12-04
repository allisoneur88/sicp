(define (make-interval a b)
  (cons a b))

(define (lower-bound i)
  (car i))

(define (upper-bound i)
  (cdr i))

(define (add-interval x y)
  (make-interval
    (+ (lower-bound x) (lower-bound y))
    (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let((p1 (* (lower-bound x) (lower-bound y)))
       (p2 (* (lower-bound x) (upper-bound y)))
       (p3 (* (upper-bound x) (lower-bound y)))
       (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
       (max p1 p2 p3 p4))))

(define (print-interval i)
  (newline)
  (display (lower-bound i))
  (display " .. ")
  (display (upper-bound i)))
 
(define (test a b c d)
  (let ((i1 (make-interval a b))
        (i2 (make-interval c d)))
    (print-interval i1)
    (print-interval i2)
    (print-interval (add-interval i1 i2))
    (print-interval (mul-interval i1 i2))))

(test 1.9 2.1 2.8 3.2)
