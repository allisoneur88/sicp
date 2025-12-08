#lang sicp

(define (square x)
  (* x x))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y)) 
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y)) 
            (* (numer y) (denom x))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y)) 
     (* (numer y) (denom x))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define one-half (make-rat 1 2))
(define one-third (make-rat 1 3))
(print-rat (div-rat one-half one-third))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")"))

(define (make-segment start end)
  (cons start end))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (print-segment s)
  (newline)
  (print-point (start-segment s))
  (display " ->>  ")
  (print-point (end-segment s)))

(define (midpoint-segment s)
  (let((start-point (start-segment s)) (end-point (end-segment s)))
    (make-point
      (/ (+ (x-point start-point) (x-point end-point)) 2)
      (/ (+ (y-point start-point) (y-point end-point)) 2))))



(define segment (make-segment point1 point2))
(print-segment segment)

(define mid (midpoint-segment segment))
(print-point mid)

(define make-rect make-segment)

(define (a-point-rect r)
  (car r))

(define (d-point-rect r)
  (cdr r))

(define (distance-points p1 p2)
  (let((x1 (x-point p1))
       (y1 (y-point p1))
       (x2 (x-point p2))
       (y2 (y-point p2)))
    (sqrt (+ (square (- x2 x1)) (square (- y2 y1))))))

(define (area-rect r)
  (let((a (a-point-rect r)) (d (d-point-rect r)))
    (let((ax (x-point a))
         (ay (y-point a))
         (dx (x-point d))
         (dy (y-point d)))
      (let((b (make-point dx ay))
           (c (make-point ax dy)))
       (let ((ab (distance-points a b))
             (ac (distance-points a c)))
        (* ab ac))))))

(define (area-rect2 r)
  (let((a (a-point-rect r))
       (d (d-point-rect r)))
    (let ((width (abs (- (x-point d) (x-point a))))
          (height (abs (- (y-point d) (y-point a)))))
      (* width height))))

(define (perimeter-rect r)
  (let((a (a-point-rect r))
       (d (d-point-rect r)))
    (let ((width (abs (- (x-point d) (x-point a))))
          (height (abs (- (y-point d) (y-point a)))))
      (* (+ width height) 2))))
      
(define point1 (make-point 3 5))
(define point2 (make-point 5 7))
(print-point point1)
(print-point point2)

(distance-points point1 point2)

(define rect (make-rect point1 point2))
(area-rect rect)
(area-rect2 rect)
(perimeter-rect rect)
