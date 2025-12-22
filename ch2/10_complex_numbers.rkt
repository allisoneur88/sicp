#lang sicp

(define (square x) (* x x))
; high-level arithmetic operations
(define (add-complex z1 z2)
  (make-from-real-imag
    (+ (real-part z1) (real-part z2))
    (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag
    (- (real-part z1) (real-part z2))
    (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang
    (* (magnitude z1) (magnitude z2))
    (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang
    (/ (magnitude z1) (magnitude z2))
    (- (angle z1) (angle z2))))

; first representation - ben bitdiddle
(define (real-part1 z) (car z))
(define (imag-part1 z) (cdr z))
(define (magnitude1 z)
  (sqrt (+ (square (real-part1 z)) (square (imag-part1 z)))))
(define (angle1 z)
  (atan (imag-part1 z) (real-part1 z)))

(define (make-from-real-imag1 x y)
  (cons x y))
(define (make-from-mag-ang1 r a)
  (cons (* r (cos a)) (* r (sin a))))

; second representation - alyssa
(define (real-part2 z) (* (magnitude2 z) (cos (angle2 z))))
(define (imag-part2 z) (* (magnitude2 z) (sin (angle2 z))))
(define (magnitude2 z) (car z))
(define (angle2 z) (cdr z))
(define (make-from-real-imag2 x y)
  (cons
    (sqrt (+ (square x) (square y)))
    (atan y x)))
(define (make-from-mag-ang2 r a)
  (cons r a))

; tagged data
(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))
(define (polar? z)
  (eq? (type-tag z) 'polar))

; first representation
(define (real-part-rect z) (car z))
(define (imag-part-rect z) (cdr z))
(define (magnitude-rect z)
  (sqrt (+ (square (real-part-rect z)) (square (imag-part-rect z)))))
(define (angle-rect z)
  (atan
    (imag-part-rect z)
    (real-part-rect z)))

(define (make-from-real-imag-rect x y)
  (attach-tag 'rectangular (cons x y)))
(define (make-from-mag-ang-rect r a)
  (attach-tag 'rectangular
              (cons (* r (cos a)) (* r (sin a)))))

; second representation
(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))
(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))
(define (magnitude-polar z)
  (car z))
(define (angle-polar z)
  (cdr z))

(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
              (cons
               (sqrt (+ (square x) (square y)))
               (atan y x))))
(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar
              (cons r a)))

; generic selectors
(define (real-part-generic z)
  (cond
    ((rectangular? z)
     (real-part-rect (contents z)))
    ((polar? z)
     (real-part-polar (contents z)))
    (else
      (error "Unknown-type: REAL-PART-GENERIC" z))))

(define (imag-part-generic z)
  (cond
    ((rectangular? z)
     (imag-part-rect (contents z)))
    ((polar? z)
     (imag-part-polar (contents z)))
    (else
      (error "Unknown type: IMAG-PART-GENERIC" z))))

(define (magnitude-generic z)
  (cond
    ((rectangular? z)
     (magnitude-rect (contents z)))
    ((polar? z)
     (magnitude-polar (contents z)))
    (else
      (error "Unknown type: MAGNITUDE-GENERIC" z))))

(define (angle-generic z)
  (cond
    ((rectangular? z)
     (angle-rect (contents z)))
    ((polar? z)
     (angle-polar (contents z)))
    (else
      (error "Unknown type: ANGLE-GENERIC" z))))

; constuctors can be like this
(define (make-from-real-imag-generic x y)
  (make-from-real-imag-rect x y))
(define (make-from-mag-ang-generic r a)
  (make-from-mag-ang-polar r a))

; implementing arithmetics is straightforward
; for example addition
(define (add-complex-generic z1 z2)
  (make-from-real-imag-generic
    (+ (real-part-generic z1) (real-part-generic z2))
    (+ (imag-part-generic z1) (imag-part-generic z2))))

