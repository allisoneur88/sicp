#lang sicp

(define (square x) (* x x))
(define (attach-tag tag object)
  (cons tag object))

;rectangular package
(define (install-rectangular-package)
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))

  (define (magnitude z)
    (sqrt
      (+ (square (real-part z)) (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  (define (tag x) (attach-tag 'rectangular x))

  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;polar package
(define (install-polar-package)
  (define (real-part z) 
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons
      (sqrt (+ (square x) (square y)))
      (atan y x)))

  (define (magnitude z)
    (car z))
  (define (angle z)
    (cdr z))
  (define (make-from-mag-ang r a)
    (cons r a))

  (define (tag x) (attach-tag 'polar x))

  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;type-tag definition can be found in the previous listing
;as well as contents
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
       (apply proc (map contents args))
       (error
        "No method for these types: APPLY-GENERIC"
        (list op type-tags))))))

;now we can extract constructors from the op table
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))


; message passing
(define (make-from-real-imag-mes-pas x y)
  (define (dispatch op)
    (cond
      ((eq? op 'real-part) x)
      ((eq? op 'imag-part) y)
      ((eq? op 'magnitude) (sqrt (+ (square x) (square y))))
      ((eq? op 'angle) (atang y x))
      (else (error "Unknown op: MAKE-FROM-REAL-IMAG-MES-PAS" op))))
  dispatch)

(define (make-from-mag-ang-mes-pas r a)
  (define (dispatch op)
    (cond
      ((eq? op 'magnitude) r)
      ((eq? op 'angle) a)
      ((eq? op 'real-part) (* r (cos a)))
      ((eq? op 'imag-part) (* r (sin a)))
      (else (error "Unknown op: MAKE-FROM-MAG-ANG-MES-PAS" op))))
  dispatch)

(define (apply-generic-mes-pas op arg)
  (arg op))

(define num (make-from-real-imag-mes-pas 3 5))

(define (real-part-mes-pas z)
  (apply-generic-mes-pas 'real-part z))
(define (imag-part-mes-pas z)
  (apply-generic-mes-pas 'imag-part z))

(display (real-part-mes-pas num))
(display (imag-part-mes-pas num))
