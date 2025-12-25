#lang sicp

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
       (apply proc (map contents args))
       (error
        "No method for these types: APPLY-GENERIC"
        (list op type-tags))))))

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


; our target
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

; ordinary numbers
(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  
  (put 'make 'scheme-number (lambda (x) (tag x)))

  (put 'equ? 'scheme-number
       (lambda (n1 n2) (= n1 n2)))

  'done)

; then users of the package can do
(define (make-scheme-number x)
  ((get 'make 'scheme-number) x))
; and so on

; rational numbers
(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
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
    
  (define (tag x) (attach-tag 'rational x))
  
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  
  (put 'make 'rational
       (lambda n d) (tag (make-rat n d)))

  (put 'equ? 'rational
       (lambda (r1 r2)
        (and (= (numer r1) (numer r2))
             (= (denom r1) (denom r2)))))

  'done)

; then users can do
(define (make-rational n d)
  ((get 'make 'rational) n d))

; complex package
(define (install-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))

  (define (tag z) (attach-tag 'complex z))

  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))

  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))

  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)

  (put 'equ? 'complex
       (lambda (z1 z2)
        (if (eq? (type-tag z1) (type-tag z2))
         (cond ((eq? (type-tag z1) 'rectangular)
                (and (= (real-part z1) (real-part z2))))
               ((eq? (type-tag z1) 'polar)
                (and (= (magnitude z1) (magnitude z2))
                     (= (angle z1) (angle z2))))
               (else (error "Unknown complex type: 'equ? 'complex" (list z1 z2))))
         #f)))

  'done)

; then users can do
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (install-generic-package)
  (install-scheme-number-package)
  (install-rational-package)
  (install-complex-package))

(define (equ? n1 n2)
  (if (eq? (type-tag n1) (type-tag n2))
      (cond
       ((eq? (type-tag n1) 'scheme-number)
        (= (contents n1) (contents n2)))
       ((eq? (type-tag n1) 'rational)
        (and (= (numer (contents n1)) (numer (contents n2)))
             (= (denom (contents n1)) (denom (contents n2)))))
       ((eq? (type-tag n1) 'complex)
        (let ((z1 (contents n1))
              (z2 (contents n2)))
         (if (eq? (type-tag z1) (type-tag z2))
          (cond
           ((eq? (type-tag z1) 'rectangular)
            (and (= (real-part (contents z1)) (real-part (contents z2)))
                 (= (imag-part (contents z1)) (imag-part (contents z2)))))
           ((eq? (type-tag z1) 'polar)
            (and (= (magnitude (contents z1)) (magnitude (contents z2)))
                 (= (angle (contents z1)) (angle (contents (z2))))))
           (else (error "Unknown complex type: EQU?" (list n1 n2)))))
         false))
       (else (error "Unknown number type: EQU?" (list n1 n2))))
      false))
