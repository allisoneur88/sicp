#lang sicp

(define pi (acos -1))
(define (square x) (* x x))
; get and put impls
(define *operation-table* '())

(define (put op type-tags proc)
  (let ((record (assoc (list op type-tags) *operation-table*)))
    (if record
     (set-cdr! record proc)
     (set! *operation-table*
       (cons (cons (list op type-tags) proc)
        *operation-table*))))
  'ok)

(define (get op type-tags)
  (let ((record (assoc (list op type-tags) *operation-table*)))
    (if record
     (cdr record)
     #f)))

(define (show-table)
  (for-each (lambda (entry)
             (display (car entry))
             (display " => ")
             (display (cdr entry))
             (newline))
      *operation-table*))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
       (apply proc (map contents args))
       (error
        "No method for these types: APPLY-GENERIC"
        (list op type-tags))))))

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else (error "Bad tagged datum: TYPE-TAG" datum))))
      
(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Bad tagged datum: CONTENTS" datum)))) 

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

  (put 'equ? '(scheme-number scheme-number)
       (lambda (n1 n2) (= n1 n2)))

  (put '=zero? '(scheme-number)
       (lambda (n) (= n 0)))

  (put 'negate '(scheme-number)
       (lambda (n) (tag (* n -1))))

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
       (lambda (n d) (tag (make-rat n d))))

  (put 'equ? '(rational rational)
       (lambda (r1 r2)
        (and (= (numer r1) (numer r2))
             (= (denom r1) (denom r2)))))

  (put '=zero? '(rational)
       (lambda (r) (= (numer r) 0)))

  (put 'negate '(rational)
       (lambda (r)
        (make-rat (* (numer r) -1) (denom r))))

  'done)

; then users can do
(define (make-rational n d)
  ((get 'make 'rational) n d))

; previous complex packages - rect and polar
; rectangular package
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

  (put 'equ? '(rectangular rectangular)
       (lambda (z1 z2)
        (and (= (real-part z1) (real-part z2))
             (= (imag-part z1) (imag-part z2)))))

  (put '=zero? '(rectangular)
       (lambda (z)
        (and (= (real-part z) 0)
             (= (imag-part z) 0))))

  (put 'negate '(rectangular)
       (lambda (z)
        (tag (make-from-real-imag
              (* (real-part z) -1)
              (* (imag-part z) -1)))))
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

  (put 'equ? '(polar polar)
       (lambda (z1 z2)
        (and (= (magnitude z1) (magnitude z2))
             (= (angle z1) (angle z2)))))

  (put '=zero? '(polar)
       (lambda (z)
        (= (magnitude z) 0)))

  (put 'negate '(polar)
       (lambda (z)
        (tag (make-from-mag-ang
              (magnitude z)
              (+ (angle z) pi)))))

  'done)

; new complex package
(define (install-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  (define (real-part z)
    (apply-generic 'real-part z))
  (define (imag-part z)
    (apply-generic 'imag-part z))
  (define (magnitude z)
    (apply-generic 'magnitude z))
  (define (angle z)
    (apply-generic 'angle z))

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

  (define (equ? z1 z2)
    (apply-generic 'equ? z1 z2))

  (define (=zero? z)
    (apply-generic '=zero? z))

  (define (negate z)
    (tag (apply-generic 'negate z)))

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

  (put 'equ? '(complex complex) equ?)

  (put '=zero? '(complex) =zero?)
  (put 'negate '(complex) negate)

  'done)

; then users can do
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (install-generic-package)
  (install-rectangular-package)
  (install-polar-package)
  (install-scheme-number-package)
  (install-rational-package)
  (install-complex-package))


(install-generic-package)

(display (make-scheme-number 5))
(display (make-rational 3 5))

(display (make-complex-from-real-imag 3 5))
(display (make-complex-from-mag-ang 3 5))

(define (rp z)
  (apply-generic 'real-part z))
(display (rp (make-complex-from-real-imag 3 5)))

(show-table)

(define (equ? n1 n2)
  (apply-generic 'equ? n1 n2))

(define (=zero? n)
  (apply-generic '=zero? n))

(define (negate n)
  (apply-generic 'negate n))

(equ? (make-scheme-number 3) (make-scheme-number 3))
(equ? (make-rational 3 6) (make-rational 3 5))
(equ? (make-complex-from-real-imag 3 5) (make-complex-from-real-imag 3 5))
(equ? (make-complex-from-mag-ang 3 5) (make-complex-from-mag-ang 3 5))
(equ? (make-complex-from-real-imag 3 5) (make-complex-from-mag-ang 3 5))

(=zero? (make-scheme-number 3))
(=zero? (make-scheme-number 0))

(=zero? (make-rational 3 5))
(=zero? (make-rational 0 5))

(=zero? (make-complex-from-real-imag 3 5))
(=zero? (make-complex-from-real-imag 0 5))
(=zero? (make-complex-from-real-imag 3 0))
(=zero? (make-complex-from-real-imag 0 0))

(=zero? (make-complex-from-mag-ang 3 5))
(=zero? (make-complex-from-mag-ang 3 0))
(=zero? (make-complex-from-mag-ang 0 5))
(=zero? (make-complex-from-mag-ang 0 0))

(define schnum1 (make-scheme-number 3))
(define schnum2 (make-scheme-number 5))
(define rat1 (make-rational 3 5))
(define rat2 (make-rational 4 7))
(define comp1 (make-complex-from-real-imag 3 5))
(define comp2 (make-complex-from-mag-ang 4 6))

(display (add schnum1 schnum2))
(display (sub schnum1 schnum2))
(display (mul schnum1 schnum2))
(display (div schnum1 schnum2))

(display (add rat1 rat2))
(display (sub rat1 rat2))
(display (mul rat1 rat2))
(display (div rat1 rat2))

(display (add comp1 comp1))
(display (sub comp1 comp1))
(display (mul comp1 comp1))
(display (div comp1 comp1))

(display (add comp2 comp2))
(display (sub comp2 comp2))
(display (mul comp2 comp2))
(display (div comp2 comp2))

(display (add comp1 comp2))
(display (sub comp1 comp2))
(display (mul comp1 comp2))
(display (div comp1 comp2))

(display (negate schnum1))
(display (negate rat1))
(display (negate comp1))
(display (negate comp2))

(display (add comp1 (negate comp1)))
(display (add comp2 (negate comp2)))
