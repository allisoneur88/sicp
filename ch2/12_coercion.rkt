#lang sicp
(load "11_generic_arithmetic_operations.rkt")

; we can include something like this into 
; complex package to achieve cross-type
; interaction

;(define (add-complex-to-schemenum z x)
  ;(make-from-real-imag (+ (real-part z) x) (imag-part z)))

;(put 'add '(complex scheme-number)
     ;(lambda (z x) (tag (add-complex-to-schemenum z x))))

; but it's cumbersome

; lets try coercion instead
; first lets create another table and associated methods

(define *coercion-table* '())

(define (put-coercion type1 type2 proc)
  (let ((record (assoc (list type1 type2) *coercion-table*)))
    (if record
     (set-cdr! record proc)
     (set! *coercion-table*
       (cons (cons (list type1 type2) proc)
        *coercion-table*))))
  'ok)

(define (get-coercion type1 type2)
  (let ((record (assoc (list type1 type2) *coercion-table*)))
    (if record
     (cdr record)
     #f)))

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))
;(display (scheme-number->complex schnum1))

(put-coercion 'scheme-number
              'complex
              scheme-number->complex)

; now we need to take into account
; our coercion plan in apply-generic
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
       (apply proc (map contents args))
       (if (= (length args) 2)
        (let ((type1 (car type-tags))
              (type2 (cadr type-tags))
              (a1 (car args))
              (a2 (cadr args)))
         (let ((t1->t2 (get-coercion type1 type2))
               (t2->t1 (get-coercion type2 type1)))
          (cond (t1->t2
                 (apply-generic op (t1->t2 a1) a2))
                (t2->t1
                 (apply-generic op a1 (t2->t1 a2)))
                (else (error "No method for these types: APPLY-GENERIC"
                             (list op type-tags))))))
        (error "No method for these types: APPLY-GENERIC"
               (list op type-tags)))))))

; now we can add complex to scheme-number
(display (add schnum1 comp1))
(display (add comp1 schnum1))
