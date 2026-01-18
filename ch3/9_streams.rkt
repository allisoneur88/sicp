#lang sicp

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-line x)
  (newline)
  (display x))

(define (display-stream s)
  (stream-for-each display-line s))

(define (cons-stream a b)
  (cons a (delay b)))

(define (stream-car stream)
  (car stream))
(define (stream-cdr stream)
  (force (cdr stream)))

; prime? predicate real quick
(define (prime? n)
  (cond ((< n 2) #f)
        (else (smallest-divisor n))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (* test-divisor test-divisor) n) #t)
        ((divides? test-divisor n) #f)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

; let's alanlyze this computation
(stream-car
  (stream-cdr
    (stream-filter prime?
                   (stream-enumerate-interval 10000 1000000))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
                   (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred (stream-cdr stream))))
        (else
         (stream-filter pred (stream-cdr stream)))))

; what about delay and force?
(define (delay expression)
  (lambda () expression))

(define (force delayed-object)
  (delayed-object))

; there is an important optimization that
; we can include - memoization
(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
       (begin (set! result (proc))
              (set! already-run? true)
              result)
       result))))

; delay is then defined as
(define (delay expression)
  (memo-proc (lambda () expression)))

; ex 3.51
(define (show x)
  (display-line x)
  x)

(define x
  (stream-map show
              (stream-enumerate-interval 0 10)))

(stream-ref x 5)
(stream-ref x 7)
              
