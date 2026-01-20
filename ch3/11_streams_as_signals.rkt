#lang sicp

; just to practice a little
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc stream)
  (if (null-stream? stream) 
      the-empty-stream
      (cons-stream (proc (stream-car stream))
                   (stream-map proc (stream-cdr stream)))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (scale-stream s factor)
  (stream-map (lambda (x) (* x factor))
              s))

(define (add-streams s1 s2) (stream-map + s1 s2))

; integral
(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

; ex. 3.73
; RC-circuit
; v = v0 + (1/C) * (∫i dt) + R*i
(define (RC r c dt)
  (define (model i-stream v0)
    (define int
      (cons-stream v0
                   (add-streams (scale-stream (scale-stream i-stream dt) (/ 1 c))
                                int)))
    (add-streams int (scale-stream i-stream r)))
  model)

(define RC1 (RC 5 1 0.5))

