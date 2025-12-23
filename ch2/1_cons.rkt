#lang sicp

(define (struct x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1: STRUCT" m))))
  dispatch)
(define (fir s)
  (s 0))
(define (bfir s)
  (s 1))

(define str
  (struct 1 2))
(fir str)
(bfir str)


; fn which takes 2 obj's and returns a fn that takes a single fn argument and this fn should take 2 arguments itself.
; those 2 args are our x and y
(define (kons x y)
  (lambda (m) (m x y)))
;takes a single arg - a kons. 
(define (kar k)
  (k (lambda (p q) p)))
(define (kdr k)
  (k (lambda (p q) q)))

(define kon
  (kons 3 4))
(kar kon)
(kdr kon)

