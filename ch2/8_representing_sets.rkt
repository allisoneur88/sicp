#lang sicp

; Unordered list

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? (car set) x) true)
        (else 
         (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
    set
    (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else
         (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else
         (cons (car set1) (union-set (cdr set1) set2)))))

(define empt '())
(define set (adjoin-set 5 empt))
(element-of-set? 5 set)

(define set1 '(1 2 3 4 5))
(define set2 '(3 4 5 6 7))

(display (intersection-set set1 set2))
(display (intersection-set set1 empt))
(display (union-set set1 set2))
(display (union-set set1 empt))

(define (adjoin-set-dupl x set)
  (cons x set))

(display (adjoin-set-dupl 2 set1))

(define (union-set-dupl set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (cons (car set1) (union-set-dupl (cdr set1) set2)))))

(define (intersection-set-dupl set1 set2)
  (cond ((or (null? set1) (null? set2) '()))
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set-dupl (cdr set1) set2)))
        (else
         (intersection-set-dupl (cdr set1) set2))))
