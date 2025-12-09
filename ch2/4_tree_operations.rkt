#lang sicp

(define onetofour (list 1 2 3 4))
(display onetofour)

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(display (list-ref onetofour 2))

(define (append-rec list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append-rec (cdr list1) list2))))

(display (append-rec onetofour onetofour))

(define (reverse-rec items)
  (if (null? items)
      '()
      (append-rec (reverse-rec (cdr items)) (list (car items)))))

(display (reverse-rec onetofour))

(define (map-rec fn items)
  (if (null? items)
      '()
      (cons (fn (car items)) (map-rec fn (cdr items)))))

(display (map-rec
          (lambda (x) (* x x))
          onetofour))

(define (deep-reverse tree)
  (if (null? tree)
      '()
      (append-rec
       (deep-reverse (cdr tree))
       (if (list? (car tree))
        (list (deep-reverse (car tree)))
        (list (car tree))))))

(define tree (list (list 1 2) (list 3 4)))
(display tree)
(display (deep-reverse tree))

(define (fringe tree)
  (if (null? tree)
      '()
      (append-rec
       (if (list? (car tree))
        (fringe (car tree))
        (list (car tree)))
       (fringe (cdr tree)))))

(display (fringe (cons tree tree)))

(define (scale-tree tree factor)
  (if (null? tree)
      '()
      (append-rec
       (if (list? (car tree))
        (list (scale-tree (car tree) factor))
        (list (* factor (car tree))))
       (scale-tree (cdr tree) factor))))

(display (scale-tree tree 10))

(define (scale-tree-book tree factor)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* factor tree))
        (else (cons
               (scale-tree (car tree) factor)
               (scale-tree (cdr tree) factor)))))

(display (scale-tree-book tree 10))

(define (scale-tree-map tree factor)
  (map-rec
    (lambda (subtree)
      (if (pair? subtree)
       (scale-tree-map subtree factor)
       (* subtree factor)))
    tree))

(display (scale-tree-map tree 10))

(define (square-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (square tree))
        (else (cons
               (square-tree (car tree))
               (square-tree (cdr tree))))))

(display (square-tree tree))

(define (map-tree fn tree)
  (map-rec
    (lambda (subtree)
      (if (pair? subtree)
       (map-tree fn subtree)
       (fn subtree)))
    tree))

(define (square x) (* x x))

(define (square-tree-map tree)
  (map-tree square tree))

(display (square-tree-map tree))

