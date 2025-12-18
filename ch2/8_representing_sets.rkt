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

; Ordered list
(define (ord-element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((> (car set) x) false)
        (else
           (ord-element-of-set? x (cdr set)))))

(define (ord-adjoin-set x set)
  (cond ((null? set) (list x))
        ((= (car set) x)
         set)
        ((> (car set) x)
         (cons x set))
        (else
         (cons (car set) (ord-adjoin-set x (cdr set))))))

(define ordset (list 1 3 5 7))
(display (ord-adjoin-set 4 ordset))

(define (ord-intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((= (car set1) (car set2))
         (cons (car set1) (ord-intersection-set (cdr set1) (cdr set2))))
        ((> (car set1) (car set2)) (ord-intersection-set set1 (cdr set2)))
        ((> (car set2) (car set1)) (ord-intersection-set (cdr set1) set2))))

(define ordset2 (list 5 7 9))
(display (ord-intersection-set ordset ordset2))

(define (ord-union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((= (car set1) (car set2))
         (cons (car set1) (ord-union-set (cdr set1) (cdr set2))))
        ((> (car set1) (car set2))
         (cons (car set2) (ord-union-set set1 (cdr set2))))
        ((> (car set2) (car set1))
         (cons (car set1) (ord-union-set (cdr set1) set2)))))

(display (ord-union-set ordset ordset2))

; Binary tree
(define (entry tree)
  (car tree))
(define (left-branch tree)
  (cadr tree))
(define (right-branch tree)
  (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-tree? e tree)
  (cond
    ((null? tree) false)
    ((= e (entry tree)) true)
    ((< e (entry tree))
     (element-of-tree? e (left-branch tree)))
    ((> e (entry tree))
     (element-of-tree? e (right-branch tree)))))
   
(define (adjoin-tree e tree)
  (cond
    ((null? tree) (make-tree e '() '()))
    ((= e (entry tree)) tree)
    ((< e (entry tree))
     (make-tree
       (entry tree)
       (adjoin-tree e (left-branch tree))
       (right-branch tree)))
    ((> e (entry tree))
     (make-tree
       (entry tree)
       (left-branch tree)
       (adjoin-tree e (right-branch tree))))))

(define (tree->list-1 tree)
  (if (null? tree) '()
      (append
       (tree->list-1 (left-branch tree))
       (cons (entry tree) (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
     result-list
     (copy-to-list (left-branch tree)
       (cons (entry tree) (copy-to-list (right-branch tree) result-list)))))
  (copy-to-list tree '()))
     
(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
       (let ((left-result (partial-tree elts left-size)))
        (let ((left-tree (car left-result))
              (non-left-elts (cdr left-result))
              (right-size (- n (+ left-size 1))))
         (let ((this-entry (car non-left-elts))
               (right-result
                (partial-tree
                 (cdr non-left-elts)
                 right-size)))
          (let ((right-tree (car right-result))
                (remaining-elts
                 (cdr right-result)))
           (cons (make-tree this-entry
                            left-tree
                            right-tree)
                 remaining-elts))))))))


(display (tree->list-2(list->tree (list 1 2 3))))
