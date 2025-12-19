#lang sicp

; make a leaf
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(display (make-leaf 'x 10))

; check if leaf
(define (leaf? object)
  (and (pair? object) (eq? (car object) 'leaf)))
(leaf? (make-leaf 'x 10))

; get symbol of the leaf
(define (symbol-leaf x) (cadr x))
(symbol-leaf (make-leaf 'x 10))

; get weight of the leaf
(define (weight-leaf x) (caddr x))
(weight-leaf (make-leaf 'x 10))

; general node of the huffman's tree
(define (make-code-tree left right)
  (list
    left
    right
    (append (symbols left) (symbols right))
    (+ (weight left) (weight right))))

(display
  (make-code-tree
    (make-leaf 'A 4)
    (make-leaf 'B 2)))

; get left branch of the node
(define (left-branch  tree) (car  tree))
; ger right branch of the node
(define (right-branch tree) (cadr tree))
; get symbols stored in the node. if leaf? call symbol-leaf
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
; get weight of the node. if leaf? call weight-leaf
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

; takes list of bits and a huffman tree
; traverse the tree repeatedly following the (car bits) jumping back to the root when finding a leaf
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
     '()
     (let ((next-branch
            (choose-branch (car bits) current-branch)))
       (if (leaf? next-branch)
        (cons (symbol-leaf next-branch)
              (decode-1 (cdr bits) tree))
        (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond
    ((= bit 0) (left-branch  branch))
    ((= bit 1) (right-branch branch))
    (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond
    ((null? set) (list x))
    ((< (weight x) (weight (car set))) (cons x set))
    (else
      (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs) 
      '()
      (let ((pair (car pairs)))
       (adjoin-set (make-leaf (car pair)    ; leaf's symbol
                              (cadr pair))  ; leaf's weight
                   (make-leaf-set (cdr pairs))))))

(define sample-tree
  (make-code-tree (make-leaf 'a 4)
                  (make-code-tree
                   (make-leaf 'b 2)
                   (make-code-tree
                    (make-leaf 'd 1)
                    (make-leaf 'c 1)))))

(define sample-tree
  (make-code-tree
    (make-leaf 'A 4)
    (make-code-tree
      (make-leaf 'B 2)
      (make-code-tree
       (make-leaf 'D 1)
       (make-leaf 'C 1)))))

(define sample-message
  '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(display (decode sample-message sample-tree))
  
