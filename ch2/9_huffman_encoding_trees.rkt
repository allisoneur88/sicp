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
  (make-code-tree
    (make-leaf 'A 4)  ;	      (A B D C):8
    (make-code-tree   ;		 /    \
      (make-leaf 'B 2);	       A:4  (B D C):4
      (make-code-tree ;		    /     \
       (make-leaf 'D 1);          B:2  (D, C):2
       (make-leaf 'C 1)))));	        /    \
                      ;		      D:1    C:1

(display (car sample-tree))

(define sample-message
  '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(define sample-decoded
  '(A D A B B C A))
  

(display (decode sample-message sample-tree)) ; (A  D  A  B  B  C  A)
                                              ; 0  110 0 10 10 111 0
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))
  
(define (is-in? symbol symbols)
  (cond
    ((null? symbols) false)
    ((eq? symbol (car symbols)) true)
    (else (is-in symbol (cdr symbols)))))

(define (first set)
  (car set))

(define (encode-symbol symbol tree)
  (cond
    ((null? tree) '())
    ((not (is-in? symbol (symbols tree)))
     (error "Symbol is not part of the tree: ENCODE-SYMBOL" symbol))
    ((leaf? tree) '())
    ((is-in? symbol (symbols (left-branch tree)))
     (cons '0 (encode-symbol symbol (left-branch tree))))
    (else
      (cons '1 (encode-symbol symbol (right-branch tree))))))

(display (encode sample-decoded sample-tree))
