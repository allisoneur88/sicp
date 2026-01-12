; going to assume, for now,
; that we have a poly data structure,
; which consists of variable and
; a collection of terms.
; also assuming that we have
; selectors variable an term-list
; constructor make-poly

; we will need a table
; we are linking to the generic arithmetics package

; let's make a package

(define (install-polynomial-package)
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p)
    (car p))
  (define (term-list p)
    (cdr p))

  (define (same-variable? v1 v2)
    (eq? v1 v2))
  (define (variable? v)
    (symbol? v))

  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
     term-list
     (cons term term-list)))

  (define (the-empty-term-list) '())
  (define (first-term term-list)
    (car term-list))
  (define (rest-terms term-list)
    (cdr term-list))
  (define (empty-termlist? term-list)
    (null? term-list))
  (define (make-term order coeff)
    (list order coeff))
  (define (order term)
    (car term))
  (define (coeff term)
    (cadr term))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
     (make-poly (variable p1)
                (add-terms (term-list p1) (term-list p2)))
     (error "Polys not in same var: ADD-POLY" (list p1 p2))))

  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1))
                 (t2 (first-term L2)))
            (cond ((> (order t1) (order t2))
                   (adjoin-term
                    t1 (add-terms (rest-terms L1) L2)))
                  ((< (order t1) (order t2))
                   (adjoin-term
                    t2 (add-terms L1 (rest-terms L2))))
                  (else
                   (adjoin-term
                    (make-term (order t1)
                               (add (coeff t1) (coeff t2)))
                    (add-terms (rest-terms L1) (rest-terms L2)))))))))

  (define (mul-poly p1 p2)
    (if (same-varibale? (variable p1) (variable p2))
     (make-poly (variable p1)
                (mul-terms (term-list p1) (term-list p2)))
     (error "Polys not in same var: MUL-POLY" (list p1 p2))))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
     (the-empty-termlist)
     (add-terms
       (mul-term-by-all-terms (first-term L1) L2)
       (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
     (the-empty-termlist)
     (let ((t2 (first-term L)))
       (adjoin-term
        (make-term (+ (order t1) (order t2))
                   (mul (coeff t1) (coeff t2)))
        (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (attach-tag tag item)
    (cons tag item))
  (define (tag p) (attach-tag 'polynomial p))

  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))

  'done)

;TODO: tie to the generic arithmetics package

(install-polynomial-package)
(install-generic-package)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(define (msn n) (make-scheme-number n))
(define (mr n) (make-rational n 1))

(define poly1
  (make-polynomial 'x (list (list (msn 2) (msn 3)) (list (msn 1) (msn 2)) (list (msn 0) (msn 1)))))
(define poly2
  (make-polynomial 'x (list (list (msn 2) (msn 3)) (list (msn 1) (msn 2)) (list (msn 0) (msn 1)))))

(define poly3
  (make-polynomial 'y (list (list (msn 2) (mr 3)) (list (msn 1) (mr 2)) (list (msn 0) (mr 1)))))
(define poly4
  (make-polynomial 'y (list (list (msn 2) (mr 3)) (list (msn 1) (mr 2)) (list (msn 0) (mr 1)))))

(display poly1)
(display poly2)
(display poly3)
(display poly4)
(display (add poly1 poly2))

(display (add poly3 poly4))
