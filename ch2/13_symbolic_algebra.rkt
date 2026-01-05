; going to assume, for now,
; that we have a poly data structure,
; which consists of variable and
; a collection of terms.
; also assuming that we have
; selectors variable an term-list
; constructor make-poly

; we will need a table
(define *operation-table* '())

(define (put op type-tags proc)
  (let ((record (assoc (list op type-tags) *operation-table*)))
    (if record
     (set-cdr! record proc)
     (set! *operation-table*
       (cons (cons (list op type-tags) proc)
        *operation-table*))))
  'ok)

(define (get op type-tags)
  (let ((record (assoc (list op type-tags) *operation-table*)))
    (if record
     (cdr record)
     #f)))

(define (show-table)
  (for-each (lambda (entry)
             (display (car entry))
             (display " => ")
             (display (cdr entry))
             (newline))
      *operation-table*))

; let's make a package

(define install-polynomial-package
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

  (define (adjoin-term))
  (define (coeff))

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


