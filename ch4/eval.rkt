#lang sicp

(define *env (make-table))

(define (*eval exp env)
  (cond
    ((number? exp) exp)
    ((symbol? exp) (lookup exp env))
    ((define? exp) (eval-define exp env))
    ((if? exp) (eval-if exp env))
    ((application? exp) (*apply (*eval (car exp) env)
                               (map (lambda (e) (*eval e env)) (cdr exp))))
    (else
      (error "Unknown Expression type: EVAL" exp))))

(define scheme-apply apply)

(define (*apply operator operands)
  (if (primitive? operator)
      (scheme-apply (get-scheme-procedure operator) operands)
      (error "Operator not a procedure: " operator)))

(define prim-tag 'primitive)
(define (make-primitive scheme-proc)
  (list prim-tag scheme-proc))
(define (primitive? exp) (tag? exp prim-tag))
(define (get-scheme-procedure prim) (cadr prim))


(define (install-primitives)
  (begin
    (put-env! '*+ (make-primitive +))
    (put-env! '*> (make-primitive >))
    (put-env! '*true #t))
  'ok)

(install-primitives)

; tag-check
(define (tag? exp tag)
  (eq? (car exp) tag))

(define (application? exp)
  (pair? exp))

(define (define? exp)
  (tag? exp '*define))
(define (if? exp)
  (tag? exp '*if))

; define
(define (defenee exp)
  (cadr exp))
(define (define-value exp)
  (caddr exp))
(define (eval-define exp env)
  (put! (defenee exp) (*eval (define-value exp) env) env))

; if
(define (predicate if-exp)
  (cadr if-exp))
(define (consequent if-exp)
  (caddr if-exp))
(define (alternative if-exp)
  (cadddr if-exp))
(define (eval-if exp env)
  (let ((predic (predicate exp))
        (conseq (consequent exp))
        (altern (alternative exp)))
    (let ((test (*eval predic env)))
      (cond ((eq? test #t)
             (*eval conseq env))
            ((eq? test #f)
             (*eval altern env))
            (else
             (error "Predicate does not eval to #t or #f: EVAL-IF" test))))))

(define (lookup thing env)
  (get thing env))

; tests
(*eval '(*+ 5 (*+ 2 3)) *env)

(*eval
  '(*define x 5) *env)

(*eval
  '(*+ x (*+ 2 3)) *env)

(*eval
  '(*if (*> x 6)
    x
    (*+ x 66)) *env)

(*eval
  '(*if *true 5 6) *env)

; support table
(define (get key table)
  (let ((record (assoc key (cdr table))))
    (if record
     (cdr record)
     false)))

(define (put! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
     (set-cdr! record value)
     (set-cdr! table
               (cons (cons key value)
                     (cdr table)))))
  'ok)

(define (put-env! key value)
  (put! key value *env))

(define (make-table)
  (list '*table*))
