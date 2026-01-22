#lang sicp

; *genv is a list of tables
(define *genv (list (make-table)))

(define (*eval exp env)
  (cond
    ((number? exp) exp)
    ((symbol? exp) (lookup exp env))
    ((define? exp) (eval-define exp env))
    ((if? exp) (eval-if exp env))
    ((lambda? exp) (eval-lambda exp env))
    ((application? exp) (*apply (*eval (car exp) env)
                               (map (lambda (e) (*eval e env)) (cdr exp))))
    (else
      (error "Unknown Expression type: EVAL" exp))))

(define scheme-apply apply)

(define (*apply operator operands)
  (if (primitive? operator)
      (scheme-apply (get-scheme-procedure operator) operands)
      (error "Operator not a procedure: " operator)))

(define (*apply operator operands)
  (cond ((primitive? operator)
         (scheme-apply (get-scheme-procedure operator) operands))
        ((compound? operator)
         (*eval (operator-body operator)
                (extend-env-with-new-frame
                 (operator-parameters operator) operands (operator-env operator))))))

(define prim-tag 'primitive)
(define (make-primitive scheme-proc)
  (list prim-tag scheme-proc))
(define (primitive? exp) (tag? exp prim-tag))
(define (get-scheme-procedure prim) (cadr prim))

(define compound-tag 'compound)
(define (make-compound parameters body env)
  (list compound-tag parameters body env))
(define (compound? exp) (tag? exp compound-tag))
(define (operator-parameters compound-exp) (cadr compound-exp))
(define (operator-body compound-exp) (caddr compound-exp))
(define (operator-env compound-exp) (cadddr compound-exp))

(define (extend-env-with-new-frame vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (error "Arguments/Parameters mismatch")))
(define (make-frame vars vals)
  (let ((table (make-table)))
    (for-each (lambda (var val) (put! var val table))
              vars
              vals)
    table))

(define (install-primitives)
  (begin
    (put-genv! '*+ (make-primitive +))
    (put-genv! '** (make-primitive *))
    (put-genv! '*> (make-primitive >))
    (put-genv! '*true #t))
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
(define (lambda? exp)
  (tag? exp '*lambda))

; define
(define (defenee exp)
  (cadr exp))
(define (define-value exp)
  (caddr exp))
(define (eval-define exp env)
  (let ((current-frame (car env)))
    (put! (defenee exp) (*eval (define-value exp) env) current-frame)))

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

; lambda
; (*lambda (x) (*+ x x))
(define (lambda-arguments exp)
  (cadr exp))
(define (lambda-body exp)
  (caddr exp))
(define (eval-lambda exp env)
  (make-compound (lambda-arguments exp) (lambda-body exp) env))

(define (lookup thing env)
  (if (null? env)
      (error "Unbound variable: " thing)
      (let ((found (get thing (car env))))
       (if found
        found
        (lookup thing (cdr env))))))

; tests
(*eval '(*define fact-iter
         (*lambda (product counter max-count)
          (*if (*> counter max-count)
           product
           (fact-iter (** product counter) (*+ counter 1) max-count)))) *genv)

(*eval (list '*+ '2 '3) *genv)

(*eval '(*+ 2 3) *genv)


(*eval '(*define factorial
         (*lambda (n)
          (fact-iter 1 1 n))) *genv)

(*eval '(factorial 5) *genv)

(*eval '(*define factorial
         (*define fact-iter
          (*lambda (product counter max-count)
           (*if (*> counter max-count)
            product
            (fact-iter (** product counter) (*+ counter 1) max-count))))
         (*lambda (n)
          (fact-iter 1 0 n))) *genv)

(*eval '(factorial 5) *genv)

(*eval '(*define square
         (*lambda (x) (** x x))) *genv)
(*eval '(square 5) *genv)
(*eval '(*+ 5 (*+ 2 3)) *genv)

(*eval
  '(*define x 5) *genv)

(*eval
  '(*+ x (*+ 2 3)) *genv)

(*eval
  '(*if (*> x 6)
    x
    (*+ x 66)) *genv)

(*eval
  '(*if *true 5 6) *genv)

(*eval
  '(*+ 5 6 7 x) *genv)

(*eval
  '((*lambda (x) (*+ x x)) 5) *genv)

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

(define (put-genv! key value)
  (put! key value (car *genv)))

(define (make-table)
  (list '*table*))
