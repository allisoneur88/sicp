#lang sicp

; 4.1.1 The core of the Evaluator
; eval
(define (meval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (meval (cond->if exp) env))
        ((application? exp)
         (mapply (meval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type: EVAL" exp))))

; apply
(define (mapply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
   (else
     (error "Unknown procedure type: APPLY" procedure))))

; list-of-values
(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (cons (meval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))

; eval-if
(define (eval-if exp env)
  (if (true? (meval (if-predicate exp) env))
    (meval (if-consequent exp) env)
    (meval (if-alternative exp) env)))

; eval-sequence
(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (meval (first-exp exps) env))
        (else
         (meval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env))))

; assignment and definition
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (meval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (meval (definition-value exp) env)
                    env)
  'ok)

; ex. 4.1
(define (list-of-vals-left-right exps env)
  (if (no-operands? exps)
      '()
      (let ((eval-first (meval (first-operand exps) env)))
       (cons eval-first
             (list-of-vals-left-right (rest-operands exps) env)))))

(define (list-of-vals-right-left exps env)
  (if (no-operands? exps)
      '()
      (let ((eval-first (list-of-vals-right-left (rest-operands exps) env)))
       (cons (meval (first-operand exps) env)
             eval-first))))
              

; 4.1.2 Representing Expressions

; self-evaluating?
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

; variable?
(define (variable? exp) (symbol? exp))

; quotations
(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

; tagged-list?
(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    false))

; assigments
; (set! <var> <value>)
(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

; definitions
; (define <var> <value>)
; or
; (define (<var> <param-1> ... <param-n>) <body>)
(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
    (cadr exp)
    (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
    (caddr exp)
    (make-lambda (cdadr exp)    ; formal parameters
                 (cddr exp))))  ; body

; lambda
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

; lambda constructor
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

; conditionals
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
    (cadddr exp)
    'false))

; if-constructor
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

; begin
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

; we also include a constructor sequence->exp
; (for use by cond->if) that transforms a sequence
; into a single expression, using begin if necessary:
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        ((else (make-begin seq)))))

(define (make-begin seq) (cons 'begin seq))

; a procedure application is any compound expression
; that is not one of the above expression types.
; The car of the expression is the operator,
; and the cdr is the list of operands
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

; derived expressions
; cond
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
    'false                          ; no else clause
    (let ((first (car clauses))
          (rest (cdr clauses)))
      (if (cond-else-clause? first)
       (if (null? rest)
        (sequence->exp (cond-actins first))
        (error "ELSE clause isn't last: COND->IF"
               clauses))
       (make-if (cond-predicate first)
                (sequence->exp (cond-actions first))
                (expand-clauses rest))))))

; TODO: ex. 4.2 -> 4.10
; ex. 4.2
(define (meval exp env)
  (cond ((self-evaluating? exp) exp)
        ((call? exp) (eval-call exp env))
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (meval (cond->if exp) env))
        ((application? exp)
         (mapply (meval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type: EVAL" exp))))

(define (call? exp) (tagged-list? exp 'call))
(define (call-operator exp)
  (cadr exp))
(define (call-operands exp)
  (cddr exp))
(define (eval-call exp env)
  (mapply (meval (call-operator exp) env)
          (list-of-values (call-operands exp) env)))

; ex. 4.3 eval in data-directed style
(define (generic-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((generic-or-application? exp) (handle-generic-or-application exp env))
        (else (error "Unknown expressin type" exp))))

(define (generic-or-application? exp)
  (pair? exp))
(define (handle-generic-or-application exp env)
  (let ((generic-proc (get-handler (get-type exp))))
    (if generic-proc
     (generic-proc (other-pieces exp) env)
     (handle-application exp env))))

; ex. 4.4 'and' and 'or' special forms
(define (meval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (meval (cond->if exp) env))
        ((and? exp) (eval-and (and-expressions exp) env))
        ((or? exp) (eval-or (or-expressions exp) env))
        ((application? exp)
         (mapply (meval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type: EVAL" exp))))

(define (and? exp)
  (tagged-list? exp 'and))
(define (or? exp)
  (tagged-list? exp 'or))

;(and (pred-1)...(pred-n))
;(or  (pred-1)...(pred-n))
(define (and-expressions exp) (cdr exp))
(define (or-expressions exp) (cdr exp))
(define (first-expression exps) (car exps))
(define (rest-expressions exps) (cdr exps))
(define (last-expression? exps) (null? (cdr exps)))

(define (eval-and exps env)
  (cond ((null? exps) true)
        ((last-expression? exps)
         (meval (first-expression exps) env))
        (else
         (let ((test (meval (first-expression exps) env)))
          (if test
           (eval-and (rest-expressions exps) env)
           false)))))

(define (eval-or exps env)
  (cond ((null? exps) false)
        ((last-expression? exps)
         (meval (first-expression exps) env))
        (else
         (let ((test (meval (first-expression exps) env)))
          (if test
           test
           (eval-or (rest-expressions exps) env))))))

; ex. 4.5 addititonal syntax for cond
; (<test> => <recipient>)

;(cond ((assoc 'b '((a 1) (b 2))) => cadr)
      ;(else false)))
; TODO: 4.5

; ex. 4.6 let expressions
; (let ((var-1 (exp-1))
;       (var-2 (exp-2))
;       ...
;       (var-n (exp-n)))
;   <body>)

; is equal to
; ((lambda (var-1 var-2 ... var-n)
;    <body>)
;    exp-1 exp-2 ... exp-n)

; 'let => '( (x (+ 2 3)) (y (* 2 3)) ) => '(+ x y)

(define (let? exp) (tagged-list? exp 'let))
(define (let-bindings exp) (cadr exp))
(define (let-body exp) (cddr exp))

(define (let->combination exp)
  (let ((vars (map car (let-bindings exp)))
        (exps (map cadr (let-bindings exp)))
        (body (let-body exp)))
    (cons (make-lambda vars body) exps)))
  
(define (meval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (meval (cond->if exp) env))
        ((and? exp) (eval-and (and-expressions exp) env))
        ((or? exp) (eval-or (or-expressions exp) env))
        ((let? exp) (meval (let->combination exp) env))
        ((application? exp)
         (mapply (meval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type: EVAL" exp))))


; 4.1.3 Evaluator Data Structures

; Testing of predicates
(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

; Representing procedures

; to handle primitives, we assume that we have
; available the following procedures:
; (apply-primitive-procedure <proc> <args>)
; (primitive-procedure? <proc>)

; compound procedures are constructed from parameters,
; procedure bodies, and environments using the 
; constructor make-procedure
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

; Operations on Environments

; We use the following operations
; for manipulating environments:
; (lookup-variable-valur <var> <env>)
; (extend-environment <variables> <values> <base-env>)
; (define-variable! <var> <value> <env>)
; (set-variable-value! <var> <value> <env>)

; To implement these operations we represent
; and environment as a list of frames.
; The enclosing environment of an environment
; is the cdr of the list. The empty environment
; is simply the empty list.

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

; Each frame of an environment is represented
; as a pair of lists: a list of the variables
; bound in that frame and a list of the 
; associated values.

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
    (cons (make-frame vars vals) base-env)
    (if (< (length vars) (length vals))
      (error "Too many arguments supplied" vars vals)
      (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
       (scan (frame-variables frame)
             (frame-values frame)))))
  (env-loop env))

(define (set-variabale-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable: SET!" var)
      (let ((frame (first-frame env)))
       (scan (frame-variables frame)
             (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame) (frame-values frame))))

; 4.1.4 Running the Evaluator as a Program
(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define ge (setup-environment))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list
    (list 'cons cons)
    (list 'car car)
    (list 'cdr cdr)
    (list 'null? null?)
    (list '+ +)
    (list '- -)
    (list '* *)
    (list '/ /)
    (list '> >)
    (list '< <)
    (list '>= >=)
    (list '<= <=)
    (list 'square (lambda (x) (* x x)))
    (list 'cube (lambda (x) (* x x x)))))

(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define apply-in-underlying-scheme apply)

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
    (primitive-implementation proc) args))

(meval '(+ 2 3) ge)
(meval
  '(define (factorial n)
     (define (fact-iter product counter max-count)
      (if (> counter max-count)
       product
       (fact-iter (* product counter) (+ counter 1) max-count)))
     (fact-iter 1 1 n)) ge)

(meval '(factorial 5) ge)

; driver loop
(define input-prompt  ";;; M-Eval Input:")
(define output-prompt ";;; M-Eval Value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (meval input ge)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))
(define (announe-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

