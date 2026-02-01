#lang sicp

; Our scheme evaluator register machine
; includes a stack and seven registers:
; exp, env, val, continue, proc, argl, unev.

; op list
(define eceval-operations
  (list
    (list 'self-evaluating? self-evaluating?)
    (list 'variable? variable?)
    (list 'quoted? quoted?)
    (list 'assignment? assignment?)
    (list 'definition? definition?)
    (list 'if? if?)
    (list 'lambda? lambda?)
    (list 'begin? begin?)
    (list 'application? application?)
    (list 'empty-arglist empty-arglist)
    (list 'lookup-variable-value lookup-variable-value)
    (list 'text-of-quotation text-of-quotation)
    (list 'lambda-parameters lambda-parameters) (list 'lambda-body lambda-body)
    (list 'make-procedure make-procedure)
    (list 'operands operands) (list 'operator operator)
    (list 'first-operand first-operand) (list 'rest-operands rest-operands)
    (list 'no-operands? no-operands?) (list 'last-operand? last-operand?)
    (list 'adjoin-arg adjoin-arg)
    (list 'primitive-procedure? primitive-procedure?)
    (list 'compound-procedure? compound-procedure?)
    (list 'apply-primitive-procedure apply-primitive-procedure)
    (list 'procedure-parameters procedure-parameters) (list 'procedure-body procedure-body)
    (list 'procedure-environment procedure-environment)
    (list 'extend-environment extend-environment)
    (list 'begin-actions begin-actions) (list 'first-exp first-exp) (list 'last-exp? last-exp?)
    (list 'rest-exps rest-exps) (list 'if-predicate if-predicate)
    (list 'if-consequent if-consequent) (list 'if-alternative if-alternative)
    (list 'true? true?)
    (list 'assignment-variable assignment-variable)
    (list 'assignment-value assignment-value)
    (list 'set-variable-value! set-variable-value!)
    (list 'definition-variable definition-variable)
    (list 'definition-value definition-value)
    (list 'define-variable! define-variable!)
    (list 'prompt-for-input prompt-for-input)
    (list 'read read)
    (list 'get-global-environment get-global-environment)
    (list 'announce-output announce-output)
    (list 'user-print user-print)))


; The core

(define god-machine
  (make-machine
    '(exp env val continue proc argl unev)
    eceval-operations
    '(start-eval
       (assign continue (label eval-done))
       (goto (label eval-dispatch))
      eval-dispatch
       (test (op self-evaluating?) (reg exp)) ; self-evaluating?
       (branch (label ev-self-eval))  
       (test (op variable?) (reg exp))        ; variable?
       (branch (label ev-variable))
       (test (op quoted?) (reg exp))          ; quoted?
       (branch (label ev-quoted))
       (test (op assignment?) (reg exp))      ; assignment?
       (branch (label ev-assignment))
       (test (op definition?) (reg exp))       ; definition?
       (branch (label ev-definition))
       (test (op if?) (reg exp))              ; if?
       (branch (label ev-if))
       (test (op lambda?) (reg exp))          ; lambda?
       (branch (label ev-lambda))
       (test (op begin?) (reg exp))           ; begin?
       (branch (label ev-begin))
       (test (op application?) (reg exp))     ; application?
       (branch (label ev-application))
       (goto (label unknown-expression-type))
      ev-self-eval
       (assign val (reg exp))
       (goto (reg continue))
      ev-variable
       (assign val (op lookup-variable-value) (reg exp) (reg env))      ; lookup-variable-value
       (goto (reg continue))
      ev-quoted
       (assign val (op text-of-quotation) (reg exp))                    ; text-of-quotation
       (goto (reg continue))
      ev-lambda
       (assign unev (op lambda-parameters) (reg exp))                   ; lambda-parameters
       (assign exp (op lambda-body) (reg exp))                          ; lambda-body
       (assign val (op make-procedure) (reg unev) (reg exp) (reg env))  ; make-procedure
       (goto (reg continue))
      ev-application
       (save continue)
       (save env)
       (assign unev (op operands) (reg exp))                            ; operands
       (save unev)
       (assign exp (op operator) (reg exp))                             ; operator
       (assign continue (label ev-appl-did-operator))
       (goto (label eval-dispatch))
      ev-appl-did-operator
       (restore unev)                        ; operands
       (restore env)
       (assign argl (op empty-arglist))
       (assign proc (reg val))               ; operator
       (test (op no-operands?) (reg unev))                              ; no-operands?
       (branch (label apply-dispatch))
       (save proc)
      ev-appl-operand-loop
       (save argl)
       (assign exp (op first-operand) (reg unev))                       ; first-operand
       (test (op last-operand?) (reg unev))                             ; last-operand?
       (branch (label ev-appl-last-arg))
       (save env)
       (save unev)
       (assign continue (label ev-appl-accumulate-arg))
       (goto (label eval-dispatch))
      ev-appl-accumulate-arg
       (restore unev)
       (restore env)
       (restore argl)
       (assign argl (op adjoin-arg) (reg val) (reg argl))               ; adjoin-arg
       (assign unev (op rest-operands) (reg unev))                      ; rest-operands
       (goto (label ev-appl-operand-loop))
      ev-appl-last-arg
       (assign continue (label ev-appl-accum-last-arg))
       (goto (label eval-dispatch))
      ev-appl-accum-last-arg
       (restore argl)
       (assign argl (op adjoin-arg) (reg val) (reg argl))
       (restore proc)
       (goto (label apply-dispatch))
      apply-dispatch
       (test (op primitive-procedure?) (reg proc))
       (branch (label primitive-apply))
       (test (op compound-procedure?) (reg proc))
       (branch (label compound-apply))
       (goto (label unknown-procedure-type))
      primitive-apply
       (assign val (op apply-primitive-procedure)
                   (reg proc)
                   (reg argl))
       (restore continue)
       (goto (reg continue))
      compound-apply
       (assign unev (op procedure-parameters) (reg proc))
       (assign env (op procedure-environment) (reg proc))
       (assign env (op extend-environment)
                   (reg unev) (reg argl) (reg env))
       (assign unev (op procedure-body) (reg proc))
       (goto (label ev-sequence))
      ev-begin
       (assign unev (op begin-actions) (reg exp))
       (save continue)
       (goto (label ev-sequence))
      ev-sequence
       (assign exp (op first-exp) (reg unev))
       (test (op last-exp?) (reg unev))
       (branch (label ev-sequence-last-exp))
       (save unev)
       (save env)
       (assign continue (label ev-sequence-continue))
       (goto (label eval-dispatch))
      ev-sequence-continue
       (restore env)
       (restore unev)
       (assign unev (op rest-exps) (reg unev))
       (goto (label ev-sequence))
      ev-sequence-last-exp
       (restore continue)
       (goto (label eval-dispatch))
      ev-if
       (save exp)
       (save env)
       (save continue)
       (assign continue (label ev-if-decide))
       (assign exp (op if-predicate) (reg exp))
       (goto (label eval-dispatch))
      ev-if-decide
       (restore continue)
       (restore env)
       (restore exp)
       (test (op true?) (reg val))
       (branch (label ev-if-consequent))
      ev-if-alternative
       (assign exp (op if-alternative) (reg exp))
       (goto (label eval-dispatch))
      ev-if-consequent
       (assign exp (op if-consequent) (reg exp))
       (goto (label eval-dispatch))
      ev-assignment
       (assign unev (op assignment-variable) (reg exp))  ; save variable for later
       (save unev)
       (assign exp (op assignment-value) (reg exp))
       (save env)
       (save continue)
       (assign continue (label ev-assignment-1))
       (goto (label eval-dispatch))
      ev-assignment-1
       (restore continue)
       (restore env)
       (restore unev)
       (perform
        (op set-variable-value!) (reg unev) (reg val) (reg env))
       (assign val (const ok))
       (goto (reg continue))
      ev-definition
       (assign unev (op definition-variable) (reg exp))
       (save unev)
       (assign exp (op definition-value) (reg exp))
       (save env)
       (save continue)
       (assign continue (label ev-definition-1))
       (goto (label eval-dispatch))
      ev-definition-1
       (restore continue)
       (restore env)
       (restore unev)
       (perform
        (op define-variable!) (reg unev) (reg val) (reg env))
       (assign val (const ok))
       (goto (reg continue))
      unknown-expression-type
       (assign val (const unknown-expression-type-error))
      unknown-procedure-type
       (assign val (const unknown-procedure-type))
      eval-done)))

       
      ;read-eval-print-loop
      ; (perform (op initialize-stack))
      ; (perform
      ;  (op prompt-for-input) (const ";;EC-Eval input:"))
      ; (assign exp (op read))
      ; (assign env (op get-global-environment))
      ; (assign continue (label print-result))
      ; (goto (label eval-dispatch))
      ;print-result
      ; (perform (op announce-output) (const ";;EC-Eval value:"))
      ; (perform (op user-print) (reg val))
      ; (goto (label read-eval-print-loop))
      ;unknown-expression-type
      ; (assign val (const unknown-expression-type-error))
      ; (goto (label signal-error))
      ;unknown-procedure-type
      ; (restore continue)
      ; (assign val (const unknown-procedure-type-error))
      ; (goto (label signal-error))
      ;signal-error
      ; (perform (op user-print) (reg val))
      ; (goto (label read-eval-print-loop)))))
       

; testing the machine

(define machine
  (let ((god god-machine))
    (set-register-contents! god 'env (get-global-environment))
    (define (process exp)
      (begin
       (set-register-contents! god 'exp exp)
       (start god)
       (display (get-register-contents god 'val))
       (display ((god 'stack) 'print-statistics))
       ((god 'stack) 'initialize)))
    process))

(machine
  '(define factorial
     (lambda (n)
       (if (< n 3)
        n
        (* n (factorial (- n 1)))))))

(machine '(factorial 5))

(machine
  '(define fact-iter
     (lambda (n)
       (define helper
        (lambda (product counter max-count)
         (if (> counter max-count)
          product
          (helper (* product counter) (+ counter 1) max-count))))
       (helper 1 1 n))))

(machine '(fact-iter 5))
