#lang sicp

; (make-machine <register-names> <operations> <controller>)
; constructs ard returns a model of the machine with
; the given register, operations and controller
; 
; (set-register-contents! <machine-model> <register-name> <value>)
; stores a value in a simulated register in the given machine
;
; (get-register-contents <machine-model> <register-name>)
; returns the contents of a simulated register in the given machine
;
; (start <machine-model>)
; simulates the execution of the given machine, starting from
; the beginning of the controller sequence and stopping
; when it reaches the end of the sequence

; as an example of how these procedures are used:
(define gcd-machine
  (make-machine
    '(a b t)
    (list (list 'rem remainder) (list '= =))
    '(test-b 
       (test (op =) (reg b) (const 0))
       (branch (label gcd-done))
       (assign t (op rem) (reg a) (reg b))
       (assign a (reg b))
       (assign b (reg t))
       (goto (label test-b))
      gcd-done)))

; to compute GCDs with this machine, we set the input
; registers, start the machine, and examine the result
; when the simulation terminates
;		      <machine> <name> <value>
(set-register-contents! gcd-machine 'a 206)
; done
(set-register-contents! gcd-machine 'b 40)
; done
(start gcd-machine)
; done
(get-register-contents gcd-machine 'a)
; 2

; 5.2.1 THE MACHINE MODEL

(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each
      (lambda (register-name)
       ((machine 'allocate-register) register-name))
      register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

; REGISTERS

(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value) (set! contents value)))
            (else
             (error "Unknown request: REGISTER" message))))
    dispatch))

(define (get-contents register) (register 'get))
(define (set-contents! register value)
  ((register 'set) value))

; THE STACK

(define (make-stack)
  (let ((s '()))
    (define (push x) (set! s (cons x s)))
    (define (pop)
      (if (null? s)
       (error "Empty stack: POP")
       (let ((top (car s)))
        (set! s (cdr s))
        top)))
    (define (initialize)
      (set! s '())
      'done)
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            (else (error "Unknown request: STACK" message))))
    dispatch))

(define (pop stack) (stack 'pop))
(define (push stack vallue) ((stack 'push) value))

; THE BASIC MACHINE

(define (make-new-machine)
  (let ((pc (make-register 'pc))         ; program counter
        (flag (make-register 'flag))     ; a single flag for branching
        (stack (make-stack))             ; stack
        (the-instruction-sequence '()))  ; 
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))

      (define (allocate-register name)
       (if (assoc name register-table)
           (error "Multiply defined register: " name)
           (set! register-table
            (cons (list name (make-register name))
                  register-table)))
       'register-allocated)

      (define (lookup-register name)
       (let ((val (assoc name register-table)))
        (if val
         (cadr val)
         (error "Unknown register: " name))))

      (define (execute)
       (let ((insts (get-contents pc)))
        (if (null? insts)
         'done
         (begin
          ((instruction-execution-proc (car insts)))
          (execute)))))

      (define (dispatch message)
       (cond ((eq? message 'start)
              (set-contents! pc the-instruction-sequence)
              (execute))
             ((eq? message 'install-instruction-sequence)
              (lambda (seq)
               (set! the-instruction-sequence seq)))
             ((eq? message 'allocate-register)
              allocate-register)
             ((eq? message 'get-register)
              lookup-register)
             ((eq? message 'install-operations)
              (lambda (ops)
               (set! the-ops (append the-ops ops))))
             ((eq? message 'stack) stack)
             ((eq? message 'operations) the-ops)
             (else (error "Unknown request: MACHINE" message))))
      dispatch)))

(define (start machine) (machine 'start))
(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))
(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name)
                 value)
  'done)

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

; 5.2.2 THE ASSEMBLER

(define (assemble controller-text machine)
  (extract-labels
    controller-text
    (lambda (insts labels)
      (update-insts! insts labels machine)
      insts)))

(define (extract-labels text receive)
  (if (null? text)
    (receive '() '())
    (extract-labels
      (cdr text)
      (lambda (insts labels)
       (let ((next-inst (car text)))
        (if (symbol? next-inst)
         (receive insts
          (cons (make-label-entry next-inst insts)
                labels))
         (receive (cons (make-instruction next-inst)
                        insts)
                  labels)))))))

; ...
; this receive trick is essentially a way to elegantly
; return multiple values
; normal way extract-labes and assemble are as follows:

;(define (extract-labels text)
;  (if (null? text)
;      (cons '() '())
;      (let ((result (extract-labels (cdr text))))
;       (let ((insts (car result)) (labels (cdr result)))
;        (let ((next-inst (car text)))
;         (if (symbol? next-inst)
;          (cons insts
;                (cons (make-label-entry next-inst insts)
;                      labels))
;          (cons (cons (make-instruction next-inst) insts)
;                labels)))))))

; which would be called by assemble as follows:
;(define (assemble controller-text machine)
;  (let ((result (extract-labels controller-text)))
;    (let ((insts (car result)) (lables (cdr result)))
;      (update-insts! insts labels machine)
;      insts)))
; ...

; update-insts! modifies the instructions list, which
; initially contains only the text of the instuctions,
; to include the corresponding execution procedures:

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each
      (lambda (inst)
       (set-instruction-execution-proc!
        inst
        (make-execution-procedure
         (instruction-text inst)
         labels machine pc flag stack ops)))
      insts)))

; the machine instruction data structure simply pairs
; the instruction text with the corresponding execution
; procedure. The execution procedure is not yet available
; when extract-labels constructs the instruction,
; and is inserted later by update-insts!

(define (make-instruction text) (cons text '()))
(define (instruction-text inst) (car inst))
(define (instruction-execution-proc inst) (cdr inst))
(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))

; the instruction text is not used by our simulator,
; but it is handy to keep around for debugging

; elements of the label table are pairs:
(define (make-label-entry label-name insts)
  (cons label-name insts))

; entries will be looked up in the table with:
(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
     (cdr val)
     (error "Undefined label: ASSEMBLE" label-name))))

; TODO: ex. 5.8

; 5.2.3
; GENERATING EXECUTION PROCEDURES FOR INSTRUCTIONS

(define (make-execution-procedure
         inst labels machine pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        (else
         (error "Unknown instruction type: ASSEMBLE" inst))))

; assign instructions
(define (make-assign inst machine labels operations pc)
  (let ((target (get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
           (if (operation-exp? value-exp)
            (make-operation-exp
             value-exp machine labels operations)
            (make-primitive-exp
             (car value-exp) machine labels))))
      (lambda ()
       (set-contents! target (value-proc))
       (advance-pc pc)))))
         
(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))
(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))  

; advance-pc is the normal termination for all
; instructions except branch and goto.

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

; test, branch and goto instructions
(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
     (let ((condition-proc
            (make-operation-exp
             condition machine labels operations)))
       (lambda ()
        (set-contents! flag (condition-proc))
        (advance-pc pc)))
     (error "Bad TEST instruction: ASSEMBLE" inst))))

(define (test-condition test-instruction)
  (cdr test-instruction))


(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
     (let ((insts
            (lookup-label labels (label-exp-label dest))))
       (lambda ()
        (if (get-contents flag)
         (set-contents! pc insts)
         (advance-pc pc))))
     (error "Bad BRANCH instruction: ASSEMBLE" inst))))

(define (branch-dest branch-instruction)
  (cadr branch-instruction))

(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts
                  (lookup-label labels (label-exp-label dest))))
            (lambda () (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg (get-register
                       machine
                       (register-exp-reg dest))))
            (lambda () (set-contents! pc (get-contents reg)))))
          (else
           (error "Bad GOTO instruction: ASSEMBLE" inst)))))
           
(define (goto-dest goto-instruction)
  (cadr goto-instruction))

; stack instructions
; save and restore simply use the stack with 
; the designated register and advance the pc
(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))

(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

; make-perform
; generates an execution procedure for the action
; to be performed. At simulation time, the action
; procedure is executed and pc advanced
(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
     (let ((action-proc
            (make-operation-exp
             action machine labels operations)))
       (lambda () (action-proc) (advance-pc pc))))))
         
(define (perform-action perform-instruction)
  (cdr perform-instruction))

; Execution procedures for subexpressions
(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
          (lambda () c)))
        ((label-exp? exp)
         (let ((insts (lookup-label
                       labels
                       (label-exp-label exp))))
          (lambda () insts)))
        ((register-exp? exp)
         (let ((r (get-register machine (register-exp-reg exp))))
          (lambda () (get-contents r))))
        (else (error "Unknown expression type: ASSEMBLE" exp))))

; the syntax of reg, label, and const expressions is determined by:
(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    false))

(define (register-exp? exp) (tagged-list? exp 'reg))
(define (register-exp-reg exp) (cadr exp))
(define (label-exp? exp) (tagged-list? exp 'label))
(define (label-exp-label exp) (cadr exp))
(define (constant-exp? exp) (tagged-list? exp 'const))
(define (constant-exp-value exp) (cadr exp))

; assign, perform, and test instructions may include
; the application of a machine operation (specified
; by an op expression) to some operands (specified
; by reg and const expressions). The following proc-
; -edure produces an execution procedure for an
; "operation expression" - a list containing the 
; operation and operand expressions from the instruction:
(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp)
                         operations))
        (aprocs
         (map (lambda (e)
               (make-primitive-exp e machine labels))
              (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

; the syntax of operation expressions is determined by:
(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))
(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))
(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

; the simulation procedure is found by looking up
; the operation name in the operation table for the machine
(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
     (cadr val)
     (error "Unknown operation: ASSEMBLE" symbol))))
