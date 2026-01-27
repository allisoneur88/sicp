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
(defien gcd-machine
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
    ((machine 'install-insturction-sequence)
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
  (machine 'get-register) reg-name)

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
          
                         
