#lang sicp

; Chapter 3.3.5

(define C (make-connector))
(define F (make-connector))

(celsius-fahrenheit-converter C F)

(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

(define (probe name connector)
  (define (print-probe)
    (newline)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display (get-value connector)))
  (define (process-new-value)
    (print-probe))
  (define (process-forget-value)
    (newline)
    (display "Probe: ")
    (display name)
    (display " = ?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: PROBE" request))))
  (connect connector me)
  me)

(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)

(set-value! C 25 'user)

; implementing the constraint system

; the basic operations on connectors are as following:
; (has-value? <connector>)
; (get-value <connector>)
; (set-value! <connector> <new-value> <informant>)
; (forget-value! <connector> <retractor>)
; (connect <connector> <new-constraint>)

; the connectors communicate with with the constraints
; by means of procedures
; 'inform-about-value'
; which tells the given constraint that the
; connector has a value, and
; 'inform-about-no-value',
; which tells the constraint that 
; the connector has lost its value
;
; adder constructs an adder constraint among
; summand connectors a1 and a2 and a sum connector.

(define (adder a1 a2 sum)

  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me))))

  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))

  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: ADDER" request))))

  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)


(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? m1) (has-value? product))
           (set-value! m2
                       (/ (get-value product) (get-value m1))
                       me))
          ((and (has-value? m2) (has-value? product))
           (set-value! m1
                       (/ (get-value product) (get-value m2))
                       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: MULTIPLIER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (error "Unknown request: CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

; representing connectors
(define (make-connector)
  (let ((value false) (informant false) (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant false)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
       'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
       (set! constraints (cons new-constraint constraints)))
      (if (has-value? me)
       (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant true false))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown request: CONNECTOR" request))))
    me))

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'value))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

; ex. 3.33 Averager
(define (averager n1 n2 avg)
  (let ((u (make-connector))
        (v (make-connector)))
    (multiplier v u avg)
    (adder n1 n2 v)
    (constant 0.5 u)
    'ok))

(define N1 (make-connector))
(define N2 (make-connector))
(define AVG (make-connector))

(define avger (averager N1 N2 AVG))

(probe 'N1 N1)
(probe 'N2 N2)
(probe 'AVG AVG)

(set-value! N1 5 'user)
(set-value! N2 7 'user)

; ex. 3.34 Squarer
;(define (squarer n sq)
  ;(multiplier n n sq))

(define N (make-connector))
(define SQ (make-connector))
(define sqrer (squarer N SQ))
(probe 'N N)
(probe 'SQ SQ)
(set-value! N 5 'user)
(forget-value! N 'user)
(set-value! SQ 144.0 'user)
(forget-value! SQ 'user)
; so setting SQ to a value will not calculate N.
; solution is to define a new primitive constraint
(define (squarer n sq)
  (define (process-new-value)
    (if (has-value? sq)
     (if (< (get-value sq) 0)
      (error "square less than 0: SQUARER" (get-value sq))
      (set-value! n
                  (sqrt (get-value sq))
                  me))
     (if (has-value? n)
      (set-value! sq
                  (* (get-value n) (get-value n))
                  me))))
  (define (process-forget-value)
    (forget-value! n me)
    (forget-value! sq me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: SQUARER" request))))
  (connect n me)
  (connect sq me)
  me)
