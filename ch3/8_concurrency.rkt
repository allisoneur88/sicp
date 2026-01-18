#lang sicp

; serializers
; suppose we defined
; (parallel-execute <p-1> <p-2> ... <p-k>)
; each <p> must be a procedure of no arguments
; parallel-execute creates a separate process
; for each <p>, which applies <p> (to no arguments)
; these processes all run concurrently
;
; an example of how this is used
(define x 10)
(parallel-execute
  (lambda () (set! x (* x x)))
  (lambda () (set! x (+ x 1))))

; after the execution there are 5(!)
; possible values for x

; we can constrain the concurrency
; by using serialized procedures,
; which are created by serializers.
;
(define s (make-serializer))
(define y 10)
(parallel-execute
  (s (lambda () (set! y (* y y))))
  (s (lambda () (set! y (+ y 1)))))
; this can only produce to possible
; values for y - 101 and 121
; because serializer makes it so
; processes P1 and P2 can not be interleaved.
;
; here is the version of make-account
; where the deposits and withdrawals
; have been serialized
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
     (begin (set! balance (- balance amount))
            balance)
     "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
            ((eq? m 'deposit) (protected deposit))
            ((eq? m 'balance) balance)
            (else (error "Unknown request" m))))
    dispatch))


