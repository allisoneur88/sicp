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

; we implement serializers in terms
; of a more primitive synchronization
; mechanism called a mutex.

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
       (mutex 'acquire)
       (let ((val (apply p args)))
        (mutex 'release)
        val))
      serialized-p)))

(define (make-test)
  (lambda (p)
    (define (tested-p . args)
      (print! "Mutex acquired")
      (let ((val (apply p args)))
       (print! "Mutex released")
       (newline)
       val))
    tested-p))

(define (square x) (* x x))
(define test (make-test))
(define tested-square (test square))
(tested-square 5)

(define (print! string)
  (newline)
  (display string))

; Mutex
(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell) (set-car! cell false))

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin
       (set-car! cell true)
       false)))

; there is a crucial subtlety here
; the test-and-set! operation
; must be performed atomically!

; the actual implementation of
; test-and-set! depends on the
; details of how our system runs
; concurrent processes

; in mit-scheme for a single processor,
; which uses a time-slicing model,
; test-and-set! can be implemented as follows:
(define (single-processor-time-slicing-test-and-set! cell)
  (without-interrupts
    (lambda ()
      (if (car cell)
       true
       (begin! (set-car! cell true)
               false)))))

; alternatively, multiprocessing computers
; provide instructions that support atomic
; operations directly in hardware

; naive semaphore
(define (make-semaphore-n n)
  (let ((counter n))
    (define (test-and-dec!)
      (if (= counter 0)
       true
       (begin (set! counter (- counter 1))
              false)))
    (define (inc!)
      (set! counter (+ counter 1)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (if (test-and-dec!)
              (the-semaphore 'acquire)))
            ((eq? m 'release) (inc!))))
    the-semaphore))

; we need an atomic primitive
; we use a serializer built 
; top of atomic primitives
(define (make-semaphore-n n)
  (let ((counter n)
        (s (make-serializer)))
    (define (acquire)
      (s (lambda ()
          (if (> counter 0)
           (set! counter (- counter 1))
           (acquire)))))
    (define (release)
      (s (lambda ()
          (set! counter (+ counter 1)))))
    (lambda (m)
      (cond ((eq? m 'acquire) (acquire))
            ((eq? m 'release) (release))))))
