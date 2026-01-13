#lang sicp

(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
       balance)
      "Insufficient funds"))

(withdraw 30)
(display balance)

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
       (begin (set! balance (- balance amount))
              balance)
       "Insufficient funds"))))

(new-withdraw 40)

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
     (begin (set! balance (- balance amount))
            balance)
     "Insufficient funds")))

(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))

(W1 30)
(W2 40)

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
     (begin (set! balance (- balance amount))
            balance)
     "Insufficient funds"))
  (define (deposit amount)
    (begin (set! balance (+ balance amount))
           balance))

  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: MAKE-ACCOUNT"
                       m))))
  dispatch)

(define A1 (make-account 100))
(define A2 (make-account 200))
((A1 'withdraw) 40)
((A2 'deposit) 5)


; ex. 3.1
(define (make-accumulator initial)
  (lambda (amount)
    (begin (set! initial (+ initial amount))
           initial)))

(define A (make-accumulator 0))
(A -10)
(define B (make-accumulator 100))
(B 50)

; ex. 3.2
(define (make-monitored f)
  (let ((times-called 0))
    (define (how-many-calls?)
      times-called)
    (define (reset-count)
      (set! times-called 0))
    (define (dispatch m)
      (cond ((eq? m 'how-many-calls?) (how-many-calls?))
            ((eq? m 'reset-count) (reset-count))
            (else
             (begin
              (set! times-called (+ times-called 1))
              (f m)))))
    dispatch))

(define s (make-monitored sqrt))
(s 1000)
(s 'how-many-calls?)
(s 'reset-count)

; ex. 3.3
(define (make-account-pw balance password)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (begin (set! balance (+ balance amount))
           balance))
  (define (dispatch pw m)
    (if (eq? pw password)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else (error "Unknown request: MAKE-ACCOUNT-PW" m)))
      (error "Incorrect password")))
  dispatch)

(define acc (make-account-pw 1000 'secret))
((acc 'secret 'withdraw) 100)

; ex. 3.4
(define (make-account-cops balance password)
  (let ((bad-attempt-count 0))
    (define (withdraw amount)
      (if (>= balance amount)
       (begin (set! balance (- balance amount))
              balance)
       "Insufficient funds"))
    (define (deposit amount)
      (begin (set! balance (+ balance amount))
             balance))
    (define (call-the-cops)
      "Calling 911...")
    (define (reset-attempt-count)
      (set! bad-attempt-count 0))
    (define (inc-attempt-count)
      (set! bad-attempt-count (+ bad-attempt-count 1)))
    (define (dispatch pw m)
      (if (eq? pw password)
       (begin
        (reset-attempt-count)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request: MAKE-ACCOUNT-COPS" m))))
       (begin
        (inc-attempt-count)
        (if (>= bad-attempt-count 7)
         (call-the-cops)
         "Incorrect password"))))
    dispatch))

(define ac (make-account-cops 1000 'pass))
((ac 'pass 'withdraw) 300)
((ac 'pass 'deposit) 500)
((ac 'pas 'withdraw) 500)

; bad example that won't give correct statistical properties
(define random-init 100)
(define (rand-update n)
  (if (> n 100)
    (- n 17)
    (+ n 13)))
(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

(define random-num (rand))
(display random-num)

; monte-carlo to estimate pi
(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (random 100) (random 100)) 1))
(display (cesaro-test))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1)
                 trials-passed))))
  (iter trials 0))

(define pee (estimate-pi 1000000))
(display pee)

; ex. 3.5
(define (square x) (* x x))
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (* (* (abs (- x2 x1)) (abs (- y2 y1))) (monte-carlo trials (integral-test P x1 x2 y1 y2))))

(define (integral-test P x1 x2 y1 y2)
  (lambda ()
    (P (random-in-range x1 x2) (random-in-range y1 y2))))

(define (test-P x y)
  (<= (+ (square (- x 5)) (square (- y 7))) (square 3)))

(define (test-P-pi x y)
  (<= (+ (square (- x 1.0)) (square (- y 1.0))) (square 1.0)))

(estimate-integral test-P 2 8 4 10 10000)
(estimate-integral test-P-pi 0.0 2.0 0.0 2.0 100000)

(define (test-P-big-pi x y)
  (<= (+ (square (- x 2000)) (square (- y 2000))) (square 1000)))
(/ (estimate-integral test-P-big-pi 1000 3000 1000 3000 10000) (square 1000))
