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
