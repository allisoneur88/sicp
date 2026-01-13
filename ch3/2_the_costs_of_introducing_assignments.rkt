#lang sicp
; eval file 1
(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))

(define W (make-simplified-withdraw 100))

(W 40)

(define (make-decrementer balance)
  (lambda (amount)
    (- balance amount)))

(define D (make-decrementer 100))
(D 40)

(define peter-acc (make-account 100))
(define paul-acc peter-acc)

((peter-acc 'withdraw) 10)
((paul-acc 'withdraw) 10)

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
      product
      (iter (* counter product) (+ counter 1))))
  (iter 1 1))

(factorial 5)

(define (factorial-imp n)
  (let ((product 1)
        (counter 1))
    (define (iter)
      (if (> counter n)
       product
       (begin (set! product (* counter product))
              (set! counter (+ counter 1))
              (iter))))
    (iter)))

(factorial-imp 5)

; ex. 3.7
(define (make-account-pw balance password)
  (define (login)
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
            (else (error "Unknown request (logged in): MAKE-ACCOUNT-PW" m))))
    dispatch)
  (define (dispatch pw m)
    (if (eq? pw password)
      (cond ((eq? m 'login) (login))
            (else (error "Unknown request (not logged in)")))
      (error "Incorrect password")))
  dispatch)

(define (make-joint acc accs-pw new-pw)
  (let ((logged-in-acc (acc accs-pw 'login)))
    (define (dispatch pw m)
      (if (eq? pw new-pw)
       (cond ((eq? m 'login) logged-in-acc)
             (else (error "Unknown request (not logged in)")))
       (error "Incorrect password for joint-acc")))
    dispatch))

(define a1 (make-account-pw 100 'red))
(define a1l (a1 'red 'login))

(define a2 (make-joint a1 'red 'blue))
(define a2l (a2 'blue 'login))

(define a3 (make-joint a2 'blu 'green))

