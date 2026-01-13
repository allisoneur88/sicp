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
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (begin (set! balance (+ balance amount))
           balance))
  (define (pw-correct? attempt)
    (eq? password attempt))
  (define (dispatch pw m)
    (if (eq? pw password)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'pw-correct?) pw-correct?)
            (else (error "Unknown request: MAKE-ACCOUNT-PW" m)))
      (error "Incorrect password")))
  dispatch)

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

(define A (make-account-pw 100 'rose))
(define A-logged-in (A 'rose 'login))
((A-logged-in 'deposit) 100)
((A-logged-in 'withdraw) 50)

(define (make-joint acc accs-pw new-pw)
  (define (withdraw amount))
  (define (dispatch pw m)
    (if (eq?))))


