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
