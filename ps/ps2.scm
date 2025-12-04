(define (play-hand strategy my-hand opponent-up-card)
  (cond ((> (hand-total my-hand) 21) my-hand)
   ((strategy my-hand opponent-up-card)
    (play-hand strategy
         (hand-add-card my-hand (deal))
     opponent-up-card))
   (else my-hand)))

(define (make-hand up-card total)
  (cons up-card total))

(define (hand-up-card hand)
  (car hand))

(define (hand-total hand)
  (cdr hand))

(define (make-new-hand first-card)
  (make-hand first-card first-card))

(define (hand-add-card hand new-card)
  (make-hand (hand-up-card hand) (+ new-card (hand-total hand))))

(define (deal) (+ 1 (random 10)))

(define (twenty-one player-strategy house-strategy)
  (let ((house-initial-hand (make-new-hand (deal))))
    (let ((player-hand
           (play-hand player-strategy
            (make-new-hand(deal))
            (hand-up-card house-initial-hand))))
      (if (> (hand-total player-hand) 21)
       0
       (let ((house-hand
              (play-hand house-strategy
               house-initial-hand
               (hand-up-card player-hand))))
        (cond ((> (hand-total house-hand) 21)
               1)
         ((> (hand-total player-hand) (hand-total house-hand))
          1)
         (else 0)))))))

(define (hit? your-hand opponent-up-card)
  (newline)
  (display "Opponent up card ")
  (display opponent-up-card)
  (newline)
  (display "Your Total: ")
  (display (hand-total your-hand))
  (newline)
  (display "Hit? ")
  (user-says-y?))

(define (user-says-y?) (eq? (read) 'y))

(define (stop-at n)
  (lambda (hand opponent-up-card)
    (if (< (hand-total hand) n)
      #t
      #f)))

(define (test-strategy p-strat h-strat n)

 (define (show-stats p-score h-score)
   (newline)
   (list 'player p-score 'house h-score))

 (define (test-strategy-iter p-strat h-strat counter max-count p-score h-score)
   (if (> counter max-count)
     (show-stats p-score h-score)
     (if (eq? (twenty-one p-strat h-strat) 1)
      (test-strategy-iter p-strat h-strat (+ counter 1) max-count (+ p-score 1) h-score)
      (test-strategy-iter p-strat h-strat (+ counter 1) max-count p-score (+ h-score 1)))))

 (test-strategy-iter p-strat h-strat 1 n 0 0))

(twenty-one hit? hit?)
(twenty-one (stop-at 16) (stop-at 17))

(test-strategy (stop-at 17) (stop-at 15) 1000)

(define (member? x lst)
  (if (member x lst) #t #f))

(define (buzz n)
  (cond ((eq? (modulo n 7) 0) 'buzz)
        ((member? 7 n) 'buzz')
        (else n)))

(buzz 15)
(buzz 14)
(buzz 27)
