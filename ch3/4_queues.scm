; doesn't work!
(define (make-q)
  '())

(define (empty-q? q)
  (null? q))

(define (front-q q)
  (if (empty-q? q)
    (error "Empty queue")
    (car q)))

(define (q! q obj)
  (set! q (cons q obj)))

(define (deq! q)
  (if (empty-q? q)
    (error "Empty q")
    (let ((head (car q)))
      (begin (set! q (cdr q)))
      head)))

(define q (make-q))
(q! q 'a)

; now lets go sicp way
