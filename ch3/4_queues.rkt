#lang sicp

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
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item)
  (set-car! queue item))
(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue")
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called on an empty queue"))
        (else (set-front-ptr! queue (cdr (front-ptr queue)))
              queue)))

(define q (make-queue))
(display q)
(empty-queue? q)
(insert-queue! q 'a)
(display q)
(insert-queue! q 'b)
(display q)
(insert-queue! q 'c)
(display q)
(delete-queue! q)
(display q)
