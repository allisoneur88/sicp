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
(define (make-queue) (cons '() '()))

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item)
  (set-car! queue item))
(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

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
           (set-cdr! (rear-ptr queue) new-pair) ; this also mutates the front ptr
           (set-rear-ptr! queue new-pair)       ; because both front and rear
           queue))))                            ; point to the same cons cell

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called on an empty queue"))
        (else (set-front-ptr! queue (cdr (front-ptr queue)))
              queue)))

; ex. 3.21
(define (print-queue queue)
  (define (print-list list)
    (if (null? (cdr list))
     (begin
       (display (car list))
       (display " <= rear"))
     (begin
       (display (car list))
       (display " <- ")
       (print-list (cdr list)))))
  (cond ((empty-queue? queue)
         (error "PRINT called on an empty queue"))
        (else
         (display "front <= ")
         (print-list (front-ptr queue)))))

(define q (make-queue))
(insert-queue! q 'a)
(insert-queue! q 'b)
(insert-queue! q 'c)
(print-queue q)
(delete-queue! q)
(print-queue q)

; ex. 3.22
(define (make-queue-mp)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty-queue?)
      (null? front-ptr))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
       (cond ((empty-queue?)
              (set! front-ptr new-pair)
              (set! rear-ptr new-pair))
             (else
              (set-cdr! rear-ptr new-pair)
              (set! rear-ptr new-pair)))))
    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE called on empty queue"))
            (else
             (set! front-ptr (cdr front-ptr)))))
    (define (print-queue)
      (define (print-iter list)
       (cond ((null? (cdr list))
              (display (car list))
              (display " <- rear"))
             (else
              (display (car list))
              (display " <- ")
              (print-iter (cdr list)))))
      (cond ((empty-queue?)
             (display "Empty queue"))
            (else
             (display "front <- ")
             (print-iter front-ptr))))
             
    (define (dispatch m)
      (cond ((eq? m 'insert!) insert-queue!)
            ((eq? m 'delete!) delete-queue!)
            ((eq? m 'empty?) empty-queue?)
            ((eq? m 'print) print-queue)
            (else (error "Unknown dispatch mode"))))
    dispatch))
 
(define (insert-mp! queue item)
  ((queue 'insert!) item))
(define (delete-mp! queue)
  ((queue 'delete!)))
(define (empty-mp? queue)
  ((queue 'empty?)))
(define (print-mp queue)
  ((queue 'print)))

(define qq (make-queue-mp))
(insert-mp! qq 1)
(insert-mp! qq 2)
(print-mp qq)
(empty-mp? qq)
(delete-mp! qq)
