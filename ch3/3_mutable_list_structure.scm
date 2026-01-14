; ex. 3.12
; append defined in 2.2.1
(define (append x y)
  (if (null? x)
    y
    (cons (car x) (append (cdr x) y))))

; append! is similar to append, but it's a mutator
; rather than constructor. It appends the lists
; by splicing them together, modifying the final
; pair of x so that its cdr is now y.
(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
    x
    (last-pair (cdr x))))

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))

(define (show list)
  list)

(show z)
(cdr x)

(define w (append! x y))
(show w)
(cdr x)

; ex. 3.13
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

;(define z (make-cycle (list 'a 'b 'c)))
;(show z)
;(last-pair z)

; ex 3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
      y
      (let ((temp (cdr x)))
       (set-cdr! x y)
       (loop temp x))))
  (loop x '()))
;     x       y     temp
; (a b c d)  ()
; -----------------------
;            ()    (b c d)
;    (a)
;  (b c d)   (a)
; -----------------------
;                   (c d)
;   (b a)
;   (c d)    (b a)
; -----------------------
;                     (d)
;  (c b a)
;    (d)    (c b a)
; -----------------------
;                     ()
; (d c b a)
;    ()    (d c b a)
; -----------------------
; => (d c b a)

(define v (list 'a 'b 'c 'd))
(show v)

(define w (mystery v))
(show w)

; ex. 3.16
(define (count-pairs x)
  (if (not (pair? x))
    0
    (+ (count-pairs (car x))
       (count-pairs (cdr x))
       1)))

(define a (list 'a 'b 'c))
(count-pairs a)
(count-pairs (list a a))

(define symb 'a) 
(define b (list symb symb symb))
(count-pairs b)

(define pair (cons 'a 'b))
(define c (list pair pair pair))
(count-pairs c)

(define d (cons 'x 'y)) ;=> (x . y)
(define e (cons d d))   ;=> ((x . y) x . y)
(define f (cons e e))   ;=> (((x . y) x . y) (x . y) x . y)

(count-pairs d)
(count-pairs e)
(count-pairs f)

(define j (cons e d))   ;=> (((x . y) x . y) x . y)
(count-pairs j)

(define g (cons e 'a))  ;=> (((x . y) x . y) . a)
(count-pairs g)

; ex. 3.17
(define (count-distinct-pairs x)
  (let ((seen (list)))
    (define (iter obj)
      (if (not (pair? obj))
       0
       (if (memq obj seen)
        0
        (begin (set! seen (cons obj seen))
               (+ (iter (car obj))
                  (iter (cdr obj))
                1)))))
    (iter x)))

(count-distinct-pairs d)
(count-distinct-pairs e)
(count-distinct-pairs f)
(count-distinct-pairs j)
(count-distinct-pairs g)

; ex. 3.18
(define (cycle? x)
  (let ((seen (list)))
    (define (iter obj)
      (if (not (pair? obj))
       #f
       (if (memq obj seen)
        #t
        (begin (set! seen (cons obj seen))
               (iter (cdr obj))))))
    (iter x)))

(cycle? d)
(cycle? e)
(cycle? f)
(cycle? j)
(cycle? g)

(cycle? (make-cycle (list 'a 'b)))

; ex. 3.19
; constant-space cycle?
; tortoise and hare
(define (cs-cycle? x)
  (define (iter slow fast)
    (cond ((not (pair? fast)) #f)
          ((not (pair? (cdr fast))) #f)
          ((eq? slow fast) #t)
          (else (iter (cdr slow) (cddr fast)))))
  (iter x (cdr x)))

(cs-cycle? d)
(cs-cycle? f)
  
(cs-cycle? (make-cycle (list 'a 'b)))  

; mutation is just assignment
(define (kons x y)
  (define (dispatch m)
    (cond ((eq? m 'kar) x)
          ((eq? m 'kdr) y)
          (else (error "Undefined operation" m))))
  dispatch)
(define (kar z) (z 'kar))
(define (kdr z) (z 'kdr))

(define k (kons 'z 'y))
(kar k)
(kdr k)

(define (mkons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'kar) x)
          ((eq? m 'kdr) y)
          ((eq? m 'set-kar!) set-x!)
          ((eq? m 'set-kdr!) set-y!)
          (else (error "Undefined operation" m))))
  dispatch)

(define (kar z) (z 'kar))
(define (kdr z) (z 'kdr))
(define (set-kar! z new-val)
  ((z 'set-kar!) new-val)
  z)
(define (set-kdr! z new-val)
  ((z 'set-kdr!) new-val)
  z)

(define mk (mkons 'a 'b))
(kar mk)
(kdr mk)
(set-kar! mk 'aa)
(kar mk)
(set-kdr! mk 'bb)
(kdr mk)
