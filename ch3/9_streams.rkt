#lang sicp

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-for-n proc s n)
  (if (= n 0)
    'done
    (begin (proc (stream-car s))
           (stream-for-n proc (stream-cdr s) (- n 1)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-line x)
  (newline)
  (display x))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-stream-n s n)
  (stream-for-n display-line s n))

;(define (cons-stream a b)
  ;(cons a (delay b)))

(define (stream-car stream)
  (car stream))
(define (stream-cdr stream)
  (force (cdr stream)))

; prime? predicate real quick
(define (prime? n)
  (cond ((< n 2) #f)
        (else (smallest-divisor n))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (* test-divisor test-divisor) n) #t)
        ((divides? test-divisor n) #f)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

; let's alanlyze this computation
(stream-car
  (stream-cdr
    (stream-filter prime?
                   (stream-enumerate-interval 10000 1000000))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
                   (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred (stream-cdr stream))))
        (else
         (stream-filter pred (stream-cdr stream)))))

; what about delay and force?
;(define (delay expression)
  ;(lambda () expression))

;(define (force delayed-object)
  ;(delayed-object))

; there is an important optimization that
; we can include - memoization
;(define (memo-proc proc)
;  (let ((already-run? false) (result false))
;    (lambda ()
;      (if (not already-run?)
;       (begin (set! result (proc))
;              (set! already-run? true)
;              result)
;       result))))

; delay is then defined as
;(define (delay expression)
 ;(memo-proc (lambda () expression)))

; ex 3.51
(define (show x)
  (display-line x)
  x)

(define x
  (stream-map show
              (stream-enumerate-interval 0 10)))

(stream-ref x 5)
(stream-ref x 7)

; ex. 3.52
(define sum 0)
(define (accum x) (set! sum (+ x sum)) sum)
(define seq
  (stream-map accum
        (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z
  (stream-filter (lambda (x) (= (remainder x 5) 0))
                 seq))
(stream-ref y 7)
(display-stream z)

; infinite streams
; stream of positive integers
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

; stream of integers that are not divisible by 7
(define (divisible? x y) (= (remainder x y) 0))
(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))
(stream-ref no-sevens 100)

; infinite stream of fibonacci numbers
(define (fibgen a b) (cons-stream a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))

; infinite stream of prime numbers
; using sieve of Eratosphenes
(define (sieve stream)
  (cons-stream
    (stream-car stream)
    (sieve (stream-filter
            (lambda (x)
             (not (divisible? x (stream-car stream))))
            (stream-cdr stream)))))
(define primes (sieve (integers-starting-from 2)))
(stream-ref primes 1000)

; defining streams implicitly
; infinite stream of ones
(define ones (cons-stream 1 ones))

; addings streams element-wise
(define (add-streams s1 s2) (stream-map + s1 s2))
; then integers can be defined
(define integers
  (cons-stream 1 (add-streams ones integers)))

(stream-ref integers 666)

; fibs can be difined
(define fibs
  (cons-stream
    0
    (cons-stream 1 (add-streams (stream-cdr fibs) fibs))))

(stream-ref fibs 2)

; scale-stream is another useful procedure
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))

(define evens (scale-stream integers 2))

(define odds (stream-map
              (lambda (x) (- x 1))
              (scale-stream integers 2)))

; powers of 2
(define double (cons-stream 1 (scale-stream double 2)))

; ex. 3.53
(define s (cons-stream 1 (add-streams s s)))
; conjecture - powers of 2

; ex. 3.54
(define (mul-streams s1 s2) (stream-map * s1 s2))
(define factorials
  (cons-stream 1 (mul-streams factorials integers)))
(stream-ref factorials 6)

; ex. 3.55
(define (partial-sums stream)
  (cons-stream (stream-car stream)
               (add-streams
                (partial-sums stream)
                (stream-cdr stream))))

(define y (partial-sums integers))
(stream-ref y 4)

; ex. 3.56
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
          (cond ((< s1car s2car)
                 (cons-stream
                  s1car
                  (merge (stream-cdr s1) s2)))
                ((> s1car s2car)
                 (cons-stream
                  s2car
                  (merge s1 (stream-cdr s2))))
                (else
                 (cons-stream
                  s1car
                  (merge (stream-cdr s1) (stream-cdr s2)))))))))

(define S
  (cons-stream
    1
    (merge (scale-stream S 2)
           (merge 
                 (scale-stream S 3) (scale-stream S 5)))))

; ex. 3.57
; How many additions are performed when we compute
; the nth Fibonacci number using following proc
(define fibs
  (cons-stream
    0
    (cons-stream 1 (add-streams (stream-cdr fibs) fibs))))
; this performs n additions for nth Fib.
; if we would implement delay without
; memoization the number of additions
; would be exponentially greater

; ex. 3.58
(define (expand num den radix)
  (cons-stream
    (quotient (* num radix) den)
    (expand (remainder (* num radix) den) den radix)))

(define X (expand 3 8 10))
(stream-ref X 3)
; this is a long divisin algorithm
; generates the stream of digits
; representing the fractional part
; of the rational number num/den in
; the base radix

; ex. 3.59
(define (div-streams s1 s2)
  (stream-map / s1 s2))

(define fractions (div-streams ones integers))

(define (integrate-series stream)
  (mul-streams stream fractions))

; we can generate the series for e^x as:
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

; cosine and sine
(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(define cosine-series
  (cons-stream 1
               (integrate-series
                               (scale-stream
                                           sine-series -1))))

(stream-ref sine-series 5)

; ex. 3.60
(define (add-series s1 s2)
  (add-streams s1 s2))
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                            (mul-series (stream-cdr s1) s2))))

(define result
  (add-series
    (mul-series sine-series sine-series)
    (mul-series cosine-series cosine-series)))

(display-stream-n result 10)

; ex. 3.61
(define (invert-unit-series s)
  (cons-stream 1
               (scale-stream
                           (mul-series (stream-cdr s) (invert-unit-series s))
                           -1)))

(define x
  (mul-series
    cosine-series
    (invert-unit-series cosine-series)))

(display-stream-n x 10)

; ex. 3.62
(define (invert-series s)
  (let ((c (stream-car s)))
    (if (= c 0)
      (error "Constant term is zero" c)
      (let ((unit-series (scale-stream s (/ 1 c))))
       (scale-stream (invert-unit-series unit-series) (/ 1 c))))))

(define (div-series s1 s2)
  (mul-series
    s1
    (invert-series s2)))

(define tangent-series (div-series sine-series cosine-series))
(display-stream-n tangent-series 10)
