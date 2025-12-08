#lang sicp

(define (factorial1 n)
  (if (< n 3)
      n
      (* n (factorial (- n 1)))))
  
  
(factorial1 5)

(define (factorial2 n)
  (factorial_iter 1 1 n))

(define (factorial_iter product counter max_count
         (if (> counter max_count)
          product
          (factorial_iter (* product counter) (+ counter 1) max_count))))

(factorial2 5)

(define (fib1 n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(fib1 7)

(define (fib2 n)
  (fib_iter 1 0 n))

(define (fib_iter a b count)
  (if (= count 0)
      b
      (fib_iter (+ a b) a (- count 1))))

(fib2 8)

(define (pee) 3.1415)
(pee)
