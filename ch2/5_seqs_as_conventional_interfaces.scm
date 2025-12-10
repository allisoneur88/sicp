(define tree (list (list 1 2) (list 3 4)))
(display tree)

(define (fib_rec k)
  (if (< k 2)
    k
    (+ (fib (- k 1)) (fib (- k 2)))))

(define (fib k)
  (define (helper a b count)
    (if (= count 0)
      b
      (helper (+ a b) a (- count 1))))
  (helper 1 0 k))

(fib_rec 7)

(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree)
          (square tree)
          0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))

(define (sum-odd-squares-fancy tree)
  (accumulate + 0 (map square (filter odd? (enumerate-tree tree)))))

(sum-odd-squares tree)
(sum-odd-squares-fancy tree)

(define (even-fibs n)
  (define (next k)
    (if (> k n)
      '()
      (let ((f (fib k)))
       (if (even? f)
        (cons f (next (+ k 1)))
        (next (+ k 1))))))
  (next 0))
(define (even-fibs-fancy n)
  (accumulate cons nil (filter even? (map fib (enumerate-interval 0 n)))))

(even-fibs 15)
(even-fibs-fancy 15)

(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
          (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(filter
  (lambda (x) (eq? (modulo x 10) 0))
  (even-fibs 15))

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
     (accumulate op initial (cdr sequence)))))

(accumulate + 0 (list 1 2 3 4 5))
(accumulate * 1 (list 1 2 3 4 5))
(accumulate cons nil (list 1 2 3 4 5))

(define (enumerate-interval low high)
  (if (> low high)
    '()
    (cons low (enumerate-interval (+ low 1) high))))

(enumerate-interval 3 9)

(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not(pair? tree)) (list tree))
        (else (append
               (enumerate-tree (car tree))
               (enumerate-tree (cdr tree))))))

(enumerate-tree tree)

(define (xxx n)
  (accumulate
    append '() (map (lambda (i)
                     (map (lambda (j) (list i j))
                      (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n))))

(xxx 10)

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (prime? n)
  (define (helper i)
    (cond ((> (* i i) n) #t)
          ((= (remainder n i) 0) #f)
          (else (helper (+ i 1)))))
  (helper 2))

(prime? 16)

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (flatmap
                           (lambda (i)
                            (map (lambda (j) (list i j))
                             (enumerate-interval 1 (- i 1))))
                           (enumerate-interval 1 n)))))

(prime-sum-pairs 7)

(define (permutations s)
  (if (null? s)
    (list '())
    (flatmap (lambda (x)
              (map (lambda (p) (cons x p))
               (permutations (remove x s))))
       s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
    sequence))

(permutations (list 1 2 3))
