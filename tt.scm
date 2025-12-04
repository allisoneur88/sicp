(define (fib n)
  (if (< n 2) 
    n
    (+ 
      (fib (- n 1))
      (fib (- n 2)))))

(fib 7)

(define (fibi n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
    b
    (fib-iter (+ a b) a (- count 1))))

(fibi 8)

(define (ex1_11 n)
  (if (< n 3)
    n
    (+ (ex1_11 (- n 1)) (* 2 (ex1_11 (- n 2))) (* 3 (ex1_11 (- n 3))))))

(ex1_11 11)

(define (ex1_11i n)
  (ex1_11-iter 2 1 0 n))

(define (ex1_11-iter a b c count)
  (if (= count 0)
    c
    (ex1_11-iter (+ a (* 2 b) (* 3 c)) a b (- count 1))))
 
(ex1_11i 11)

(define (ex1_12 row col)
  (cond
    ((= row 1) 1)
    ((= row 2) 1)
    ((= col 1) 1)
    ((= col row) 1)
    ((< col 1) -1)
    ((< row 1) -1)
    ((> col row) -1)
    (else (+ (ex1_12 (- row 1) (- col 1)) (ex1_12 (- row 1) col)))))

(ex1_12 5 3)

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond (( = amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                  (- kinds-of-coins 1))
                 (cc ( - amount
                      (first-denomination kinds-of-coins))
                  kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond 
    ((= kinds-of-coins 1) 1)
    ((= kinds-of-coins 2) 5)
    ((= kinds-of-coins 3) 10)
    ((= kinds-of-coins 4) 25)
    ((= kinds-of-coins 5) 50)))

(count-change 11)
    

(define (sum-integers a b)
  (if (> a b)
    0
    (+ a (sum-integers ( + a 1) b))))

(define (sum-integers2 a b)
  (sum a b identity inc))

(define (identity x) x)

(define (cube x) (* x x x))

(define (sum-cubes a b)
  (if (> a b)
    0
    (+ 
      (cube a) 
      (sum-cubes (+ a 1) b))))

(define (pi-sum a b)
  (if (> a b)
    0
    (+
      (/ 1.0 (* a (+ a 2)))
      (pi-sum (+ a 4) b))))

(pi-sum 1 11)

(define (sum a b term next)
  (if (> a b)
    0
    (+
     (term a)
     (sum (next a) b term next))))

(define (inc n) (+ n 1))

(define (sum-cubes2 a b)
  (sum a b cube inc))

(sum-cubes 1 5)
(sum-cubes2 1 5)

(sum-integers 1 10)
(sum-integers2 1 10)

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum (+ a (/ dx 2.0)) b f add-dx) dx))

(integral cube 0 1 0.01)

(define (integral-lambda f a b dx)
  (*
    (sum (+ a (/ dx 2.0)) b f (lambda (x) (+ x dx)))
    dx))


(integral-lambda cube 0 1 0.01)

((lambda (x y z) (+ x y (square z))) 1 2 3)

(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

(define (f g) (g 2))
(f f)

(define (average x y) (/ (+ x y) 2))
(define (search f neg pos)
  (let ((mid (average neg pos)))
    (define (close-enough? x y) (< (abs (- x y)) 0.001)
      (if (close-enough? neg pos)
       mid
       (let ((test-val (f mid)))
        (cond ((positive? test-val)
               (search f neg mid))
              ((negative? test-val)
               (search f mid pos))
              (else mid)))))))

(define (half-interval-method f a b)
  (let ((a-val (f a))
        (b-val (f b)))
    (cond ((and (negative? a-val) (positive? b-val))
           (search f a b))
          ((and (negative? b-val) (positive? a-val))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))


(search (lambda (x) (+ x 10)) (- 15) (- 5))
(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))
          1.0
          2.0)

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
       next
       (try next))))
  (try first-guess))

(fixed-point cos 1.0)
(cos 0.7390822985224024)

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)

(define (make-adder num)
  (lambda (x) (+ num x)))

((make-adder 2) 5)

(define (compose f g)
  (lambda (x) (f (g x))))

((compose (make-adder 2) (make-adder 3)) 5)
