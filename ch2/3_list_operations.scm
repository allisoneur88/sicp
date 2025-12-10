(define onetofour (list 1 2 3 4))
(display onetofour)

(car onetofour)
(car (cdr onetofour))
(cadr onetofour)
(caddr onetofour)
(cadddr onetofour)

(define (list-ref items n)
  (if (eq? n 0)
    (car items)
    (nth (cdr items) (- n 1))))

(list-ref onetofour 3)

(define (length-rec items)
  (if (null? items)
    0
    (+ 1 (length (cdr items)))))

(length-rec onetofour)

(define (length-iter items)
  (define (iter items count)
    (if (null? items)
      count
      (iter (cdr items) (+ count 1))))
  (iter items 0))
    
(length-iter onetofour)

(define (append-rec list1 list2)
  (if (null? list1)
    list2
    (cons (car list1) (append (cdr list1) list2))))

(append-rec onetofour onetofour)

(define nil '())

(define (last-pair items)
  (if (null? items)
    '()
    (if (eq? (cdr items) nil) 
      (car items)
      (last-pair (cdr items)))))

(last-pair (list))
(last-pair onetofour)

(define (reverse-rec items)
  (if (null? items)
    items
    (append-rec (reverse-rec (cdr items)) (list (car items)))))

(reverse-rec onetofour)

(define (listcar items)
  (list (car items)))

(listcar onetofour)
(car onetofour)

(define (same-parity . items)
  (let ((parity (even? (car items))))
    (if (eq? (even? (car items)) parity)
      (cons (car items) (same-parity (cdr items)))
      (same-parity (cdr items)))))

(define (same-parity . items)
  (define (helper l)
    (let ((parity (even? (car l))))
      (if (eq? (even? (car l)) parity)
       (cons (car l) (helper (cdr l)))
       (helper (cdr l)))))
  (helper items))

(define (same-parity . items)
  (let ((parity (even? (car items))))
    (define (helper l)
      (if (null? l)
       '()
       (if (eq? (even? (car l)) parity)
        (cons (car l) (helper (cdr l)))
        (helper (cdr l)))))
    (helper items)))

(same-parity 1 2 3 4 5 6)
(same-parity 2 4 5 6 9 12 14)

(define (scale-list items factor)
  (if (null? items)
    '()
    (cons (* factor (car items)) (scale-list (cdr items) factor))))

(scale-list onetofour 5)


