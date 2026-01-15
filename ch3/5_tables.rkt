#lang sicp

; 1-dimensional table
(define (assoc key records)
  (cond ((null? records) false)
        ((equal? (caar records) key) (car records))
        (else (assoc key (cdr records)))))

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
     (cdr record)
     false)))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
     (set-cdr! record value)
     (set-cdr! table
               (cons (cons key value)
                     (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))

(define table (make-table))
(insert! 'name 'sasha table)
(insert! 'wife-name 'lena table)

(lookup 'name table)
(lookup 'wife-name table)

; 2-dimensional table
(define (lookup2d key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
     (let ((record (assoc key-2 (cdr subtable))))
       (if record
        (cdr record)
        false))
     false)))

(define (insert2d! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
     (let ((record (assoc key-2 (cdr subtable))))
       (if record
        (set-cdr! record value)
        (set-cdr! subtable
                  (cons (cons key-2 value)
                        (cdr subtable)))))
     (set-cdr! table
               (cons (list key-1 
                           (cons key-2 value))
                     (cdr table)))))
  'ok)

(define (make-table2d)
  (list '*table*))

(define table2d (make-table2d))
(insert2d! 'math '+ 43 table2d)
(insert2d! 'math '- 45 table2d)
(insert2d! 'math '* 42 table2d)
(insert2d! 'letters 'a 97 table2d)
(insert2d! 'letters 'b 98 table2d)
(lookup2d 'math '+ table2d)
(lookup2d 'math '* table2d)
(lookup2d 'letters 'a table2d)
(lookup2d 'tractors 'big table2d)

; message-passing style procedural table
(define (make-table-mp)
  (let ((local-table (list '*table)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
       (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
         (if record
          (cdr record)
          false))
        false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
       (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
         (if record
          (set-cdr! record value)
          (set-cdr! subtable
                    (cons (cons key-2 value)
                          (cdr subtable)))))
        (set-cdr! local-table
                  (cons (list key-1 (cons key-2 value))
                        (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operaton: TABLE" m))))
    dispatch))

(define operation-table (make-table-mp))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(put 'letters 'a 97)
(put 'letters 'b 98)
(put 'math '* 42)
(put 'math '+ 43)
(get 'letters 'a)
(get 'letters 'b)
(get 'letters 'c)
(get 'math '*)
(get 'math '-)
(get 'meth 'lots)
