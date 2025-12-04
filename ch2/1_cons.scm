(define (struct x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1: " m))))
  dispatch)

(define (kar s)
  (s 0))

(define (kdr s)
  (s 1))

(define str 
  (struct 1 2))

(kar str)
(kdr str)
