(define (empty-arglist) '())

(define (adjoin-arg arg arglist) (append arglist (list arg)))

(define (last-operand? ops) (null? (cdr ops)))
