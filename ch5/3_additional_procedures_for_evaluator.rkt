#lang sicp

(define (empty-arglist) '())

(define (adjoin-arg arg arglist) (append arglist (list arg)))

(define (last-operand? ops) (null? (cdr ops)))

(define (no-more-exps? seq) (null? seq))

(define the-global-environment (setup-environment))
(define (get-global-environment) the-global-environment)
