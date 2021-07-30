#lang racket/base

(provide syntax-local-value*)
  
(define (syntax-local-value* id ok?)
  (define-values (v next) (syntax-local-value/immediate id (lambda () (values #f #f))))
  (cond
    [(ok? v) v]
    [next (syntax-local-value* next ok?)]
    [else #f]))
