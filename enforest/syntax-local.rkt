#lang racket/base

(provide syntax-local-value*)
  
(define (syntax-local-value* id ref)
  (define-values (v next) (syntax-local-value/immediate id (lambda () (values #f #f))))
  (cond
    [(ref v) => (lambda (v) v)]
    [next (syntax-local-value* next ref)]
    [else #f]))
