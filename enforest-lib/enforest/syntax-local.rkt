#lang racket/base
(require "deprecated.rkt"
         (submod "deprecated.rkt" struct))

(provide syntax-local-value*)
  
(define (syntax-local-value* id ref)
  (define-values (v next) (syntax-local-value/immediate id (lambda () (values #f #f))))
  (when (deprecated-rename-transformer? v)
    (warn-deprecated! (deprecated-rename-transformer-name v)
                      (deprecated-rename-transformer-date v)))
  (cond
    [(ref v) => (lambda (v) v)]
    [next (syntax-local-value* next ref)]
    [else #f]))
