#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre))

(provide field-case-lambda)

(define-syntax (field-case-lambda stx)
  (syntax-parse stx
    [(_ get [_ (#f . _)])
     #'(case-lambda get)]
    [(_ . clauses)
     #'(case-lambda . clauses)]))
