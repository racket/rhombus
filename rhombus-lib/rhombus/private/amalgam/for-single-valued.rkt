#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre))

(provide for/single-valued)

(define-syntax (for/single-valued stx)
  (syntax-parse stx
    [(_ clauses body)
     #'(for clauses (void body))]))
