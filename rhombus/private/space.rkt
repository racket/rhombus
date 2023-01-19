#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/property
                     "introducer.rkt"))

(provide (for-syntax space-syntax)
         define-space-syntax)

(begin-for-syntax
  (provide (property-out space-name)
           space-name-ref
           space-name-symbol
           in-space-space)

  (property space-name (symbol))

  (define in-space-space (make-interned-syntax-introducer/add 'rhombus/space))

  (define-syntax-rule (space-syntax sym)
    (space-name 'sym)))

(define-syntax (define-space-syntax stx)
  (syntax-parse stx
    [(_ id:identifier rhs)
     #`(define-syntax #,(in-space-space #'id)
         rhs)]))
