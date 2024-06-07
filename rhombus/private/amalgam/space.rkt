#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/property
                     "introducer.rkt"
                     (for-syntax racket/base)))

(provide (for-syntax space-syntax)
         define-space-syntax)

(begin-for-syntax
  (provide (property-out space-name)
           space-name-ref
           space-name-symbol
           in-space-space
           space-quote)

  (property space-name (symbol))

  (define in-space-space (make-interned-syntax-introducer/add 'rhombus/space))
  (define-syntax (space-quote stx)
    (syntax-case stx ()
      [(_ id) #`(quote-syntax #,((make-interned-syntax-introducer 'rhombus/space) #'id))]))

  (define-syntax-rule (space-syntax sym)
    (space-name 'sym)))

(define-syntax (define-space-syntax stx)
  (syntax-parse stx
    [(_ id:identifier rhs)
     #`(define-syntax #,(in-space-space #'id)
         rhs)]))
