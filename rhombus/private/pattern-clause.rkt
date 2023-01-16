#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/transformer
                     enforest/property
                     "name-path-op.rkt"
                     "introducer.rkt")
         "name-root-ref.rkt")

(provide define-pattern-clause-syntax)

(module+ for-class
  (provide (for-syntax in-pattern-clause-space)))

(begin-for-syntax
  (provide (property-out pattern-clause-transformer)
           :pattern-clause
           pattern-clause?)

  (property pattern-clause-transformer transformer)

  (define in-pattern-clause-space (make-interned-syntax-introducer/add 'rhombus/pattern_clause))

  (define-transform
    #:syntax-class :pattern-clause
    #:desc "pattern clause"
    #:predicate pattern-clause?
    #:in-space in-pattern-clause-space
    #:name-path-op name-path-op
    #:name-root-ref name-root-ref
    #:name-root-ref-root name-root-ref-root
    #:transformer-ref pattern-clause-transformer-ref))

(define-syntax (define-pattern-clause-syntax stx)
  (syntax-parse stx
    [(_ id:identifier rhs)
     #`(define-syntax #,(in-pattern-clause-space #'id)
         rhs)]))
