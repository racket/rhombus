#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/transformer
                     enforest/property
                     "introducer.rkt")
         "enforest.rkt")

(provide define-pattern-clause-syntax)

(module+ for-class
  (provide (for-syntax in-pattern-clause-space)))

(begin-for-syntax
  (provide (property-out pattern-clause-transformer)
           :pattern-clause
           pattern-clause?)

  (property pattern-clause-transformer transformer)

  (define in-pattern-clause-space (make-interned-syntax-introducer/add 'rhombus/pattern_clause))

  (define-rhombus-transform
    #:syntax-class :pattern-clause
    #:desc "pattern clause"
    #:predicate pattern-clause?
    #:in-space in-pattern-clause-space
    #:transformer-ref pattern-clause-transformer-ref))

(define-syntax (define-pattern-clause-syntax stx)
  (syntax-parse stx
    [(_ id:identifier rhs)
     #`(define-syntax #,(in-pattern-clause-space #'id)
         rhs)]))
