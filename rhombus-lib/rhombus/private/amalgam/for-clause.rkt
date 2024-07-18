#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/transformer
                     enforest/property
                     "introducer.rkt")
         "enforest.rkt")

(provide define-for-clause-syntax)

(begin-for-syntax
  (provide (property-out for-clause-transformer)
           :for-clause
           for-clause?)

  (property for-clause-transformer transformer)

  (define in-for-clause-space (make-interned-syntax-introducer/add 'rhombus/for_clause))

  (define-rhombus-transform
    #:syntax-class :for-clause
    #:desc "for clause"
    #:parsed-tag #:rhombus/for_clause
    #:in-space in-for-clause-space
    #:predicate for-clause?
    #:transformer-ref for-clause-transformer-ref))

(define-syntax (define-for-clause-syntax stx)
  (syntax-parse stx
    [(_ id:identifier rhs)
     #`(define-syntax #,(in-for-clause-space #'id)
         rhs)]))
