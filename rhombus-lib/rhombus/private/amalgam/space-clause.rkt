#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/transformer
                     enforest/property
                     enforest/proc-name
                     "introducer.rkt"
                     "realm.rkt")
         "enforest.rkt")

(provide define-space-clause-syntax
         (for-syntax in-space-clause-space))

(begin-for-syntax
  (provide (property-out space-clause-transformer)
           :space-clause
           space-clause?)

  (property space-clause-transformer transformer)

  (define in-space-clause-space (make-interned-syntax-introducer/add 'rhombus/space_clause))

  (define-rhombus-transform
    #:syntax-class :space-clause
    #:predicate space-clause?
    #:desc "space transform clause"
    #:parsed-tag #:rhombus/space_clause
    #:in-space in-space-clause-space
    #:transformer-ref space-clause-transformer-ref))

(define-syntax (define-space-clause-syntax stx)
  (syntax-parse stx
    [(_ id:identifier rhs)
     #`(define-syntax #,(in-space-clause-space #'id)
         rhs)]))
