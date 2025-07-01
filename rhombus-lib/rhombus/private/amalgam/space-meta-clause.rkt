#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/transformer
                     enforest/property
                     enforest/proc-name
                     "introducer.rkt"
                     "realm.rkt"
                     (for-syntax racket/base))
         "enforest.rkt")

(provide define-space-meta-clause-syntax)

(module+ for-class
  (provide (for-syntax in-space-meta-clause-space)))

(begin-for-syntax
  (provide (property-out space-meta-clause-transformer)
           :space-meta-clause
           space-meta-clause?)

  (property space-meta-clause-transformer transformer)

  (define in-space-meta-clause-space (make-interned-syntax-introducer/add 'rhombus/space_meta_clause))
  (define-syntax (space-meta-clause-quote stx)
    (syntax-case stx ()
      [(_ id) #`(quote-syntax #,((make-interned-syntax-introducer 'rhombus/space_meta_clause) #'id))]))

  (define-rhombus-transform
    #:syntax-class :space-meta-clause
    #:predicate space-meta-clause?
    #:desc "class clause"
    #:parsed-tag #:rhombus/space_meta_clause
    #:in-space in-space-meta-clause-space
    #:transformer-ref space-meta-clause-transformer-ref))

(define-syntax (define-space-meta-clause-syntax stx)
  (syntax-parse stx
    [(_ id:identifier rhs)
     #`(define-syntax #,(in-space-meta-clause-space #'id)
         rhs)]))
