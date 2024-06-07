#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/transformer
                     enforest/property
                     "introducer.rkt"
                     (for-syntax racket/base))
         "enforest.rkt")

(provide define-syntax-class-clause-syntax)

(module+ for-class
  (provide (for-syntax in-syntax-class-clause-space
                       syntax-class-clause-quote)))

(begin-for-syntax
  (provide (property-out syntax-class-clause-transformer)
           :syntax-class-clause)

  (property syntax-class-clause-transformer transformer)

  (define in-syntax-class-clause-space (make-interned-syntax-introducer/add 'rhombus/syntax_class_clause))
  (define-syntax (syntax-class-clause-quote stx)
    (syntax-case stx ()
      [(_ id) #`(quote-syntax #,((make-interned-syntax-introducer 'rhombus/syntax_class_clause) #'id))]))

  (define-rhombus-transform
    #:syntax-class :syntax-class-clause
    #:desc "syntax class clause"
    #:parsed-tag #:rhombus/syntax_class_clause
    #:in-space in-syntax-class-clause-space
    #:transformer-ref syntax-class-clause-transformer-ref))

(define-syntax (define-syntax-class-clause-syntax stx)
  (syntax-parse stx
    [(_ id:identifier rhs)
     #`(define-syntax #,(in-syntax-class-clause-space #'id)
         rhs)]))
