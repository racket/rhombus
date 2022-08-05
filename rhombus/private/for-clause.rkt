#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest/transformer
                     enforest/property
                     enforest/proc-name
                     "name-path-op.rkt"
                     "introducer.rkt"
                     "realm.rkt")
         "name-root-ref.rkt")

(provide define-for-clause-syntax)

(module+ for-class
  (provide (for-syntax in-for-clause-space)))

(begin-for-syntax
  (provide (property-out for-clause-transformer)
           :for-clause
           :for-clause-form)

  (property for-clause-transformer transformer)

  (define-syntax-class :for-clause-form
    (pattern [parsed ...]))

  (define (check-for-clause-result form proc)
    (syntax-parse (if (syntax? form) form #'#f)
      [_::for-clause-form form]
      [_ (raise-result-error (proc-name proc) rhombus-realm "For_Clause_Syntax" form)]))

  (define in-for-clause-space (make-interned-syntax-introducer/add 'rhombus/for-clause))
  
  (define-transform
    #:syntax-class :for-clause
    #:desc "for clause"
    #:in-space in-for-clause-space
    #:name-path-op name-path-op
    #:name-root-ref name-root-ref
    #:transformer-ref for-clause-transformer-ref
    #:check-result check-for-clause-result))

(define-syntax (define-for-clause-syntax stx)
  (syntax-parse stx
    [(_ id:identifier rhs)
     #`(define-syntax #,(in-for-clause-space #'id)
         rhs)]))
