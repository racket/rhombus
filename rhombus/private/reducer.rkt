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

(provide define-reducer-syntax)

(module+ for-class
  (provide (for-syntax in-reducer-space)))

(begin-for-syntax
  (provide (property-out reducer-transformer)
           :reducer
           :reducer-form)

  (property reducer-transformer transformer)

  (define-syntax-class :reducer-form
    (pattern [wrapper
              ([id:identifier init-expr] ...)
              body-wrapper
              static-infos]
             #:attr binds #'([id init-expr] ...)))

  (define (check-reducer-result form proc)
    (syntax-parse (if (syntax? form) form #'#f)
      [_::reducer-form form]
      [_ (raise-result-error (proc-name proc) rhombus-realm "Reducer_Syntax" form)]))

  (define in-reducer-space (make-interned-syntax-introducer/add 'rhombus/reducer))
  
  (define-transform
    #:syntax-class :reducer
    #:desc "reducer"
    #:in-space in-reducer-space
    #:name-path-op name-path-op
    #:name-root-ref name-root-ref
    #:name-root-ref-root name-root-ref-root
    #:transformer-ref reducer-transformer-ref
    #:check-result check-reducer-result))

(define-syntax (define-reducer-syntax stx)
  (syntax-parse stx
    [(_ id:identifier rhs)
     #`(define-syntax #,(in-reducer-space #'id)
         rhs)]))
