#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/transformer
                     enforest/property
                     enforest/proc-name
                     "introducer.rkt"
                     "macro-result.rkt")
         "enforest.rkt")

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
              static-infos
              data]
             #:attr binds #'([id init-expr] ...)))

  (define (check-reducer-result form proc)
    (syntax-parse (if (syntax? form) form #'#f)
      [_::reducer-form form]
      [_ (raise-bad-macro-result (proc-name proc) "reducer" form)]))

  (define in-reducer-space (make-interned-syntax-introducer/add 'rhombus/reducer))
  
  (define-rhombus-transform
    #:syntax-class :reducer
    #:desc "reducer"
    #:in-space in-reducer-space
    #:transformer-ref reducer-transformer-ref
    #:check-result check-reducer-result
    #:accept-parsed? #t))

(define-syntax (define-reducer-syntax stx)
  (syntax-parse stx
    [(_ id:identifier rhs)
     #`(define-syntax #,(in-reducer-space #'id)
         rhs)]))
