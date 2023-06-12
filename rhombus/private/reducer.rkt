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
           :reducer-form
           reducer
           reducer/no-break)

  (property reducer-transformer transformer)

  (define-syntax-class :reducer-form
    (pattern [wrapper:id
              (~and binds ([id:identifier init-expr] ...))
              body-wrapper:id
              break-whener
              final-whener
              finisher:id
              static-infos
              data]))

  (define (reducer wrapper binds body-wrapper break-whener final-whener finisher static-infos data)
    #`(#,wrapper #,binds #,body-wrapper #,break-whener #,final-whener #,finisher #,static-infos #,data))

  (define (reducer/no-break wrapper binds body-wrapper static-infos data)
    #`(bounce-to-wrapper
       #,binds
       bounce-to-body-wrapper
       #f
       #f
       bounce-finisher
       #,static-infos
       [#,wrapper #,body-wrapper finish #,data]))
  
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

(define-syntax (define-wrapped stx)
  (syntax-parse stx
    [(_ finish body-wrapper data expr)
     #`(define (finish)
         (body-wrapper data expr))]))

(define-syntax (bounce-to-wrapper stx)
  (syntax-parse stx
    [(_ [wrapper body-wrapper finish data] e)
     #'(wrapper data e)]))

(define-syntax (bounce-to-body-wrapper stx)
  (syntax-parse stx
    [(_ [wrapper body-wrapper finish data] e)
     #'(define (finish) (body-wrapper data e))]))

(define-syntax (bounce-finisher stx)
  (syntax-parse stx
    [(_ [wrapper body-wrapper finish data])
     #'(finish)]))
