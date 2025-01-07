#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/operator
                     enforest/property
                     enforest/proc-name
                     "introducer.rkt"
                     "macro-result.rkt"
                     (for-syntax racket/base))
         "enforest.rkt")

(provide define-reducer-syntax)

(module+ for-class
  (provide (for-syntax in-reducer-space)))

(begin-for-syntax
  (provide (property-out reducer-prefix-operator)
           (property-out reducer-infix-operator)
           reducer-transformer
           :reducer
           :reducer-form
           :infix-op+reducer+tail
           reducer
           reducer/no-break
           reducer-quote)

  (property reducer-prefix-operator prefix-operator)
  (property reducer-infix-operator infix-operator)

  (define (reducer-transformer proc)
    (reducer-prefix-operator #f '((default . stronger)) 'macro proc))

  (define-syntax-class :reducer-form
    #:description "reducer"
    (pattern [wrapper:id
              (~and binds ([id:identifier init-expr] ...))
              (~and pre-clause-former (~or* #f _:id))
              body-wrapper:id
              (~and break-whener (~or* #f _:id))
              (~and final-whener (~or* #f _:id))
              finisher:id
              static-infos
              data]))

  (define (reducer wrapper binds
                   pre-clause-former
                   body-wrapper break-whener final-whener finisher
                   static-infos data)
    #`(#,wrapper
       #,binds
       #,pre-clause-former
       #,body-wrapper
       #,break-whener
       #,final-whener
       #,finisher
       #,static-infos
       #,data))

  (define (reducer/no-break wrapper binds
                            body-wrapper static-infos data
                            #:pre-clause [pre-clause-former #f])
    #`(bounce-to-wrapper
       #,binds
       #,(and pre-clause-former #'bounce-to-pre-clause-former)
       bounce-to-body-wrapper
       #f
       #f
       bounce-finisher
       #,static-infos
       [#,wrapper #,pre-clause-former #,body-wrapper finish #,data]))

  (define (check-reducer-result form proc)
    (syntax-parse (if (syntax? form) form #'#f)
      [_::reducer-form form]
      [_ (raise-bad-macro-result (proc-name proc) "reducer" form)]))

  (define in-reducer-space (make-interned-syntax-introducer/add 'rhombus/reducer))
  (define-syntax (reducer-quote stx)
    (syntax-case stx ()
      [(_ id) #`(quote-syntax #,((make-interned-syntax-introducer 'rhombus/reducer) #'id))]))

  (define-rhombus-enforest
    #:syntax-class :reducer
    #:prefix-more-syntax-class :prefix-op+reducer+tail
    #:infix-more-syntax-class :infix-op+reducer+tail
    #:desc "reducer"
    #:parsed-tag #:rhombus/reducer
    #:in-space in-reducer-space
    #:prefix-operator-ref reducer-prefix-operator-ref
    #:infix-operator-ref reducer-infix-operator-ref
    #:check-result check-reducer-result))

(define-syntax (define-reducer-syntax stx)
  (syntax-parse stx
    [(_ id:identifier rhs)
     #`(define-syntax #,(in-reducer-space #'id)
         rhs)]))

(define-syntax (bounce-to-wrapper stx)
  (syntax-parse stx
    [(_ [wrapper pre-clause-former body-wrapper finish data] e)
     #'(wrapper data e)]))

(define-syntax (bounce-to-pre-clause-former stx)
  (syntax-parse stx
    [(_ [wrapper pre-clause-former body-wrapper finish data])
     #'(pre-clause-former data)]))

(define-syntax (bounce-to-body-wrapper stx)
  (syntax-parse stx
    [(_ [wrapper pre-clause-former body-wrapper finish data] e)
     #'(define (finish) (body-wrapper data e))]))

(define-syntax (bounce-finisher stx)
  (syntax-parse stx
    [(_ [wrapper pre-clause-former body-wrapper finish data])
     #'(finish)]))
