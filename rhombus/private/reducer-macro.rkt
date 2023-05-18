#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/proc-name
                     enforest/transformer-result
                     "pack.rkt"
                     "tail-returner.rkt"
                     "macro-result.rkt"
                     "realm.rkt"
                     "static-info-pack.rkt"
                     "name-root.rkt"
                     (submod "syntax-class-primitive.rkt" for-syntax-class))
         "space-provide.rkt"
         "provide.rkt"
         "expression.rkt"
         "reducer.rkt"
         "space.rkt"
         "name-root.rkt"
         "macro-macro.rkt"
         "parens.rkt"
         "parse.rkt")

(provide (for-syntax (for-space rhombus/namespace
                                reducer_meta)))

(define+provide-space reducer rhombus/reducer
  #:fields
  (macro))

(begin-for-syntax
  (define-name-root reducer_meta
    #:fields
    (pack
     unpack
     Parsed)))

(define-identifier-syntax-definition-transformer macro
  rhombus/reducer
  #'make-reducer-transformer)

(begin-for-syntax
  (define-transformer-syntax-class
    Parsed :reducer))

(define-for-syntax (make-reducer-transformer proc)
  (reducer-transformer
   (lambda (stx)
     (define form
       (syntax-parse stx
         [(head . tail) (proc (pack-tail #'tail) #'head)]))
     (extract-reducer form proc))))

(define-for-syntax (extract-reducer form proc)
  (syntax-parse (if (syntax? form)
                    (unpack-group form proc #f)
                    #'#f)
    [r::reducer #'r.parsed]
    [_ (raise-bad-macro-result (proc-name proc) "reducer" form)]))

(define-for-syntax (pack final-id binds step-id static-infos data)
  (unless (identifier? final-id) (raise-argument-error* 'reducer.pack rhombus-realm "Identifier" final-id))
  (unless (identifier? step-id) (raise-argument-error* 'reducer.pack rhombus-realm "Identifier" step-id))
  (unless (syntax? binds) (raise-argument-error* 'reducer.pack rhombus-realm "Syntax" binds))
  (define packed-binds
    (syntax-parse binds
      #:datum-literals (group op =)
      [(_::parens (group id:identifier (op =) e ...) ...)
       #`([id (rhombus-expression (group e ...))] ...)]
      [else
       (raise-arguments-error* 'reducer.pack "binding syntax object has invalid shape"
                               "syntax object" binds)]))
  (define si (pack-static-infos (unpack-term static-infos 'reducer.pack #f) 'reducer.pack))
  (unless (syntax? data) (raise-argument-error* 'reducer.pack rhombus-realm "Syntax" data))
  (pack-term #`(parsed (chain-to-final
                        #,packed-binds
                        chain-to-step
                        #,si
                        [#,final-id #,step-id #,data]))))

(define-syntax (chain-to-final stx)
  (syntax-parse stx
    [(_ [final-id step-id data] e)
     #'(rhombus-expression (group final-id data (parsed e)))]))

(define-syntax (chain-to-step stx)
  (syntax-parse stx
    [(_ [final-id step-id data] e)
     #'(rhombus-expression (group step-id data (parsed e)))]))

(define-for-syntax (unpack stx)
  (syntax-parse (unpack-term stx 'reducer_meta.unpack #f)
    [(parsed r::reducer-form)
     #`(parens (group chain-to-wrapper)
               (group (parens (group r.id (op =) (parsed r.init-expr))
                              ...))
               (group chain-to-body-wrapper)
               (group #,(unpack-static-infos #'r.static-infos))
               (group (parsed (r.wrapper r.body-wrapper r.data))))]
    [else
     (raise-arguments-error* 'reducer.unpack "not a parsed reducer form"
                             "syntax object" stx)]))

(define-syntax chain-to-wrapper
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (parsed group)
       [(_ (parsed (wrapper body-wrapper data)) e)
        (values #'(wrapper data (rhombus-expression (group e))) #'())]))))

(define-syntax chain-to-body-wrapper
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (parsed group)
       [(_ (parsed (wrapper body-wrapper data)) e)
        (values #'(body-wrapper data (rhombus-expression (group e))) #'())]))))
