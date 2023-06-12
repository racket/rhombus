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
         "definition.rkt"
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

(define-for-syntax (pack wrapper-id binds step-id maybe-break-id maybe-final-id finish-id static-infos data)
  (unless (identifier? wrapper-id) (raise-argument-error* 'reducer.pack rhombus-realm "Identifier" wrapper-id))
  (unless (identifier? step-id) (raise-argument-error* 'reducer.pack rhombus-realm "Identifier" step-id))
  (unless (or (not maybe-break-id) (identifier? maybe-break-id))
    (raise-argument-error* 'reducer.pack rhombus-realm "maybe(Identifier)" maybe-break-id))
  (unless (or (not maybe-final-id) (identifier? maybe-final-id))
    (raise-argument-error* 'reducer.pack rhombus-realm "maybe(Identifier)" maybe-final-id))
  (unless (identifier? finish-id) (raise-argument-error* 'reducer.pack rhombus-realm "Identifier" finish-id))
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
  (pack-term #`(parsed #,(reducer
                          #'chain-to-wrapper
                          packed-binds
                          #'chain-to-body-wrapper
                          (and maybe-break-id #'chain-to-breaker)
                          (and maybe-final-id #'chain-to-finaler)
                          #'chain-to-finisher
                          si
                          #`[#,wrapper-id #,step-id #,maybe-break-id #,maybe-final-id #,finish-id #,data]))))

(define-syntax (chain-to-wrapper stx)
  (syntax-parse stx
    [(_ [wrapper-id step-id break-id final-id finish-id data] e)
     #'(rhombus-expression (group wrapper-id data (parsed e)))]))

(define-syntax (chain-to-body-wrapper stx)
  (syntax-parse stx
    [(_ [wrapper-id step-id break-id final-id finish-id data] e)
     #'(rhombus-definition (group step-id data (parsed e)))]))

(define-syntax (chain-to-breaker stx)
  (syntax-parse stx
    [(_ [wrapper-id step-id break-id final-id finish-id data])
     #'(rhombus-expression (group break-id data))]))

(define-syntax (chain-to-finaler stx)
  (syntax-parse stx
    [(_ [wrapper-id step-id break-id final-id finish-id data])
     #'(rhombus-expression (group final-id data))]))

(define-syntax (chain-to-finisher stx)
  (syntax-parse stx
    [(_ [wrapper-id step-id break-id final-id finish-id data])
     #'(rhombus-expression (group finish-id data))]))

(define-for-syntax (unpack stx)
  (syntax-parse (unpack-term stx 'reducer_meta.unpack #f)
    [(parsed r::reducer-form)
     #`(parens (group chain-back-to-wrapper)
               (group (parens (group r.id (op =) (parsed r.init-expr))
                              ...))
               (group chain-back-to-body-wrapper)
               (group #,(and (syntax-e #'r.break-whener) #'chain-back-to-breaker))
               (group #,(and (syntax-e #'r.final-whener) #'chain-back-to-finaler))
               (group chain-back-to-finisher)
               (group #,(unpack-static-infos #'r.static-infos))
               (group (parsed (r.wrapper r.body-wrapper r.break-whener r.final-whener r.finisher r.data))))]
    [else
     (raise-arguments-error* 'reducer.unpack "not a parsed reducer form"
                             "syntax object" stx)]))

(define-syntax chain-back-to-wrapper
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (parsed group)
       [(_ (parsed (wrapper body-wrapper breaker finaler finisher data)) e)
        (values #'(wrapper data (rhombus-expression (group e))) #'())]))))

(define-syntax chain-back-to-body-wrapper
  (definition-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (parsed group)
       [(_ (parsed (wrapper body-wrapper breaker finaler finisher data)) e)
        (list #'(body-wrapper data (rhombus-expression (group e))))]))))

(define-syntax chain-back-to-breaker
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (parsed group)
       [(_ (parsed (wrapper body-wrapper breaker finaler finisher data)))
        (values #'(finisher data) #'())]))))

(define-syntax chain-back-to-finaler
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (parsed group)
       [(_ (parsed (wrapper body-wrapper breaker finaler finisher data)))
        (values #'(finaler data) #'())]))))

(define-syntax chain-back-to-finisher
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (parsed group)
       [(_ (parsed (wrapper body-wrapper breaker finaler finisher data)))
        (values #'(finisher data) #'())]))))
