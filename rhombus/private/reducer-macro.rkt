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
                     (submod "syntax-class-primitive.rkt" for-syntax-class)
                     "realm.rkt"
                     "define-arity.rkt"
                     (submod "syntax-object.rkt" for-quasiquote)
                     "call-result-key.rkt"
                     (for-syntax racket/base))
         "space-provide.rkt"
         "definition.rkt"
         "expression.rkt"
         "reducer.rkt"
         "macro-macro.rkt"
         "parens.rkt"
         "parse.rkt"
         (only-in "equal.rkt"
                  [= rhombus=])
         (submod "equal.rkt" for-parse))

(provide (for-syntax (for-space rhombus/namespace
                                reducer_meta)))

(define+provide-space reducer rhombus/reducer
  #:fields
  (macro))

(begin-for-syntax
  (define-name-root reducer_meta
    #:fields
    ([pack reducer_meta.pack]
     [unpack reducer_meta.unpack]
     Parsed
     AfterPrefixParsed
     AfterInfixParsed)))

(define-operator-definition-transformer macro
  'macro
  rhombus/reducer
  #'make-reducer-prefix-operator
  #'make-reducer-infix-operator
  #'reducer-prefix+infix-operator)

(begin-for-syntax
  (define-operator-syntax-classes
    Parsed :reducer #:rhombus/reducer
    AfterPrefixParsed :prefix-op+reducer+tail
    AfterInfixParsed :infix-op+reducer+tail)

  (struct reducer-prefix+infix-operator (prefix infix)
    #:property prop:reducer-prefix-operator (lambda (self) (reducer-prefix+infix-operator-prefix self))
    #:property prop:reducer-infix-operator (lambda (self) (reducer-prefix+infix-operator-infix self))))

(define-for-syntax (wrap-parsed stx)
  #`(parsed #:rhombus/reducer #,stx))

(define-for-syntax (extract-reducer form proc)
  (syntax-parse (if (syntax? form)
                    (unpack-group form proc #f)
                    #'#f)
    [b::reducer #'b.parsed]
    [_ (raise-bad-macro-result (proc-name proc) "reducer" form)]))

(define-for-syntax (make-reducer-infix-operator name prec protocol proc assc)
  (reducer-infix-operator
   name
   prec
   protocol
   (if (eq? protocol 'macro)
       (lambda (form1 tail)
         (define-values (form new-tail)
           (tail-returner
            proc
            (syntax-parse tail
              [(head . tail) (proc (wrap-parsed form1) (pack-tail #'tail #:after #'head) #'head)])))
         (check-transformer-result (extract-reducer form proc)
                                   (unpack-tail new-tail proc #f)
                                   proc))
       (lambda (form1 form2 stx)
         (extract-reducer (proc (wrap-parsed form1) (wrap-parsed form2) stx)
                          proc)))
   assc))

(define-for-syntax (make-reducer-prefix-operator name prec protocol proc)
  (reducer-prefix-operator
   name
   prec
   protocol
   (if (eq? protocol 'macro)
       (lambda (tail)
         (define-values (form new-tail)
           (tail-returner
            proc
            (syntax-parse tail
              [(head . tail) (proc (pack-tail #'tail #:after #'head) #'head)])))
         (check-transformer-result (extract-reducer form proc)
                                   (unpack-tail new-tail proc #f)
                                   proc))
       (lambda (form stx)
         (extract-reducer (proc (wrap-parsed form) stx)
                          proc)))))

(define-for-syntax (check-syntax who s)
  (unless (syntax? s)
    (raise-argument-error* who rhombus-realm "Syntax" s)))

(define-for-syntax (check-identifier who id)
  (unless (identifier? id)
    (raise-argument-error* who rhombus-realm "Identifier" id)))

(define-for-syntax (check-maybe-identifier who maybe-id)
  (unless (or (not maybe-id) (identifier? maybe-id))
    (raise-argument-error* who rhombus-realm "maybe(Identifier)" maybe-id)))

(begin-for-syntax
  (define/arity (reducer_meta.pack wrapper-id-in
                                   binds
                                   step-id-in
                                   maybe-break-id-in
                                   maybe-final-id-in
                                   finish-id-in
                                   static-infos
                                   data)
    #:static-infos ((#%call-result #,syntax-static-infos))
    (define wrapper-id (unpack-term/maybe wrapper-id-in))
    (define step-id (unpack-term/maybe step-id-in))
    (define maybe-break-id (unpack-term/maybe maybe-break-id-in))
    (define maybe-final-id (unpack-term/maybe maybe-final-id-in))
    (define finish-id (unpack-term/maybe finish-id-in))
    (check-identifier who wrapper-id)
    (check-syntax who binds)
    (check-identifier who step-id)
    (check-maybe-identifier who maybe-break-id)
    (check-maybe-identifier who maybe-final-id)
    (check-identifier who finish-id)
    (check-syntax who static-infos)
    (check-syntax who data)
    (define packed-binds
      (syntax-parse binds
        #:datum-literals (group)
        [(_::parens (group id:identifier _::equal e ...) ...)
         #'([id (rhombus-expression (group e ...))] ...)]
        [_ (raise-arguments-error* who rhombus-realm
                                   "ill-formed accumulator bindings"
                                   "syntax object" binds)]))
    (define si (pack-static-infos who (unpack-term static-infos who #f)))
    (pack-term #`(parsed #:rhombus/reducer
                         #,(reducer
                            #'chain-to-wrapper
                            packed-binds
                            #'chain-to-body-wrapper
                            (and maybe-break-id #'chain-to-breaker)
                            (and maybe-final-id #'chain-to-finaler)
                            #'chain-to-finisher
                            si
                            #`[#,wrapper-id #,step-id #,maybe-break-id #,maybe-final-id #,finish-id #,data])))))

(define-syntax (chain-to-wrapper stx)
  (syntax-parse stx
    [(_ [wrapper-id step-id break-id final-id finish-id data] e)
     #'(rhombus-expression (group wrapper-id data (parsed #:rhombus/expr e)))]))

(define-syntax (chain-to-body-wrapper stx)
  (syntax-parse stx
    [(_ [wrapper-id step-id break-id final-id finish-id data] e)
     #'(rhombus-definition (group step-id data (parsed #:rhombus/expr e)))]))

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

(begin-for-syntax
  (define/arity (reducer_meta.unpack stx)
    #:static-infos ((#%call-result #,syntax-static-infos))
    (check-syntax who stx)
    (syntax-parse (unpack-term stx who #f)
      [(parsed #:rhombus/reducer r::reducer-form)
       #`(parens (group chain-back-to-wrapper)
                 (group (parens (group r.id rhombus= (parsed #:rhombus/expr r.init-expr))
                                ...))
                 (group chain-back-to-body-wrapper)
                 (group #,(and (syntax-e #'r.break-whener) #'chain-back-to-breaker))
                 (group #,(and (syntax-e #'r.final-whener) #'chain-back-to-finaler))
                 (group chain-back-to-finisher)
                 (group #,(unpack-static-infos who #'r.static-infos))
                 (group (parsed #:rhombus/reducer/chain
                                (r.wrapper r.body-wrapper r.break-whener r.final-whener r.finisher r.data))))]
      [_ (raise-arguments-error* who rhombus-realm
                                 "not a parsed reducer form"
                                 "syntax object" stx)])))

(define-syntax chain-back-to-wrapper
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (parsed group)
       [(_ (parsed #:rhombus/reducer/chain
                   (wrapper body-wrapper breaker finaler finisher data)) e)
        (values #'(wrapper data (rhombus-expression (group e))) #'())]))))

(define-syntax chain-back-to-body-wrapper
  (definition-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (parsed group)
       [(_ (parsed #:rhombus/reducer/chain
                   (wrapper body-wrapper breaker finaler finisher data)) e)
        (list #'(body-wrapper data (rhombus-expression (group e))))]))))

(define-syntax chain-back-to-breaker
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (parsed group)
       [(_ (parsed #:rhombus/reducer/chain
                   (wrapper body-wrapper breaker finaler finisher data)))
        (values #'(finisher data) #'())]))))

(define-syntax chain-back-to-finaler
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (parsed group)
       [(_ (parsed #:rhombus/reducer/chain
                   (wrapper body-wrapper breaker finaler finisher data)))
        (values #'(finaler data) #'())]))))

(define-syntax chain-back-to-finisher
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (parsed group)
       [(_ (parsed #:rhombus/reducer/chain
                   (wrapper body-wrapper breaker finaler finisher data)))
        (values #'(finisher data) #'())]))))
