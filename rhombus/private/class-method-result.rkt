#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/syntax-local
                     shrubbery/print
                     "srcloc.rkt"
                     "tag.rkt"
                     "with-syntax.rkt"
                     "annotation-string.rkt")
         (submod "annotation.rkt" for-class)
         "binding.rkt"
         "static-info.rkt"
         (submod "define-arity.rkt" for-info)
         "indirect-static-info-key.rkt"
         "call-result-key.rkt"
         "function-arity-key.rkt"
         ;; see `gen-bounce`
         "index-result-key.rkt"
         "index-key.rkt"
         "append-indirect-key.rkt"
         "if-blocked.rkt")

(provide define-method-result-syntax)

(begin-for-syntax
  (provide (struct-out method-result)
           method-result-ref
           syntax-local-method-result))

(begin-for-syntax
  (struct method-result (handler-expr predicate? annot-str static-infos arity))
  (define (method-result-ref v)
    (and (method-result? v) v))

  (define (syntax-local-method-result id)
    (or (syntax-local-value* id method-result-ref)
        (raise-syntax-error #f "could not get method result information" id))))

(define-syntax (define-method-result-syntax stx)
  (define (parse check? result-handler result-annot-str result-static-infos predicate-handler?)
    (syntax-parse stx
      #:datum-literals (op)
      [(_ id _ (super-result-id ...) maybe-final-id convert-ok? kind arity
          maybe-call-statinfo-id
          maybe-ref-statinfo-id+id
          maybe-set-statinfo-id+id
          maybe-append-statinfo-id+id)
       #:do [(define super-results
               (map syntax-local-method-result
                    (syntax->list #'(super-result-id ...))))
             (define-values (handler-stx annot-str)
               (for/fold ([handler (and check? result-handler)]
                          [annot-str (or result-annot-str annotation-any-string)])
                         ([r (in-list super-results)]
                          #:do [(define super-pred
                                  (method-result-handler-expr r))]
                          #:when super-pred
                          #:do [(define super-annot-str
                                  (method-result-annot-str r))])
                 (if handler
                     (values (if predicate-handler?
                                 #`(let ([p #,handler]
                                         [pp #,super-pred])
                                     (lambda (v)
                                       (and (p v) (pp v))))
                                 #`(let ([c #,handler]
                                         [pp #,super-pred])
                                     (lambda (v fail-k)
                                       (let ([v (c v fail-k)])
                                         (if (pp v)
                                             v
                                             (fail-k))))))
                             (annotation-string-and annot-str
                                                    super-annot-str))
                     (values super-pred super-annot-str))))]
       #:with handler handler-stx
       #:with (static-info ...) result-static-infos
       #:with ((super-static-info ...) ...) (map method-result-static-infos super-results)
       #:with all-static-infos #'(static-info ... super-static-info ... ...)
       #:with handler-id (and (syntax-e #'handler)
                              (not (identifier? #'handler))
                              ((make-syntax-introducer)
                               (datum->syntax #f (string->symbol
                                                  (format "~a-result-handler" (syntax-e #'id))))))
       (define def
         #`(begin
             #,@(if (syntax-e #'handler-id)
                    (list #'(define handler-id handler))
                    null)
             (define-syntax id (method-result #,(if (syntax-e #'handler)
                                                    (if (syntax-e #'handler-id)
                                                        #'(quote-syntax handler-id)
                                                        #'(quote-syntax handler))
                                                    #'#f)
                                              #,predicate-handler?
                                              '#,annot-str
                                              (quote-syntax all-static-infos)
                                              (quote arity)))))
       (cond
         [(or (syntax-e #'maybe-final-id)
              (syntax-e #'maybe-call-statinfo-id)
              (syntax-e #'maybe-ref-statinfo-id+id)
              (syntax-e #'maybe-set-statinfo-id+id)
              (syntax-e #'maybe-append-statinfo-id+id))
          (define (gen id)
            (if (syntax-e id)
                #`((define-static-info-syntax #,id
                     #,(if (eq? (syntax-e #'kind) 'property)
                           #`(#%call-results-at-arities ((1 all-static-infos)))
                           #`(#%call-result all-static-infos))
                     #,@(if (syntax-e #'arity)
                            #`((#%function-arity arity))
                            #'())
                     (#%indirect-static-info indirect-function-static-info)))
                #'()))
          (define (gen-bounce ind-id+id key result-key)
            (if (syntax-e ind-id+id)
                (syntax-parse ind-id+id
                  [(ind-id id)
                   #`((define-static-info-syntax ind-id
                        (#,key id)
                        #,@(if result-key
                               #`((#,result-key all-static-infos))
                               null)))])
                #'()))
          #`(begin
              #,def
              #,@(gen #'maybe-final-id)
              #,@(gen #'maybe-call-statinfo-id)
              #,@(gen-bounce #'maybe-ref-statinfo-id+id '#%index-get '#%index-result)
              #,@(gen-bounce #'maybe-set-statinfo-id+id '#%index-set #f)
              #,@(gen-bounce #'maybe-append-statinfo-id+id '#%append/checked #f))]
         [else def])]))
  (syntax-parse stx
    #:datum-literals ()
    [(_ _ () . _)
     (parse #f #'#f #f #'() #t)]
    [(_ _ (op::annotate-op ret ...) _ _ convert-ok? . _)
     #:do [(define annot #`(#,group-tag ret ...))]
     #:with c::annotation (respan annot)
     #:do [(define annot-str (shrubbery-syntax->string annot))]
     (syntax-parse #'c.parsed
       [c-parsed::annotation-predicate-form
        (parse (syntax-e #'op.check?) #'c-parsed.predicate annot-str #'c-parsed.static-infos #t)]
       [c-parsed::annotation-binding-form
        (unless (syntax-e #'convert-ok?)
          (raise-syntax-error #f
                              "declared result annotation must be a predicate annotation"
                              #'id
                              #'c))
        (with-syntax-parse ([arg-parsed::binding-form #'c-parsed.binding]
                            [arg-impl::binding-impl #'(arg-parsed.infoer-id () arg-parsed.data)]
                            [arg-info::binding-info #'arg-impl.info]
                            [((bind-id bind-use . bind-static-infos) ...) #'arg-info.bind-infos])
          (define converter #`(lambda (tmp-id fail-k)
                                (arg-info.matcher-id tmp-id
                                                     arg-info.data
                                                     if/blocked
                                                     (begin
                                                       (arg-info.committer-id tmp-id arg-info.data)
                                                       (arg-info.binder-id tmp-id arg-info.data)
                                                       (define-static-info-syntax/maybe bind-id . bind-static-infos)
                                                       ...
                                                       c-parsed.body)
                                                     (fail-k))))
          (parse (syntax-e #'op.check?) converter annot-str #'c-parsed.static-infos #f))])]))
