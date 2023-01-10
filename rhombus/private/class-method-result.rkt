#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/syntax-local
                     "srcloc.rkt"
                     "tag.rkt")
         (only-in "annotation.rkt" ::)
         (submod "annotation.rkt" for-class)
         "static-info.rkt"
         "call-result-key.rkt"
         "function-arity-key.rkt")

(provide define-method-result-syntax)

(begin-for-syntax
  (provide (struct-out method-result)
           method-result-ref
           syntax-local-method-result))

(begin-for-syntax
  (struct method-result (predicate-expr static-infos arity))
  (define (method-result-ref v)
    (and (method-result? v) v))

  (define (syntax-local-method-result id)
    (or (syntax-local-value* id method-result-ref)
        (raise-syntax-error #f "could not get method result information" id))))

(define-syntax (define-method-result-syntax stx)
  (define (parse mode result-predicate result-static-infos)
    (syntax-parse stx
      #:datum-literals (op)
      [(_ id _ (super-result-id ...) maybe-final-id kind arity)
       #:do [(define super-results (map syntax-local-method-result
                                        (syntax->list #'(super-result-id ...))))]
       #:with pred (for/fold ([pred (if (and mode
                                             (free-identifier=? mode #'::))
                                        result-predicate
                                        #f)])
                             ([r (in-list super-results)]
                              #:when (method-result-predicate-expr r))
                     (define super-pred (method-result-predicate-expr r))
                     (if pred
                         #`(let ([p #,pred]
                                 [pp #,super-pred])
                             (lambda (v) (and (p v) (pp v))))
                         super-pred))
       #:with (static-info ...) result-static-infos
       #:with ((super-static-info ...) ...) (map method-result-static-infos super-results)
       #:with all-static-infos #'(static-info ... super-static-info ... ...)
       (define def
         #`(define-syntax id (method-result #,(if (syntax-e #'pred)
                                                  #'(quote-syntax pred)
                                                  #'#f)
                                            (quote-syntax all-static-infos)
                                            (quote arity))))
       (cond
         [(syntax-e #'maybe-final-id)
          #`(begin
              #,def
              (define-static-info-syntax maybe-final-id
                #,(if (eq? (syntax-e #'kind) 'property)
                      #`(#%call-results-at-arities ((1 all-static-infos)))
                      #`(#%call-result all-static-infos))
                #,@(if (syntax-e #'arity)
                       #`((#%function-arity arity))
                       #'())))]
         [else def])]))
  (syntax-parse stx
    #:datum-literals (op)
    [(_ _ () . _)
     (parse #f #'#f #'())]
    [(_ _ ((op mode) ret ...) . _)
     #:with c::annotation (no-srcloc #`(#,group-tag ret ...))
     #:with c-parsed::annotation-form #'c.parsed
     (parse #'mode #'c-parsed.predicate #'c-parsed.static-infos)]))

