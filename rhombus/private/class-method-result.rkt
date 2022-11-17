#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest/syntax-local
                     "srcloc.rkt"
                     "tag.rkt")
         (only-in "annotation.rkt" ::)
         (submod "annotation.rkt" for-class)
         "static-info.rkt"
         "call-result-key.rkt")

(provide define-method-result-syntax)

(begin-for-syntax
  (provide (struct-out method-result)
           method-result-ref
           syntax-local-method-result))

(begin-for-syntax
  (struct method-result (predicate-expr static-infos))
  (define (method-result-ref v)
    (and (method-result? v) v))

  (define (syntax-local-method-result id)
    (or (syntax-local-value* id method-result-ref)
        (raise-syntax-error #f "could not get method result information" id))))

(define-syntax (define-method-result-syntax stx)
  (syntax-parse stx
    #:datum-literals (op)
    [(_ id ((op mode) ret ...) (super-result-id ...) maybe-final-id)
     #:with c::annotation (no-srcloc #`(#,group-tag ret ...))
     #:with c-parsed::annotation-form #'c.parsed
     #:do [(define super-results (map syntax-local-method-result
                                      (syntax->list #'(super-result-id ...))))]
     #:with pred (for/fold ([pred (if (free-identifier=? #'mode #'::)
                                      #'c-parsed.predicate
                                      #f)])
                           ([r (in-list super-results)]
                            #:when (method-result-predicate-expr r))
                   (define super-pred (method-result-predicate-expr r))
                   (if pred
                       #`(let ([p #,pred]
                               [pp #,super-pred])
                           (lambda (v) (and (p v) (pp v))))
                       super-pred))
     #:with (static-info ...) #'c-parsed.static-infos
     #:with ((super-static-info ...) ...) (map method-result-static-infos super-results)
     #:with all-static-infos #'(static-info ... super-static-info ... ...)
     (define def
       #`(define-syntax id (method-result #,(if (syntax-e #'pred)
                                                #'(quote-syntax pred)
                                                #'#f)
                                          (quote-syntax all-static-infos))))
     (cond
       [(and (pair? (syntax-e #'all-static-infos))
             (syntax-e #'maybe-final-id))
        #`(begin
            #,def
            (define-static-info-syntax maybe-final-id (#%call-result all-static-infos)))]
       [else def])]))
