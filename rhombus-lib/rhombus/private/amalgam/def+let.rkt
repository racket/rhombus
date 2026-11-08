#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "group.rkt"
                     "srcloc.rkt"
                     "origin.rkt")
         "definition.rkt"
         "binding.rkt"
         "parse.rkt"
         "static-info.rkt"
         "forwarding-sequence.rkt"
         (submod "equal.rkt" for-parse)
         "parens.rkt"
         "if-blocked.rkt")

(provide (for-space rhombus/defn
                    def
                    (rename-out [rhombus-let let])))

(define-for-syntax (make-def #:make-wrap-definition [make-wrap-definition (lambda (stx) values)]
                             #:check-context [check-context void]
                             #:check-bind-uses [check-bind-uses void]
                             #:let? [let? #f])
  (definition-transformer
    (lambda (stx name-prefix effect-id)
      (check-context stx)
      (define form-id (syntax-parse stx [(form-id . _) #'form-id]))
      (define wrap-definition (make-wrap-definition stx))
      (define (build g rhs)
        (build-definitions form-id
                           g rhs
                           wrap-definition
                           #:check-bind-uses check-bind-uses
                           #:stx stx
                           #:let? let?))
      (syntax-parse stx
        [(_ any ...+ _::equal rhs ...+)
         (check-multiple-equals stx)
         (build (regroup #'(any ...)) (regroup #'(rhs ...)))]
        [(_ any ...+ (~and rhs (_::block body ...)))
         (build (regroup #'(any ...)) #'rhs)]
        [(_ ... (a::alts (b::block . _) . _))
         (raise-syntax-error #f
                             "alternatives are not supported here"
                             stx
                             #'b)]))))

(define-defn-syntax def
  (make-def))

(define-defn-syntax rhombus-let
  (make-def #:make-wrap-definition (lambda (stx)
                                     (lambda (defn)
                                       (syntax-parse stx
                                         [(head . _)
                                          (relocate+reraw stx
                                                          #`(#,(relocate-id #'head #'rhombus-forward)
                                                             #:enter
                                                             #,defn))])))
            #:let? #t
            #:check-context (lambda (stx)
                              (when (eq? (syntax-local-context) 'top-level)
                                (raise-syntax-error #f
                                                    "not allowed in a top-level context"
                                                    stx)))
            #:check-bind-uses (lambda (form-id form id uses)
                                (when (for/or ([use (in-list (syntax->list uses))])
                                        (eq? (syntax-e use) '#:no_let))
                                  (raise-syntax-error #f
                                                      "pattern requires early binding of its names"
                                                      form-id
                                                      form)))))

(define-for-syntax (build-definitions form-id g-stx rhs-stx wrap-definition
                                      #:check-bind-uses [check-bind-uses void]
                                      #:stx [stx form-id]
                                      #:let? [let? #f])
  (syntax-parse g-stx
    [lhs::binding
     #:with lhs-e::binding-values-form #'lhs.parsed
     #:with rhs (rhombus-local-expand (enforest-expression-block rhs-stx))
     #:do [(define arity (syntax-e #'lhs-e.arity))]
     #:with (static-infos ...) (normalize-static-infos/values
                                arity
                                (extract-static-infos #'rhs))
     #:with (~var lhs-impl (:binding-impl arity)) #'(lhs-e.infoer-id (static-infos ...) lhs-e.data)
     #:with lhs-i::binding-info #'lhs-impl.info
     #:with (lhs-extends ...) (for/list ([bind-uses (in-list (syntax->list #'(lhs-i.bind-uses ...)))])
                                (syntax-parse bind-uses
                                  [(_ ... [#:extends extends] . _) #'extends]
                                  [_ #'#f]))
     #:with (lhs-name-id ...) #'lhs-i.name-ids
     #:with (tmp-id ...) (generate-temporaries #'lhs-i.name-ids)
     (for ([id (in-list (syntax->list #'(lhs-i.bind-id ...)))]
           [uses (in-list (syntax->list #'(lhs-i.bind-uses ...)))])
       (check-bind-uses form-id #'lhs id uses))
     (list
      #`(rhombus-forward
         #:suspend #,let?
         #,@(top-level-decls #'(lhs-i.bind-id ...))
         (lhs-i.oncer-id lhs-i.data)
         (define-values (tmp-id ...) #,(transfer-origin
                                        #'lhs.parsed
                                        #`(let-values ([(lhs-name-id ...) #,(discard-static-infos #'rhs)])
                                            (values lhs-name-id ...))))
         (lhs-i.matcher-id (tmp-id ...)
                           lhs-i.data
                           if/flattened
                           (begin)
                           (rhs-binding-failure '#,form-id (list tmp-id ...) 'lhs-i.annotation-str))
         (lhs-i.committer-id (tmp-id ...) lhs-i.evidence-ids lhs-i.data))
      (wrap-definition
       #`(begin
           #,(relocate+reraw stx #`(lhs-i.binder-id (tmp-id ...) lhs-i.evidence-ids lhs-i.data))
           (define-static-info-syntax/maybe/maybe-extension lhs-i.bind-id lhs-extends lhs-i.bind-static-info ...)
           ...
           #,@(maybe-end-def))))]))

(define-for-syntax (top-level-decls ids-stx)
  (cond
    [(eq? 'top-level (syntax-local-context))
     (list
      #`(define-syntaxes #,ids-stx (values)))]
    [else null]))

(define-for-syntax (maybe-end-def)
  (case (syntax-local-context)
    [(top-level module) '()]
    [else (list #'(define-values () (values)))]))

(define (rhs-binding-failure who vals binding-str)
  (if (and (pair? vals)
           (null? (cdr vals)))
      (raise-binding-failure who "value" (car vals) binding-str)
      (raise-bindings-failure who "values" vals binding-str)))
