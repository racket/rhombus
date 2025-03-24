#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/name-parse
                     "tag.rkt"
                     "srcloc.rkt")
         "definition.rkt"
         "binding.rkt"
         "parse.rkt"
         "static-info.rkt"
         "forwarding-sequence.rkt"
         (only-in "values.rkt"
                  [values rhombus-values])
         (submod "equal.rkt" for-parse)
         "parens.rkt"
         "if-blocked.rkt")

(provide (for-space rhombus/defn
                    def
                    (rename-out [rhombus-let let])))

(module+ for-define
  (provide (for-syntax build-value-definitions
                       build-values-definitions)))

(begin-for-syntax
  (define-syntax-class :values-id
    #:attributes (name)
    #:description "the literal `values`"
    #:opaque
    (pattern ::name
             #:when (free-identifier=? (in-binding-space #'name)
                                       (bind-quote rhombus-values)))))

(define-for-syntax (make-def #:make-wrap-definition [make-wrap-definition (lambda (stx) values)]
                             #:check-context [check-context void]
                             #:check-bind-uses [check-bind-uses void])
  (definition-transformer
    (lambda (stx name-prefix)
      (check-context stx)
      (define wrap-definition (make-wrap-definition stx))
      (syntax-parse stx
        #:datum-literals (group)
        [(form-id (~optional op::values-id) (_::parens g ...) (~and rhs (_::block body ...)))
         (build-values-definitions #'form-id
                                   #'(g ...) #'rhs
                                   wrap-definition
                                   #:check-bind-uses check-bind-uses)]
        [(form-id (~optional op::values-id) (_::parens g ...) _::equal rhs ...+)
         (build-values-definitions #'form-id
                                   #'(g ...) #`(#,group-tag rhs ...)
                                   wrap-definition
                                   #:check-bind-uses check-bind-uses)]
        [(form-id any ...+ _::equal rhs ...+)
         (check-multiple-equals stx)
         (build-value-definitions #'form-id
                                  (no-srcloc #`(#,group-tag any ...))
                                  #`(#,group-tag rhs ...)
                                  wrap-definition
                                  #:check-bind-uses check-bind-uses)]
        [(form-id any ...+ (~and rhs (_::block body ...)))
         (build-value-definitions #'form-id
                                  (no-srcloc #`(#,group-tag any ...))
                                  #'rhs
                                  wrap-definition
                                  #:check-bind-uses check-bind-uses)]
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

(define-for-syntax (build-value-definitions form-id g-stx rhs-stx wrap-definition
                                            #:check-bind-uses [check-bind-uses void])
  (syntax-parse g-stx
    [lhs::binding
     #:with lhs-e::binding-form #'lhs.parsed
     #:with rhs (rhombus-local-expand (enforest-expression-block rhs-stx))
     #:with static-infos (normalize-static-infos (extract-static-infos #'rhs))
     #:with lhs-impl::binding-impl #'(lhs-e.infoer-id static-infos lhs-e.data)
     #:with lhs-i::binding-info #'lhs-impl.info
     #:with (lhs-extends ...) (for/list ([bind-uses (syntax->list #'(lhs-i.bind-uses ...))])
                                (syntax-parse bind-uses
                                  [(_ ... [#:extends extends:identifier] . _) #'extends]
                                  [_ #'#f]))
     (for ([id (in-list (syntax->list #'(lhs-i.bind-id ...)))]
           [uses (in-list (syntax->list #'(lhs-i.bind-uses ...)))])
       (check-bind-uses form-id #'lhs id uses))
     (list
      #`(rhombus-forward
         #:suspend
         #,@(top-level-decls #'(lhs-i.bind-id ...))
         (lhs-i.oncer-id lhs-i.data)
         (define tmp-id (let ([lhs-i.name-id #,(discard-static-infos #'rhs)])
                          lhs-i.name-id))
         (lhs-i.matcher-id tmp-id
                           lhs-i.data
                           if/flattened
                           (begin)
                           (rhs-binding-failure '#,form-id tmp-id 'lhs-i.annotation-str))
         (lhs-i.committer-id tmp-id lhs-i.evidence-ids lhs-i.data))
      (wrap-definition
       #`(begin
           (lhs-i.binder-id tmp-id lhs-i.evidence-ids lhs-i.data)
           (define-static-info-syntax/maybe/maybe-extension lhs-i.bind-id lhs-extends lhs-i.bind-static-info ...)
           ...
           #,@(maybe-end-def))))]))

(define-for-syntax (build-values-definitions form-id gs-stx rhs-stx wrap-definition
                                             #:check-bind-uses [check-bind-uses void])
  (syntax-parse gs-stx
    [(lhs::binding ...)
     #:with (lhs-e::binding-form ...) #'(lhs.parsed ...)
     #:with rhs (rhombus-local-expand (enforest-expression-block rhs-stx))
     #:with (static-infos ...) (normalize-static-infos/values
                                (length (syntax->list gs-stx))
                                (extract-static-infos #'rhs))
     #:with (lhs-impl::binding-impl ...) #'((lhs-e.infoer-id static-infos lhs-e.data) ...)
     #:with (lhs-i::binding-info ...) #'(lhs-impl.info ...)
     #:with ((lhs-extends ...) ...) (for/list ([bind-usess (syntax->list #'((lhs-i.bind-uses ...) ...))])
                                      (for/list ([bind-uses (syntax->list bind-usess)])
                                        (syntax-parse bind-uses
                                          [(_ ... [#:extends extends:identifier] . _) #'extends]
                                          [_ #'#f])))
     #:with (tmp-id ...) (generate-temporaries #'(lhs-i.name-id ...))
     #:with (pos ...) (for/list ([lhs (in-list (syntax->list #'(lhs ...)))]
                                 [i (in-naturals 1)])
                        i)
     (for ([ids (in-list (syntax->list #'((lhs-i.bind-id ...) ...)))]
           [usess (in-list (syntax->list #'((lhs-i.bind-uses ...) ...)))])
       (for ([lhs (in-list (syntax->list #'(lhs ...)))]
             [id (in-list (syntax->list ids))]
             [uses (in-list (syntax->list usess))])
         (check-bind-uses form-id lhs id uses)))
     (append
      (top-level-decls #'(lhs-i.bind-id ... ...))
      (list
       #`(begin
           (lhs-i.oncer-id lhs-i.data)
           ...)
       #`(define-values (tmp-id ...) (let-values ([(lhs-i.name-id ...) #,(discard-static-infos #'rhs)])
                                       (values lhs-i.name-id ...)))
       #`(begin
           (lhs-i.matcher-id tmp-id
                             lhs-i.data
                             if/flattened
                             (begin)
                             (rhs-binding-failure '#,form-id tmp-id 'lhs-i.annotation-str
                                                  #:position 'pos))
           ...
           (lhs-i.committer-id tmp-id lhs-i.evidence-ids lhs-i.data)
           ...)
       (wrap-definition
        #`(begin
            (lhs-i.binder-id tmp-id lhs-i.evidence-ids lhs-i.data)
            ...
            (define-static-info-syntax/maybe/maybe-extension lhs-i.bind-id lhs-extends lhs-i.bind-static-info ...)
            ... ...
            #,@(maybe-end-def)))))]))

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

(define (rhs-binding-failure who val binding-str
                             #:position [pos #f])
  (define (n->th n)
    (string-append (number->string n)
                   (case (modulo n 100)
                     [(11 12 13) "th"]
                     [else (case (modulo n 10)
                             [(1) "st"]
                             [(2) "nd"]
                             [(3) "rd"]
                             [else "th"])])))
  (apply raise-binding-failure
         who "value" val binding-str
         (if pos
             (list "position" (unquoted-printing-string (n->th pos)))
             '())))
