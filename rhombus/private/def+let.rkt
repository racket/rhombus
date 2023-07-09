#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/name-parse
                     shrubbery/print
                     "infer-name.rkt"
                     "tag.rkt"
                     "srcloc.rkt")
         "definition.rkt"
         "binding.rkt"
         "parse.rkt"
         "implicit.rkt"
         "annotation.rkt"
         "call-result-key.rkt"
         "values-key.rkt"
         "static-info.rkt"
         "forwarding-sequence.rkt"
         (only-in "values.rkt"
                  [values rhombus-values])
         (submod "equal.rkt" for-parse)
         (only-in "equal.rkt"
                  [= rhombus=])
         "parens.rkt")

(provide (for-space rhombus/defn
                    def
                    (rename-out [rhombus-let let])))

(module+ for-define
  (provide (for-syntax build-value-definitions
                       build-values-definitions)))

(define-for-syntax (make-def #:wrap-definition [wrap-definition values]
                             #:check-context [check-context void]
                             #:check-bind-uses [check-bind-uses void])
  (definition-transformer
    (lambda (stx)
      (check-context stx)
      (syntax-parse stx
        #:datum-literals (parens group)
        [(_ ... a::equal _ ... b::equal . _)
         (raise-syntax-error #f
                             (string-append "multiple immediate equals not allowed in this group"
                                            "\n use parentheses to disambiguate")
                             stx
                             #'a
                             (list #'b))]
        [(form-id (~optional op::name) (parens g ...) (~and rhs (_::block body ...)))
         #:when (or (not (attribute op))
                    (free-identifier=? (in-binding-space #'op.name) (bind-quote rhombus-values)))
         (build-values-definitions #'form-id
                                   #'(g ...) #'rhs
                                   wrap-definition
                                   #:show-values? (attribute op)
                                   #:check-bind-uses check-bind-uses)]
        [(form-id (~optional op::name) (parens g ...) _::equal rhs ...+)
         #:when (or (not (attribute op))
                    (free-identifier=? (in-binding-space #'op.name) (bind-quote rhombus-values)))
         (build-values-definitions #'form-id
                                   #'(g ...) #`(#,group-tag rhs ...)
                                   wrap-definition
                                   #:show-values? (attribute op)
                                   #:check-bind-uses check-bind-uses)]
        [(form-id any::not-equal ...+ _::equal rhs ...+)
         #:with g-tag group-tag
         (build-value-definitions #'form-id
                                  (no-srcloc #'(g-tag any ...))
                                  #`(#,group-tag rhs ...)
                                  wrap-definition
                                  #:check-bind-uses check-bind-uses)]
        [(form-id any ...+ (~and rhs (_::block body ...)))
         #:with g-tag group-tag
         (build-value-definitions #'form-id
                                  (no-srcloc #'(g-tag any ...))
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
  (make-def #:wrap-definition (lambda (defn) #`(rhombus-forward #,defn))
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
     #:with static-infos (single-valued-static-info (extract-static-infos #'rhs))
     #:with lhs-impl::binding-impl #'(lhs-e.infoer-id static-infos lhs-e.data)
     #:with lhs-i::binding-info #'lhs-impl.info
     (for ([id (in-list (syntax->list #'(lhs-i.bind-id ...)))]
           [uses (in-list (syntax->list #'(lhs-i.bind-uses ...)))])
       (check-bind-uses form-id #'lhs id uses))
     (append
      (top-level-decls #'(lhs-i.bind-id ...))
      (list
       #'(define tmp-id (let ([lhs-i.name-id rhs])
                          lhs-i.name-id))
       #`(lhs-i.matcher-id tmp-id
                           lhs-i.data
                           flattened-if
                           (void)
                           (rhs-binding-failure '#,form-id tmp-id 'lhs-i.annotation-str))
       #`(lhs-i.committer-id tmp-id lhs-i.data)
       (wrap-definition
        #`(begin
            (lhs-i.binder-id tmp-id lhs-i.data)
            (define-static-info-syntax/maybe lhs-i.bind-id lhs-i.bind-static-info ...)
            ...))))]))

(define-for-syntax (build-values-definitions form-id gs-stx rhs-stx wrap-definition
                                             #:show-values? [show-values? #f]
                                             #:check-bind-uses [check-bind-uses void])
  (syntax-parse gs-stx
    [(lhs::binding ...)
     #:with (lhs-e::binding-form ...) #'(lhs.parsed ...)
     #:with rhs (rhombus-local-expand (enforest-expression-block rhs-stx))
     #:with (static-infos ...) (let ([si (extract-static-infos #'rhs)])
                                 (define lhss (syntax->list #'(lhs ...)))
                                 (syntax-parse (static-info-lookup si #'#%values)
                                   [(si ...)
                                    #:when (= (length (syntax->list #'(si ...)))
                                              (length lhss))
                                    (map single-valued-static-info (syntax->list #'(si ...)))]
                                   [_ (for/list ([lhs (in-list lhss)]) #'())])) 
     #:with (lhs-impl::binding-impl ...) #'((lhs-e.infoer-id static-infos lhs-e.data) ...)
     #:with (lhs-i::binding-info ...) #'(lhs-impl.info ...)
     #:with (tmp-id ...) (generate-temporaries #'(lhs-i.name-id ...))
     #:with lhs-str (let ([str (apply
                                string-append
                                (for/list ([lhs (in-list (syntax->list #'(lhs ...)))]
                                           [i (in-naturals)])
                                  (string-append
                                   (if (zero? i) "" ", ")
                                   (shrubbery-syntax->string lhs))))])
                      (if show-values?
                          (string-append "values(" str ")")
                          str))
     (for ([ids (in-list (syntax->list #'((lhs-i.bind-id ...) ...)))]
           [usess (in-list (syntax->list #'((lhs-i.bind-uses ...) ...)))])
       (for ([lhs (in-list (syntax->list #'(lhs ...)))]
             [id (in-list (syntax->list ids))]
             [uses (in-list (syntax->list usess))])
         (check-bind-uses form-id lhs id uses)))
     (append
      (top-level-decls #'(lhs-i.bind-id ... ...))
      (list
       #'(define-values (tmp-id ...) (let-values ([(lhs-i.name-id ...) rhs])
                                       (values lhs-i.name-id ...)))
       #`(begin
           (lhs-i.matcher-id tmp-id
                             lhs-i.data
                             flattened-if
                             (begin)
                             (rhs-binding-failure '#,form-id tmp-id 'lhs-str))
           ...
           (lhs-i.committer-id tmp-id lhs-i.data)
           ...)
       (wrap-definition
        #`(begin
            (lhs-i.binder-id tmp-id lhs-i.data)
            ...
            (begin
              (define-static-info-syntax/maybe lhs-i.bind-id lhs-i.bind-static-info ...)
              ...)
            ...))))]))

(define-for-syntax (top-level-decls ids-stx)
  (cond
    [(eq? 'top-level (syntax-local-context))
     (list
      #`(define-syntaxes #,ids-stx (values)))]
    [else null]))

(define-syntax (flattened-if stx)
  (syntax-parse stx
    [(_ #t success-expr _) #'success-expr]
    [(_ check-expr success-expr fail-expr)
     #'(begin
         (unless check-expr fail-expr)
         success-expr)]))

(define (rhs-binding-failure who val binding-str)
  (raise-binding-failure who "value" val binding-str))

(define-for-syntax (single-valued-static-info si)
  (static-infos-remove si #'#%values))
