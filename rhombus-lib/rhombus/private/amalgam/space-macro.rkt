#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "space-meta-macro.rkt"
                     "expose.rkt"
                     (only-in "static-info.rkt"
                              get-empty-static-infos)
                     (for-syntax racket/base))
         "provide.rkt"
         "space-provide.rkt"
         "name-root.rkt"
         "declaration.rkt"
         "space.rkt"
         "parens.rkt"
         "parse.rkt"
         "macro-macro.rkt"
         "space-clause.rkt"
         "forwarding-sequence.rkt"
         "export-check.rkt"
         (submod "space-clause-primitive.rkt" for-space-macro)
         (submod "namespace.rkt" for-exports)
         (submod "meta.rkt" for-bridge)
         "sentinel-declaration.rkt")

(provide (for-syntax
          (for-spaces (rhombus/namespace
                       rhombus/space)
                      space_meta_clause)
          (for-spaces (rhombus/annot)
                       SpaceMeta)))

(define+provide-space space rhombus/space
  #:fields
  (enforest
   transform))

(define+provide-space space_clause rhombus/space_clause
  #:fields
  ())

(define-decl-syntax enforest
  (declaration-transformer
    (lambda (stx)
      (syntax-parse stx
        #:datum-literals (group)
        [(_  name:identifier (_::block . clauses))
         (define data #`[#,stx #,(syntax-local-introduce #'base-stx) scope-stx
                         name enforest-meta define-operator-definition-transformer])
         #`((rhombus-mixed-nested-forwarding-sequence
             (enforest-finish #,data) rhombus-space-clause
             (enforest-body-step . #,(syntax-local-introduce #'clauses))))]))))

(define-decl-syntax transform
  (declaration-transformer
    (lambda (stx)
      (syntax-parse stx
        #:datum-literals (group)
        [(_  name:identifier (_::block . clauses))
         (define data #`[#,stx #,(syntax-local-introduce #'base-stx) scope-stx
                         name transform-meta define-identifier-syntax-definition-transformer*])
         #`((rhombus-mixed-nested-forwarding-sequence
             (enforest-finish #,data) rhombus-space-clause
             (enforest-body-step . #,(syntax-local-introduce #'clauses))))]))))

(define-syntax enforest-body-step
  (lambda (stx)
    (syntax-parse stx
      [(_) #'(begin)]
      [(_ . forms)
       ;; Like `class-body-step` in "class-step.rkt"
       (if (ormap (lambda (e) (or (nestable-declaration? e)
                                  (space-clause? e)))
                  (syntax->list #'forms))
           #'(enforest-body-step/to-clause-or-decl . forms)
           #'(quote-syntax (rhombus-space-clause (#:post-forms ((rhombus-nested
                                                                 #f
                                                                 . forms))))
                           #:local))])))

(define-syntax enforest-body-step/to-clause-or-decl
  (lambda (stx)
    ;; parse the first form as a space clause, if possible, otherwise assume
    ;; an expression or definition
    (syntax-parse stx
      [(_ form . rest)
       #:with clause::space-clause (syntax-local-introduce #'form)
       (syntax-parse (syntax-local-introduce #'clause.parsed)
         #:datum-literals (group parsed)
         [((group (parsed #:rhombus/space_clause p)) ...)
          #`(begin p ... (enforest-body-step . rest))]
         [(form ...)
          #`(enforest-body-step form ...  (group sentinel_declaration) . rest)])]
      [(_ form . rest)
       #`(rhombus-top-step
          #,(if (nestable-declaration? #'form)
                #'enforest-body-step
                #'enforest-body-step/to-clause-or-decl)
          #f
          #f
          ()
          form . rest)]
      [(_) #'(begin)])))

(define-syntax enforest-finish
  (lambda (stx)
    (syntax-parse stx
      [(_ [orig-stx base-stx init-scope-stx
           name
           enforest-meta
           define-operator-definition-transformer]
          [#:ctx forward-base-ctx forward-ctx]
          exports
          [option stx-params]
          ...)
       #:with scope-stx ((make-syntax-delta-introducer #'forward-ctx #'forward-base-ctx) #'init-scope-stx)
       (define options (parse-space-clause-options #'orig-stx #'(option ...)))
       (define space-path-name (hash-ref options '#:space_path #f))
       (unless space-path-name
         (raise-syntax-error #f "space path must be declared" #'orig-stx))
       (define meta-namespace (hash-ref options '#:meta_namespace #f))
       (unless meta-namespace
         (raise-syntax-error #f "meta namespace must be declared" #'orig-stx))
       (define define-macro (hash-ref options '#:export_macro #'#f))
       (define macro-kws (hash-ref options '#:export_macro_keywords #'()))
       (define define-bridge (hash-ref options '#:export_bridge #'#f))
       (define private-kws (hash-ref options '#:private #hasheq()))
       (define post-forms (hash-ref options '#:post-forms null))
       (define exs (parse-exports #'(combine-out . exports) (make-expose #'scope-stx #'base-stx)))
       (check-distinct-exports (exports->names exs) define-macro define-bridge #'orig-stx)
       (register-field-check #`(base-ctx scope-ctx . #,exs))
       ;; The expansion here mostly sets up a bridge. The interesting
       ;; part is generated by `enforest-meta` at the end.
       (with-syntax ([(extra-kw ...) macro-kws])
         #`(begin
             (define-space-syntax name
               (space-syntax #,space-path-name))
             (define-name-root name
               #:fields
               (#,@(filter-missing-or-private
                    private-kws
                    #`([#,define-macro #,define-macro #:export_macro]
                       [#,define-bridge #,define-bridge #:export_bridge]))
                . #,exs))
             (maybe-skip
              #,define-macro
              (define-operator-definition-transformer #,define-macro
                'macro
                #,space-path-name
                #:extra ([extra-kw get-empty-static-infos value] ...)
                #'make-prefix-operator
                #'make-infix-operator
                #'make-prefix+infix-operator))
             (maybe-skip
              #,define-bridge
              (define-syntax #,define-bridge
                (make-bridge-definer '#,space-path-name)))
             (begin-for-syntax
               (enforest-meta
                #,(car meta-namespace)
                base-stx scope-stx
                [name #,space-path-name
                      make-prefix-operator make-infix-operator make-prefix+infix-operator
                      (extra-kw ...)]
                #,(cdr meta-namespace)))
             #,@post-forms))])))

(define-syntax-rule (define-identifier-syntax-definition-transformer* _define-macro
                      protocol
                      space-path-name
                      #:extra extras
                      make-prefix-operator
                      ignored-make-infix-operator
                      ignored-new-prefix+infix-operator)
  (define-identifier-syntax-definition-transformer _define-macro
    space-path-name
    #:extra extras
    make-prefix-operator))

(define-syntax (maybe-skip stx)
  (syntax-parse stx
    [(_ #f . _) #'(begin)]
    [(_ _ def) #'def]))

(define-for-syntax (filter-missing-or-private private-kws flds)
  (for/list ([fld (in-list (syntax->list flds))]
             #:when (syntax-parse fld
                      [[#f . _] #f]
                      [[_ _ kw]
                       #:when (hash-ref private-kws (syntax-e #'kw) #f)
                       #f]
                      [_ #t]))
    (syntax-parse fld
      [[a b . _] #'[a b]])))

(define-for-syntax (check-distinct-exports ex-ht define-macro define-bridge orig-stx)
  (define (check-one what id)
    (when (and (syntax-e id)
               (hash-ref ex-ht (syntax-e id) #f))
      (raise-syntax-error #f
                          (format "exported name conflicts with ~s name" what)
                          orig-stx
                          (hash-ref ex-ht (syntax-e id) #f))))
  (check-one "macro definer" define-macro)
  (check-one "bridge definer" define-bridge))
