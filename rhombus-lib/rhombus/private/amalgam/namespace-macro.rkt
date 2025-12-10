#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     racket/treelist
                     racket/promise
                     "dotted-sequence.rkt"
                     "realm.rkt"
                     "binding-failure.rkt"
                     "annotation-failure.rkt"
                     "pack.rkt"
                     "parse.rkt"
                     "expose.rkt"
                     "namespace-export-meta.rkt"
                     (submod "namespace-export-meta.rkt" for-namespace)
                     "name-root.rkt"
                     "srcloc.rkt"
                     "pack.rkt")
         "space.rkt"
         "space-provide.rkt"
         "forwarding-sequence.rkt"
         "parens.rkt"
         "parse.rkt"
         "name-root.rkt"
         "definition.rkt"
         "sentinel-declaration.rkt"
         "export-check.rkt"
         "syntax-parameter.rkt"
         (submod "namespace.rkt" for-exports))

(provide (for-space rhombus/space
                    namespace)
         (for-syntax (for-space rhombus/namespace
                                namespace_meta)))

(begin-for-syntax
  (define-name-root namespace_meta
    #:fields
    ([Exports namespace_meta.Exports])))

(define+provide-space namespace rhombus/namespace
  #:fields
  (interleave))

(begin-for-syntax
  (define-splicing-syntax-class :rhs
    #:description "expression or block"
    #:opaque
    #:attributes (expr)
    (pattern (~seq (tag::block g))
             #:with expr #'(rhombus-expression g))
    (pattern (~seq (tag::block g ...))
             #:with expr #'(rhombus-body-at tag g ...))
    (pattern (~seq term ...+)
             #:with expr #'(rhombus-expression (group term ...))))

  (define-splicing-syntax-class :quoted-rhs
    #:description "quoted form"
    #:opaque
    #:attributes (form)
    #:datum-literals (group)
    (pattern (~seq (_::block (group (_::quotes g))))
             #:with form #'g)
    (pattern (~seq (_::block (group (_::quotes g ...))))
             #:with form #'(multi g ...))
    (pattern (~seq (group (_::quotes g)))
             #:with form #'g)
    (pattern (~seq (group (_::quotes g ...)))
             #:with form #'(multi g ...)))

  (define (check-arity e)
    #;
    (check-arity #'predicate.expr #'predicate 2 0 #f null #f #f 'function #:always? [always? #f])
    (void))

  (define (close-expr who stx-params form)
    (define g (unpack-group form #f #f))
    (unless g (raise-annotation-failure who form "Group"))
    #`(group (parsed #:rhombus/expr
                     (with-syntax-parameters #,stx-params
                       (rhombus-expression #,g)))))

  (define (close-defn who stx-params form)
    (unless (syntax? form) (raise-annotation-failure who form "Syntax"))
    (with-syntax ([(defn ...) (unpack-multi form #f #f)])
      #`(group (parsed #:rhombus/defn
                       ((with-syntax-parameters #,stx-params
                          (begin
                            (rhombus-definition defn)
                            ...)))))))

  (define (call-parse parse form user-data extras)
    (define who '|interleaved parse|)
    (define stx-params (continuation-mark-set-first
                        #f
                        syntax-parameters-key))
    (let* ([extras (hash-set extras
                             'close_expr
                             (lambda (form) (close-expr 'close_expr stx-params form)))]
           [extras (hash-set extras
                             'close_defn
                             (lambda (form) (close-defn 'close_defn stx-params form)))])
      (call-with-values
       (lambda () (parse form user-data extras))
       (case-lambda
         [(defns new-user-data)
          (unless (syntax? defns) (raise-binding-failure who "result" defns "Syntax"))
          (unless (syntax? new-user-data) (raise-binding-failure who "result" new-user-data "Syntax"))
          (values (unpack-multi defns #f #f)
                  new-user-data)]
         [args
          (apply raise-result-arity-error* who rhombus-realm 2 #f args)]))))

  (define (call-complete complete user-data extras)
    (define who '|interleaved complete|)
    (call-with-values
     (lambda () (complete user-data extras))
     (case-lambda
       [(result)
        (unless (syntax? result) (raise-binding-failure who "result" result "Syntax"))
        result]
       [args
        (apply raise-result-arity-error* who rhombus-realm 1 #f args)]))))

(define rhombus-namespacespace-clause void)

(define-defn-syntax interleave
  (definition-transformer
    (lambda (stx name-prefix effect-id)
      (syntax-parse stx
        #:datum-literals (group)
        [(_  (_::block (~alt (~optional (group #:is_clause ~! predicate::rhs))
                             (~optional (group #:parse_clause ~! parse::rhs))
                             (~optional (group #:init ~! init-data::quoted-rhs))
                             (~optional (group #:complete ~! complete::rhs))
                             (~optional (group #:name ~! (~or (~seq name::dotted-identifier-sequence)
                                                              (~seq (_::block (group name::dotted-identifier-sequence))))))
                             (~optional (group (~and defer-tail #:defer_tail) ~!)
                                        #:defaults ([defer-tail #'#f]))
                             (~optional (group (~and no-exports #:no_exports) ~!)
                                        #:defaults ([no-exports #'#f])))
                       ...
                       . clauses))
         (unless (attribute predicate)
           (raise-syntax-error #f "expected a `~is_clause` clause" stx))
         (unless (attribute parse)
           (raise-syntax-error #f "expected a `~parse_clause` clause" stx))
         (unless (attribute init-data)
           (raise-syntax-error #f "expected a `~data` clause" stx))
         (unless (attribute complete)
           (raise-syntax-error #f "expected a `~complete` clause" stx))
         (define reflect-name (if (attribute name)
                                  (build-dot-symbol #'name #:skip-dots? #f)
                                  name-prefix))
         (define intro (make-syntax-introducer #t))
         (define data #`[defer-tail no-exports #,reflect-name #,effect-id
                          clause? parse-clause complete-clause
                          name-prefix
                          #,(intro #'base-stx) scope-stx])
         #`((define-syntax (clause? stx) (predicate.expr stx #hasheq()))
            (define-syntax (parse-clause form user-data)
              (let ([extras #hasheq()])
                (call-parse parse.expr form user-data extras)))
            (define-syntax (complete-clause user-data extras) (call-complete complete.expr user-data extras))
            (rhombus-mixed-nested-forwarding-sequence
             (namespace-finish #,data) rhombus-namespacespace-clause
             (namespace-body-step [#,data init-data.form] . #,(intro #'clauses))))]))))

(define-syntax namespace-body-step
  (lambda (stx)
    (syntax-parse stx
      [(_ (_ user-data))
       #'(quote-syntax (rhombus-namespace-clause (#:user-data user-data))
                       #:local)]
      [(_ (~and data ([defer-tail no-exports reflect-name effect-id clause? . _] user-data)) . forms)
       (define is-clause? (syntax-local-value #'clause?))
       (if (or (not (syntax-e #'defer-tail))
               (ormap (lambda (e) (or (nestable-declaration? e)
                                      (is-clause? e)))
                      (syntax->list #'forms)))
           #'(namespace-body-step/to-clause-or-decl data . forms)
           #'(quote-syntax (rhombus-namespace-clause (#:user-data user-data . forms))
                           #:local))])))

(define-syntax namespace-body-step/to-clause-or-decl
  (lambda (stx)
    ;; parse the first form as a space clause, if possible, otherwise assume
    ;; an expression or definition
    (syntax-parse stx
      [(_ (~and state ([defer-tail #:no_exports . _] _)) form . rest)
       #:when (nestable-declaration? #'form)
       (syntax-parse #'form
         [(group (~literal sentinel_declaration))
          #'(namespace-body-step state . rest)]
         [else
          (raise-syntax-error #f
                              "nestable declarations not allowed in this context"
                              (respan #'form))])]
      [(_ ((~and config [defer-tail no-exports reflect-name effect-id clause? parse-clause . _]) user-data) form . rest)
       #:do [(define is-clause? (syntax-local-value #'clause?))]
       #:when (is-clause? #'form)
       (define do-parse-clause (syntax-local-value #'parse-clause))
       (define-values (ps new-user-data) (do-parse-clause #'form #'user-data))
       #`(namespace-body-step (config #,new-user-data)
                              #,@ps
                              #,@(if (and (syntax-e #'defer-tail)
                                          (pair? (syntax-e #'rest)))
                                     (list #'(group sentinel_declaration))
                                     null)
                              . rest)]
      [(_ (~and data ([defer-tail no-exports reflect-name effect-id . _] user-data)) form . rest)
       #`(rhombus-top-step
          #,(if (and (nestable-declaration? #'form)
                     (syntax-e #'defer-tail))
                #'namespace-body-step
                #'namespace-body-step/to-clause-or-decl)
          #:no-shortcut
          reflect-name
          effect-id
          (data)
          form . rest)]
      [(_ (_ user-data))
       #'(quote-syntax (rhombus-namespace-clause (#:user-data user-data))
                       #:local)])))

(define-syntax namespace-finish
  (lambda (stx)
    (syntax-parse stx
      #:datum-literals (rhombus-namespace-clause)
      [(_ [defer-tail no-exports
            reflect-name effect-id clause? parse-clause complete
            orig-name-prefix
            base-stx init-scope-stx]
          [#:ctx forward-base-ctx forward-ctx]
          exports
          [(rhombus-namespace-clause (#:user-data user-data . forms)) post-stx-param])
       (define scope-stx ((make-syntax-delta-introducer #'forward-ctx #'forward-base-ctx) #'init-scope-stx))
       (define expose (make-expose scope-stx #'base-stx))
       (define exs-ht (parse-exports-to-ht #'(combine-out . exports) expose))
       (define do-complete (syntax-local-value #'complete))
       (let* ([extras (hasheq)]
              [extras (if (syntax-e #'defer-tail)
                          (hash-set extras 'tail #'(group
                                                    (parsed
                                                     #:rhombus/defn
                                                     ((with-syntax-parameters
                                                        post-stx-param
                                                        (rhombus-nested
                                                         reflect-name
                                                         effect-id
                                                         . forms))))))
                          extras)]
              [extras (if (syntax-e #'no-exports)
                          extras
                          (hash-set extras 'exports
                                    (let loop ([exs-ht exs-ht])
                                      (define exs (delay (exports-ht->exports exs-ht)))
                                      (namespace-exports
                                       (lambda (query)
                                         (case query
                                           [(external_names)
                                            (for/treelist ([ex (in-list (force exs))])
                                              (syntax-parse ex
                                                [id:identifier ex]
                                                [(ext-id int-id . rule) #'ext-id]
                                                [_ (error "unrecognized export")]))]
                                           [(include_spaces)
                                            (for/treelist ([ex (in-list (force exs))])
                                              (syntax-parse ex
                                                [id:identifier 'all]
                                                [(ext-id int-id . rule) (rule->include-spaces #'rule)]
                                                [_ (error "unrecognized export")]))]
                                           [(exclude_spaces)
                                            (for/treelist ([ex (in-list (force exs))])
                                              (syntax-parse ex
                                                [id:identifier (treelist)]
                                                [(ext-id int-id . rule) (rule->exclude-spaces #'rule)]
                                                [_ (error "unrecognized export")]))]
                                           [(adder)
                                            (lambda (who external-id internal-id [include-spaces 'all] [exclude-spaces (treelist)])
                                              (unless (identifier? external-id)
                                                (raise-annotation-failure who external-id "Identifier"))
                                              (unless (identifier? internal-id)
                                                (raise-annotation-failure who internal-id "Identifier"))
                                              (loop (merge-export* who exs-ht external-id internal-id include-spaces exclude-spaces)))]
                                           [(declarer)
                                            (lambda (who name)
                                              (unless (identifier? name)
                                                (raise-annotation-failure who name "Identifier"))
                                              (let ([name (syntax-local-introduce name)])
                                                #`(group (parsed #:rhombus/decl/nestable
                                                                 ((namespace-export #,name #,(force exs) forward-base-ctx forward-ctx))))))]
                                           [else
                                            (raise-arguments-error 'exports "unregnized request"
                                                                   "request" query)]))))))]
              [extras (hash-set extras 'expose expose)])
         (define defns
           (do-complete #'user-data extras))
         #`(rhombus-nested
            orig-name-prefix
            effect-id
            . #,(unpack-multi defns #f #f)))])))

(define-syntax (namespace-export stx)
  (syntax-parse stx
    [(_ name fields forward-base-ctx forward-ctx)
     (register-field-check #'(forward-base-ctx forward-ctx . fields))
     #'(define-name-root name
         #:fields
         fields)]))

(define-for-syntax (rule->include-spaces rule)
  (syntax-parse rule
    [() 'all]
    [(#:only space ...) (list->treelist (syntax->datum #'(space ...)))]
    [(#:exclude space ...) 'all]
    [(#:space ([space val-id] ...) . rule)
     (define incs (rule->include-spaces #'rule))
     (cond
       [(eq? incs 'all) 'all]
       [else (treelist-append incs
                              (list->treelist (syntax->datum #'(space ...))))])]
    [else (error 'rule->include-spaces "unrecognized rule")]))

(define-for-syntax (rule->exclude-spaces rule)
  (syntax-parse rule
    [() (treelist)]
    [(#:only space ...) (treelist)]
    [(#:exclude space ...) (list->treelist (syntax->list #'(space ...)))]
    [(#:space ([space val-id] ...) . rule)
     (define spaces (for/hasheq ([space (in-list (syntax->list #'(space ...)))])
                      (syntax-e space)))
     (for/treelist ([space (in-treelist rule->include-spaces #'rule)]
                    #:unless (hash-ref spaces (syntax-e space) #f))
       space)]))

(define-for-syntax (merge-export* who exs-ht external-id internal-id include-spaces exclude-spaces)
  (cond
    [(eq? include-spaces 'all)
     (unless (treelist? exclude-spaces)
       (raise-annotation-failure who exclude-spaces "List.of(Symbol)"))
     (if (treelist-empty? exclude-spaces)
         (merge-export exs-ht external-id internal-id  #f #f)
         (merge-export exs-ht external-id internal-id '#:except (treelist->list exclude-spaces)))]
    [(treelist? include-spaces)
     (unless (treelist? exclude-spaces)
       (raise-annotation-failure who exclude-spaces "List.of(Symbol)"))
     (cond
       [(treelist-empty? include-spaces)
        exs-ht]
       [else
        (define new-exs-ht
          (merge-export exs-ht external-id internal-id '#:only (treelist->list include-spaces)))
        (if (treelist-empty? exclude-spaces)
            new-exs-ht
            (merge-export new-exs-ht external-id internal-id '#:except (treelist->list exclude-spaces)))])]
    [else
     (raise-annotation-failure who include-spaces "One.of(#'all) || List.of(Symbol)")]))
