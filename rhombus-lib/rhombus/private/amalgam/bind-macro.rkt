#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/proc-name
                     enforest/transformer-result
                     "name-root.rkt"
                     "group.rkt"
                     "pack.rkt"
                     "parse.rkt"
                     "parens.rkt"
                     "static-info-pack.rkt"
                     "uses-pack.rkt"
                     "operator-syntax-class.rkt"
                     (submod "quasiquote.rkt" convert)
                     "binding.rkt"
                     "tail-returner.rkt"
                     "macro-result.rkt"
                     "realm.rkt"
                     "annotation-failure.rkt"
                     "define-arity.rkt"
                     (submod "syntax-object.rkt" for-quasiquote)
                     "call-result-key.rkt"
                     "syntax-wrap.rkt"
                     "origin.rkt"
                     "origin-check.rkt"
                     "number.rkt"
                     "to-list.rkt"
                     (only-in "dollar.rkt"
                              $)
                     (only-in "underscore.rkt"
                              [_ rhombus_])
                     (for-syntax racket/base
                                 syntax/parse/pre))
         (only-in "space.rkt" space-syntax)
         "space-provide.rkt"
         "forwarding-sequence.rkt"
         "definition.rkt"
         "expression.rkt"
         "macro-macro.rkt"
         "binding.rkt"
         "parse.rkt"
         "parens.rkt"
         (only-in (submod "function-parse.rkt" for-build)
                  :rhombus-kw-opt-binding
                  :rhombus-ret-annotation)
         "operator-compare.rkt")

(provide (for-syntax (for-space rhombus/namespace
                                bind_meta)))

(module+ for-class
  (provide (for-syntax make-binding-prefix-operator)))

(define+provide-space bind rhombus/bind
  #:fields
  (macro
   infoer
   oncer
   matcher
   committer
   binder
   forward_sequence))

(begin-for-syntax
  (define-name-root bind_meta
    #:fields
    (space
     [pack bind_meta.pack]
     [pack_values bind_meta.pack_values]
     [pack_info bind_meta.pack_info]
     [unpack bind_meta.unpack]
     [unpack_values bind_meta.unpack_values]
     [unpack_info bind_meta.unpack_info]
     [get_arity bind_meta.get_arity]
     [get_info bind_meta.get_info]
     [get_values_info bind_meta.get_values_info]
     [is_immediate bind_meta.is_immediate]
     [relative_precedence bind_meta.relative_precedence]
     [ends_parse bind_meta.ends_parse]
     Parsed
     AfterPrefixParsed
     AfterInfixParsed
     NameStart
     Argument
     Result)))

(define-operator-definition-transformer macro
  'macro
  rhombus/bind
  #'make-binding-prefix-operator
  #'make-binding-infix-operator
  #'binding-prefix+infix-operator)

(begin-for-syntax
  (define-operator-syntax-classes
    Parsed :binding #:rhombus/bind
    NameStart in-binding-space
    AfterPrefixParsed :prefix-op+binding+tail
    AfterInfixParsed :infix-op+binding+tail)

  (define-syntax-class-syntax Argument
    (make-syntax-class #':rhombus-kw-opt-binding
                       #:kind 'group
                       #:fields #'((parsed #f parsed 0 (unpack-parsed* '#:rhombus/bind) stx)
                                   (maybe_keyword #f maybe_keyword 0 unpack-maybe-term* stx)
                                   (maybe_expr #f maybe_expr 0 unpack-maybe-group* stx))))

  (define-syntax-class-syntax Result
    (make-syntax-class #':rhombus-ret-annotation
                       #:kind 'term
                       #:splicing? #t
                       #:fields #'((count #f count 0 unpack-element* stx)
                                   (maybe_converter #f maybe_converter 0 unpack-term* stx)
                                   (static_info #f static_info 0 unpack-term* stx)
                                   (annotation_string #f annotation_string 0 unpack-element* stx))))

  (define-syntax-class :unpacked-evidence-id-tree
    #:attributes (packed)
    #:datum-literals (group)
    (pattern (group id:identifier)
             #:with packed #'id)
    (pattern (group (_::parens t::unpacked-evidence-id-tree ...))
             #:with packed #'(t.packed ...))))

(define-for-syntax space
  (space-syntax rhombus/bind))

(define-for-syntax (check-syntax who s)
  (unless (syntax*? s)
    (raise-annotation-failure who s "Syntax")))

(begin-for-syntax
  (define/arity (bind_meta.unpack stx)
    #:static-infos ((#%call-result #,(get-syntax-static-infos)))
    (check-syntax who stx)
    (syntax-parse (unpack-term stx who #f)
      #:datum-literals (parsed)
      [(parsed #:rhombus/bind b::binding-form)
       (pack-term
        #'(parens (group chain-to-infoer)
                  (group (parsed #:rhombus/bind/chain (b.infoer-id b.data)))))]))

  (define/arity (bind_meta.unpack_values stx)
    #:static-infos ((#%call-result #,(get-syntax-static-infos)))
    (check-syntax who stx)
    (syntax-parse (unpack-term stx who #f)
      #:datum-literals (parsed)
      [(parsed #:rhombus/bind b::binding-values-form)
       (pack-term
        #`(parens #,(unpack-group (pack-tail #'b.bind-form) #f #f)
                  (group b.arity)
                  (group chain-to-values-infoer)
                  (group (parsed #:rhombus/bind/chain/values (b.infoer-id b.arity b.data)))))]))

  (define/arity (bind_meta.unpack_info stx)
    #:static-infos ((#%call-result #,(get-syntax-static-infos)))
    (check-syntax who stx)
    (syntax-parse (unpack-term stx who #f)
      #:datum-literals (parsed)
      [(parsed #:rhombus/bind/info b::binding-info)
       #:with (unpacked-uses ...) (map (lambda (v) (unpack-uses who v))
                                       (syntax->list #'(b.bind-uses ...)))
       #:with (unpacked-static-infos ...) (map (lambda (v) (unpack-static-infos who v))
                                               (syntax->list #'((b.bind-static-info ...) ...)))
       (pack-term
        #`(parens (group b.annotation-str)
                  (group #,(or (attribute b.name-id)
                               (with-syntax ([(name-id ...) #'b.name-ids])
                                 #'(parens (group name-id) ...))))
                  (group #,(unpack-static-infos who #'b.static-infos))
                  (group (parens (group (parens (group b.bind-id)
                                                (group unpacked-uses)
                                                (group unpacked-static-infos)))
                                 ...))
                  (group chain-to-oncer)
                  (group chain-to-matcher)
                  #,(unpack-evidence-tree #'b.evidence-ids)
                  (group chain-to-committer)
                  (group chain-to-binder)
                  (group (parsed #:rhombus/bind/info/chain
                                 (b.oncer-id b.matcher-id b.committer-id b.binder-id b.data)))))]))

  (define/arity (bind_meta.pack stx #:track [components-in null])
    #:static-infos ((#%call-result #,(get-syntax-static-infos)))
    (check-syntax who stx)
    (define components (check-origins who components-in))
    (syntax-parse (unpack-term stx who #f)
      #:literals (chain-to-infoer)
      #:datum-literals (group parsed)
      [(~or* (_::parens (group chain-to-infoer) ; shortcut: remove indirection
                        (group (parsed #:rhombus/bind/chain (infoer-id data))))
             (_::parens (group infoer-id:identifier)
                        (group data)))
       (pack-term #`(parsed #:rhombus/bind #,(transfer-origins
                                              components
                                              (binding-form #'infoer-id
                                                            #'data))))]
      [_ (raise-arguments-error* who rhombus-realm
                                 "ill-formed unpacked single-value binding"
                                 "syntax object" stx)]))

  (define/arity (bind_meta.pack_values stx #:track [components-in null])
    #:static-infos ((#%call-result #,(get-syntax-static-infos)))
    (check-syntax who stx)
    (define components (check-origins who components-in))
    (syntax-parse (unpack-term stx who #f)
      #:literals (chain-to-values-infoer)
      #:datum-literals (group parsed)
      [(~or* (_::parens bind-form
                        (group arity:exact-nonnegative-integer)
                        (group chain-to-values-infoer) ; shortcut: remove indirection
                        (group (parsed #:rhombus/bind/chain/values (infoer-id saved-arity data))))
             (_::parens bind-form
                        (group arity:exact-nonnegative-integer)
                        (group infoer-id:identifier)
                        (group data)))
       (pack-term #`(parsed #:rhombus/bind #,(transfer-origins
                                              components
                                              (binding-values-form (unpack-tail #'bind-form who #f)
                                                                   #'arity
                                                                   #'infoer-id
                                                                   #'data))))]
      [_ (raise-arguments-error* who rhombus-realm
                                 "ill-formed unpacked multiple-value binding"
                                 "syntax object" stx)]))

  (define/arity (bind_meta.pack_info stx)
    #:static-infos ((#%call-result #,(get-syntax-static-infos)))
    (pack-term #`(parsed #:rhombus/bind/info #,(pack-info who stx))))

  (define/arity (bind_meta.get_arity stx)
    #:static-infos ((#%call-result #,(get-int-static-infos)))
    (check-syntax who stx)
    (syntax-parse (unpack-term stx who #f)
      #:datum-literals (parsed)
      [(parsed #:rhombus/bind b) (extract-binding-arity #'b)]
      [_ (raise-arguments-error* who rhombus-realm
                                 "not a parsed binding"
                                 "syntax object" stx)]))

  (define/arity (bind_meta.get_info stx unpacked-static-infos)
    #:static-infos ((#%call-result #,(get-syntax-static-infos)))
    (check-syntax who stx)
    (check-syntax who unpacked-static-infos)
    (syntax-parse (unpack-term stx who #f)
      #:datum-literals (parsed)
      [(parsed #:rhombus/bind b::binding-form)
       (define static-infos
         (pack-static-infos who (unpack-term unpacked-static-infos who #f)))
       (syntax-parse #`(b.infoer-id #,static-infos b.data)
         [impl::binding-impl #'(parsed #:rhombus/bind/info impl.info)])]
      [_ (raise-arguments-error* who rhombus-realm
                                 "not a parsed single-value binding"
                                 "syntax object" stx)]))

  (define/arity (bind_meta.get_values_info stx unpacked-static-infoss-in)
    #:static-infos ((#%call-result #,(get-syntax-static-infos)))
    (check-syntax who stx)
    (define unpacked-static-infoss
      (cond
        [(to-list #f unpacked-static-infoss-in)
         => (lambda (unpacked-static-infoss)
              (and (for/and ([s (in-list unpacked-static-infoss)])
                     (syntax*? s))
                   unpacked-static-infoss))]
        [else #f]))
    (unless unpacked-static-infoss
      (raise-annotation-failure who unpacked-static-infoss-in "Listable.to_list && List.of(Syntax)"))
    (syntax-parse (unpack-term stx who #f)
      #:datum-literals (parsed)
      [(parsed #:rhombus/bind b::binding-values-form)
       (define given-num (length unpacked-static-infoss))
       (define arity (syntax-e #'b.arity))
       (define static-infoss
         (for/list ([s (in-list unpacked-static-infoss)])
           (pack-static-infos who (unpack-term s who #f))))
       ;; no ill-formed unpacked static infos, but wrong number
       (unless (eqv? given-num arity)
         (raise-arguments-error* who rhombus-realm
                                 (format "binding has arity ~a, but got ~a unpacked static info~a"
                                         arity given-num (if (eqv? given-num 1) "" "s"))
                                 "binding" stx
                                 "unpacked static infos" unpacked-static-infoss))
       (syntax-parse #`(b.infoer-id (#,@static-infoss) b.data)
         [(~var impl (:binding-impl arity)) #'(parsed #:rhombus/bind/info impl.info)])]
      [_ (raise-arguments-error* who rhombus-realm
                                 "not a parsed multiple-value binding"
                                 "syntax object" stx)]))

  (define/arity (bind_meta.is_immediate stx)
    (check-syntax who stx)
    (syntax-parse stx
      #:datum-literals (parsed)
      [(parsed #:rhombus/bind/info info::binding-info)
       (free-identifier=? #'info.matcher-id #'always-succeed)]
      [_ (raise-arguments-error* who rhombus-realm
                                 "not a packed binding info"
                                 "syntax object" stx)])))

(define-defn-syntax infoer
  (definition-transformer
    (lambda (stx name-prefix effect-id)
      (syntax-parse stx
        #:datum-literals (group)
        [(form-id (_::quotes (group infoer-id:identifier . _))
                  . _)
         (list
          #`(define-syntax infoer-id
              (infoer-rhs infoer-id #,stx)))]))))

(define-for-syntax (unpack-maybe-static-infoss who stx)
  (syntax-parse stx
    [((~and static-infos ((_ ...) ...)) ...)
     #`(parens #,@(for/list ([static-info (in-list (syntax->list #'(static-infos ...)))])
                    (regroup #`(#,(unpack-static-infos who static-info)))))]
    [_
     (unpack-static-infos who stx)]))

(begin-for-syntax
  (define-syntax (infoer-rhs stx)
    (syntax-parse stx
      [(_ who orig-stx)
       (syntax-parse #'orig-stx
         #:datum-literals (group)
         [(form-id (_::quotes (group infoer-id:identifier
                                     (~and pat
                                           (_::parens maybe-infos-pattern
                                                      data-pattern))))
                   (block-tag::block body ...))
          (define-values (converted-pattern idrs sidrs vars can-be-empty?)
            (convert-pattern #`(group (op $) rhombus_ pat)))
          (with-syntax ([((id id-ref) ...) idrs]
                        [(((sid ...) sid-ref . _) ...) sidrs])
            #`(lambda (stx)
                (syntax-parse stx
                  [(form-id info data)
                   (syntax-parse (regroup #`(form-id
                                             (parens #,(regroup #`(#,(unpack-maybe-static-infoss 'who #'info)))
                                                     #,(regroup #'(data)))))
                     [#,converted-pattern
                      (let* ([id id-ref] ...)
                        (let-syntaxes ([(sid ...) sid-ref] ...)
                          (pack-info
                           'who
                           (rhombus-body-at block-tag body ...))))])])))])])))

(define-defn-syntax oncer
  (definition-transformer
    (lambda (stx name-prefix effect-id)
      (syntax-parse stx
        #:datum-literals (group)
        [(form-id (_::quotes (group builder-id:identifier . _))
                  . _)
         (list
          #`(define-syntax builder-id
              (oncer-rhs builder-id #,stx)))]))))

(begin-for-syntax
  (define-syntax (oncer-rhs stx)
    (syntax-parse stx
      [(_ who orig-stx)
       (syntax-parse #'orig-stx
         #:datum-literals (group)
         [(form-id (_::quotes (group builder-id:identifier
                                     (~and pat
                                           (_::parens data-pattern))))
                   (block-tag::block body ...))
          (define-values (converted-pattern idrs sidrs vars can-be-empty?)
            (convert-pattern #`(group (op $) rhombus_ pat)))
          (with-syntax ([((id id-ref . _) ...) idrs]
                        [(((sid ...) sid-ref . _) ...) sidrs])
            #`(lambda (stx)
                (syntax-parse stx
                  [(form-id data)
                   (syntax-parse (regroup #`(form-id
                                             (parens #,(regroup #'(data)))))
                     [#,converted-pattern
                      (let* ([id id-ref] ...)
                        (let-syntaxes ([(sid ...) sid-ref] ...)
                          (unwrap-block
                           'who
                           (rhombus-body-at block-tag body ...))))])])))])])))

(define-defn-syntax matcher
  (definition-transformer
    (lambda (stx name-prefix effect-id)
      (syntax-parse stx
        #:datum-literals (group)
        [(form-id (_::quotes (group builder-id:identifier . _))
                  . _)
         (list
          #`(define-syntax builder-id
              (matcher-rhs builder-id #,stx)))]))))

(define-for-syntax (unpack-maybe-ids who stx)
  (syntax-parse stx
    [(id ...) #'(parens (group id) ...)]
    [_ stx]))

(begin-for-syntax
  (define-syntax (matcher-rhs stx)
    (syntax-parse stx
      [(_ who orig-stx)
       (syntax-parse #'orig-stx
         #:datum-literals (group)
         [(form-id (_::quotes (group builder-id:identifier
                                     (~and pat
                                           (_::parens maybe-arg-ids-pattern
                                                      data-pattern
                                                      IF-id-pattern
                                                      success-id-pattern
                                                      fail-id-pattern))))
                   (block-tag::block body ...))
          (define-values (converted-pattern idrs sidrs vars can-be-empty?)
            (convert-pattern #`(group (op $) rhombus_ pat)))
          (with-syntax ([((id id-ref . _) ...) idrs]
                        [(((sid ...) sid-ref . _) ...) sidrs])
            #`(lambda (stx)
                (syntax-parse stx
                  [(form-id maybe-arg-ids data IF success fail)
                   (syntax-parse (regroup #`(form-id
                                             (parens #,(regroup #`(#,(unpack-maybe-ids 'who #'maybe-arg-ids)))
                                                     #,(regroup #'(data))
                                                     #,(regroup #'(if-bridge))
                                                     #,(regroup #'((parsed #:rhombus/expr (maybe-definition success))))
                                                     ;; putting `if-bridge` in `fail-id` helps make sure it's used correctly
                                                     #,(regroup #'((parsed #:rhombus/expr (maybe-definition (if-bridge IF fail))))))))
                     [#,converted-pattern
                      (let* ([id id-ref] ...)
                        (let-syntaxes ([(sid ...) sid-ref] ...)
                          (unwrap-block
                           'who
                           (rhombus-body-at block-tag body ...))))])])))])])))

(define-for-syntax (parse-if-bridge stx)
  ;; depending on `IF`, `if-bridge` will be used in an expression
  (syntax-parse stx
    [(form-id e ... (_::alts (_::block success ...)
                             (_::block . fail-case)))
     (syntax-parse #'fail-case
       #:datum-literals (group parsed)
       #:literals (maybe-definition if-bridge)
       [((group (parsed #:rhombus/expr (maybe-definition (if-bridge IF fail)))))
        #`(IF (rhombus-expression (group e ...))
              (rhombus-body-sequence success ...)
              fail)]
       [_ (raise-syntax-error #f
                              "not the given failure form in the failure branch"
                              stx)])]))

(define-syntax if-bridge
  (expression-transformer
   (lambda (stx) (values (parse-if-bridge stx) #'()))))

(define-defn-syntax if-bridge
  (definition-transformer
    (lambda (stx name-prefix effect-id) (list (parse-if-bridge stx)))))

(define-syntax (chain-to-infoer stx)
  (syntax-parse stx
    #:datum-literals (group parsed)
    [(_ (_::parens (group up-static-infos)
                   (group (parsed #:rhombus/bind/chain (infoer-id data)))))
     #:do [(define who (syntax-e #'infoer-id))
           (define packed-static-infos (pack-static-infos who #'up-static-infos))]
     #:with impl::binding-impl #`(infoer-id #,packed-static-infos data)
     #'impl.info]))

(define-syntax (chain-to-values-infoer stx)
  (syntax-parse stx
    #:datum-literals (group parsed)
    [(_ (_::parens (group (_::parens (group up-static-infos) ...))
                   (group (parsed #:rhombus/bind/chain/values (infoer-id arity data)))))
     #:do [(define who (syntax-e #'infoer-id))
           (define packed-static-infoss (map (lambda (v) (pack-static-infos who v))
                                             (syntax->list #'(up-static-infos ...))))]
     #:with (~var impl (:binding-impl (syntax-e #'arity))) #`(infoer-id (#,@packed-static-infoss) data)
     #'impl.info]))

(define-for-syntax (parse-chain-to-oncer rhombus stx)
  (syntax-parse stx
    #:datum-literals (group parsed)
    [(_ (_::parens (group (parsed #:rhombus/bind/info/chain
                                  (oncer-id matcher-id committer-id binder-id data)))))
     #'(oncer-id data)]))

(define-syntax chain-to-oncer
  (expression-transformer
   (lambda (stx) (values (parse-chain-to-oncer #'rhombus-body stx) #'()))))

(define-defn-syntax chain-to-oncer
  (definition-transformer
    (lambda (stx name-prefix effect-id) (list (parse-chain-to-oncer #'rhombus-body-sequence stx)))))

(define-for-syntax (parse-chain-to-matcher rhombus stx)
  ;; depends on `IF` like `if-bridge` does
  (syntax-parse stx
    #:datum-literals (group parsed)
    [(_ (_::parens (~or* (group (_::parens (group multi-arg-id:identifier) ...))
                         (group single-arg-id:identifier))
                   (group (parsed #:rhombus/bind/info/chain
                                  (oncer-id matcher-id committer-id binder-id data)))
                   (group if-id)
                   (~and success-g (group . _))
                   (~and fail-g (group . _))))
     (syntax-parse #'fail-g
       #:datum-literals (group parsed)
       #:literals (maybe-definition if-bridge)
       [(group (parsed #:rhombus/expr (maybe-definition (if-bridge IF fail))))
        #:with rhombus rhombus
        (syntax-parse #'success-g
          #:datum-literals (group)
          [(group (_::block g ...))
           #'(matcher-id (~? (multi-arg-id ...) single-arg-id) data IF (rhombus g ...) fail)]
          [_
           #'(matcher-id (~? (multi-arg-id ...) single-arg-id) data IF (rhombus success-g) fail)])]
       [_
        #:with s-expr (syntax-parse #'success-g
                        #:datum-literals (group)
                        [(group (tag::block g ...))
                         #'(rhombus-body-at tag g ...)]
                        [_
                         #'(rhombus-expression success-g)])
        #'(matcher-id (~? (multi-arg-id ...) single-arg-id) data
                      if-via-rhombus
                      s-expr
                      (if-reverse-bridge if-id fail-g))])]))

(define-syntax chain-to-matcher
  (expression-transformer
   (lambda (stx) (values (parse-chain-to-matcher #'rhombus-body stx) #'()))))

(define-defn-syntax chain-to-matcher
  (definition-transformer
    (lambda (stx name-prefix effect-id) (list (parse-chain-to-matcher #'rhombus-body-sequence stx)))))

(define-syntax (if-via-rhombus stx)
  (syntax-parse stx
    #:literals (if-reverse-bridge)
    [(_ tst thn (if-reverse-bridge if-id els))
     #'(rhombus-expression (group if-id (parsed #:rhombus/expr tst)
                                  (alts (block (group (parsed #:rhombus/expr (let () thn))))
                                        (block els))))]
    [_
     (raise-syntax-error #f "misuse of binding conditional" stx)]))

(define-syntax (if-reverse-bridge stx)
  (raise-syntax-error #f "misuse of binding conditional" stx))

(define-for-syntax (parse-chain-to-committer rhombus stx)
  (syntax-parse stx
    #:datum-literals (parsed group)
    [(_ (_::parens (~or* (group (_::parens (group multi-arg-id:identifier) ...))
                         (group single-arg-id:identifier))
                   e::unpacked-evidence-id-tree
                   (group (parsed #:rhombus/bind/info/chain
                                  (oncer-id matcher-id committer-id binder-id data)))))
     #`(committer-id (~? (multi-arg-id ...) single-arg-id) e.packed data)]))

(define-syntax chain-to-committer
  (expression-transformer
   (lambda (stx) (values (parse-chain-to-committer #'rhombus-body stx) #'()))))

(define-defn-syntax chain-to-committer
  (definition-transformer
    (lambda (stx name-prefix effect-id) (list (parse-chain-to-committer #'rhombus-body-sequence stx)))))

(define-for-syntax (parse-chain-to-binder rhombus stx)
  (syntax-parse stx
    #:datum-literals (parsed group)
    [(_ (_::parens (~or* (group (_::parens (group multi-arg-id:identifier) ...))
                         (group single-arg-id:identifier))
                   e::unpacked-evidence-id-tree
                   (group (parsed #:rhombus/bind/info/chain
                                  (oncer-id matcher-id committer-id binder-id data)))))
     #`(binder-id (~? (multi-arg-id ...) single-arg-id) e.packed data)]))

(define-syntax chain-to-binder
  (expression-transformer
   (lambda (stx) (values (parse-chain-to-binder #'rhombus-body stx) #'()))))

(define-defn-syntax chain-to-binder
  (definition-transformer
    (lambda (stx name-prefix effect-id) (list (parse-chain-to-binder #'rhombus-body-sequence stx)))))

(define-for-syntax binder-or-committer
  (definition-transformer
    (lambda (stx name-prefix effect-id)
      (syntax-parse stx
        #:datum-literals (group)
        [(form-id (_::quotes (group builder-id:identifier
                                    . _))
                  . _)
         (list
          #`(define-syntax builder-id
              (binder-or-committer-rhs builder-id #,stx)))]))))

(define-for-syntax (pack-info who stx)
  (check-syntax who stx)
  (let loop ([stx (unpack-term stx who #f)])
    (syntax-parse stx
      #:datum-literals (parsed group)
      #:literals (chain-to-oncer
                  chain-to-matcher
                  chain-to-committer
                  chain-to-binder)
      [(_::parens name-str-g
                  name-id-or-ids-g
                  static-infos-g
                  bind-ids-g
                  (group chain-to-oncer)
                  (group chain-to-matcher)
                  evidence-ids-g
                  (group chain-to-committer)
                  (group chain-to-binder)
                  (group (parsed #:rhombus/bind/info/chain
                                 (orig-oncer-id orig-matcher-id orig-committer-id orig-binder-id orig-data))))
       ;; hacky: remove indirection to get back to Racket forms
       (loop #'(parens name-str-g
                       name-id-or-ids-g
                       static-infos-g
                       bind-ids-g
                       (group orig-oncer-id)
                       (group orig-matcher-id)
                       evidence-ids-g
                       (group orig-committer-id)
                       (group orig-binder-id)
                       (group orig-data)))]
      [(_::parens (group name-str:string)
                  (group (~or* single-name-id:identifier
                               (_::parens (group multi-name-id:identifier) ...)))
                  (group static-infos)
                  (group bind-ids)
                  (group oncer-id:identifier)
                  (group matcher-id:identifier)
                  e::unpacked-evidence-id-tree
                  (group committer-id:identifier)
                  (group binder-id:identifier)
                  (group data))
       #:with (_::parens (group (_::parens (group bind-id)
                                           (group bind-uses)
                                           (group bind-static-infos)))
                         ...)
       #'bind-ids
       #:with (packed-bind-uses ...) (map (lambda (v) (pack-uses who v))
                                          (syntax->list #'(bind-uses ...)))
       #:with (packed-bind-static-infos ...) (map (lambda (v) (pack-static-infos who v))
                                                  (syntax->list #'(bind-static-infos ...)))
       (binding-info #'name-str
                     #'(~? single-name-id
                           (multi-name-id ...))
                     (pack-static-infos who #'static-infos)
                     #'((bind-id packed-bind-uses . packed-bind-static-infos) ...)
                     #'oncer-id
                     #'matcher-id
                     #'e.packed
                     #'committer-id
                     #'binder-id
                     #'data)]
      [_ (raise-arguments-error* who rhombus-realm
                                 "ill-formed unpacked binding info"
                                 "syntax object" stx)])))

(define-for-syntax (unpack-evidence-tree tree)
  (syntax-parse tree
    [_:identifier (regroup #`(#,tree))]
    [(tree ...) (regroup #`((parens #,@(map unpack-evidence-tree (syntax->list #'(tree ...))))))]))

(define-defn-syntax binder binder-or-committer)
(define-defn-syntax committer binder-or-committer)

(begin-for-syntax
  (define-syntax (binder-or-committer-rhs stx)
    (syntax-parse stx
      [(_ who orig-stx)
       (syntax-parse #'orig-stx
         #:datum-literals (group)
         [(form-id (_::quotes (group builder-id:identifier
                                     (~and pat
                                           (_::parens maybe-arg-ids-pattern
                                                      evidence-pattern
                                                      data-pattern))))
                   (block-tag::block body ...))
          (define-values (converted-pattern idrs sidrs vars can-be-empty?)
            (convert-pattern #`(group (op $) rhombus_ pat)))
          (with-syntax ([((id id-ref . _) ...) idrs]
                        [(((sid ...) sid-ref . _) ...) sidrs])
            #`(lambda (stx)
                (syntax-parse stx
                  [(form-id maybe-arg-ids evidence-tree data)
                   (syntax-parse (regroup #`(form-id
                                             (parens #,(regroup #`(#,(unpack-maybe-ids 'who #'maybe-arg-ids)))
                                                     #,(unpack-evidence-tree #'evidence-tree)
                                                     #,(regroup #'(data)))))
                     [#,converted-pattern
                      (let* ([id id-ref] ...)
                        (let-syntaxes ([(sid ...) sid-ref] ...)
                          (unwrap-block
                           'who
                           (rhombus-body-at block-tag body ...))))])])))])])))

(define-for-syntax (unwrap-block who stx)
  #`(rhombus-body-sequence #,@(unpack-multi stx who #f)))

(define-for-syntax (wrap-parsed stx)
  #`(parsed #:rhombus/bind #,stx))

(define-for-syntax (extract-binding form proc)
  (syntax-parse (if (syntax*? form)
                    (unpack-group form #f #f)
                    #'#f)
    [b::binding #'b.parsed]
    [_ (raise-bad-macro-result (proc-name proc) "binding" form)]))

(define-for-syntax (make-binding-infix-operator order prec protocol proc assc)
  (binding-infix-operator
   order
   prec
   protocol
   (if (eq? protocol 'macro)
       (lambda (form1 tail)
         (define-values (form new-tail)
           (tail-returner
            proc
            (syntax-parse tail
              [(head . tail) (proc (wrap-parsed form1) (pack-tail #'tail #:after #'head) #'head)])))
         (check-transformer-result (extract-binding form proc)
                                   (unpack-tail new-tail proc #f)
                                   proc))
       (lambda (form1 form2 stx)
         (extract-binding (proc (wrap-parsed form1) (wrap-parsed form2) stx)
                          proc)))
   assc))

(define-for-syntax (make-binding-prefix-operator order prec protocol proc)
  (binding-prefix-operator
   order
   prec
   protocol
   (if (eq? protocol 'macro)
       (lambda (tail)
         (define-values (form new-tail)
           (tail-returner
            proc
            (syntax-parse tail
              [(head . tail) (proc (pack-tail #'tail #:after #'head) #'head)])))
         (check-transformer-result (extract-binding form proc)
                                   (unpack-tail new-tail proc #f)
                                   proc))
       (lambda (form stx)
         (extract-binding (proc (wrap-parsed form) stx)
                          proc)))))

(begin-for-syntax
  (define/arity (bind_meta.relative_precedence left-mode left-stx right-stx)
    (get-relative-precedence who left-mode left-stx right-stx
                             'rhombus/bind binding-relative-precedence))

  (define/arity (bind_meta.ends_parse left-mode left-stx tail)
    (ends-parse? who left-mode left-stx tail
                 'rhombus/bind
                 binding-relative-precedence
                 binding-infix-operator-ref)))

(define-defn-syntax forward_sequence
  (definition-transformer
    (lambda (stx name-prefix effect-id)
      (syntax-parse stx
        [(_ (_::block g ...))
         (list
          #'(rhombus-forward #:maybe-enter (rhombus-definition g) ...))]))))
