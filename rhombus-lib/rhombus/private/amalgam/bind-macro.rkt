#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/proc-name
                     enforest/transformer-result
                     "name-root.rkt"
                     "pack.rkt"
                     "parse.rkt"
                     "parens.rkt"
                     "static-info-pack.rkt"
                     "uses-pack.rkt"
                     (submod "syntax-class-primitive.rkt" for-syntax-class)
                     (only-in (submod "syntax-class-primitive.rkt" for-syntax-class-syntax)
                              define-syntax-class-syntax)
                     (submod "quasiquote.rkt" convert)
                     "op-literal.rkt"
                     "binding.rkt"
                     "tail-returner.rkt"
                     "macro-result.rkt"
                     "realm.rkt"
                     "annotation-failure.rkt"
                     "define-arity.rkt"
                     (submod "syntax-object.rkt" for-quasiquote)
                     "call-result-key.rkt"
                     "syntax-wrap.rkt"
                     (for-syntax racket/base
                                 syntax/parse/pre))
         (only-in "space.rkt" space-syntax)
         "space-provide.rkt"
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
   binder))

(begin-for-syntax
  (define-name-root bind_meta
    #:fields
    (space
     [pack bind_meta.pack]
     [pack_info bind_meta.pack_info]
     [unpack bind_meta.unpack]
     [unpack_info bind_meta.unpack_info]
     [get_info bind_meta.get_info]
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
                  (group b.name-id)
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

  (define/arity (bind_meta.pack stx)
    #:static-infos ((#%call-result #,(get-syntax-static-infos)))
    (check-syntax who stx)
    (syntax-parse (unpack-term stx who #f)
      #:datum-literals (group)
      [(_::parens (group infoer-id:identifier)
                  (group data))
       (pack-term #`(parsed #:rhombus/bind #,(binding-form #'infoer-id
                                                           #'data)))]
      [_ (raise-arguments-error* who rhombus-realm
                                 "ill-formed unpacked binding"
                                 "syntax object" stx)]))

  (define/arity (bind_meta.pack_info stx)
    #:static-infos ((#%call-result #,(get-syntax-static-infos)))
    (pack-term #`(parsed #:rhombus/bind/info #,(pack-info who stx))))

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
                                 "not a parsed binding form"
                                 "syntax object" stx)]))

  (define/arity (bind_meta.is_immediate stx)
    (check-syntax who stx)
    (syntax-parse stx
      #:datum-literals (parsed)
      [(parsed #:rhombus/bind/info info::binding-info)
       (free-identifier=? #'info.matcher-id #'always-succeed)]
      [_ (raise-arguments-error* who rhombus-realm
                                 "not a packed binding info"
                                 "syntax object" stx)]))
  )

(define-defn-syntax infoer
  (definition-transformer
    (lambda (stx name-prefix)
      (syntax-parse stx
        #:datum-literals (group)
        [(form-id (_::quotes (group infoer-id:identifier . _))
                  . _)
         (list
          #`(define-syntax infoer-id
              (infoer-rhs infoer-id #,stx)))]))))

(begin-for-syntax
  (define-syntax (infoer-rhs stx)
    (syntax-parse stx
      [(_ who orig-stx)
       (syntax-parse #'orig-stx
         #:datum-literals (group)
         [(form-id (_::quotes (group infoer-id:identifier
                                     (_::parens info-pattern
                                                data-pattern)))
                   (block-tag::block body ...))
          (define-values (converted-info-pattern info-idrs info-sidrs info-vars info-can-be-empty?) (convert-pattern #'info-pattern))
          (define-values (converted-data-pattern data-idrs data-sidrs data-vars data-can-be-empty?) (convert-pattern #'data-pattern))
          (with-syntax ([((info-id info-id-ref) ...) info-idrs]
                        [(((info-sid ...) info-sid-ref . _) ...) info-sidrs]
                        [((data-id data-id-ref) ...) data-idrs]
                        [(((data-sid ...) data-sid-ref . _ ) ...) data-sidrs])
            #`(lambda (stx)
                (syntax-parse stx
                  [(_ info data)
                   (syntax-parse #`(group #,(unpack-static-infos 'who #'info))
                     [#,converted-info-pattern
                      (syntax-parse #'(group data)
                        [#,converted-data-pattern
                         (let* ([arg-id #'arg-id]
                                [info-id info-id-ref] ...
                                [data-id data-id-ref] ...)
                           (let-syntaxes ([(info-sid ...) info-sid-ref]
                                          ...
                                          [(data-sid ...) data-sid-ref]
                                          ...)
                             (pack-info
                              'who
                              (rhombus-body-at block-tag body ...))))])])])))])])))

(define-defn-syntax oncer
  (definition-transformer
    (lambda (stx name-prefix)
      (syntax-parse stx
        #:datum-literals (group)
        [(form-id (_::quotes (group builder-id:identifier . _))
                  . _)
         (list
          #`(define-syntax builder-id
              (oncer-rhs #,stx)))]))))

(begin-for-syntax
  (define-syntax (oncer-rhs stx)
    (syntax-parse stx
      [(_ orig-stx)
       (syntax-parse #'orig-stx
         #:datum-literals (group)
         [(form-id (_::quotes (group builder-id:identifier
                                     (_::parens data-pattern)))
                   (block-tag::block body ...))
          (define-values (converted-pattern idrs sidrs vars can-be-empty?) (convert-pattern #'data-pattern))
          (with-syntax ([((id id-ref . _) ...) idrs]
                        [(((sid ...) sid-ref . _) ...) sidrs])
            #`(lambda (stx)
                (syntax-parse stx
                  [(_ data)
                   (syntax-parse #'(group data)
                     [#,converted-pattern
                      (let* ([id id-ref] ...)
                        (let-syntaxes ([(sid ...) sid-ref] ...)
                          (unwrap-block
                           (rhombus-body-at block-tag body ...))))])])))])])))

(define-defn-syntax matcher
  (definition-transformer
    (lambda (stx name-prefix)
      (syntax-parse stx
        #:datum-literals (group)
        [(form-id (_::quotes (group builder-id:identifier . _))
                  . _)
         (list
          #`(define-syntax builder-id
              (matcher-rhs #,stx)))]))))

(begin-for-syntax
  (define-syntax (matcher-rhs stx)
    (syntax-parse stx
      [(_ orig-stx)
       (syntax-parse #'orig-stx
         #:datum-literals (group)
         [(form-id (_::quotes (group builder-id:identifier
                                     (_::parens (group _::$-bind arg-id:identifier)
                                                data-pattern
                                                (group _::$-bind IF-id:identifier)
                                                (group _::$-bind success-id:identifier)
                                                (group _::$-bind fail-id:identifier))))
                   (block-tag::block body ...))
          (define-values (converted-pattern idrs sidrs vars can-be-empty?) (convert-pattern #'data-pattern))
          (with-syntax ([((id id-ref . _) ...) idrs]
                        [(((sid ...) sid-ref . _) ...) sidrs])
            #`(lambda (stx)
                (syntax-parse stx
                  [(_ arg-id data IF success fail)
                   (syntax-parse #'(group data)
                     [#,converted-pattern
                      (let* ([id id-ref] ... [arg-id #'arg-id])
                        (let-syntaxes ([(sid ...) sid-ref] ...)
                          (let ([IF-id #'if-bridge])
                            (let ([success-id #'(parsed #:rhombus/expr (maybe-definition success))]
                                  ;; putting `if-bridge` in `fail-id`
                                  ;; helps make sure it's used correctly
                                  [fail-id #'(parsed #:rhombus/expr (maybe-definition (if-bridge IF fail)))])
                              (unwrap-block
                               (rhombus-body-at block-tag body ...))))))])])))])])))

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
    (lambda (stx name-prefix) (list (parse-if-bridge stx)))))

(define-for-syntax (parse-chain-to-oncer rhombus stx)
  (syntax-parse stx
    #:datum-literals (group)
    [(_ (_::parens (group (parsed #:rhombus/bind/info/chain
                                  (oncer-id matcher-id committer-id binder-id data)))))
     #'(oncer-id data)]))

(define-syntax chain-to-oncer
  (expression-transformer
   (lambda (stx) (values (parse-chain-to-oncer #'rhombus-body stx) #'()))))

(define-defn-syntax chain-to-oncer
  (definition-transformer
    (lambda (stx name-prefix) (list (parse-chain-to-oncer #'rhombus-body-sequence stx)))))

(define-for-syntax (parse-chain-to-matcher rhombus stx)
  ;; depends on `IF` like `if-bridge` does
  (syntax-parse stx
    #:datum-literals (group)
    [(_ (_::parens (group arg-id:identifier)
                   (group (parsed #:rhombus/bind/info/chain
                                  (oncer-id matcher-id committer-id binder-id data)))
                   (group if-id)
                   (group success ...)
                   (group fail ...)))
     (syntax-parse #'(fail ...)
       #:datum-literals (parsed)
       #:literals (maybe-definition if-bridge)
       [((parsed #:rhombus/expr (maybe-definition (if-bridge IF fail))))
        #:with rhombus rhombus
        (syntax-parse #'(success ...)
          [((_::block g ...))
           #'(matcher-id arg-id data IF (rhombus g ...) fail)]
          [_
           #'(matcher-id arg-id data IF (rhombus (group success ...)) fail)])]
       [_
        #:with s-expr (syntax-parse #'(success ...)
                        [((tag::block g ...))
                         #'(rhombus-body-at tag g ...)]
                        [_ #'(rhombus-expression (group success ...))])
        #'(matcher-id arg-id data
                      if-via-rhombus
                      s-expr
                      (if-reverse-bridge if-id (group fail ...)))])]))

(define-syntax chain-to-matcher
  (expression-transformer
   (lambda (stx) (values (parse-chain-to-matcher #'rhombus-body stx) #'()))))

(define-defn-syntax chain-to-matcher
  (definition-transformer
    (lambda (stx name-prefix) (list (parse-chain-to-matcher #'rhombus-body-sequence stx)))))

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
  ;; depends on `IF` like `if-bridge` does
  (syntax-parse stx
    #:datum-literals (parsed group)
    [(_ (_::parens (group arg-id:identifier)
                   e::unpacked-evidence-id-tree
                   (group (parsed #:rhombus/bind/info/chain
                                  (oncer-id matcher-id committer-id binder-id data)))))
     #:with rhombus rhombus
     #`(committer-id arg-id e.packed data)]))

(define-syntax chain-to-committer
  (expression-transformer
   (lambda (stx) (values (parse-chain-to-committer #'rhombus-body stx) #'()))))

(define-defn-syntax chain-to-committer
  (definition-transformer
    (lambda (stx name-prefix) (list (parse-chain-to-committer #'rhombus-body-sequence stx)))))

(define-for-syntax (parse-chain-to-binder rhombus stx)
  ;; depends on `IF` like `if-bridge` does
  (syntax-parse stx
    #:datum-literals (parsed group)
    [(_ (_::parens (group arg-id:identifier)
                   e::unpacked-evidence-id-tree
                   (group (parsed #:rhombus/bind/info/chain
                                  (oncer-id matcher-id committer-id binder-id data)))))
     #:with rhombus rhombus
     #`(binder-id arg-id e.packed data)]))

(define-syntax chain-to-binder
  (expression-transformer
   (lambda (stx) (values (parse-chain-to-binder #'rhombus-body stx) #'()))))

(define-defn-syntax chain-to-binder
  (definition-transformer
    (lambda (stx name-prefix) (list (parse-chain-to-binder #'rhombus-body-sequence stx)))))

(define-for-syntax binder-or-committer
  (definition-transformer
    (lambda (stx name-prefix)
      (syntax-parse stx
        #:datum-literals (group)
        [(form-id (_::quotes (group builder-id:identifier
                                    . _))
                  . _)
         (list
          #`(define-syntax builder-id
              (binder-or-committer-rhs #,stx)))]))))

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
                  name-id-g
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
                       name-id-g
                       static-infos-g
                       bind-ids-g
                       (group orig-oncer-id)
                       (group orig-matcher-id)
                       evidence-ids-g
                       (group orig-committer-id)
                       (group orig-binder-id)
                       (group orig-data)))]
      [(_::parens (group name-str:string)
                  (group name-id:identifier)
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
                     #'name-id
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
    [_:identifier #`(group #,tree)]
    [(tree ...) #`(group (parens #,@(map unpack-evidence-tree (syntax->list #'(tree ...)))))]))

(define-defn-syntax binder binder-or-committer)
(define-defn-syntax committer binder-or-committer)

(begin-for-syntax
  (define-syntax (binder-or-committer-rhs stx)
    (syntax-parse stx
      [(_ orig-stx)
       (syntax-parse #'orig-stx
         #:datum-literals (group)
         [(form-id (_::quotes (group builder-id:identifier
                                     (~and pat
                                           (_::parens (group _::$-bind arg-id:identifier)
                                                      evidence-pattern
                                                      data-pattern))))
                   (block-tag::block body ...))
          (define-values (converted-data-pattern data-idrs data-sidrs data-vars data-can-be-empty?)
            (convert-pattern #'pat))
          (with-syntax ([((data-id data-id-ref . _) ...) data-idrs]
                        [(((data-sid ...) data-sid-ref . _) ...) data-sidrs])
            #`(lambda (stx)
                (syntax-parse stx
                  [(_ arg-id evidence-tree data)
                   (syntax-parse (list 'parens
                                       (list 'group #'arg-id)
                                       (unpack-evidence-tree #'evidence-tree)
                                       (list 'group #'data))
                     [#,converted-data-pattern
                      (let* ([data-id data-id-ref] ...)
                        (let-syntaxes ([(data-sid ...) data-sid-ref] ...)
                          (unwrap-block
                           (rhombus-body-at block-tag body ...))))])])))])])))

(define-for-syntax (unwrap-block stx)
  #`(rhombus-body-sequence #,@(unpack-multi stx 'bin.binder #f)))

(define-for-syntax (wrap-parsed stx)
  #`(parsed #:rhombus/bind #,stx))

(define-for-syntax (extract-binding form proc)
  (syntax-parse (if (syntax*? form)
                    (unpack-group form proc #f)
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
