#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/proc-name
                     enforest/transformer-result
                     "srcloc.rkt"
                     "pack.rkt"
                     "static-info-pack.rkt"
                     "uses-pack.rkt"
                     (submod "syntax-class-primitive.rkt" for-syntax-class)
                     (only-in (submod "syntax-class-primitive.rkt" for-syntax-class-syntax)
                              define-syntax-class-syntax)
                     (only-in "repetition.rkt" in-repetition-space)
                     (for-syntax racket/base
                                 syntax/parse/pre)
                     "tail-returner.rkt"
                     "macro-result.rkt")
         (only-in "space.rkt" space-syntax)
         "space-provide.rkt"
         "name-root.rkt"
         "definition.rkt"
         "expression.rkt"
         "expression+definition.rkt"
         "macro-macro.rkt"
         "binding.rkt"
         (for-syntax
          "quasiquote.rkt"
          (submod "quasiquote.rkt" convert)
          "op-literal.rkt"
          "binding.rkt")
         "parse.rkt"
         "parens.rkt"
         ;; for `matcher` and `binder`:
         (for-syntax "parse.rkt")
         ;; for `bind_meta`:
         (for-syntax "name-root.rkt")
         (only-in (submod "function-parse.rkt" for-build)
                  :rhombus-kw-opt-binding
                  :rhombus-ret-annotation)
         ;; for `block` in `if-reverse-bridge`:
         (only-in "implicit.rkt" #%body))

(provide (for-syntax (for-space rhombus/namespace
                                bind_meta)))

(module+ for-class
  (provide (for-syntax make-binding-prefix-operator)))

(define+provide-space bind rhombus/bind
  #:fields
  (macro
   infoer
   matcher
   committer
   binder))

(begin-for-syntax
  (define-name-root bind_meta
    #:fields
    (space
     pack
     pack_info
     unpack
     unpack_info
     get_info
     is_immediate
     Parsed
     AfterPrefixParsed
     AfterInfixParsed
     Argument
     Result)))

(define-operator-definition-transformer macro
  'macro
  rhombus/bind
  #'make-binding-prefix-operator
  #'make-binding-infix-operator
  #'prefix+infix)

(begin-for-syntax
  (define-operator-syntax-classes
    Parsed :binding
    AfterPrefixParsed :prefix-op+binding+tail
    AfterInfixParsed :infix-op+binding+tail)

  (define-syntax-class-syntax Argument
    (make-syntax-class #':rhombus-kw-opt-binding
                       #:kind 'group
                       #:fields #'((parsed #f parsed 0 unpack-parsed*)
                                   (maybe_keyword #f maybe_keyword 0 unpack-maybe-term*)
                                   (maybe_expr #f maybe_expr 0 unpack-maybe-group*))))

  (define-syntax-class-syntax Result
    (make-syntax-class #':rhombus-ret-annotation
                       #:kind 'term
                       #:splicing? #t
                       #:fields #'((count #f count 0 unpack-element*)
                                   (maybe_converter #f maybe_converter 0 unpack-term*)
                                   (static_info #f static_info 0 unpack-term*)
                                   (annotation_string #f annotation_string 0 unpack-element*)))))

(define-for-syntax space
  (space-syntax rhombus/bind))

(begin-for-syntax
  (struct prefix+infix (prefix infix)
    #:property prop:binding-prefix-operator (lambda (self) (prefix+infix-prefix self))
    #:property prop:binding-infix-operator (lambda (self) (prefix+infix-infix self))))

(define-for-syntax (unpack stx)
  (syntax-parse (unpack-term stx 'bind_meta.unpack #f)
    [((~datum parsed) b::binding-form)
     (pack-term #'(parens (group chain-to-infoer)
                          (group (parsed (b.infoer-id b.data)))))]))

(define-for-syntax (unpack_info stx)
  (syntax-parse (unpack-term stx 'bind_meta.unpack_info #f)
    [((~datum parsed) b::binding-info)
     #:with (unpacked-uses ...) (map (lambda (v) (unpack-uses v))
                                     (syntax->list #'(b.bind-uses ...)))
     #:with (unpacked-static-infos ...) (map (lambda (v) (unpack-static-infos v))
                                             (syntax->list #'((b.bind-static-info ...) ...)))
     (pack-term
      #`(parens (group b.annotation-str)
                (group b.name-id)
                (group #,(unpack-static-infos #'b.static-infos))
                (group (parens (group (parens (group b.bind-id)
                                              (group unpacked-uses)
                                              (group unpacked-static-infos))) ...))
                (group chain-to-matcher)
                (group chain-to-committer)
                (group chain-to-binder)
                (group (parsed (b.matcher-id b.committer-id b.binder-id b.data)))))]))

(define-for-syntax (pack stx)
  (syntax-parse (unpack-term stx 'bind_meta.pack #f)
    #:datum-literals (parens group)
    [(parens (group infoer-id:identifier)
             (group data))
     (pack-term #`(parsed #,(binding-form #'infoer-id
                                          #'data)))]
    [_ (raise-syntax-error 'bind_meta.pack
                           "ill-formed unpacked binding"
                           stx)]))

(define-for-syntax (pack-info stx)
  (syntax-parse (unpack-term stx 'bind_meta.pack_info #f)
    #:datum-literals (parens group parsed)
    [(parens name-str-g
             name-id-g
             static-infos-g
             bind-ids-g
             (group (~literal chain-to-matcher))
             (group (~literal chain-to-committer))
             (group (~literal chain-to-binder))
             (group (parsed (orig-matcher-id orig-committer-id orig-binder-id orig-data))))
     ;; hacky: remove indirection to get back to Racket forms
     (pack-info #'(parens name-str-g
                          name-id-g
                          static-infos-g
                          bind-ids-g
                          (group orig-matcher-id)
                          (group orig-committer-id)
                          (group orig-binder-id)
                          (group orig-data)))]
    [(parens (group name-str:string)
             (group name-id:identifier)
             (group static-infos)
             (group bind-ids)
             (group matcher-id:identifier)
             (group committer-id:identifier)
             (group binder-id:identifier)
             (group data))
     #:with (parens (group (parens (group bind-id) (group bind-uses) (group bind-static-infos))) ...) #'bind-ids
     #:with (packed-bind-uses ...) (map (lambda (v) (pack-uses v 'bind_meta.pack))
                                        (syntax->list #'(bind-uses ...)))
     #:with (packed-bind-static-infos ...) (map (lambda (v) (pack-static-infos v 'bind_meta.pack))
                                                (syntax->list #'(bind-static-infos ...)))
     (binding-info #'name-str
                   #'name-id
                   (pack-static-infos #'static-infos 'bind_meta.pack)
                   #'((bind-id packed-bind-uses . packed-bind-static-infos) ...)
                   #'matcher-id
                   #'committer-id
                   #'binder-id
                   #'data)]
    [_ (raise-syntax-error 'bind_meta.pack_info
                           "ill-formed unpacked binding info"
                           stx)]))

(define-for-syntax (pack_info stx)
  (pack-term #`(parsed #,(pack-info stx))))

(define-for-syntax (get_info stx unpacked-static-infos)
  (syntax-parse (unpack-term stx 'bind_meta.get_info #f)
    #:datum-literals (parsed group)
    [(parsed b::binding-form)
     (define static-infos (pack-static-infos (unpack-term unpacked-static-infos 'bind_meta.get_info #f)
                                             'bind_meta.get_info))
     (syntax-parse #`(b.infoer-id #,static-infos b.data)
       [impl::binding-impl #'(parsed impl.info)])]
    [_
     (raise-argument-error 'bind_meta.get_info
                           "binding-form?"
                           stx)]))

(define-for-syntax (is_immediate stx)
  (syntax-parse (and (syntax? stx)
                     stx)
    [(parsed info::binding-info)
     (free-identifier=? #'info.matcher-id #'always-succeed)]
    [_
     (raise-argument-error 'bind_meta.is_immediate
                           "binding-info?"
                           stx)]))

(define-syntax infoer
  (definition-transformer
    (lambda (stx)
      (syntax-parse stx
        #:datum-literals (op parens group block quotes)
        [(form-id (quotes (group infoer-id:identifier . _))
                  . _)
         (list
          #`(define-syntax infoer-id
              (infoer-rhs #,stx)))]))))

(begin-for-syntax
  (define-syntax (infoer-rhs stx)
    (syntax-parse stx
      [(_ orig-stx)
       (syntax-parse #'orig-stx
         #:datum-literals (op parens group block quotes)
         [(form-id (quotes (group infoer-id:identifier
                                  (parens info-pattern
                                          data-pattern)))
                   ((~and block-tag block) body ...))
          (define-values (converted-info-pattern info-idrs info-sidrs info-vars info-can-be-empty?) (convert-pattern #'info-pattern))
          (define-values (converted-data-pattern data-idrs data-sidrs data-vars data-can-be-empty?) (convert-pattern #'data-pattern))
          (with-syntax ([((info-id info-id-ref) ...) info-idrs]
                        [(((info-sid ...) info-sid-ref) ...) info-sidrs]
                        [((data-id data-id-ref) ...) data-idrs]
                        [(((data-sid ...) data-sid-ref) ...) data-sidrs])
            #`(lambda (stx)
                (syntax-parse stx
                  [(_ info data)
                   (syntax-parse #`(group #,(unpack-static-infos #'info))
                     [#,converted-info-pattern
                      (syntax-parse #'(group data)
                        [#,converted-data-pattern
                         (let ([arg-id #'arg-id]
                               [info-id info-id-ref] ...
                               [data-id data-id-ref] ...)
                           (let-syntaxes ([(info-sid ...) info-sid-ref]
                                          ...
                                          [(data-sid ...) data-sid-ref]
                                          ...)
                             (pack-info
                              (rhombus-body-at block-tag body ...))))])])])))])])))

(define-syntax matcher
  (definition-transformer
    (lambda (stx)
      (syntax-parse stx
        #:datum-literals (op group quotes)
        [(form-id (quotes (group builder-id:identifier . _))
                  . _)
         (list
          #`(define-syntax builder-id
              (matcher-rhs #,stx)))]))))

(begin-for-syntax
  (define-syntax (matcher-rhs stx)
    (syntax-parse stx
      [(_ orig-stx)
       (syntax-parse #'orig-stx
         #:datum-literals (op parens group block quotes)
         [(form-id (quotes (group builder-id:identifier
                                  (parens (group _::$-bind arg-id:identifier)
                                          data-pattern
                                          (group _::$-bind IF-id:identifier)
                                          (group _::$-bind success-id:identifier)
                                          (group _::$-bind fail-id:identifier))))
                   ((~and block-tag block) body ...))
          (define-values (converted-pattern idrs sidrs vars can-be-empty?) (convert-pattern #'data-pattern))
          (with-syntax ([((id id-ref) ...) idrs]
                        [(((sid ...) sid-ref) ...) sidrs])
            #`(lambda (stx)
                (syntax-parse stx
                  [(_ arg-id data IF success fail)
                   (syntax-parse #'(group data)
                     [#,converted-pattern
                      (let ([id id-ref] ... [arg-id #'arg-id])
                        (let-syntaxes ([(sid ...) sid-ref] ...)
                          (let ([IF-id #'if-bridge])
                            (let ([success-id #'(parsed success)]
                                  ;; putting `if-bridge` in `fail-id`
                                  ;; helps make sure it's used correctly
                                  [fail-id #'(parsed (if-bridge IF fail))])
                              (unwrap-block
                               (rhombus-body-at block-tag body ...))))))])])))])])))

(define-syntax if-bridge
  ;; depending on `IF`, `if-bridge` will be used in an expression
  ;; or definition context
  (let ([parse (lambda (stx)
                 (syntax-parse stx
                   #:datum-literals (alts block parsed)
                   [(form-id e ... (alts (block success ...)
                                         (block . fail-case)))
                    (syntax-parse #'fail-case
                      #:datum-literals (group parsed)
                      #:literals (if-bridge)
                      [((group (parsed (if-bridge IF fail))))
                       #`(IF (rhombus-expression (group e ...))
                             (rhombus-body-sequence success ...)
                             fail)]
                      [_ (raise-syntax-error #f
                                             "not the given failure form in the failure branch"
                                             stx)])]))])
    (make-expression+definition-transformer
     (expression-transformer
      (lambda (stx) (values (parse stx) #'())))
     (definition-transformer
       (lambda (stx) (list (parse stx)))))))

(define-syntax chain-to-matcher
  ;; depends on `IF` like `if-bridge` does
  (let ([parse (lambda (rhombus stx)
                 (syntax-parse stx
                   #:datum-literals (parsed group parens)
                   [(_ (parens (group arg-id:identifier)
                               (group (parsed (matcher-id committer-id binder-id data)))
                               (group if-id)
                               (group success ...)
                               (group fail ...)))
                    (syntax-parse #'(fail ...)
                      #:literals (if-bridge)
                      [((parsed (if-bridge IF fail)))
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
                                     (if-reverse-bridge if-id (group fail ...)))])]))])
    (make-expression+definition-transformer
     (expression-transformer
      (lambda (stx) (values (parse #'rhombus-body stx) #'())))
     (definition-transformer
       (lambda (stx) (list (parse #'rhombus-body-sequence stx)))))))

(define-syntax (if-via-rhombus stx)
  (syntax-parse stx
    #:literals (if-reverse-bridge)
    [(_ tst thn (if-reverse-bridge if-id els))
     #'(rhombus-expression (group if-id (parsed tst)
                                  (alts (block (group (parsed (let () thn))))
                                        (block els))))]
    [_
     (raise-syntax-error #f "misuse of binding conditional" stx)]))

(define-syntax (if-reverse-bridge stx)
  (raise-syntax-error #f "misuse of binding conditional" stx))

(define-syntax chain-to-committer
  ;; depends on `IF` like `if-bridge` does
  (let ([parse (lambda (rhombus stx)
                 (syntax-parse stx
                   #:datum-literals (parsed group parens)
                   #:literals (if-bridge)
                   [(_ (parens (group arg-id:identifier)
                               (group (parsed (matcher-id committer-id binder-id data)))))
                    #:with rhombus rhombus
                    #`(committer-id arg-id data)]))])
    (make-expression+definition-transformer
     (expression-transformer
      (lambda (stx) (values (parse #'rhombus-body stx) #'())))
     (definition-transformer
       (lambda (stx) (list (parse #'rhombus-body-sequence stx)))))))

(define-syntax chain-to-binder
  ;; depends on `IF` like `if-bridge` does
  (let ([parse (lambda (rhombus stx)
                 (syntax-parse stx
                   #:datum-literals (parsed group parens)
                   #:literals (if-bridge)
                   [(_ (parens (group arg-id:identifier)
                               (group (parsed (matcher-id committer-id binder-id data)))))
                    #:with rhombus rhombus
                    #`(binder-id arg-id data)]))])
    (make-expression+definition-transformer
     (expression-transformer
      (lambda (stx) (values (parse #'rhombus-body stx) #'())))
     (definition-transformer
       (lambda (stx) (list (parse #'rhombus-body-sequence stx)))))))

(define-for-syntax binder-or-committer
  (definition-transformer
    (lambda (stx)
      (syntax-parse stx
        #:datum-literals (op group quotes)
        [(form-id (quotes (group builder-id:identifier
                                 . _))
                  . _)
         (list
          #`(define-syntax builder-id
              (binder-rhs #,stx)))]))))

(define-syntax binder binder-or-committer)
(define-syntax committer binder-or-committer)

(begin-for-syntax
  (define-syntax (binder-rhs stx)
    (syntax-parse stx
      [(_ orig-stx)
       (syntax-parse #'orig-stx
         #:datum-literals (op parens group block quotes)
         [(form-id (quotes (group builder-id:identifier
                                  (parens (group _::$-bind arg-id:identifier)
                                          data-pattern)))
                   ((~and block-tag block) body ...))
          (define-values (converted-data-pattern data-idrs data-sidrs data-vars data-can-be-empty?) (convert-pattern #'data-pattern))
          (with-syntax ([((data-id data-id-ref) ...) data-idrs]
                        [(((data-sid ...) data-sid-ref) ...) data-sidrs])
            #`(lambda (stx)
                (syntax-parse stx
                  [(_ arg-id data)
                   (syntax-parse #'(group data)
                     [#,converted-data-pattern
                      (let ([arg-id #'arg-id]
                            [data-id data-id-ref] ...)
                        (let-syntaxes ([(data-sid ...) data-sid-ref] ...)
                          (unwrap-block
                           (rhombus-body-at block-tag body ...))))])])))])])))

(define-for-syntax (unwrap-block stx)
  #`(rhombus-body-sequence #,@(unpack-multi stx 'bin.binder #f)))

(define-for-syntax (wrap-parsed stx)
  #`(parsed #,stx))

(define-for-syntax (extract-binding form proc)
  (syntax-parse (if (syntax? form)
                    (unpack-group form proc #f)
                    #'#f)
    [b::binding #'b.parsed]
    [_ (raise-bad-macro-result (proc-name proc) "binding" form)]))

(define-for-syntax (make-binding-infix-operator name prec protocol proc assc)
  (binding-infix-operator
   name
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

(define-for-syntax (make-binding-prefix-operator name prec protocol proc)
  (binding-prefix-operator
   name
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
