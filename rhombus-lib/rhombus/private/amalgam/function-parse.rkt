#lang racket/base
(require (for-syntax racket/base
                     racket/keyword
                     syntax/parse/pre
                     enforest/name-parse
                     shrubbery/print
                     "treelist.rkt"
                     "srcloc.rkt"
                     "tag.rkt"
                     "same-expression.rkt"
                     "static-info-pack.rkt"
                     "entry-point-adjustment.rkt"
                     (only-in "annotation-string.rkt" annotation-any-string)
                     "to-list.rkt"
                     "sorted-list-subset.rkt"
                     "dotted-sequence.rkt")
         racket/unsafe/undefined
         "treelist.rkt"
         "to-list.rkt"
         "parens.rkt"
         "binding.rkt"
         "parse.rkt"
         "nested-bindings.rkt"
         "call-result-key.rkt"
         "function-arity-key.rkt"
         "unsafe-key.rkt"
         "unsafe-key.rkt"
         "static-info.rkt"
         "repetition.rkt"
         "op-literal.rkt"
         (submod "ellipsis.rkt" for-parse)
         (only-in "list.rkt" List)
         (submod "annotation.rkt" for-class)
         (submod "equal.rkt" for-parse)
         "not-block.rkt"
         "lambda-kwrest.rkt"
         "dot-parse.rkt"
         "realm.rkt"
         "compound-repetition.rkt"
         "function-arity.rkt"
         "wrap-expression.rkt"
         "rest-bind.rkt"
         (submod "list.rkt" for-compound-repetition)
         (submod "map.rkt" for-info)
         (submod "define-arity.rkt" for-info)
         (submod "symbol.rkt" for-static-info)
         "if-blocked.rkt"
         "realm.rkt"
         "mutability.rkt"
         (only-in "values.rkt"
                  [values rhombus-values])
         "rhombus-primitive.rkt")

(module+ for-build
  (provide (for-syntax :kw-binding
                       :kw-opt-binding
                       :rhombus-kw-opt-binding
                       :ret-annotation
                       :rhombus-ret-annotation
                       :maybe-arg-rest
                       :non-...-binding
                       check-arg-for-unsafe
                       build-function
                       build-case-function
                       build-unsafe-function
                       build-unsafe-case-function
                       maybe-add-function-result-definition
                       maybe-add-unsafe-definition
                       parse-anonymous-function-shape))
  (begin-for-syntax
    (provide (struct-out converter))))

(module+ for-call
  (provide (for-syntax parse-function-call
                       wrap-annotation-check
                       build-anonymous-function)
           raise-result-failure))

(module+ for-non-special
  (provide (for-syntax :non-special)))

(begin-for-syntax
  (define-syntax-class :non-...
    #:datum-literals (group)
    (pattern (~and (~not (group _::...-bind))
                   (~not (group (~or* _::&-bind _::~&-bind) . _))
                   (group . _))))
  (define-syntax-class :non-...-binding
    #:attributes (parsed)
    (pattern form::non-...
             #:with ::binding #'form))

  (define-syntax-class :non-special
    #:datum-literals (group)
    (pattern (~and (~not (group _::...-bind))
                   (~not (group (~or* _::&-bind _::~&-bind) . _))
                   (~not (group _:keyword . _))
                   (~not (group _::_-expr))
                   (group . _))))

  (define (keyword->id-group kw)
    #`(#,group-tag
       #,(datum->syntax kw
                        (string->symbol (keyword->immutable-string (syntax-e kw)))
                        kw)))

  (define-syntax-class :has-kw-binding
    #:attributes (kw parsed)
    #:datum-literals (group)
    (pattern (group kw:keyword (_::block (group a ...+)))
             #:cut
             #:with ::binding #`(#,group-tag a ...))
    (pattern (group kw:keyword)
             #:cut
             #:with ::binding (keyword->id-group #'kw)))

  (define-syntax-class :plain-binding
    #:attributes (kw parsed)
    (pattern ::non-...-binding
             #:with kw #'#f))

  (define-syntax-class :kw-binding
    #:attributes (kw parsed)
    (pattern ::has-kw-binding)
    (pattern ::plain-binding))

  ;; used when just extracting an arity:
  (define-syntax-class :kw-arity-arg
    #:attributes (kw)
    #:datum-literals (group)
    (pattern (group kw:keyword . _))
    (pattern (group kw:keyword))
    (pattern _::non-...
             #:with kw #'#f))

  (define-syntax-class :kw-opt-binding
    #:attributes (kw parsed default)
    #:datum-literals (group)
    (pattern (group kw:keyword (_::block (~and g
                                               (group a ...+ eq::equal e ...+))))
             #:do [(check-multiple-equals #'g)]
             #:cut
             #:with default #`(#,group-tag e ...)
             #:do [(check-argument-annot #'default #'eq)]
             #:with ::binding #`(#,group-tag a ...))
    (pattern (group kw:keyword (_::block (group a ...+ (b-tag::block b ...))))
             #:cut
             #:with default #`(#,group-tag (parsed #:rhombus/expr (rhombus-body-at b-tag b ...)))
             #:with ::binding #`(#,group-tag a ...))
    (pattern (group kw:keyword eq::equal e ...+)
             #:cut
             #:with default #`(#,group-tag e ...)
             #:do [(check-argument-annot #'default #'eq)]
             #:with ::binding (keyword->id-group #'kw))
    (pattern ::has-kw-binding
             #:with default #'#f)
    (pattern (~and g
                   (group a ...+ eq::equal e ...+))
             #:do [(check-multiple-equals #'g)]
             #:cut
             #:with kw #'#f
             #:with default #`(#,group-tag e ...)
             #:do [(check-argument-annot #'default #'eq)]
             #:with ::binding #`(#,group-tag a ...))
    (pattern (group a ...+ (b-tag::block b ...))
             #:cut
             #:with (~not (_:keyword)) #'(a ...)
             #:with kw #'#f
             #:with default #`(#,group-tag (parsed #:rhombus/expr (rhombus-body-at b-tag b ...)))
             #:with ::binding #`(#,group-tag a ...))
    (pattern ::plain-binding
             #:with default #'#f))

  (define-syntax-class :rhombus-kw-opt-binding
    #:attributes (parsed maybe_keyword maybe_expr)
    (pattern ::kw-opt-binding
             #:attr maybe_keyword (and (syntax-e #'kw)
                                       #'kw)
             #:attr maybe_expr (and (syntax-e #'default)
                                    #'default)))

  ;; used when just extracting an arity:
  (define-syntax-class :kw-opt-arity-arg
    #:attributes (kw default)
    #:datum-literals (group)
    (pattern (group kw:keyword (_::block (~and g
                                               (group _ ...+ _::equal _ ...+))))
             #:do [(check-multiple-equals #'g)]
             #:with default #'#t)
    (pattern (group kw:keyword (_::block (group _ ...+ (b-tag::block . _))))
             #:with default #'#t)
    (pattern (group kw:keyword _::equal _ ...+)
             #:with default #'#t)
    (pattern (~and g
                   (group _ ...+ _::equal _ ...+))
             #:do [(check-multiple-equals #'g)]
             #:with default #'#t
             #:with kw #'#f)
    (pattern (group a ...+ (_::block . _))
             #:with (~not (_:keyword)) #'(a ...)
             #:with default #'#t
             #:with kw #'#f)
    (pattern ::kw-arity-arg
             #:with default #'#f))

  (define (check-argument-annot g eq-op)
    (syntax-parse g
      #:datum-literals (group)
      [(group _ ... ann-op::annotate-op _ ...)
       (raise-syntax-error #f
                           (string-append
                            "immediate annotation operator not allowed in default-value expression;"
                            "\n use parentheses around the expression if the annotation was intended,"
                            "\n since parentheses avoid the appearance of annotating the binding"
                            "\n instead of the expression")
                           #'ann-op.name
                           #f
                           (list eq-op))]
      [(group _ ... eq2::equal _ ...)
       (raise-syntax-error #f
                           (string-append "multiple immediate equals not allowed in this group;"
                                          "\n use parentheses to disambiguate")
                           eq-op
                           #f
                           (list #'eq2))]
      [_ (void)]))

  (struct converter (proc       ; `(lambda (arg ... success-k fail-k) ....)` with one `arg` for each result, or `#f`
                     predicate? ; all predicate annotations?
                     count))    ; the expected number of values

  (define-syntax-class :values-id
    #:attributes (name)
    #:description "the literal `values`"
    #:opaque
    (pattern ::name
             #:when (free-identifier=? (in-annotation-space #'name)
                                       (annot-quote rhombus-values))))

  (define-splicing-syntax-class :ret-annotation
    #:attributes (static-infos ; can be `((#%values (static-infos ...)))` for multiple results
                  converter    ; a `converter` struct, or `#f`
                  annot-str)   ; the raw text of annotation, or `#f`
    #:description "return annotation"
    #:datum-literals (group)
    (pattern (~seq ann-op::annotate-op (~optional op::values-id) (~and p (_::parens g ...)))
             #:do [(define gs #'(g ...))]
             #:with (c::annotation ...) gs
             #:with (arg ...) (generate-temporaries gs)
             #:do [(define cnt (length (syntax->list gs)))
                   (define-values (sis cvtr)
                     (syntax-parse #'(c.parsed ...)
                       [(c-parsed::annotation-predicate-form ...)
                        (values #'((#%values (c-parsed.static-infos ...)))
                                (converter
                                 (and (attribute ann-op.check?)
                                      #'(lambda (arg ... success-k fail-k)
                                          (if (and (c-parsed.predicate arg) ...)
                                              (success-k arg ...)
                                              (fail-k))))
                                 #t
                                 cnt))]
                       [(c-parsed::annotation-binding-form ...)
                        #:do [(unless (attribute ann-op.check?)
                                (for ([c (in-list (syntax->list #'(c ...)))]
                                      [c-p (in-list (syntax->list #'(c.parsed ...)))])
                                  (syntax-parse c-p
                                    [_::annotation-predicate-form (void)]
                                    [_ (raise-unchecked-disallowed #'ann-op.name c)])))]
                        #:with (arg-parsed::binding-form ...) #'(c-parsed.binding ...)
                        #:with (arg-impl::binding-impl ...) #'((arg-parsed.infoer-id () arg-parsed.data) ...)
                        (values #'((#%values (c-parsed.static-infos ...)))
                                (converter
                                 #`(lambda (arg ... success-k fail-k)
                                     #,(for/foldr ([next #'(success-k arg ...)])
                                                  ([arg (in-list (syntax->list #'(arg ...)))]
                                                   [arg-impl-info (in-list (syntax->list #'(arg-impl.info ...)))]
                                                   [body (in-list (syntax->list #'(c-parsed.body ...)))])
                                         (syntax-parse arg-impl-info
                                           [arg-info::binding-info
                                            #`(arg-info.matcher-id #,arg
                                                                   arg-info.data
                                                                   if/blocked
                                                                   (begin
                                                                     (arg-info.committer-id #,arg arg-info.evidence-ids arg-info.data)
                                                                     (arg-info.binder-id #,arg arg-info.evidence-ids arg-info.data)
                                                                     (define-static-info-syntax/maybe arg-info.bind-id
                                                                       arg-info.bind-static-info ...)
                                                                     ...
                                                                     (let ([#,arg #,body]) #,next))
                                                                   (fail-k))])))
                                 #f
                                 cnt))]))]
             #:with static-infos sis
             #:attr converter cvtr
             #:attr annot-str (shrubbery-syntax->string #`(#,group-tag (~? op) p)))
    (pattern (~seq ann-op::annotate-op ctc0::not-block ctc::not-block ...)
             #:do [(define annot #`(#,group-tag ctc0 ctc ...))]
             #:with c::annotation (no-srcloc annot)
             #:do [(define-values (sis cvtr)
                     (syntax-parse #'c.parsed
                       [c-parsed::annotation-predicate-form
                        (values #'c-parsed.static-infos
                                (converter
                                 (and (attribute ann-op.check?)
                                      #'(lambda (v success-k fail-k)
                                          (if (c-parsed.predicate v)
                                              (success-k v)
                                              (fail-k))))
                                 #t
                                 1))]
                       [c-parsed::annotation-binding-form
                        #:do [(unless (attribute ann-op.check?)
                                (raise-unchecked-disallowed #'ann-op.name #'c))]
                        #:with arg-parsed::binding-form #'c-parsed.binding
                        #:with arg-impl::binding-impl #'(arg-parsed.infoer-id () arg-parsed.data)
                        #:with arg-info::binding-info #'arg-impl.info
                        (values #'c-parsed.static-infos
                                (converter
                                 #'(lambda (v success-k fail-k)
                                     (arg-info.matcher-id v
                                                          arg-info.data
                                                          if/blocked
                                                          (begin
                                                            (arg-info.committer-id v arg-info.evidence-ids arg-info.data)
                                                            (arg-info.binder-id v arg-info.evidence-ids arg-info.data)
                                                            (define-static-info-syntax/maybe arg-info.bind-id
                                                              arg-info.bind-static-info ...)
                                                            ...
                                                            (success-k c-parsed.body))
                                                          (fail-k)))
                                 #f
                                 1))]))]
             #:with static-infos sis
             #:attr converter cvtr
             #:attr annot-str (shrubbery-syntax->string annot))
    (pattern (~seq)
             #:with static-infos #'()
             #:attr converter #f
             #:attr annot-str #f))

  (define-splicing-syntax-class :rhombus-ret-annotation
    #:attributes (count
                  is_predicate
                  maybe_converter
                  static_info
                  annotation_string)
    (pattern r::ret-annotation
             #:do [(define-values (cnt pred? proc si annot-str)
                     (cond
                       [(attribute r.converter)
                        => (lambda (cvtr)
                             (values (converter-count cvtr)
                                     (converter-predicate? cvtr)
                                     (cond
                                       [(converter-proc cvtr)
                                        => (lambda (proc) #`(parsed #:rhombus/expr #,proc))]
                                       [else #f])
                                     (unpack-static-infos 'bind_meta.Result #'r.static-infos)
                                     (string->immutable-string (attribute r.annot-str))))]
                       [else (values #f
                                     #t
                                     #f
                                     (unpack-static-infos 'bind_meta.Result #'())
                                     annotation-any-string)]))]
             #:attr count cnt
             #:attr is_predicate pred?
             #:attr maybe_converter proc
             #:with static_info si
             #:attr annotation_string annot-str))

  (define-splicing-syntax-class :pos-rest
    #:attributes (arg parsed)
    #:datum-literals (group)
    (pattern (~seq (group _::&-bind a ...))
             #:with arg::non-...-binding #`(#,group-tag rest-bind #,(get-treelist-static-infos)
                                            #:annot-prefix? #f
                                            (#,group-tag a ...))
             #:with parsed #'arg.parsed)
    (pattern (~seq e::non-...-binding (~and ooo (group _::...-bind)))
             #:with arg::non-...-binding #`(#,group-tag List (parens e ooo))
             #:with parsed #'arg.parsed))

  (define-splicing-syntax-class :kwp-rest
    #:attributes (kwarg kwparsed)
    #:datum-literals (group)
    (pattern (~seq (group _::~&-bind a ...))
             #:with kwarg::non-...-binding #`(#,group-tag rest-bind #,(get-map-static-infos)
                                              #:annot-prefix? #f
                                              (#,group-tag a ...))
             #:with kwparsed #'kwarg.parsed))

  (define-splicing-syntax-class :maybe-arg-rest
    #:attributes (arg parsed kwarg kwparsed)
    #:datum-literals (group)
    (pattern (~seq
              (~alt (~optional ::pos-rest #:defaults ([arg #'#f] [parsed #'#f]))
                    (~optional ::kwp-rest #:defaults ([kwarg #'#f] [kwparsed #'#f])))
              ...)))

  ;; used when just extracting an arity:
  (define-splicing-syntax-class :pos-arity-rest
    #:attributes (rest?)
    #:datum-literals (group)
    (pattern (~seq (group _::&-bind _ ...))
             #:with rest? #'#t)
    (pattern (~seq _::non-... (group _::...-bind))
             #:with rest? #'#t))
  (define-splicing-syntax-class :kwp-arity-rest
    #:attributes (kwrest?)
    #:datum-literals (group)
    (pattern (~seq (group _::~&-bind _ ...))
             #:with kwrest? #'#t))
  (define-splicing-syntax-class :maybe-rest-arity-arg
    #:attributes (rest? kwrest?)
    #:datum-literals (group)
    (pattern (~seq
              (~alt (~optional ::pos-arity-rest #:defaults ([rest? #'#f]))
                    (~optional ::kwp-arity-rest #:defaults ([kwrest? #'#f])))
              ...)))

  (define-syntax-class :unsafe-compatible-binding
    #:datum-literals (group)
    (pattern (group _::...-bind))
    (pattern (group _:identifier))
    (pattern (group _:identifier _::annotate-op (~not eq::equal) ...)))

  (define (check-arg-for-unsafe stx arg kw-ok?)
    (let loop ([arg arg] [kw-ok? kw-ok?])
      (syntax-parse arg
        [_::unsafe-compatible-binding #t]
        [(group (_::parens arg)) (loop #'arg #f)]
        [(group _:keyword (_::block arg))
         #:when kw-ok?
         (loop #'arg #f)]
        [_ (raise-syntax-error #f
                               "binding does not fit limitations of an `~unsafe` clause"
                               stx
                               arg)]))))

(define-for-syntax (parse-anonymous-function-shape stx)
  (define (build arity name)
    (let* ([ht (hash)]
           [ht (if arity (hash-set ht 'arity arity) ht)]
           [ht (if name (hash-set ht 'name name) ht)])
      ht))
  (define (extract-name tail)
    (syntax-parse tail
      #:datum-literals (group)
      [(_ ... (_::block (~alt (~optional (group #:name n::dotted-operator-or-identifier-sequence))
                              (~optional (group #:who . _)))
                        ...
                        . _))
       (and (attribute n)
            (build-dot-symbol (syntax->list #'n) #:skip-dots? #t))]
      [_ #false]))
  (syntax-parse stx
    [(form-id tail ...
              (alts-tag::alts
               (_::block (group (_::parens arg::kw-arity-arg ... rest::maybe-rest-arity-arg)
                                . _))
               ...+))
     (build
      (and-arity-summaries
       (for/list ([arg-kws (in-list (syntax->list #'((arg.kw ...) ...)))]
                  [rest? (in-list (syntax->list #'(rest.rest? ...)))]
                  [kw-rest? (in-list (syntax->list #'(rest.kwrest? ...)))])
         (define kws (syntax->list arg-kws))
         (summarize-arity kws (map (lambda (_) #'#f) kws) (syntax-e rest?) (syntax-e kw-rest?))))
      (extract-name #'(tail ...)))]
    [(form-id (parens-tag::parens arg::kw-opt-arity-arg ... rest::maybe-rest-arity-arg) . tail)
     (build
      (summarize-arity #'(arg.kw ...) #'(arg.default ...) (syntax-e #'rest.rest?) (syntax-e #'rest.kwrest?))
      (extract-name #'tail))]
    [_ (build #f #f)]))

(begin-for-syntax

  (struct fcase (kws
                 args arg-parseds rest-arg rest-arg-parsed kwrest-arg kwrest-arg-parsed
                 converter annot-str
                 rhs))

  ;; usage: (fcase-pos fcase-args fc) or (fcase-pos fcase-arg-parseds fc)
  (define (fcase-pos get-args fc)
    (for/list ([kw (in-list (fcase-kws fc))]
               [arg (in-list (get-args fc))]
               #:when (not (syntax-e kw)))
      arg))

  (define (build-function adjustments argument-static-infoss
                          function-name outer-who inner-who
                          kws args arg-parseds defaults
                          rest-arg rest-parsed
                          kwrest-arg kwrest-parsed
                          converter annot-str
                          rhs
                          src-ctx)
    (syntax-parse arg-parseds
      [(arg-parsed::binding-form ...)
       #:with (in-static-infos ...) (extract-added-static-infos argument-static-infoss (length (syntax->list arg-parseds)))
       #:with (arg-impl::binding-impl ...) #'((arg-parsed.infoer-id in-static-infos arg-parsed.data) ...)
       #:with (arg-info::binding-info ...) #'(arg-impl.info ...)
       #:with (tmp-id ...) (generate-temporaries #'(arg-info.name-id ...))
       #:with (arg ...) args
       #:with (maybe-rest-tmp (maybe-rest-tmp* ...) (rest-def ...) (maybe-match-rest ...))
       (if (syntax-e rest-arg)
           (syntax-parse rest-parsed
             [rest::binding-form
              #:with rest-impl::binding-impl #'(rest.infoer-id () rest.data)
              #:with rest-info::binding-info #'rest-impl.info
              #`(rest-tmp-lst
                 (#:rest rest-tmp-lst)
                 ((define rest-tmp (list->treelist rest-tmp-lst)))
                 ((rest-tmp rest-info #,rest-arg #f)))])
           #'(() () () ()))
       #:with ((maybe-kwrest-tmp ...) (maybe-match-kwrest ...))
       (if (syntax-e kwrest-arg)
           (syntax-parse kwrest-parsed
             [kwrest::binding-form
              #:with kwrest-impl::binding-impl #'(kwrest.infoer-id () kwrest.data)
              #:with kwrest-info::binding-info #'kwrest-impl.info
              #`((#:kwrest kwrest-tmp) ((kwrest-tmp kwrest-info #,kwrest-arg #f)))])
           #'(() ()))
       #:with (((arg-form ...) arg-default) ...)
       (for/list ([kw (in-list (syntax->list kws))]
                  [tmp-id (in-list (syntax->list #'(tmp-id ...)))]
                  [default (in-list (syntax->list defaults))])
         ;; FIXME: if `default` is simple enough, then
         ;; use it instead of `unsafe-undefined`, and
         ;; then `define` has the opportunity to inline it
         (define arg+default
           (cond
             [(not (syntax-e default))
              tmp-id]
             [else
              #`[#,tmp-id unsafe-undefined]]))
         (cond
           [(not (syntax-e kw))
            (list (list arg+default) default)]
           [else
            (list (list kw arg+default) default)]))
       (define arity (summarize-arity kws defaults (syntax-e rest-arg) (syntax-e kwrest-arg)))
       (define shifted-arity
         (shift-arity arity (treelist-length (entry-point-adjustment-prefix-arguments adjustments))))
       (define body
         (wrap-expression
          ((entry-point-adjustment-wrap-body adjustments)
           arity
           #`(parsed
              #:rhombus/expr
              #,(add-who
                 outer-who function-name
                 #`(nested-bindings
                    #,function-name
                    #f ; try-next
                    #t ; eager-bind?
                    argument-binding-failure
                    (tmp-id arg-info arg arg-default)
                    ...
                    maybe-match-rest ...
                    maybe-match-kwrest ...
                    (begin
                      #,(add-annotation-check
                         function-name converter annot-str
                         (add-who
                          inner-who function-name
                          #`(rhombus-body-expression #,rhs))))))))))
       (define (adjust-args args)
         (append (treelist->list (entry-point-adjustment-prefix-arguments adjustments))
                 args))
       (values
        (relocate+reraw
         (respan src-ctx)
         ;; Racket `define` needs to recognize an immediate `lambda`
         (if (syntax-e kwrest-arg)
             #`(lambda/kwrest
                #:name #,function-name
                #:arity #,shifted-arity
                #:method #,(entry-point-adjustment-method? adjustments)
                maybe-rest-tmp* ...
                maybe-kwrest-tmp ...
                #,(adjust-args #'(arg-form ... ...))
                rest-def ...
                #,body)
             (let* ([proc #`(lambda #,(adjust-args #'(arg-form ... ... . maybe-rest-tmp))
                              rest-def ...
                              #,body)]
                    [proc (syntax-property proc 'inferred-name function-name)]
                    [proc (if (entry-point-adjustment-method? adjustments)
                              (syntax-property proc 'method-arity-error #t)
                              proc)])
               proc)))
        shifted-arity)]))

  (define (build-unsafe-function kws args arg-parseds
                                 rest-arg rest-parsed
                                 body
                                 src-ctx)
    (syntax-parse body
      #:datum-literals (group)
      [(group unsafe-kw (tag::block body ...+))
       (syntax-property
        #`(lambda #,@(unsafe-args kws args arg-parseds
                                  rest-arg rest-parsed)
            (rhombus-body-at tag body ...))
        'body-as-unsafe
        #t)]))

  (define (build-case-function adjustments argument-static-infoss
                               function-name outer-whos inner-whos
                               main-converter main-annot-str
                               kwss-stx argss-stx arg-parsedss-stx
                               rest-args-stx rest-parseds-stx
                               kwrest-args-stx kwrest-parseds-stx
                               converters annot-strs
                               rhss-stx
                               src-ctx)
    (define kwss (map syntax->list (syntax->list kwss-stx)))
    (define argss (map syntax->list (syntax->list argss-stx)))
    (define arg-parsedss (map syntax->list (syntax->list arg-parsedss-stx)))
    (define rest-args (syntax->list rest-args-stx))
    (define rest-parseds (syntax->list rest-parseds-stx))
    (define kwrest-args (syntax->list kwrest-args-stx))
    (define kwrest-parseds (syntax->list kwrest-parseds-stx))
    (define rhss (syntax->list rhss-stx))
    (define-values (ns fcss)
      (group-by-counts
       (map fcase
            kwss
            argss arg-parsedss rest-args rest-parseds kwrest-args kwrest-parseds
            converters annot-strs
            rhss)))
    (define arityss
      (for/list ([fcs (in-list fcss)])
        (for/list ([fc (in-list fcs)])
          (summarize-arity (fcase-kws fc)
                           (map (lambda (_) #'#f) (fcase-kws fc))
                           (syntax-e (fcase-rest-arg fc))
                           (syntax-e (fcase-kwrest-arg fc))))))
    (define arity (and-arity-summaries (apply append arityss)))
    (define shifted-arity
      (shift-arity arity (treelist-length (entry-point-adjustment-prefix-arguments adjustments))))
    (define kws? (pair? shifted-arity))
    (values
     (relocate+reraw
      (respan src-ctx)
      #`(case-lambda/kwrest
         #:name #,function-name
         #:arity #,shifted-arity
         #:method #,(entry-point-adjustment-method? adjustments)
         #,@(for/list ([n (in-list ns)]
                       [fcs (in-list fcss)]
                       [aritys (in-list arityss)]
                       [inner-who (in-list inner-whos)]
                       [outer-who (in-list outer-whos)])
              (with-syntax ([(try-next pos-arg-id ...) (generate-temporaries
                                                        (cons 'try-next
                                                              (fcase-pos fcase-args (find-matching-case n fcs))))]
                            [(maybe-rest-tmp ...) (if (negative? n)
                                                      #'(#:rest rest-tmp-lst)
                                                      #'())]
                            [maybe-rest-tmp-use (if (negative? n)
                                                    #'rest-tmp-lst
                                                    #''())]
                            [(rest-def ...) (if (negative? n)
                                                #'((define rest-tmp (list->treelist rest-tmp-lst)))
                                                #'())]
                            [(maybe-kwrest-tmp ...) (if kws?
                                                        #'(#:kwrest kwrest-tmp)
                                                        #'())]
                            [maybe-kwrest-tmp-use (if kws?
                                                      #'kwrest-tmp
                                                      #''#hashalw())])
                #`[maybe-rest-tmp ...
                   maybe-kwrest-tmp ...
                   (#,@(treelist->list (entry-point-adjustment-prefix-arguments adjustments))
                    pos-arg-id ...)
                   ;; possible improvement: convert to treelist in individual try instead of for
                   ;; all tries; whether that's better depends on the shapes of the cases
                   rest-def ...
                   #,(for/foldr ([next #`(cases-failure
                                          '#,function-name
                                          maybe-rest-tmp-use maybe-kwrest-tmp-use pos-arg-id ...)])
                                ([fc (in-list fcs)]
                                 [arity (in-list aritys)])
                       (define-values (this-args wrap-adapted-arguments)
                         ;; currently, `adapt-arguments-for-count` assumes tree-list `rest-tmp`
                         (adapt-arguments-for-count fc n #'(pos-arg-id ...) #'rest-tmp
                                                    (and kws? #'kwrest-tmp)
                                                    #'try-next))
                       (syntax-parse (fcase-arg-parseds fc)
                         [(arg-parsed::binding-form ...)
                          #:with (in-static-infos ...) (extract-added-static-infos argument-static-infoss (length (fcase-arg-parseds fc)))
                          #:with (arg-impl::binding-impl ...) #'((arg-parsed.infoer-id in-static-infos arg-parsed.data) ...)
                          #:with (arg-info::binding-info ...) #'(arg-impl.info ...)
                          #:with (arg ...) (fcase-args fc)
                          #:with (this-arg-id ...) this-args
                          #:with ((maybe-match-rest ...)
                                  (maybe-commit-rest ...)
                                  (maybe-bind-rest ...)
                                  (maybe-static-info-rest ...))
                          (cond
                            [(syntax-e (fcase-rest-arg fc))
                             (define rest-parsed (fcase-rest-arg-parsed fc))
                             (syntax-parse rest-parsed
                               [rest::binding-form
                                #:with rest-impl::binding-impl #'(rest.infoer-id () rest.data)
                                #:with rest-info::binding-info #'rest-impl.info
                                #`(((rest-tmp rest-info #,(fcase-rest-arg fc) #f))
                                   ((rest-info.committer-id rest-tmp rest-info.evidence-ids rest-info.data))
                                   ((rest-info.binder-id rest-tmp rest-info.evidence-ids rest-info.data))
                                   ((define-static-info-syntax/maybe rest-info.bind-id rest-info.bind-static-info ...)
                                    ...))])]
                            [else #'(() () () ())])
                          #:with ((maybe-match-kwrest ...)
                                  (maybe-commit-kwrest ...)
                                  (maybe-bind-kwrest ...)
                                  (maybe-static-info-kwrest ...))
                          (cond
                            [(syntax-e (fcase-kwrest-arg fc))
                             (define kwrest-parsed (fcase-kwrest-arg-parsed fc))
                             (syntax-parse kwrest-parsed
                               [kwrest::binding-form
                                #:with kwrest-impl::binding-impl #'(kwrest.infoer-id () kwrest.data)
                                #:with kwrest-info::binding-info #'kwrest-impl.info
                                #`(((kwrest-tmp kwrest-info #,(fcase-kwrest-arg fc) #f))
                                   ((kwrest-info.committer-id kwrest-tmp kwrest-info.evidence-ids kwrest-info.data))
                                   ((kwrest-info.binder-id kwrest-tmp kwrest-info.evidence-ids kwrest-info.data))
                                   ((define-static-info-syntax/maybe kwrest-info.bind-id kwrest-info.bind-static-info ...)
                                    ...))])]
                            [else #'(() () () ())])
                          ;; use `((lambda ....) ....)` to keep code in original order, in case
                          ;; of expansion errors.
                          #`((lambda (try-next)
                               #,(wrap-adapted-arguments
                                  (wrap-expression
                                   ((entry-point-adjustment-wrap-body adjustments)
                                    arity
                                    #`(parsed
                                       #:rhombus/expr
                                       #,(add-who
                                          outer-who function-name
                                          #`(nested-bindings
                                             #,function-name
                                             try-next
                                             #t ; eager-bind?
                                             argument-binding-failure
                                             (this-arg-id arg-info arg #f)
                                             ...
                                             maybe-match-rest ...
                                             maybe-match-kwrest ...
                                             (begin
                                               ;; eager-bind? covers all of this
                                               ;;
                                               ;; (arg-info.committer-id this-arg-id arg-info.evidence-ids arg-info.data)
                                               ;; ...
                                               ;; maybe-commit-rest ...
                                               ;; maybe-commit-kwrest ...
                                               ;; (arg-info.binder-id this-arg-id arg-info.evidence-ids arg-info.data)
                                               ;; ...
                                               ;; maybe-bind-rest ...
                                               ;; maybe-bind-kwrest ...
                                               ;; (define-static-info-syntax/maybe arg-info.bind-id arg-info.bind-static-info ...)
                                               ;; ... ...
                                               ;; maybe-static-info-rest
                                               ;; ...
                                               ;; maybe-static-info-kwrest
                                               ;; ...
                                               #,(add-annotation-check
                                                  function-name main-converter main-annot-str
                                                  (add-annotation-check
                                                   function-name (fcase-converter fc) (fcase-annot-str fc)
                                                   (add-who
                                                    inner-who function-name
                                                    #`(rhombus-body-expression #,(fcase-rhs fc)))))))))))))
                             (lambda () #,next))]))]))))
     shifted-arity))

  (define (build-unsafe-case-function kwss-stx argss-stx arg-parsedss-stx
                                      rest-args-stx rest-parseds-stx
                                      bodys
                                      src-ctx)
    (syntax-property
     #`(case-lambda
         #,@(for/list ([kws (in-list (syntax->list kwss-stx))]
                       [args (in-list (syntax->list argss-stx))]
                       [arg-parseds (in-list (syntax->list arg-parsedss-stx))]
                       [rest-arg (in-list (syntax->list rest-args-stx))]
                       [rest-parsed (in-list (syntax->list rest-parseds-stx))]
                       [body (in-list bodys)])
              (syntax-parse body
                #:datum-literals (group)
                [(group unsafe-kw (tag::block body ...+))
                 #`[#,@(unsafe-args kws args arg-parseds
                                    rest-arg rest-parsed)
                    (rhombus-body-at tag body ...)]])))
     'body-as-unsafe
     #t))

  (define (unsafe-args kws args arg-parseds
                       rest-arg rest-parsed)
    (define (extract-arg-id arg-parsed)
      (syntax-parse arg-parsed
        [arg::binding-form
         #:with arg-impl::binding-impl #'(arg.infoer-id () arg.data)
         #:with arg-info::binding-info #'arg-impl.info
         #:with (bind-id) #'(arg-info.bind-id ...)
         #'bind-id]))
    (define arg-list
      (apply
       append
       (for/list ([kw (in-list (syntax->list kws))]
                  [arg (in-list (syntax->list args))]
                  [arg-parsed (in-list (syntax->list arg-parseds))])
         (cond
           [(syntax-e kw)
            (list kw (extract-arg-id arg-parsed))]
           [else (list (extract-arg-id arg-parsed))]))))
    (cond
      [(syntax-e rest-arg)
       (syntax-parse rest-parsed
         [rest::binding-form
          #:with rest-impl::binding-impl #'(rest.infoer-id () rest.data)
          #:with rest-info::binding-info #'rest-impl.info
          #:with (bind-id) #'(rest-info.bind-id ...)
          (syntax-parse #'(rest-info.bind-uses ...)
            [((_ ... [#:repet ()] _ ...))
             (list
              (append arg-list #'bind-id))]
            [_
             ;; add list unpack
             (define tmp (car (generate-temporaries #'(bind-id))))
             (list
              (append arg-list tmp)
              #`(define-syntaxes (bind-id #,(in-repetition-space #'bind-id))
                  (make-expression+repetition #'(([(elem) (in-list #,tmp)])) #'elem (lambda () #'()))))])])]
      [(list arg-list)]))

  (define (maybe-add-function-result-definition name extends static-infoss arity unsafe-id defns)
    (define result-info?
      (and (pair? static-infoss)
           (pair? (syntax-e (car static-infoss)))
           (for/and ([static-infos (in-list (cdr static-infoss))])
             (same-expression? (car static-infoss) static-infos))))
    (cons (with-syntax ([name name]
                        [extends extends]
                        [(maybe-result-info ...)
                         (if result-info?
                             (list #`(#%call-result #,(car static-infoss)))
                             null)]
                        [(maybe-arity-info ...)
                         (if arity
                             (list #`(#%function-arity #,arity))
                             null)]
                        [(maybe-unsafe-info ...)
                         (if arity
                             (list #`(#%unsafe #,unsafe-id))
                             null)])
            #'(define-static-info-syntax/maybe/maybe-extension name extends
                maybe-result-info ...
                maybe-arity-info ...
                maybe-unsafe-info ...
                . #,(indirect-get-function-static-infos)))
          defns))

  (define (maybe-add-unsafe-definition unsafe-id unsafe-proc defns)
    (if unsafe-proc
        (cons #`(define #,unsafe-id #,unsafe-proc)
              defns)
        defns))

  ;; returns (values (listof n) (listof (listof fcase)))
  ;; where `n` is the argument count, and a negative
  ;; `n` means "-(n+1) or more"; although the `n`s
  ;; can be in any order, the `fcase`s are kept in the same
  ;; order within the group for one `n`
  (define (group-by-counts fcases)
    ;; if there is any rest clause, then other clauses
    ;; whose arity overlaps needs to be merged; a rest
    ;; clause requiring at least N arguments will merge
    ;; with any clause that accepts N or more
    (define rest-min
      (for/fold ([rest-min #f]) ([fc (in-list fcases)])
        (cond
          [(syntax-e (fcase-rest-arg fc))
           (define n (length (fcase-pos fcase-args fc)))
           (if rest-min (min rest-min n) n)]
          [else rest-min])))
    (define ht
      (for/foldr ([ht #hasheqv()]) ([fc (in-list fcases)])
        (let* ([n (length (fcase-pos fcase-args fc))]
               [n (if (and rest-min (>= n rest-min))
                      (- (add1 rest-min))
                      n)])
          (hash-set ht n (cons fc (hash-ref ht n '()))))))
    (for/lists (ns fcss) ([(n fcs) (in-hash ht)])
      (values n fcs)))

  (define (find-matching-case n fcs)
    (define find-n (if (negative? n) (- (add1 n)) n))
    (for/or ([fc (in-list fcs)])
      (define fc-n (length (fcase-pos fcase-args fc)))
      (and (eqv? find-n fc-n)
           fc)))

  ;; Inputs:
  ;;   fc: the fcase to be adapted, with positional arity n'
  ;;   n: the minimum-positional-arity of the case-lambda case to fit into
  ;;   pos-arg-ids-stx: the first n positional arguments
  ;;   rest-tmp: a possible positional-rest that may contain arguments after n
  ;;   kwrest-tmp: a possible keyword-rest
  ;;   try-next: a thunk to try the next fcase within the n case on failure
  ;; Outputs:
  ;;   new-arg-ids: positional and keyword arguments corresponding to fc
  ;;   wrap-adapted-arguments: to bind new-arg-ids, rest-tmp, and kwrest-tmp, or fail
  ;; when a clause that expects n' (or more) arguments is merged
  ;; with a clause that expects n or more arguments (so n <= n'), then
  ;; the rest argument needs to be unpacked to extra arguments
  (define (adapt-arguments-for-count fc n pos-arg-ids-stx rest-tmp kwrest-tmp try-next)
    (define base-f-n (length (fcase-pos fcase-args fc)))
    (define f-n (if (syntax-e (fcase-rest-arg fc))
                    (- (add1 base-f-n))
                    base-f-n))
    ;; adapt single arguments
    (define-values (drop-len new-arg-ids wrap/single-args)
      (for/fold ([pos-arg-ids-rem (syntax->list pos-arg-ids-stx)]
                 [pos-arg-idx 0]
                 [new-arg-ids-rev '()]
                 [wrap values]
                 #:result (let ()
                            (unless (null? pos-arg-ids-rem)
                              (error "assert failed in wrap-adapted: pos-arg-ids-rem"))
                            (values pos-arg-idx
                                    (reverse new-arg-ids-rev)
                                    wrap)))
                ([kw (in-list (fcase-kws fc))]
                 [arg (in-list (fcase-args fc))])
        (cond
          [(and (not (syntax-e kw)) (pair? pos-arg-ids-rem))
           (values (cdr pos-arg-ids-rem)
                   pos-arg-idx
                   (cons (car pos-arg-ids-rem) new-arg-ids-rev)
                   wrap)]
          [(not (syntax-e kw))
           (unless (negative? n) (error "assert failed in wrap-adapted: n 1"))
           (define tmp (car (generate-temporaries (list arg))))
           (values pos-arg-ids-rem
                   (add1 pos-arg-idx)
                   (cons tmp new-arg-ids-rev)
                   (lambda (body)
                     (wrap
                      #`(let ([#,tmp (treelist-ref #,rest-tmp '#,pos-arg-idx)])
                          #,body))))]
          [else
           (unless kwrest-tmp (error "assert failed in wrap-adapted: kwrest-tmp 1"))
           (define tmp (car (generate-temporaries (list arg))))
           (values pos-arg-ids-rem
                   pos-arg-idx
                   (cons tmp new-arg-ids-rev)
                   (lambda (body)
                     (wrap
                      ;; `unsafe-undefined` cannot be the result of a safe expression
                      #`(let ([#,tmp (hash-ref #,kwrest-tmp '#,kw unsafe-undefined)])
                          (if (eq? #,tmp unsafe-undefined)
                              (#,try-next)
                              (let ([#,kwrest-tmp (hash-remove #,kwrest-tmp '#,kw)])
                                #,body))))))])))
    ;; check compatible positional rest length
    ;; This is an outer check, because the length of tree lists can be
    ;; produced in constant time.
    (define wrap/rest
      (cond
        [(eqv? n f-n) wrap/single-args]
        [(and (eqv? drop-len 0) (negative? n))
         (lambda (body)
           #`(if (treelist-empty? #,rest-tmp)
                 (let () #,(wrap/single-args body))
                 (#,try-next)))]
        [(eqv? drop-len 0) wrap/single-args]
        [else
         (unless (negative? n) (error "assert failed in wrap-adapted: n 2"))
         (lambda (body)
           #`(if (#,(if (negative? f-n) #'>= #'=) (treelist-length #,rest-tmp) '#,drop-len)
                 (let () #,(wrap/single-args body))
                 (#,try-next)))]))
    ;; check empty keyword rest
    (define wrap/kwrest
      (cond
        [(and (not kwrest-tmp) (not (syntax-e (fcase-kwrest-arg fc))))
         wrap/rest]
        [(and kwrest-tmp (syntax-e (fcase-kwrest-arg fc)))
         wrap/rest]
        [else
         (unless kwrest-tmp (error "assert failed in wrap-adapted: kwrest-tmp 2"))
         (lambda (body)
           (wrap/rest
            #`(if (eqv? (hash-count #,kwrest-tmp) 0)
                  (let () #,body)
                  (#,try-next))))]))
    ;; produce the actual rest argument
    (define wrap/rest-body
      (cond
        [(eqv? drop-len 0) wrap/kwrest]
        [(negative? f-n)
         (unless (negative? n) (error "assert failed in wrap-adapted: n 3"))
         (lambda (body)
           (wrap/kwrest
            #`(let ([#,rest-tmp (treelist-drop #,rest-tmp '#,drop-len)])
                #,body)))]
        [else wrap/kwrest]))
    (values new-arg-ids wrap/rest-body))

  (define (extract-added-static-infos argument-static-infoss n)
    (if (= n (length argument-static-infoss))
        argument-static-infoss
        (for/list ([i (in-range n)]) #'())))

  (define (add-who who function-name e)
    (cond
      [(not who) e]
      [else #`(let ()
                (define #,who '#,function-name)
                (define-static-info-syntax #,who
                  #:getter get-symbol-static-infos)
                (let ()
                  #,e))])))

(define (argument-binding-failure who val annotation-str)
  (raise-binding-failure who "argument" val annotation-str))

(define (raise-bindings-failure who msg what vals annot-str)
  (raise
   (exn:fail:contract
    (error-message->adjusted-string
     who
     rhombus-realm
     (apply string-append
            msg
            "\n  " what "...:"
            (append
             (if (null? vals)
                 (list " [none]")
                 (for/list ([v (in-list vals)])
                   (string-append "\n   "
                                  ((error-value->string-handler)
                                   v
                                   (error-print-width)))))
             (if annot-str
                 (list "\n  annotation: "
                       (error-contract->adjusted-string
                        annot-str
                        rhombus-realm))
                 '())))
     rhombus-realm)
    (current-continuation-marks))))

(define (cases-failure who rest-args kwrest-args . base-args)
  (raise-bindings-failure who
                          "no matching case for arguments"
                          "arguments"
                          (append base-args rest-args)
                          #f))

(define-for-syntax (wrap-annotation-check who e count annot-str body-k)
  #`(call-with-values
     (lambda () #,e)
     (case-lambda
       #,@(if (eqv? count 1)
              (list #`[(val) #,(body-k
                                #'(val)
                                #`(raise-result-failure '#,who val '#,annot-str))])
              (list (with-syntax ([(val ...) (generate-temporaries
                                              (for/list ([_ (in-range count)])
                                                'val))])
                      #`[(val ...) #,(body-k
                                      #'(val ...)
                                      #`(raise-results-failure '#,who (list val ...) '#,annot-str))])
                    #`[(val) (raise-result-failure '#,who val '#,annot-str)]))
       [vals (raise-results-failure '#,who vals '#,annot-str)])))

(define-for-syntax (add-annotation-check who cvtr annot-str e)
  (cond
    [(and cvtr (converter-proc cvtr))
     => (lambda (proc)
          (wrap-annotation-check
           who e
           (converter-count cvtr) annot-str
           (lambda (vs raise)
             #`(#,proc
                #,@vs
                (lambda (#,@vs) (values #,@vs))
                (lambda () #,raise)))))]
    [else e]))

(define (raise-result-failure who val annot-str)
  (raise-binding-failure who "result" val annot-str))

(define (raise-results-failure who vals annot-str)
  (raise-bindings-failure who
                          "results do not satisfy annotation"
                          "results"
                          vals
                          annot-str))

(begin-for-syntax
  (define-syntax-class :kw-argument
    #:attributes (kw exp)
    #:datum-literals (group)
    (pattern (group kw:keyword)
             #:with exp (keyword->id-group #'kw))
    (pattern (group kw:keyword (_::block exp)))
    (pattern (group kw:keyword (btag::block g ...))
             #:with exp #`(#,group-tag (parsed #:rhombus/expr (rhombus-body-at btag g ...))))
    (pattern exp
             #:with kw #'#f)))

(define-for-syntax (parse-function-call rator-in extra-args stxes
                                        #:static? [static? #f]
                                        #:repetition? [repetition? #f]
                                        #:rator-stx [rator-stx #f] ; for error reporting
                                        #:srcloc [srcloc #f] ; for `relocate` on result
                                        #:rator-kind [rator-kind (if repetition? 'repetition 'function)]
                                        #:rator-arity [rator-arity #f]
                                        #:result-static-infos [result-static-infos #'()]
                                        #:can-anon-function? [can-anon-function? #f]
                                        #:wrap-call [wrap-call (lambda (e extra-rands) e)]
                                        #:wrap-rator [wrap-rator (lambda (rator extra-rands) rator)]
                                        #:wrap-extra-rand [wrap-extra-rand (lambda (rand extra-rands) rand)])
  (define (generate rands rsts amp dots kwrsts tag tail)
    (syntax-parse stxes
      [(_ args . _)
       (generate-call rator-in #'args extra-args rands rsts amp dots kwrsts tail
                      #:static? static?
                      #:repetition? repetition?
                      #:rator-stx rator-stx
                      #:srcloc srcloc
                      #:rator-kind rator-kind
                      #:rator-arity rator-arity
                      #:result-static-infos result-static-infos
                      #:props-stx tag ; intended to capture originalness or errortraceness
                      #:wrap-call wrap-call
                      #:wrap-rator wrap-rator
                      #:wrap-extra-rand wrap-extra-rand)]))
  (define (check-complex-allowed)
    (when (eq? rator-kind '|syntax class|)
      (raise-syntax-error #f "syntax class call cannot have splicing arguments" rator-stx)))
  (syntax-parse stxes
    #:datum-literals (group)
    [(_ (~and args (tag::parens rand ...)) . tail)
     #:when (complex-argument-splice? #'(rand ...))
     (check-complex-allowed)
     (values (complex-argument-splice-call rator-in #'args extra-args #'(rand ...)
                                           #:static? static?
                                           #:repetition? repetition?
                                           #:rator-stx rator-stx
                                           #:srcloc srcloc
                                           #:rator-kind rator-kind
                                           #:rator-arity rator-arity
                                           #:result-static-infos result-static-infos
                                           #:props-stx #'tag
                                           #:wrap-call wrap-call
                                           #:wrap-rator wrap-rator
                                           #:wrap-extra-rand wrap-extra-rand)
             #'tail
             #f)]
    [(_ (tag::parens rand ...
                     rep (group dots::...-expr)
                     (~optional (~and ((~and kwrst-tag group) _::~&-expr kwrst ...)
                                      (~parse kwrsts #'(kwrst-tag kwrst ...)))))
        . tail)
     (check-complex-allowed)
     (generate (syntax->list #'(rand ...)) #'rep #f #'dots.name (attribute kwrsts) #'tag #'tail)]
    [(_ (~or* (~and (tag::parens rand ...
                                 ((~and rst-tag group) amp::&-expr rst ...)
                                 (~optional (~and ((~and kwrst-tag group) _::~&-expr kwrst ...)
                                                  (~parse kwrsts #'(kwrst-tag kwrst ...)))))
                    (~parse rsts #'(rst-tag rst ...)))
              (~and (tag::parens rand ...
                                 ((~and kwrst-tag group) _::~&-expr kwrst ...))
                    (~parse kwrsts #'(kwrst-tag kwrst ...))))
        . tail)
     (check-complex-allowed)
     (generate (syntax->list #'(rand ...)) (attribute rsts) (and (attribute amp) #'amp.name) #f (attribute kwrsts) #'tag #'tail)]
    [(_ (~and args (tag::parens rand ...)) . tail)
     (define-values (formals rands)
       (let ([rands (syntax->list #'(rand ...))])
         (if can-anon-function?
             (convert-wildcards
              rands
              (lambda (rand)
                (syntax-parse rand
                  #:datum-literals (group)
                  [(group _::_-expr) #t]
                  [_ #f]))
              (lambda (rand id)
                (relocate+reraw
                 rand
                 #`(group (parsed #:rhombus/expr #,(relocate+reraw rand id))))))
             (values null rands))))
     (cond
       [(null? formals)
        (generate rands #f #f #f #f #'tag #'tail)]
       [else
        ;; anonymous-function shorthand
        (define static-infos (static-infos-and
                              (cond
                                [(syntax-local-static-info rator-in #'#%call-result)
                                 => (lambda (results)
                                      (find-call-result-at
                                       results
                                       (+ (length rands) (length extra-args))
                                       null
                                       #f))]
                                [else #'()])
                              result-static-infos))
        (define arity (arithmetic-shift 1 (length formals)))
        (values (let* ([fun (relocate+reraw
                             (or srcloc
                                 (respan (datum->syntax #f (list rator-in #'args))))
                             #`(lambda (#,@formals)
                                 #,(wrap-call
                                    #`(anonymous-body-call #,(wrap-rator rator-in extra-args) args #,rands
                                                           #,(for/list ([extra-arg (in-list extra-args)])
                                                               (discard-static-infos (wrap-extra-rand extra-arg extra-args)))
                                                           tag
                                                           #,static? #,rator-stx #,srcloc #,rator-kind
                                                           #,rator-arity)
                                    extra-args)))]
                       [fun (wrap-static-info* fun (indirect-get-function-static-infos))]
                       [fun (if (null? (syntax-e static-infos))
                                fun
                                (wrap-static-info fun #'#%call-result static-infos))]
                       [fun (wrap-static-info fun #'#%function-arity arity)])
                  fun)
                #'tail
                #t)])]))

(define-syntax (anonymous-body-call stx)
  ;; continue parsing a function call that was put into an anonymous-function body
  (syntax-parse stx
    [(_ rator-in args rands
        extra-args
        tag
        static? rator-stx srcloc rator-kind
        rator-arity)
     (define-values (call-e ignored-tail ignored-to-anonymous-function?)
       (generate-call #'rator-in #'args (syntax->list #'extra-args) (syntax->list #'rands) #f #f #f #f #'()
                      #:static? (syntax-e #'static?)
                      #:repetition? #f
                      #:rator-stx (let ([stx #'rator-stx])
                                    (and (syntax-e stx) stx))
                      #:srcloc (let ([stx #'srcloc])
                                 (and (syntax-e stx) stx))
                      #:rator-kind (syntax-e #'rator-kind)
                      #:rator-arity (syntax->datum #'rator-arity)
                      #:props-stx #'tag
                      #:call-result? #f))
     call-e]))

(define-for-syntax (generate-call rator-in args-stx extra-rands rands rsts amp dots kwrsts tail
                                  #:static? static?
                                  #:repetition? repetition?
                                  #:rator-stx rator-stx
                                  #:srcloc srcloc
                                  #:rator-kind rator-kind
                                  #:rator-arity rator-arity
                                  #:props-stx props-stx
                                  #:result-static-infos [extra-result-static-infos #'()]
                                  #:call-result? [call-result? #t]
                                  #:wrap-call [wrap-call (lambda (e extra-rands) e)]
                                  #:wrap-rator [wrap-rator (lambda (rator extra-rands) rator)]
                                  #:wrap-extra-rand [wrap-extra-rand (lambda (rand extra-rands) rand)])
  (values
   (syntax-parse rands
     [(rand::kw-argument ...)
      (handle-repetition
       repetition?
       (if repetition? rator-in (rhombus-local-expand rator-in))
       extra-rands
       (syntax->list #'(rand.exp ...))
       rsts amp dots
       kwrsts
       (lambda (rator extra-rands args rest-args kwrest-args rator-static-info)
         (define kws (syntax->list #'(rand.kw ...)))
         (when static?
           (define a (or rator-arity
                         (rator-static-info #'#%function-arity)))
           (when a
             (let* ([a (if (syntax? a) (syntax->datum a) a)])
               (check-arity rator-stx rator-in a (length extra-rands) kws rsts kwrsts (and static? rator-kind)))))
         (define unsafe (let ([v (rator-static-info #'#%unsafe)])
                          (and (identifier? v) v)))
         (define num-rands (length rands))
         (define arg-forms (apply append
                                  (for/list ([kw (in-list kws)]
                                             [arg (in-list args)])
                                    (if (syntax-e kw)
                                        (list kw (discard-static-infos arg))
                                        (list (discard-static-infos arg))))))
         (define w-rator (discard-static-infos (wrap-rator rator extra-rands)))
         (define w-extra-rands (for/list ([extra-rand (in-list extra-rands)])
                                 (discard-static-infos (wrap-extra-rand extra-rand extra-rands))))
         (define call-e (relocate+reraw
                         (or srcloc
                             (respan (datum->syntax #f (list (or rator-stx rator-in) args-stx))))
                         (datum->syntax
                          #'here
                          (cond
                            [kwrsts `(,(datum->syntax
                                        #'here
                                        `(,#'keyword-apply/map ,w-rator
                                                               ,@w-extra-rands
                                                               ,@arg-forms
                                                               ,(discard-static-infos rest-args)))
                                      ,(discard-static-infos kwrest-args))]
                            [rsts `(,#'apply ,w-rator
                                             ,@w-extra-rands
                                             ,@arg-forms
                                             ,(discard-static-infos rest-args))]
                            [unsafe
                             (define-values (binds args) (lift-arguments (append w-extra-rands arg-forms)))
                             #`(let #,binds
                                   (if (variable-reference-from-unsafe? (#%variable-reference))
                                       (#,unsafe #,@args)
                                       (#,w-rator #,@args)))]
                            [else `(,w-rator
                                    ,@w-extra-rands
                                    ,@arg-forms)])
                          #f
                          props-stx)))
         (define w-call-e (wrap-call call-e extra-rands))
         (define result-static-infos (static-infos-and
                                      (cond
                                        [(and call-result?
                                              (rator-static-info #'#%call-result))
                                         => (lambda (results)
                                              (define sorted-kws
                                                (sort (for/list ([kw (in-list kws)]
                                                                 #:when (syntax-e kw))
                                                        (syntax-e kw))
                                                      keyword<?))
                                              (find-call-result-at
                                               results
                                               (- (+ num-rands (length extra-rands))
                                                  (length sorted-kws))
                                               sorted-kws
                                               kwrsts))]
                                        [else #'()])
                                      extra-result-static-infos))
         (values w-call-e result-static-infos)))])
   tail
   ;; not converted to an anonymous function:
   #f))

;; does not support keyword arguments, for now
(define-for-syntax (find-call-result-at results arity kws kw-rest?)
  (syntax-parse results
    [(#:at_arities r)
     (let loop ([r #'r])
       (syntax-parse r
         [((a results) . rest)
          (define-values (mask req-kws allow-kws)
            (syntax-parse #'a
              [(mask req-kws allow-kws) (values #'mask #'req-kws #'allow-kws)]
              [mask (values #'mask #'() #'())]))
          (if (and (bitwise-bit-set? (syntax-e mask) arity)
                   (or kw-rest? (sorted-list-subset? (syntax->datum req-kws) kws))
                   (or (not (syntax-e allow-kws))
                       (sorted-list-subset? kws (syntax->datum allow-kws))))
              (if (or (not kw-rest?)
                      (and (not allow-kws)
                           (sorted-list-subset? (syntax->datum req-kws) kws)))
                  #'results
                  ;; we don't know whether the call matches or not, so stop searching
                  #'())
              (loop #'rest))]
         [_ #'()]))]
    [_ results]))

(define-for-syntax (handle-repetition repetition?
                                      rator ; already parsed as expression or repetition
                                      extra-rands
                                      rands
                                      rsts amp dots
                                      kwrsts
                                      k)
  (cond
    [(not repetition?)
     ;; parse arguments as expressions
     (define args
       (for/list ([arg (in-list rands)])
         (syntax-parse arg [e::expression #'e.parsed])))
     (define rest-args
       (cond
         [dots (repetition-as-list dots rsts 1)]
         [rsts (syntax-parse rsts [rst::expression (if amp
                                                       #`(to-list '#,amp rst.parsed)
                                                       #'rst.parsed)])]
         [else #''()]))
     (define kwrest-args
       (and kwrsts
            (syntax-parse kwrsts [kwrst::expression #'kwrst.parsed])))
     (define-values (e result-static-infos)
       (k rator extra-rands args rest-args kwrest-args (lambda (key) (syntax-local-static-info rator key))))
     (wrap-static-info* e result-static-infos)]
    [else
     ;; parse arguments as repetitions
     (define args
       (for/list ([arg (in-list rands)])
         (syntax-parse arg [rep::repetition #'rep.parsed])))
     (define n (length args))
     (let* ([args (append
                   (cons rator args)
                   (if rsts
                       (list (syntax-parse rsts
                               [rep::repetition
                                (if dots (list #'rep.parsed) #'rep.parsed)]))
                       null)
                   (if kwrsts
                       (list (syntax-parse kwrsts [rep::repetition #'rep.parsed]))
                       null))])
       (build-compound-repetition
        rator
        (append extra-rands args)
        #:is-sequence? (lambda (e) (pair? e))
        #:extract (lambda (e) (if (pair? e) (car e) e))
        (lambda one-rands+args
          (let* ([extra-rands (for/list ([r (in-list extra-rands)]
                                         [a (in-list one-rands+args)])
                                a)]
                 [one-rator+args (list-tail one-rands+args (length extra-rands))]
                 [one-rator (car one-rator+args)]
                 [one-args (cdr one-rator+args)]
                 [args (for/list ([i (in-range n)]
                                  [arg (in-list one-args)])
                         arg)]
                 [rest-args (and rsts
                                 #`(to-list '#,amp #,(list-ref one-args n)))]
                 [kwrest-args (and kwrsts (list-ref one-args (+ n (if rsts 1 0))))])
            ;; returns expression plus static infos for result elements
            (k one-rator extra-rands args (or rest-args #''()) kwrest-args
               (lambda (key)
                 (syntax-parse rator
                   [rep::repetition-info
                    (repetition-static-info-lookup #'rep.element-static-infos key)])))))))]))

(define-for-syntax (complex-argument-splice? gs-stx)
  ;; multiple `&` or `...`, or not at the end before `~&`,
  ;; or `~&` that's not at the very end?
  (define (not-kw-splice-only? gs-stx)
    (syntax-parse gs-stx
      #:datum-literals (group)
      [((group _::~&-expr rand ...)) #f]
      [() #f]
      [_ #t]))
  (let loop ([gs-stx gs-stx])
    (syntax-parse gs-stx
      #:datum-literals (group)
      [() #f]
      [((group _::&-expr rand ...) . gs)
       (or (loop #'gs) (not-kw-splice-only? #'gs))]
      [(g0 (group _::...-expr) . gs)
       (or (loop #'gs) (not-kw-splice-only? #'gs))]
      [((group _::~&-expr rand ...) . gs)
       (or (loop #'gs) (pair? (syntax-e #'gs)))]
      [(_ . gs) (loop #'gs)])))

(begin-for-syntax
  (struct arg (id stx))
  (struct arg-pos arg ())
  (struct arg-kw arg (kw))
  (struct arg-list arg ())
  (struct arg-map arg ()))

(define-for-syntax (complex-argument-splice-call rator args-stx extra-args gs-stx
                                                 #:static? static?
                                                 #:repetition? repetition?
                                                 #:rator-stx rator-stx
                                                 #:srcloc srcloc
                                                 #:rator-kind rator-kind
                                                 #:rator-arity rator-arity
                                                 #:result-static-infos result-static-infos
                                                 #:props-stx props-stx
                                                 #:wrap-call wrap-call
                                                 #:wrap-rator wrap-rator
                                                 #:wrap-extra-rand wrap-extra-rand)
  (define args
    (let loop ([gs-stx gs-stx])
      (syntax-parse gs-stx
        #:datum-literals (group)
        [() '()]
        [((group op::&-expr rand ...) . gs)
         (cons (arg-list (car (generate-temporaries '(list)))
                         #`(to-list 'op.name (rhombus-expression (#,group-tag rand ...))))
               (loop #'gs))]
        [(g0 (group dots::...-expr) . gs)
         (define-values (new-gs extras) (consume-extra-ellipses #'gs))
         (cons (arg-list (car (generate-temporaries '(list-repet)))
                         (repetition-as-list #'dots.name #'g0 1 extras))
               (loop new-gs))]
        [((group _::~&-expr rand ...) . gs)
         (cons (arg-map (car (generate-temporaries '(map)))
                        #`(rhombus-expression (#,group-tag rand ...)))
               (loop #'gs))]
        [((group kw:keyword (~optional (tag::block body ...))) . gs)
         (cons (arg-kw (car (generate-temporaries '(kw-arg)))
                       #`(~? (rhombus-body-at tag body ...)
                             (rhombus-expression #,(keyword->id-group #'kw)))
                       #'kw)
               (loop #'gs))]
        [(g . gs)
         (cons (arg-pos (car (generate-temporaries '(arg)))
                        #'(rhombus-expression g))
               (loop #'gs))])))
  (define extra-arg-ids (generate-temporaries extra-args))
  (define-values (term ignored-tail ignored-to-anon-func?)
    (let ([lists? (for/or ([arg (in-list args)])
                    (arg-list? arg))])
      (generate-call rator args-stx
                     (append
                      extra-arg-ids
                      (if lists?
                          null
                          (for/list ([arg (in-list args)]
                                     #:when (arg-pos? arg))
                            (arg-id arg))))
                     (for/list ([arg (in-list args)]
                                #:when (arg-kw? arg))
                       #`(group #,(arg-kw-kw arg)
                                (block (group (parsed #:rhombus/expr #,(arg-id arg))))))
                     (and lists?
                          #`(group
                             (parsed
                              #:rhombus/expr
                              (append
                               #,@(for/list ([arg (in-list args)]
                                             #:when (or (arg-pos? arg)
                                                        (arg-list? arg)))
                                    (cond
                                      [(arg-pos? arg) #`(list #,(arg-id arg))]
                                      [else (arg-id arg)]))))))
                     #f #f
                     (let ([maps (for/list ([arg (in-list args)]
                                            #:when (arg-map? arg))
                                   (arg-id arg))])
                       (cond
                         [(null? maps) #f]
                         [(null? (cdr maps)) #`(group (parsed #:rhombus/expr #,(car maps)))]
                         [else #`(group (parsed #:rhombus/expr (merge-keyword-argument-maps #,@maps)))]))
                     #'#f
                     #:static? static?
                     #:repetition? repetition?
                     #:rator-stx rator-stx
                     #:srcloc srcloc
                     #:rator-kind rator-kind
                     #:rator-arity rator-arity
                     #:result-static-infos result-static-infos
                     #:props-stx props-stx
                     #:wrap-call wrap-call
                     #:wrap-rator wrap-rator
                     #:wrap-extra-rand wrap-extra-rand)))
  (define term-static-infos (extract-static-infos term))
  (wrap-static-info*
   #`(let (#,@(for/list ([extra-arg-id (in-list extra-arg-ids)]
                         [extra-arg (in-list extra-args)])
                #`[#,extra-arg-id #,extra-arg])
           #,@(for/list ([arg (in-list args)])
                #`[#,(arg-id arg) #,(arg-stx arg)]))
       #,(discard-static-infos term))
   term-static-infos))

(void (set-primitive-who! 'application '|function call|))
(define function-call-who '|function call|)

(define (check-immutable-hash ht)
  (unless (immutable-hash? ht)
    (raise-arguments-error* function-call-who rhombus-realm
                            "not an immutable map for keyword arguments"
                            "given" ht)))

(define keyword-apply/map
  (make-keyword-procedure
   (lambda (kws kw-args proc . args+rest)
     ;; currying makes it easier to preserve order when `~&` is last
     (lambda (kw-ht)
       (check-immutable-hash kw-ht)
       (define all-kw-ht
         (for/fold ([ht kw-ht]) ([kw (in-list kws)]
                                 [arg (in-list kw-args)])
           (unless (eq? (hash-ref kw-ht kw unsafe-undefined)
                        unsafe-undefined)
             (raise-arguments-error* function-call-who rhombus-realm
                                     "duplicate keyword in spliced map and direct keyword arguments"
                                     "keyword" kw))
           (hash-set ht kw arg)))
       (define all-kws (sort (append kws (hash-keys kw-ht)) keyword<?))
       (keyword-apply proc
                      all-kws
                      (for/list ([kw (in-list all-kws)])
                        (hash-ref all-kw-ht kw))
                      (let loop ([args+rest args+rest])
                        (cond
                          [(null? (cdr args+rest)) (car args+rest)]
                          [else (cons (car args+rest) (loop (cdr args+rest)))])))))))

(define (merge-keyword-argument-maps ht . hts)
  (define (merge a b)
    (let-values ([(a b)
                  (if ((hash-count a) . < . (hash-count b))
                      (values b a)
                      (values a b))])
      (for/fold ([new-a a]) ([(kw arg) (in-immutable-hash b)])
        (unless (eq? (hash-ref a kw unsafe-undefined)
                     unsafe-undefined)
          (raise-arguments-error* function-call-who rhombus-realm
                                  "duplicate keyword in keyword-argument maps"
                                  "keyword" kw))
        (hash-set new-a kw arg))))
  (check-immutable-hash ht)
  (for ([ht (in-list hts)])
    (check-immutable-hash ht))
  (for/fold ([all-ht ht]) ([ht (in-list hts)])
    (merge all-ht ht)))

(define-for-syntax (build-anonymous-function terms form
                                             #:adjustments [adjustments #f]
                                             #:argument-static-infoss [argument-static-infoss '()])
  (define-values (formals convert-ones)
    (convert-wildcards
     (syntax->list terms)
     (lambda (term)
       (syntax-parse term
         [_::_-expr #t]
         [_ #f]))
     (lambda (term id)
       (lambda (static-infos)
         (relocate+reraw
          term
          #`(parsed
             #:rhombus/expr
             #,(wrap-static-info* (relocate+reraw term id) static-infos)))))))
  (define converted
    (let loop ([convert-ones convert-ones]
               [static-infoss (extract-added-static-infos argument-static-infoss (length formals))])
      (cond
        [(null? convert-ones)
         (unless (null? static-infoss)
           (error "number of wildcards must match number of static infos"))
         '()]
        [(procedure? (car convert-ones))
         (cons ((car convert-ones) (car static-infoss))
               (loop (cdr convert-ones) (cdr static-infoss)))]
        [else
         (cons (car convert-ones)
               (loop (cdr convert-ones) static-infoss))])))
  (define arity (arithmetic-shift 1 (length formals)))
  (define-values (prefix-args wrap-body)
    (if adjustments
        (values (treelist->list (entry-point-adjustment-prefix-arguments adjustments))
                (let ([wrap (entry-point-adjustment-wrap-body adjustments)])
                  (lambda (body)
                    (wrap-expression (wrap arity #`(parsed #:rhombus/expr #,body))))))
        (values '() values)))
  (define-values (body static-infos)
    (let ([body (rhombus-local-expand (wrap-body #`(rhombus-expression #,converted)))])
      (values (discard-static-infos body)
              (extract-static-infos body))))
  (define shifted-arity (arithmetic-shift arity (length prefix-args)))
  (let* ([fun (relocate+reraw
               form
               #`(lambda (#,@prefix-args #,@formals)
                   #,(discard-static-infos body)))]
         [fun (wrap-static-info* fun (indirect-get-function-static-infos))]
         [fun (if (null? static-infos)
                  fun
                  (wrap-static-info fun #'#%call-result static-infos))]
         [fun (wrap-static-info fun #'#%function-arity shifted-arity)])
    fun))

(define-for-syntax (convert-wildcards stxs convert? convert-one)
  (let loop ([stxs stxs])
    (cond
      [(null? stxs) (values '() null)]
      [else
       (define stx (car stxs))
       (define-values (formals converted) (loop (cdr stxs)))
       (cond
         [(convert? stx)
          (define id (car (generate-temporaries '(arg))))
          (values (cons id formals) (cons (convert-one stx id) converted))]
         [else
          (values formals (cons stx converted))])])))

(define-for-syntax (lift-arguments args)
  (let loop ([args args] [bind-accum null] [arg-accum null])
    (cond
      [(null? args) (values (reverse bind-accum) (reverse arg-accum))]
      [(keyword? (syntax-e (car args)))
       (loop (cdr args) bind-accum (cons (car args) arg-accum))]
      [else
       (define tmp (car (generate-temporaries (list (car args)))))
       (loop (cdr args)
             (cons #`[#,tmp #,(car args)] bind-accum)
             (cons tmp arg-accum))])))

(begin-for-syntax
  (set-parse-function-call! parse-function-call))
