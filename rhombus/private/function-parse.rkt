#lang racket/base
(require (for-syntax racket/base
                     (only-in racket/function normalize-arity)
                     racket/syntax
                     syntax/parse/pre
                     enforest/name-parse
                     shrubbery/property
                     shrubbery/print
                     "hash-set.rkt"
                     "srcloc.rkt"
                     "consistent.rkt"
                     "with-syntax.rkt"
                     "tag.rkt"
                     "same-expression.rkt"
                     "static-info-pack.rkt")
         racket/unsafe/undefined
         "provide.rkt"
         "parens.rkt"
         "expression.rkt"
         "binding.rkt"
         "definition.rkt"
         "entry-point.rkt"
         "parse.rkt"
         "nested-bindings.rkt"
         "name-root.rkt"
         "call-result-key.rkt"
         "function-arity-key.rkt"
         "function-indirect-key.rkt"
         "values-key.rkt"
         "static-info.rkt"
         "annotation.rkt"
         "repetition.rkt"
         "rest-marker.rkt"
         "op-literal.rkt"
         (submod "ellipsis.rkt" for-parse)
         (only-in "list.rkt" List)
         (submod "annotation.rkt" for-class)
         (submod "equal.rkt" for-parse)
         (only-in "equal.rkt"
                  [= rhombus=])
         "dotted-sequence-parse.rkt"
         "lambda-kwrest.rkt"
         "error.rkt"
         (submod "dot.rkt" for-dot-provider)
         "dot-parse.rkt"
         "realm.rkt"
         "compound-repetition.rkt"
         "function-arity.rkt"
         "wrap-expression.rkt")

(module+ for-build
  (provide (for-syntax :kw-binding
                       :kw-opt-binding
                       :rhombus-kw-opt-binding
                       :ret-annotation
                       :rhombus-ret-annotation
                       :maybe-arg-rest
                       :non-...-binding
                       build-function
                       build-case-function
                       maybe-add-function-result-definition
                       parse-anonymous-function-arity)))

(module+ for-call
  (provide (for-syntax parse-function-call)
           raise-result-failure))

(define-annotation-syntax Function (identifier-annotation #'procedure? #'()))

(begin-for-syntax
  (define-syntax-class :non-...
    #:attributes ()
    (pattern form
             #:when (syntax-parse #'form
                      #:datum-literals (group)
                      [(group _::...-bind) #f]
                      [(group (~or _::&-bind _::~&-bind) . _) #f]
                      [_ #t])))
  (define-syntax-class :non-...-binding
    (pattern form::non-...
             #:with arg::binding #'form
             #:with parsed #'arg.parsed))
  
  (define (keyword->binding kw)
    #`(group #,(datum->syntax kw (string->symbol (keyword->string (syntax-e kw))) kw)))

  (define-syntax-class :has-kw-binding
    #:attributes [kw parsed]
    #:datum-literals (group)
    (pattern (group kw:keyword (_::block (group a ...+)))
             #:cut
             #:with arg::binding #'(group a ...)
             #:attr parsed #'arg.parsed)
    (pattern (group kw:keyword)
             #:cut
             #:with arg::binding (keyword->binding #'kw)
             #:attr parsed #'arg.parsed))

  (define-syntax-class :plain-binding
    #:attributes [kw parsed]
    (pattern arg::non-...-binding
             #:attr kw #'#f
             #:attr parsed #'arg.parsed))

  (define-syntax-class :kw-binding
    #:attributes [kw parsed]
    (pattern ::has-kw-binding)
    (pattern ::plain-binding))

  ;; used when just extracting an arity:
  (define-syntax-class :kw-arity-arg
    #:attributes [kw]
    #:datum-literals (block group)
    (pattern (group kw:keyword . _))
    (pattern (group kw:keyword))
    (pattern arg::non-...
             #:attr kw #'#f))
  
  (define-syntax-class :kw-opt-binding
    #:attributes [kw parsed default]
    #:datum-literals (block group)
    (pattern (group kw:keyword (block (group a::not-equal ...+ eq::equal e ...+)))
             #:cut
             #:with default #'(group e ...)
             #:do [(check-argument-annot #'default #'eq)]
             #:with arg::binding #'(group a ...)
             #:attr parsed #'arg.parsed)
    (pattern (group kw:keyword (block (group a ...+ (b-tag::block b ...))))
             #:cut
             #:with arg::binding #'(group a ...)
             #:with default #'(group (parsed (rhombus-body-at b-tag b ...)))
             #:attr parsed #'arg.parsed)
    (pattern (group kw:keyword eq::equal e ...+)
             #:cut
             #:with default #'(group e ...)
             #:do [(check-argument-annot #'default #'eq)]
             #:with arg::binding (keyword->binding #'kw)
             #:attr parsed #'arg.parsed)
    (pattern ::has-kw-binding
             #:attr default #'#f)
    (pattern (group a::not-equal ...+ eq::equal e ...+)
             #:cut
             #:with default #'(group e ...)
             #:do [(check-argument-annot #'default #'eq)]
             #:with arg::binding #'(group a ...)
             #:attr kw #'#f
             #:attr parsed #'arg.parsed)
    (pattern (group a ...+ (b-tag::block b ...))
             #:cut
             #:with (~not (_:keyword)) #'(a ...)
             #:with arg::binding #'(group a ...)
             #:with default #'(group (parsed (rhombus-body-at b-tag b ...)))
             #:attr kw #'#f
             #:attr parsed #'arg.parsed)
    (pattern ::plain-binding
             #:attr default #'#f))

  (define-syntax-class :rhombus-kw-opt-binding
    #:attributes [maybe_keyword parsed maybe_expr]
    (pattern arg::kw-opt-binding
             #:attr parsed #'arg.parsed
             #:attr maybe_keyword (and (syntax-e #'arg.kw)
                                       #'arg.kw)
             #:attr maybe_expr (and (syntax-e #'arg.default)
                                    #'arg.default)))

  ;; used when just extracting an arity:
  (define-syntax-class :kw-opt-arity-arg
    #:attributes [kw default]
    #:datum-literals (block group)
    (pattern (group kw:keyword (block (group _::not-equal ...+ _::equal _ ...+)))
             #:with default #'#t)
    (pattern (group kw:keyword (block (group _ ...+ (b-tag::block . _))))
             #:with default #'#t)
    (pattern (group kw:keyword _::equal _ ...+)
             #:with default #'#t)
    (pattern (group _::not-equal ...+ _::equal _ ...+)
             #:with default #'#t
             #:attr kw #'#f)
    (pattern (group a ...+ (b-tag::block . _))
             #:with (~not (_:keyword)) #'(a ...)
             #:with default #'#t
             #:attr kw #'#f)
    (pattern ::kw-arity-arg
             #:attr default #'#f))

  (define (check-argument-annot g eq-op)
    (syntax-parse g
      #:datum-literals (group)
      [(group _ ... ann-op::annotate-op _ ...)
       (raise-syntax-error #f
                           (string-append
                            "immediate annotation operator not allowed in default-value expression;\n"
                            " use parentheses around the expression if the annotation was intended,\n"
                            " since parentheses avoid the appearance of annotating the binding\n"
                            " instead of the expression")
                           #'ann-op.name
                           #f
                           (list eq-op))]
      [(group _ ... eq2::equal _ ...)
       (raise-syntax-error #f
                           (string-append "multiple immediate equals not allowed in this group"
                                          "\n use parentheses to disambiguate")
                           eq-op
                           #f
                           (list #'eq2))]
      [_ (void)]))
  
  (define-syntax-class :not-block
    #:datum-literals (op parens braces brackets quotes)
    (pattern _:identifier)
    (pattern (op . _))
    (pattern (parens . _))
    (pattern (braces . _))
    (pattern (brackets . _))
    (pattern (quotes . _)))

  (define-splicing-syntax-class :ret-annotation
    #:attributes (static-infos ; can be `((#%values (static-infos ...)))` for multiple results
                  converter)   ; `(lambda (arg ... success-k fail-k) ....)` with one ` arg` for each result
    #:description "return annotation"
    #:datum-literals (block group)
    (pattern (~seq ann-op::annotate-op (~optional vls:identifier) (_::parens g ...))
             #:when (or (not (attribute vls))
                        (free-identifier=? #'vls #'values))
             #:with (c::annotation ...) #'(g ...)
             #:with (arg ...) (generate-temporaries #'(g ...))
             #:do [(define-values (sis cvtr)
                     (syntax-parse #'(c.parsed ...)
                       [(c-parsed::annotation-predicate-form ...)
                        (values #'((#%values (c-parsed.static-infos ...)))
                                (if (syntax-e #'ann-op.check?)
                                    #'(lambda (arg ... success-k fail-k)
                                        (if (and (c-parsed.predicate arg) ...)
                                            (success-k arg ...)
                                            (fail-k)))
                                    #'#f))]
                       [(c-parsed::annotation-binding-form ...)
                        #:do [(unless (syntax-e #'ann-op.check?)
                                (for ([c (in-list (syntax->list #'(c ...)))]
                                      [c-p (in-list (syntax->list #'(c.parsed ...)))])
                                  (syntax-parse c-p
                                    [_::annotation-predicate-form (void)]
                                    [_ (raise-unchecked-disallowed #'ann-op.name c)])))]
                        #:with (arg-parsed::binding-form ...) #'(c-parsed.binding ...)
                        #:with (arg-impl::binding-impl ...) #'((arg-parsed.infoer-id () arg-parsed.data) ...)
                        #:with (all-arg-info::binding-info ...) #'(arg-impl.info ...)
                        (values #'((#%values (c-parsed.static-infos ...)))
                                #`(lambda (arg ... success-k fail-k)
                                    #,(let loop ([args (syntax->list #'(arg ...))]
                                                 [arg-impl-infos (syntax->list #'(arg-impl.info ...))]
                                                 [bodys (syntax->list #'(c-parsed.body ...))])
                                        (cond
                                          [(null? args) #'(success-k arg ...)]
                                          [else
                                           (with-syntax-parse ([arg-info::binding-info (car arg-impl-infos)]
                                                               [((bind-id bind-use . bind-static-infos) ...) #'arg-info.bind-infos]
                                                               [v (car args)])
                                             #`(arg-info.matcher-id v
                                                                    arg-info.data
                                                                    if/blocked
                                                                    (begin
                                                                      (arg-info.committer-id v arg-info.data)
                                                                      (arg-info.binder-id v arg-info.data)
                                                                      (define-static-info-syntax/maybe bind-id . bind-static-infos)
                                                                      ...
                                                                      (let ([#,(car args) #,(car bodys)])
                                                                        #,(loop (cdr args) (cdr arg-impl-infos) (cdr bodys))))
                                                                    (fail-k)))]))))]))]
             #:attr static-infos sis
             #:attr converter cvtr)
    (pattern (~seq ann-op::annotate-op ctc0::not-block ctc::not-block ...)
             #:with c::annotation (no-srcloc #`(#,group-tag ctc0 ctc ...))
             #:do [(define-values (sis cvtr)
                     (syntax-parse #'c.parsed
                       [c-parsed::annotation-predicate-form
                        (values #'c-parsed.static-infos
                                (if (syntax-e #'ann-op.check?)
                                    #'(lambda (v success-k fail-k)
                                        (if (c-parsed.predicate v)
                                            (success-k v)
                                            (fail-k)))
                                    #'#f))]
                       [c-parsed::annotation-binding-form
                        #:do [(unless (syntax-e #'ann-op.check?)
                                (raise-unchecked-disallowed #'ann-op.name #'c))]
                        #:with arg-parsed::binding-form #'c-parsed.binding
                        #:with arg-impl::binding-impl #'(arg-parsed.infoer-id () arg-parsed.data)
                        #:with arg-info::binding-info #'arg-impl.info
                        (syntax-parse #'arg-info.bind-infos
                          [((bind-id bind-use . bind-static-infos) ...)
                           (values #'c-parsed.static-infos
                                   #'(lambda (v success-k fail-k)
                                       (arg-info.matcher-id v
                                                            arg-info.data
                                                            if/blocked
                                                            (begin
                                                              (arg-info.committer-id v arg-info.data)
                                                              (arg-info.binder-id v arg-info.data)
                                                              (define-static-info-syntax/maybe bind-id . bind-static-infos)
                                                              ...
                                                              (success-k c-parsed.body))
                                                            (fail-k))))])]))]
             #:attr static-infos sis
             #:attr converter cvtr)
    (pattern (~seq)
             #:attr static-infos #'()
             #:attr converter #'#f))

  (define-splicing-syntax-class :rhombus-ret-annotation
    #:attributes (count
                  maybe_converter
                  static_info
                  annotation_string)
    (pattern r::ret-annotation
             #:attr count (syntax-parse #'r.static-infos
                            #:literals (#%values)
                            [((#%values (si ...)))
                             (length (syntax->list #'(si ...)))]
                            [_ 1])
             #:attr maybe_converter (and (syntax-e #'r.converter)
                                         #'(parsed #:rhombus/expr r.converter))
             #:attr static_info (unpack-static-infos #'r.static-infos)
             #:attr annotation_string (syntax-parse (and (syntax-e #'r.converter) #'r)
                                        [(_ . t) (shrubbery-syntax->string #`(#,group-tag . t))]
                                        [_ "Any"])))

  (define-splicing-syntax-class :pos-rest
    #:attributes [arg parsed]
    #:datum-literals (group)
    (pattern (~seq (group _::&-bind a ...))
             #:with arg::non-...-binding #'(group a ...)
             #:with parsed #'arg.parsed)
    (pattern (~seq e::non-...-binding (~and ooo (group _::...-bind)))
             #:with (::pos-rest)
             #'((group & List (parens e ooo)))))

  (define-splicing-syntax-class :kwp-rest
    #:attributes [kwarg kwparsed]
    #:datum-literals (group)
    (pattern (~seq (group _::~&-bind a ...))
             #:with kwarg::non-...-binding #'(group a ...)
             #:with kwparsed #'kwarg.parsed
             #:attr arg #'#f
             #:attr parsed #'#f))

  (define-splicing-syntax-class :maybe-arg-rest
    #:attributes [arg parsed kwarg kwparsed]
    #:datum-literals (group)
    (pattern (~seq
              (~alt (~optional ::pos-rest #:defaults ([arg #'#f] [parsed #'#f]))
                    (~optional ::kwp-rest #:defaults ([kwarg #'#f] [kwparsed #'#f])))
              ...)))
  
  ;; used when just extracting an arity:
  (define-splicing-syntax-class :pos-arity-rest
    #:attributes [rest?]
    #:datum-literals (group)
    (pattern (~seq (group _::&-bind _ ...))
             #:attr rest? #'#t)
    (pattern (~seq _::non-... (group _::...-bind))
             #:attr rest? #'#t))
  (define-splicing-syntax-class :kwp-arity-rest
    #:attributes [kwrest?]
    #:datum-literals (group)
    (pattern (~seq (group _::~&-bind _ ...))
             #:attr kwrest? #'#t))
  (define-splicing-syntax-class :maybe-rest-arity-arg
    #:attributes [rest? kwrest?]
    #:datum-literals (group)
    (pattern (~seq
              (~alt (~optional ::pos-arity-rest #:defaults ([rest? #'#f]))
                    (~optional ::kwp-arity-rest #:defaults ([kwrest? #'#f])))
              ...))))

(define-for-syntax (parse-anonymous-function-arity stx)
  (syntax-parse stx
    #:datum-literals (group block alts)
    [(form-id (alts-tag::alts
               (block (group (_::parens arg::kw-arity-arg ... rest::maybe-rest-arity-arg)
                             . _))
               ...+))
     (union-arity-summaries
      (for/list ([arg-kws (in-list (syntax->list #'((arg.kw ...) ...)))]
                 [rest? (in-list (syntax->list #'(rest.rest? ...)))]
                 [kw-rest? (in-list (syntax->list #'(rest.kwrest? ...)))])
        (define kws (syntax->list arg-kws))
        (summarize-arity arg-kws (datum->syntax #f (map (lambda (x) #f) kws)) (syntax-e rest?) (syntax-e kw-rest?))))]
    [(form-id (parens-tag::parens arg::kw-opt-arity-arg ... rest::maybe-rest-arity-arg) . _)
     (summarize-arity #'(arg.kw ...) #'(arg.default ...) (syntax-e #'rest.rest?) (syntax-e #'rest.kwrest?))]
    [_ #f]))

(begin-for-syntax

  (struct fcase (kws args arg-parseds rest-arg rest-arg-parsed kwrest-arg kwrest-arg-parsed converter rhs))

  ;; usage: (fcase-pos fcase-args fc) or (fcase-pos fcase-arg-parseds fc)
  (define (fcase-pos get-args fc)
    (for/list ([kw (in-list (fcase-kws fc))]
               [arg (in-list (get-args fc))]
               #:when (not (syntax-e kw)))
      arg))

  (define (build-function adjustments
                          function-name
                          kws args arg-parseds defaults
                          rest-arg rest-parsed
                          kwrest-arg kwrest-parsed
                          converter
                          rhs
                          src-ctx)
    (with-syntax-parse ([(arg-parsed::binding-form ...) arg-parseds]
                        [(arg-impl::binding-impl ...) #'((arg-parsed.infoer-id () arg-parsed.data) ...)]
                        [(arg-info::binding-info ...) #'(arg-impl.info ...)])
      (with-syntax ([(tmp-id ...) (generate-temporaries #'(arg-info.name-id ...))]
                    [(arg ...) args]
                    [rhs rhs]
                    [(maybe-rest-tmp (maybe-rest-tmp* ...) maybe-match-rest)
                     (if (syntax-e rest-arg)
                         (with-syntax-parse ([rest::binding-form rest-parsed]
                                             [rest-impl::binding-impl #'(rest.infoer-id () rest.data)]
                                             [rest-info::binding-info #'rest-impl.info])
                           #`(rest-tmp (#:rest rest-tmp) (rest-tmp rest-info #,rest-arg #f)))
                         #'(() () #f))]
                    [((maybe-kwrest-tmp ...) maybe-match-kwrest)
                     (if (syntax-e kwrest-arg)
                         (with-syntax-parse ([kwrest::binding-form kwrest-parsed]
                                             [kwrest-impl::binding-impl #'(kwrest.infoer-id () kwrest.data)]
                                             [kwrest-info::binding-info #'kwrest-impl.info])
                           #`((#:kwrest kwrest-tmp) (kwrest-tmp kwrest-info #,kwrest-arg #f)))
                         #'(() #f))])
        (with-syntax ([(((arg-form ...) arg-default) ...)
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
                            (list (list kw arg+default) default)]))])
          (define arity (summarize-arity kws defaults (syntax-e rest-arg) (syntax-e kwrest-arg)))               
          (define body
            (wrap-expression
             ((entry_point_meta.Adjustment-wrap-body adjustments)
              arity
              #`(parsed
                 #:rhombus/expr
                 (nested-bindings
                  #,function-name
                  #f ; try-next
                  argument-binding-failure
                  (tmp-id arg-info arg arg-default)
                  ...
                  maybe-match-rest
                  maybe-match-kwrest
                  (begin
                    (add-annotation-check
                     #,function-name #,converter #f
                     (rhombus-body-expression rhs))))))))
          (define (adjust-args args)
            (append (entry_point_meta.Adjustment-prefix-arguments adjustments)
                    args))
          (values
           (relocate+reraw
            (respan src-ctx)
            (if (syntax-e kwrest-arg)
                #`(lambda/kwrest
                   #,(adjust-args #'(arg-form ... ...))
                   maybe-rest-tmp* ... maybe-kwrest-tmp ...
                   #,body)
                #`(lambda #,(adjust-args #'(arg-form ... ... . maybe-rest-tmp))
                    #,body)))
           arity)))))

  (define (build-case-function adjustments
                               function-name main-converter-stx
                               kwss-stx argss-stx arg-parsedss-stx
                               rest-args-stx rest-parseds-stx
                               kwrest-args-stx kwrest-parseds-stx
                               converters-stx
                               rhss-stx
                               src-ctx)
    (define kwss (map syntax->list (syntax->list kwss-stx)))
    (define argss (map syntax->list (syntax->list argss-stx)))
    (define arg-parsedss (map syntax->list (syntax->list arg-parsedss-stx)))
    (define rest-args (syntax->list rest-args-stx))
    (define rest-parseds (syntax->list rest-parseds-stx))
    (define kwrest-args (syntax->list kwrest-args-stx))
    (define kwrest-parseds (syntax->list kwrest-parseds-stx))
    (define converters (syntax->list converters-stx))
    (define rhss (syntax->list rhss-stx))
    (define n+sames
      (group-by-counts
       (map fcase kwss argss arg-parsedss rest-args rest-parseds kwrest-args kwrest-parseds converters rhss)))
    (define pos-arity
      (normalize-arity
       (let ([adj (length (entry_point_meta.Adjustment-prefix-arguments adjustments))])
         (for/list ([n+same (in-list n+sames)])
           (define n (car n+same))
           (cond
             [(negative? n) (arity-at-least (+ (- (add1 n)) adj))]
             [else (+ n adj)])))))
    (define allowed-kws
      (cond
        [(ormap syntax-e kwrest-args) #f]
        [else (sort (filter keyword? (set->list (list->set (apply append (syntax->datum kwss-stx))))) keyword<?)]))
    (define required-kws
      (cond
        [(pair? kwss)
         (sort (filter keyword? (set->list (apply set-intersect (map list->set (syntax->datum kwss-stx))))) keyword<?)]
        [else '()]))
    (define kws? (not (null? allowed-kws)))
    (define reduce-keyword-arity
      (cond
        [(null? allowed-kws) values]
        [(and (null? required-kws) (not allowed-kws)) values]
        [else
         (lambda (stx)
           #`(procedure-reduce-keyword-arity-mask/infer-name
              #,stx
              #,(arity->mask-syntax pos-arity)
              '#,required-kws
              '#,allowed-kws))]))
    (define arity (if (null? allowed-kws)
                      (arity->mask-syntax pos-arity)
                      `(,(arity->mask-syntax pos-arity) ,required-kws ,allowed-kws)))
    (values
     (relocate+reraw
      (respan src-ctx)
      (reduce-keyword-arity
       #`(case-lambda/kwrest
          #,@(for/list ([n+same (in-list n+sames)])
               (define n (car n+same))
               (define same (cdr n+same))
               (with-syntax ([(try-next pos-arg-id ...) (generate-temporaries
                                                         (cons 'try-next
                                                               (fcase-pos fcase-args (find-matching-case n same))))]
                             [(maybe-rest-tmp ...) (if (negative? n)
                                                       #'(#:rest rest-tmp)
                                                       #'())]
                             [maybe-rest-tmp-use (if (negative? n)
                                                     #'rest-tmp
                                                     #'null)]
                             [(maybe-kwrest-tmp ...) (if kws?
                                                         #'(#:kwrest kwrest-tmp)
                                                         #'())]
                             [maybe-kwrest-tmp-use (if kws?
                                                       #'kwrest-tmp
                                                       #''#hashalw())])
                 #`[(#,@(entry_point_meta.Adjustment-prefix-arguments adjustments) pos-arg-id ...)
                    maybe-rest-tmp ... maybe-kwrest-tmp ...
                    #,(let loop ([same same])
                        (cond
                          [(null? same)
                           #`(cases-failure '#,function-name maybe-rest-tmp-use maybe-kwrest-tmp-use pos-arg-id ...)]
                          [else
                           (define fc (car same))
                           (define-values (this-args wrap-adapted-arguments)
                             (adapt-arguments-for-count fc n #'(pos-arg-id ...) #'rest-tmp
                                                        (and kws? #'kwrest-tmp)
                                                        #'try-next))
                           (with-syntax-parse ([(arg ...) (fcase-args fc)]
                                               [(arg-parsed::binding-form ...) (fcase-arg-parseds fc)]
                                               [(arg-impl::binding-impl ...) #'((arg-parsed.infoer-id () arg-parsed.data) ...)]
                                               [(arg-info::binding-info ...) #'(arg-impl.info ...)]
                                               [(this-arg-id ...) this-args]
                                               [converter (fcase-converter fc)]
                                               [rhs (fcase-rhs fc)]
                                               [(maybe-match-rest (maybe-bind-rest-seq ...) (maybe-bind-rest ...))
                                                (cond
                                                  [(syntax-e (fcase-rest-arg fc))
                                                   (define rest-parsed (fcase-rest-arg-parsed fc))
                                                   (with-syntax-parse ([rest::binding-form rest-parsed]
                                                                       [rest-impl::binding-impl #'(rest.infoer-id () rest.data)]
                                                                       [rest-info::binding-info #'rest-impl.info])
                                                     #`((rest-tmp rest-info #,(fcase-rest-arg fc) #f)
                                                        ((rest-info.committer-id rest-tmp rest-info.data)
                                                         (rest-info.binder-id rest-tmp rest-info.data))
                                                        ((define-static-info-syntax/maybe rest-info.bind-id rest-info.bind-static-info ...)
                                                         ...)))]
                                                  [else
                                                   #'(#f () ())])]
                                               [(maybe-match-kwrest (maybe-bind-kwrest-seq ...) (maybe-bind-kwrest ...))
                                                (cond
                                                  [(syntax-e (fcase-kwrest-arg fc))
                                                   (define kwrest-parsed (fcase-kwrest-arg-parsed fc))
                                                   (with-syntax-parse ([kwrest::binding-form kwrest-parsed]
                                                                       [kwrest-impl::binding-impl #'(kwrest.infoer-id () kwrest.data)]
                                                                       [kwrest-info::binding-info #'kwrest-impl.info])
                                                     #`((kwrest-tmp kwrest-info #,(fcase-kwrest-arg fc) #f)
                                                        ((kwrest-info.committer-id kwrest-tmp kwrest-info.data)
                                                         (kwrest-info.binder-id kwrest-tmp kwrest-info.data))
                                                        ((define-static-info-syntax/maybe kwrest-info.bind-id kwrest-info.bind-static-info ...)
                                                         ...)))]
                                                  [else
                                                   #'(#f () ())])])
                             #`(let ([try-next (lambda () #,(loop (cdr same)))])
                                 #,(wrap-adapted-arguments
                                    #`(nested-bindings
                                       #,function-name
                                       try-next
                                       argument-binding-failure
                                       (this-arg-id arg-info arg #f)
                                       ...
                                       maybe-match-rest
                                       maybe-match-kwrest
                                       (begin
                                         (arg-info.committer-id this-arg-id arg-info.data) ...
                                         (arg-info.binder-id this-arg-id arg-info.data) ...
                                         (begin
                                           (define-static-info-syntax/maybe arg-info.bind-id arg-info.bind-static-info ...)
                                           ...)
                                         ...
                                         maybe-bind-rest-seq ...
                                         maybe-bind-rest ...
                                         maybe-bind-kwrest-seq ...
                                         maybe-bind-kwrest ...
                                         #,(wrap-expression
                                            ((entry_point_meta.Adjustment-wrap-body adjustments)
                                             (summarize-arity (fcase-kws fc) (for/list ([kw (in-list (fcase-kws fc))])
                                                                               #'#f)
                                                              (syntax-e (fcase-rest-arg fc)) (syntax-e (fcase-kwrest-arg fc)))
                                             #`(parsed
                                                #:rhombus/expr
                                                (add-annotation-check
                                                 #,function-name
                                                 converter #,main-converter-stx
                                                 (rhombus-body-expression rhs))))))))))]))])))))
     arity))

  (define (maybe-add-function-result-definition name static-infoss static-infos arity defns)
    (define result-info?
      (and (pair? static-infoss)
           (pair? (syntax-e (car static-infoss)))
           (for/and ([static-infos (in-list (cdr static-infoss))])
             (same-expression? (car static-infoss) static-infos))))
    (if (or result-info? arity (pair? (syntax-e static-infos)))
        (cons #`(define-static-info-syntax #,name
                  #,@(append
                      (if result-info?
                          (list #`(#%call-result #,(car static-infoss)))
                          null)
                      (if arity
                          (list #`(#%function-arity #,arity))
                          null)
                      (syntax->list static-infos)))
              defns)
        defns))

  ;; returns (listof (cons n (listof fcase)))
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
      (for/fold ([ht #hasheqv()]) ([fc (in-list fcases)])
        (define n (length (fcase-pos fcase-args fc)))
        (define key (if (and rest-min (>= n rest-min))
                        (- (add1 rest-min))
                        n))
        (hash-set ht key (cons fc (hash-ref ht key '())))))
    (for/list ([(key sames) (in-hash ht)])
      (cons key (reverse sames))))

  (define (find-matching-case n same)
    (define find-n (if (negative? n) (- (add1 n)) n))
    (for/or ([fc (in-list same)])
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
    (define-values (_empty new-arg-ids-rev wrap/single-args)
      (for/fold ([pos-arg-ids-rem (syntax->list pos-arg-ids-stx)]
                 [new-arg-ids-rev '()]
                 [wrap values])
                ([kw (in-list (fcase-kws fc))]
                 [arg (in-list (fcase-args fc))])
        (cond
          [(and (not (syntax-e kw)) (pair? pos-arg-ids-rem))
           (values (cdr pos-arg-ids-rem)
                   (cons (car pos-arg-ids-rem) new-arg-ids-rev)
                   wrap)]
          [(not (syntax-e kw))
           (unless (negative? n) (error "assert failed in wrap-adapted: n"))
           (define tmp (generate-temporary arg))
           (values pos-arg-ids-rem
                   (cons tmp new-arg-ids-rev)
                   (lambda (body)
                     (wrap
                      #`(if (pair? #,rest-tmp)
                            (let ([#,tmp (car #,rest-tmp)])
                              (let ([#,rest-tmp (cdr #,rest-tmp)])
                                #,body))
                            (#,try-next)))))]
          [else
           (unless kwrest-tmp (error "assert failed in wrap-adapted: kwrest-tmp 1"))
           (define tmp (generate-temporary arg))
           (values pos-arg-ids-rem
                   (cons tmp new-arg-ids-rev)
                   (lambda (body)
                     (wrap
                      #`(if (hash-has-key? #,kwrest-tmp '#,kw)
                            (let ([#,tmp (hash-ref #,kwrest-tmp '#,kw)])
                              (let ([#,kwrest-tmp (hash-remove #,kwrest-tmp '#,kw)])
                                #,body))
                            (#,try-next)))))])))
    (unless (null? _empty) (error "assert failed in wrap-adapted: pos-arg-ids-rem"))
    ;; check empty positional rest if it exists in the n case but not the fcase
    (define wrap/rest
      (cond
        [(eqv? n f-n) wrap/single-args]
        [(negative? f-n) wrap/single-args]
        [else
         (unless (negative? n) (error "assert failed in wrap-adapted: n"))
         (lambda (body)
           (wrap/single-args
            #`(if (null? #,rest-tmp)
                  (let () #,body)
                  (#,try-next))))]))
    ;; check empty keyword rest if it exists in the kwrest-tmp but not the fcase
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
            #`(if (hash-empty? #,kwrest-tmp)
                  (let () #,body)
                  (#,try-next))))]))
    (values (reverse new-arg-ids-rev)
            wrap/kwrest)))

(define (argument-binding-failure who val annotation-str)
  (raise-binding-failure who "argument" val annotation-str))

(define (cases-failure who rest-args kwrest-args . base-args)
  (define args (append base-args rest-args))
  (apply
   raise-contract-error
   who
   (apply string-append "no matching case for arguments\n"
          "  arguments...:"
          (for/list ([arg (in-list args)])
            "\n   ~e"))
   args))

(define-syntax (add-annotation-check stx)
  (syntax-parse stx
    [(_ name converter main-converter e)
     (cond
       [(or (syntax-e #'converter)
            (syntax-e #'main-converter))
        (define (multi-result? e) (syntax-parse e
                                    #:literals (lambda)
                                    [(lambda (_ _ . _) . _) #t]
                                    [(lambda () . _) #t]
                                    [_ #f]))
        (cond
          [(or (multi-result? #'converter)
               (multi-result? #'main-converter))
           (define (wrap-values converter e)
             (define (simple)
               #`(let ([result #,e])
                   (#,converter result
                    (lambda (v) v)
                    (lambda ()
                      (raise-result-failure 'name result)))))
             (syntax-parse converter
               #:literals (lambda)
               [(lambda (arg success-k fail-k) . _) (simple)]
               [(lambda (arg ... success-k fail-k) . _)
                #`(call-with-values
                   (lambda () #,e)
                   (case-lambda
                     [(arg ...)
                      (#,converter arg ...
                       values
                       (lambda ()
                         (raise-results-failure 'name (list arg ...))))]
                     [args (raise-results-failure 'name args)]))]
               [_ (simple)]))
           (cond
             [(and (syntax-e #'converter)
                   (syntax-e #'main-converter))
              (wrap-values #'main-converter (wrap-values #'converter #'e))]
             [(syntax-e #'converter)
              (wrap-values #'converter #'e)]
             [else
              (wrap-values #'main-converter #'e)])]
          [else
           #`(let ([result e])
               #,(let ([succcess-k #'(lambda (x) x)]
                       [fail-k #'(lambda () (raise-result-failure 'name result))])
                   (cond
                     [(and (syntax-e #'converter)
                           (syntax-e #'main-converter))
                      #'(let ([fail-k #,fail-k])
                          (converter result
                                     (lambda (result)
                                       (main-converter #,success-k fail-k))
                                     fail-k))]
                     [(syntax-e #'converter)
                      #'(converter result #,success-k #,fail-k)]
                     [else
                      #'(main-converter result #,success-k #,fail-k)])))])]
       [else #'e])]))

(define (raise-result-failure who val)
  (raise-contract-error
   who
   (string-append "result does not satisfy annotation\n"
                  "  result: ~v")
   val))

(define (raise-results-failure who vals)
  (raise-contract-error
   who
   (string-append "results do not satisfy annotation\n"
                  "  results...:"
                  (if (null? vals)
                      " [none]"
                      (apply
                       string-append
                       (for/list ([v (in-list vals)])
                         (format "\n   ~v" v)))))))

(begin-for-syntax
  (define-syntax-class :kw-argument
    #:attributes (kw exp)
    #:datum-literals (block group)
    (pattern (group kw:keyword (block exp)))
    (pattern exp
             #:attr kw #'#f)))

(define-for-syntax (parse-function-call rator-in extra-args stxes
                                        #:static? [static? #f]
                                        #:repetition? [repetition? #f]
                                        #:rator-stx [rator-stx #f] ; for error reporting
                                        #:srcloc [srcloc #f] ; for `relocate` on result
                                        #:rator-kind [rator-kind (if repetition? 'repetition 'function)]
                                        #:rator-arity [rator-arity #f])
  (define (generate extra-rands rands rsts dots kwrsts tail)
    (syntax-parse stxes
      [(_ args . _)
       (generate-call rator-in #'args extra-rands rands rsts dots kwrsts tail
                      #:static? static?
                      #:repetition? repetition?
                      #:rator-stx rator-stx
                      #:srcloc srcloc
                      #:rator-kind rator-kind
                      #:rator-arity rator-arity)]))
  (define (check-complex-allowed)
    (when (eq? rator-kind '|syntax class|)
      (raise-syntax-error #f "syntax class call cannot have splicing arguments" rator-stx)))
  (syntax-parse stxes
    #:datum-literals (group)
    [(_ (~and args (_::parens rand ...)) . tail)
     #:when (complex-argument-splice? #'(rand ...))
     (check-complex-allowed)
     (values (complex-argument-splice-call rator-in #'args extra-args #'(rand ...)
                                           #:static? static?
                                           #:repetition? repetition?
                                           #:rator-stx rator-stx
                                           #:srcloc srcloc
                                           #:rator-kind rator-kind
                                           #:rator-arity rator-arity)
             #'tail)]
    [(_ (_::parens rand ...
                   (group _::&-expr rst ...)
                   (group _::~&-expr kwrst ...))
        . tail)
     (check-complex-allowed)
     (generate extra-args #'(rand ...) #'(group rst ...) #f #'(group kwrst ...) #t #'tail)]
    [(_ (_::parens rand ...
                   rep (group (~var dots (:... in-expression-space)))
                   (group _::&-expr kwrst ...))
        . tail)
     (check-complex-allowed)
     (generate #'(rand ...) #'rep #'dots.name #'(group kwrst ...) #'tail)]
    [(_ (_::parens rand ... (group _::&-expr rst ...)) . tail)
     (check-complex-allowed)
     (generate extra-args #'(rand ...) #'(group rst ...) #f #f #'tail)]
    [(_ (_::parens rand ... rep (group (~var dots (:... in-expression-space)))) . tail)
     (check-complex-allowed)
     (generate extra-args #'(rand ...) #'rep #'dots.name #f #'tail)]
    [(_ (_::parens rand ... (group _::~&-expr kwrst ...)) . tail)
     (check-complex-allowed)
     (generate extra-args #'(rand ...) #f #f #'(group kwrst ...) #'tail)]
    [(_ (_::parens rand ...) . tail)
     (generate extra-args #'(rand ...) #f #f #f #'tail)]))

(define-for-syntax (generate-call rator-in args-stx extra-rands rands rsts dots kwrsts tail
                                  #:static? static?
                                  #:repetition? repetition?
                                  #:rator-stx rator-stx
                                  #:srcloc srcloc
                                  #:rator-kind rator-kind
                                  #:rator-arity rator-arity)
  (values
   (with-syntax-parse ([(rand::kw-argument ...) rands])
     (handle-repetition
      repetition?
      (if repetition? rator-in (rhombus-local-expand rator-in))
      (syntax->list #'(rand.exp ...))
      rsts dots
      kwrsts
      (lambda (rator args rest-args kwrest-args rator-static-info)
        (define kws (syntax->list #'(rand.kw ...)))
        (define (rator-static-info/indirect key
                                            #:result [result (lambda (si indirect?) si)])
          (define si (rator-static-info key))
          (if si
              (result si #f)
              (result (let loop ([c-id (rator-static-info #'#%function-indirect)])
                        (and c-id
                             (or (syntax-local-static-info c-id key)
                                 (loop (syntax-local-static-info c-id #'#%function-indirect)))))
                      #t)))
        (when static?
          (when (or (not kwrsts) (not rsts))
            (define-values (a indirect?)
              (if rator-arity
                  (values rator-arity #f)
                  (rator-static-info/indirect #'#%function-arity #:result values)))
            (when a
              (let* ([a (if (syntax? a) (syntax->datum a) a)])
                (check-arity rator-stx rator-in a (+ (length extra-rands) (if indirect? 1 0)) kws rsts kwrsts rator-kind)))))
        (define num-rands (length (syntax->list #'(rand.kw ...))))
        (with-syntax-parse ([((arg-form ...) ...) (for/list ([kw kws]
                                                             [arg (in-list args)])
                                                    (if (syntax-e kw)
                                                        (list kw arg)
                                                        (list arg)))]
                            [(head . _) args-stx])
          (define es
            (cond
              [kwrsts (list (append (list #'keyword-apply/map rator)
                                    extra-rands
                                    (syntax->list #'(arg-form ... ...))
                                    (list rest-args))
                            kwrest-args)]
              [rsts (append (list #'apply rator)
                            extra-rands
                            (syntax->list #'(arg-form ... ...))
                            (list rest-args))]
              [else (cons rator
                          (append extra-rands
                                  (syntax->list #'(arg-form ... ...))))]))
          (define e (relocate+reraw (or srcloc
                                        (respan (datum->syntax #f (list (or rator-stx rator-in) args-stx))))
                                    (datum->syntax #'here es)))
          (define result-static-infos (or (rator-static-info/indirect #'#%call-result)
                                          #'()))
          (define all-result-static-infos (or (let-values ([(r indirect?)
                                                            (rator-static-info/indirect #'#%call-results-at-arities #:result values)])
                                                (let loop ([r r])
                                                  (syntax-parse r
                                                    [((n (result ...)) . rest)
                                                     (if (equal? (syntax-e #'n) (+ num-rands (length extra-rands) (if indirect? 1 0)))
                                                         #`(result ... . #,result-static-infos)
                                                         (loop #'rest))]
                                                    [_ #f])))
                                              result-static-infos))
          (values e all-result-static-infos)))))
   tail))

(define-for-syntax (handle-repetition repetition?
                                      rator ; already parsed as expression or repetition
                                      rands
                                      rsts dots
                                      kwrsts
                                      k)
  (cond
    [(not repetition?)
     ;; parse arguments as expressions
     (define args
       (for/list ([arg (in-list rands)])
         (with-syntax-parse ([e::expression arg]) #'e.parsed)))
     (define rest-args
       (cond
         [dots (repetition-as-list dots rsts 1)]
         [rsts (with-syntax-parse ([rst::expression rsts]) #'rst.parsed)]
         [else #''()]))
     (define kwrest-args
       (and kwrsts
            (with-syntax-parse ([kwrst::expression kwrsts]) #'kwrst.parsed)))
     (define-values (e result-static-infos)
       (k rator args rest-args kwrest-args (lambda (key) (syntax-local-static-info rator key))))
     (wrap-static-info* e result-static-infos)]
    [else
     ;; parse arguments as repetitions
     (define args
       (for/list ([arg (in-list rands)])
         (with-syntax-parse ([rep::repetition arg]) #'rep.parsed)))
     (define n (length args))
     (let* ([args (append
                   (cons rator args)
                   (if rsts
                       (list (with-syntax-parse ([rep::repetition rsts])
                               (if dots (list #'rep.parsed) #'rep.parsed)))
                       null)
                   (if kwrsts
                       (list (with-syntax-parse ([rep::repetition kwrsts]) #'rep.parsed))
                       null))])
       (build-compound-repetition
        rator
        args
        #:is-sequence? (lambda (e) (pair? e))
        #:extract (lambda (e) (if (pair? e) (car e) e))
        (lambda one-args
          (let* ([one-rator (car one-args)]
                 [args (for/list ([i (in-range n)]
                                  [arg (in-list (cdr one-args))])
                         arg)]
                 [rest-args (and rsts (list-ref one-args (add1 n)))]
                 [kwrest-args (and kwrsts (list-ref one-args (+ n 1 (if rsts 1 0))))])
            ;; returns expression plus static infors for result elements
            (k one-rator args (or rest-args #''()) kwrest-args
               (lambda (key)
                 (with-syntax-parse ([rep::repetition-info rator])
                   (repetition-static-info-lookup #'rep.element-static-infos key))))))))]))

(define-for-syntax (complex-argument-splice? gs-stx)
  ;; multiple `&` or `...`, or not at the end before `~&`,
  ;; or `~&` that's not at the very end?
  (define (not-kw-splice-only? gs-stx)
    (syntax-parse gs-stx
      #:datum-literals (group)
      [((group _::~&-expr rand ...+)) #f]
      [() #f]
      [_ #t]))
  (let loop ([gs-stx gs-stx])
    (syntax-parse gs-stx
      #:datum-literals (group)
      [() #f]
      [((group _::&-expr rand ...+) . gs)
       (or (loop #'gs) (not-kw-splice-only? #'gs))]
      [(g0 (group _::...-expr) . gs)
       (or (loop #'gs) (not-kw-splice-only? #'gs))]
      [((group _::~&-expr rand ...+) . gs)
       (or (loop #'(g . gs))  (pair? (syntax-e #'gs)))]
      [(_ . gs) (loop #'gs)])))

(define-for-syntax (complex-argument-splice-call rator args-stx extra-args gs-stx
                                                 #:static? static?
                                                 #:repetition? repetition?
                                                 #:rator-stx rator-stx
                                                 #:srcloc srcloc
                                                 #:rator-kind rator-kind
                                                 #:rator-arity rator-arity)
  (define (gen-id) (car (generate-temporaries '(arg))))
  (let loop ([gs-stx gs-stx]
             [rev-args '()])
    (syntax-parse gs-stx
      #:datum-literals (group)
      [()
       (define args (reverse rev-args))
       (define extra-arg-ids (generate-temporaries extra-args))
       #`(let (#,@(for/list ([extra-arg-id (in-list extra-arg-ids)]
                             [extra-arg (in-list extra-args)])
                    #`[#,extra-arg-id #,extra-arg])
               #,@(for/list ([arg (in-list args)])
                    #`[#,(car arg) #,(caddr arg)]))
             #,(let ([lists? (for/or ([arg (in-list args)])
                               (eq? 'list (cadr arg)))])
                 (define-values (term ignored-tail)
                   (generate-call rator args-stx
                                  (append
                                   extra-arg-ids
                                   (if lists?
                                       null
                                       (for/list ([arg (in-list args)]
                                                  #:when (eq? (cadr arg) 'arg))
                                         (car arg))))
                                  (append
                                   (for/list ([arg (in-list args)]
                                              #:when (eq? (cadr arg) 'kw))
                                     #`(group #,(list-ref arg 3)
                                              (block (group (parsed #:rhombus/expr #,(car arg)))))))
                                  (and lists?
                                       #`(group
                                          (parsed
                                           #:rhombus/expr
                                           (append
                                            #,@(for/list ([arg (in-list args)]
                                                          #:when (or (eq? (cadr arg) 'arg)
                                                                     (eq? (cadr arg) 'list)))
                                                 (cond
                                                   [(eq? (cadr arg) 'arg)
                                                    #`(list #,(car arg))]
                                                   [else
                                                    (car arg)]))))))
                                  #f
                                  (let ([kwss (for/list ([arg (in-list args)]
                                                         #:when (eq? (cadr arg) 'kws))
                                                (car arg))])
                                    (cond
                                      [(null? kwss) #f]
                                      [(null? (cdr kwss)) #`(group (parsed #:rhombus/expr #,(car kwss)))]
                                      [else #`(group (parsed #:rhombus/expr (merge-keyword-argument-maps #,@kwss)))]))
                                  #'#f
                                  #:static? static?
                                  #:repetition? repetition?
                                  #:rator-stx rator-stx
                                  #:srcloc srcloc
                                  #:rator-kind rator-kind
                                  #:rator-arity rator-arity))
                 term))]
      [(((~and tag group) _::&-expr rand ...+) . gs)
       (loop #'gs
             (cons (list (gen-id) 'list #'(rhombus-expression (tag rand ...)) #f)
                   rev-args))]
      [(g0 (group (~var dots (:... in-expression-space))) . gs)
       (define-values (new-gs extras) (consume-extra-ellipses #'gs))
       (loop new-gs
             (cons (list (gen-id) 'list (repetition-as-list #'dots.name #'g0 1 extras))
                   rev-args))]
      [(((~and tag group) _::~&-expr rand ...+) . gs)
       (loop #'gs
             (cons (list (gen-id) 'kws #'(rhombus-expression (tag rand ...)))
                   rev-args))]
      [((group kw:keyword (tag::block body ...)) . gs)
       (loop #'gs
             (cons (list (gen-id) 'kw #'(rhombus-body-at tag body ...) #'kw)
                   rev-args))]
      [(g . gs)
       (loop #'gs
             (cons (list (gen-id) 'arg #'(rhombus-expression g))
                   rev-args))])))

(define function-call-who '|function call|)

(define keyword-apply/map
  (make-keyword-procedure
   (lambda (kws kw-args proc . args+rest)
     ;; currying makes it easier to preserve order when `~&` is last
     (lambda (kw-ht)
       (define all-kw-ht
         (for/fold ([ht kw-ht]) ([kw (in-list kws)]
                                 [arg (in-list kw-args)])
           (when (hash-ref kw-ht kw #f)
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
  (define (check-hash ht)
    (unless (hash? ht)
      (raise-arguments-error* function-call-who rhombus-realm
                              "not a map for keyword arguments"
                              "given" ht)))
  (check-hash ht)
  (for/fold ([accum-ht ht]) ([ht (in-list hts)])
    (check-hash ht)
    (for/fold ([accum-ht accum-ht]) ([(kw arg) (in-hash ht)])
      (when (hash-ref accum-ht kw #f)
        (raise-arguments-error* function-call-who rhombus-realm
                                "duplicate keyword in in keyword-argument maps"
                                "keyword" kw))
      (hash-set accum-ht kw arg))))

(define-syntax-rule (if/blocked tst thn els)
  (if tst (let () thn) els))

(begin-for-syntax
 (set-parse-function-call! parse-function-call))
