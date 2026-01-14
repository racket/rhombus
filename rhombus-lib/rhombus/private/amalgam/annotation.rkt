#lang racket/base
(require (for-syntax racket/base
                     racket/treelist
                     syntax/parse/pre
                     shrubbery/print
                     enforest/property
                     enforest/proc-name
                     enforest/name-parse
                     enforest/hier-name-parse
                     enforest/syntax-local
                     "srcloc.rkt"
                     "introducer.rkt"
                     "annotation-string.rkt"
                     "keyword-sort.rkt"
                     "macro-result.rkt"
                     "tag.rkt"
                     "name-path-op.rkt"
                     "annot-context.rkt"
                     "class-parse.rkt"
                     "origin.rkt"
                     "annotation-failure.rkt"
                     (for-syntax racket/base))
         "provide.rkt"
         "enforest.rkt"
         "annotation-operator.rkt"
         "expression.rkt"
         "repetition.rkt"
         "binding.rkt"
         "name-root.rkt"
         "name-root-ref.rkt"
         "name-root-space.rkt"
         "dotted-sequence-parse.rkt"
         "static-info.rkt"
         "parse.rkt"
         "parens.rkt"
         "if-blocked.rkt"
         "number.rkt"
         "is-static.rkt"
         "rhombus-primitive.rkt"
         "annotation-failure.rkt"
         "order.rkt"
         "order-primitive.rkt"
         "call-result-key.rkt"
         "index-result-key.rkt"
         "sequence-element-key.rkt"
         "values-key.rkt"
         "class-this-id.rkt"
         "treelist-statinfo.rkt"
         "map-statinfo.rkt"
         "keyword-statinfo.rkt")

(provide (for-spaces (#f
                      rhombus/repet
                      rhombus/bind)
                     ::
                     :~)
         (for-spaces (#f
                      rhombus/repet)
                     is_a)
         (for-space rhombus/annot

                    None
                    Boolean
                    PosInt
                    NegInt
                    NonnegInt
                    Nat
                    PosReal
                    NegReal
                    NonnegReal
                    Integral
                    Rational
                    Exact
                    Inexact
                    Flonum
                    Fixnum
                    Number
                    Void
                    False
                    True

                    matching
                    satisfying
                    #%literal)
         (for-spaces (rhombus/annot
                      rhombus/namespace)
                     Any
                     Real
                     Int))

(module+ for-class
  (begin-for-syntax
    (provide (property-out annotation-prefix-operator)
             (property-out annotation-infix-operator)

             identifier-annotation
             identifier-binding-annotation
             make-identifier-binding-annotation

             in-annotation-space
             annot-quote

             :annotation
             :annotation-predicate-form
             :annotation-binding-form
             :inline-annotation
             :unparsed-inline-annotation
             :annotation-infix-op+form+tail
             :annotation-prefix-op+form+tail
             :annotate-op

             annotation-predicate-form
             annotation-binding-form

             parse-annotation-of
             parse-annotation-of/chaperone

             annotation-relative-precedence
             build-annotated-expression
             raise-unchecked-disallowed

             annotation-to-be-defined!))

  (provide define-annotation-syntax
           define-annotation-constructor

           raise-annotation-failure))

(module+ for-arrow
  (provide (for-space rhombus/annot
                      ::)))

(module+ for-map-function
  (provide (for-syntax extract-call-result)))

(module+ for-function-parse
  (provide (for-syntax set-find-call-result-at!)))

(module+ for-range
  (provide (for-syntax install-range)))

(begin-for-syntax
  ;; see also "annotation-operator.rkt"

  (define in-annotation-space (make-interned-syntax-introducer/add 'rhombus/annot))

  (define-syntax (annot-quote stx)
    (syntax-case stx ()
      [(_ id) #`(quote-syntax #,((make-interned-syntax-introducer 'rhombus/annot) #'id))]))

  ;; improve some error messages, such as for a class name as an annotation,
  ;; where it's easy to refer to the name too early
  (define to-be-defined-ids null)
  (define (annotation-to-be-defined! id)
    (set! to-be-defined-ids (cons id to-be-defined-ids)))

  (define (raise-not-a-annotation id ctx)
    (when (for/or ([to-be-defined-id (in-list to-be-defined-ids)])
            (free-identifier=? id (in-annotation-space to-be-defined-id)))
      (raise-syntax-error #f
                          "use of an annotation before its definition is complete"
                          id))
    (raise-syntax-error #f
                        "not bound as an annotation"
                        id))

  (define (check-annotation-result form proc ctx)
    (syntax-parse (if (syntax? form) form #'#f)
      [(~or* _::annotation-predicate-form _::annotation-binding-form) form]
      [_ (raise-bad-macro-result (proc-name proc) "annotation" form)]))

  (define (shrubbery-tail->string tail) (shrubbery-syntax->string #`(group . #,tail)))

  (define (check-context who ctx)
    (unless (annotation-context? ctx)
      (raise-annotation-failure (case who
                                  [(:unquote-binding-prefix-op+form+tail) 'annot_meta.AfterPrefixParsed]
                                  [(:unquote-binding-infix-op+form+tail) 'annot_meta.AfterInfixParsed]
                                  [else 'annot_meta.Parsed])
                                ctx
                                "annot_meta.Context")))

  (define-rhombus-enforest
    #:syntax-class (:annotation [ctx empty-annot-context])
    #:infix-more-syntax-class :annotation-infix-op+form+tail
    #:prefix-more-syntax-class :annotation-prefix-op+form+tail
    #:check-syntax-class-arguments check-context
    #:desc "annotation"
    #:operator-desc "annotation operator"
    #:parsed-tag #:rhombus/annot
    #:in-space in-annotation-space
    #:prefix-operator-ref annotation-prefix-operator-ref
    #:infix-operator-ref annotation-infix-operator-ref
    #:check-result check-annotation-result
    #:make-identifier-form raise-not-a-annotation
    #:relative-precedence annotation-relative-precedence)

  (define-syntax-class (:annotation-seq prec-op)
    #:attributes (parsed tail)
    (pattern stxes
             #:with (~var || (:annotation-infix-op+form+tail prec-op)) #`(#,group-tag . stxes)))

  (define-splicing-syntax-class :inline-annotation
    #:attributes (converter annotation-str static-infos origins)
    (pattern (~seq op::annotate-op ctc ...)
             #:with c::annotation (no-srcloc #`(#,group-tag ctc ...))
             #:with (~var ca (:annotation-converted (attribute op.check?))) #'c.parsed
             #:do [(when (and (not (attribute op.check?))
                              (syntax-e #'ca.converter))
                     (raise-unchecked-disallowed #'op.name (respan #'(ctc ...))))]
             #:with converter #'ca.converter
             #:with annotation-str (datum->syntax #f (shrubbery-tail->string #'(ctc ...)))
             #:with static-infos #'ca.static-infos
             #:attr origins (list #'c.parsed)))

  (define-syntax-class (:annotation-converted check?)
    #:attributes (converter static-infos)
    (pattern c-parsed::annotation-predicate-form
             #:with converter (if check?
                                  #'(lambda (tmp-id who fail-k)
                                      (if (c-parsed.predicate tmp-id)
                                          tmp-id
                                          (fail-k tmp-id who)))
                                  #'#f)
             #:with static-infos #'c-parsed.static-infos)
     (pattern c-parsed::annotation-binding-form
              #:with arg-parsed::binding-form #'c-parsed.binding
              #:with arg-impl::binding-impl #'(arg-parsed.infoer-id () arg-parsed.data)
              #:with arg-info::binding-info #'arg-impl.info
              #:with ((bind-id bind-use . bind-static-infos) ...) #'arg-info.bind-infos
              #:with converter #'(let ()
                                   (arg-info.oncer-id arg-info.data)
                                   (lambda (tmp-id who fail-k)
                                     (arg-info.matcher-id tmp-id
                                                          arg-info.data
                                                          if/blocked
                                                          (begin
                                                            (arg-info.committer-id tmp-id arg-info.evidence-ids arg-info.data)
                                                            (arg-info.binder-id tmp-id arg-info.evidence-ids arg-info.data)
                                                            (define-static-info-syntax/maybe bind-id . bind-static-infos)
                                                            ...
                                                            c-parsed.body)
                                                          (fail-k tmp-id who))))
              #:with static-infos #'c-parsed.static-infos))

  (define-splicing-syntax-class :unparsed-inline-annotation
    #:attributes (seq)
    (pattern (~seq o::annotate-op ctc ...)
             #:with seq #'(o ctc ...)))

  (define-syntax-class :annotation-predicate-form
    (pattern (#:pred predicate static-infos)))
  (define-syntax-class :annotation-binding-form
    #:attributes (binding body static-infos)
    (pattern (#:bind binding body static-infos)) ; binding is `:binding-impl`, but the annotation-str of eventual `:binding-info` is not used
    (pattern (#:pred predicate static-infos)
             ;; coerce to the more general binding form (so all annotations kinds can be handled this way)
             #:with binding (binding-form #'annotation-predicate-binding-infoer
                                          #'(result predicate static-infos))
             #:with body #'result))

  (define-syntax-class :annotate-op
    #:attributes (name check?)
    #:description "an annotation operator"
    #:opaque
    (pattern ::name
             #:when (free-identifier=? (in-binding-space #'name)
                                       (bind-quote ::))
             #:attr check? #t)
    (pattern ::name
             #:when (free-identifier=? (in-binding-space #'name)
                                       (bind-quote :~))
             #:attr check? #f))

  (define (annotation-predicate-form predicate static-infos)
    #`(#:pred #,predicate #,static-infos))
  (define (annotation-binding-form binding body static-infos)
    #`(#:bind #,binding #,body #,static-infos))

  (define-syntax (identifier-annotation stx)
    (syntax-case stx (unquote-syntax)
      [(_ pred (unquote-syntax static-infos))
       #'(make-identifier-annotation (lambda () (values #`pred static-infos)))]
      [(_ pred static-infos)
       #'(make-identifier-annotation (lambda () (values #`pred #`static-infos)))]
      [(_ pred static-infos #:static-only)
       #'(make-identifier-annotation (lambda () (values #`pred #`static-infos)) #t)]))

  (define (make-identifier-annotation get [static-only? #f])
    (annotation-prefix-operator
     #f
     '((default . stronger))
     'macro
     (lambda (stx ctx)
       (when static-only? (check-static stx))
       (define-values (predicate-stx static-infos) (get))
       (define packed (annotation-predicate-form predicate-stx static-infos))
       (syntax-parse stx
         [(self . tail) (values (relocate+reraw #'self packed) #'tail)]
         [_ 'does-not-happen]))))

  (define-syntax (identifier-binding-annotation stx)
    (syntax-case stx ()
      [(_ binding body static-infos)
       #'(make-identifier-binding-annotation (lambda (stx) (values #`binding #`body #`static-infos)))]
      [(_ binding body static-infos #:static-only)
       #'(make-identifier-binding-annotation (lambda (stx) (values #`binding #`body #`static-infos)) #t)]))

  (define (make-identifier-binding-annotation get [static-only? #f])
    (annotation-prefix-operator
     #f
     '((default . stronger))
     'macro
     (lambda (stx ctx)
       (when static-only? (check-static stx))
       (define-values (binding-stx body-stx static-infos) (get stx))
       (define packed (annotation-binding-form binding-stx body-stx static-infos))
       (syntax-parse stx
         [(self . tail)
          (values (relocate+reraw #'self packed) #'tail)]
         [_ 'does-not-happen]))))

  (define (check-static stx)
    (unless (is-static-context/tail? stx)
      (raise-syntax-error #f
                          "not allowed in a dynamic context"
                          stx)))

  (define (parse-annotation-of/one stx ctx sub-n kws)
    (syntax-parse stx
      [(form-id (~and subs (_::parens g ...)) . tail)
       (define new-stx #'(form-id subs))
       (define unsorted-gs (syntax->list #'(g ...)))
       (define gs (sort-with-respect-to-keywords kws unsorted-gs new-stx
                                                 #:make-missing (lambda (kw) #'(group Any))))
       (unless (eqv? (length gs) sub-n)
         (raise-syntax-error #f
                             "wrong number of subannotations in parentheses"
                             (respan new-stx)))
       (values new-stx
               gs
               (for/list ([g (in-list gs)])
                 (syntax-parse g
                   [(~var c (:annotation ctx)) #'c.parsed]))
               (datum->syntax #f (list #'form-id #'subs))
               #'form-id
               #'tail)]))

  (define (parse-annotation-of stx ctx predicate-stx static-infos
                               sub-n kws
                               ;; predicate-maker can be #f if only a converter is supported
                               predicate-maker info-maker-id info-maker-data
                               ;; binding-maker-id can be #f or an error string if a converter is not supported
                               binding-maker-id binding-maker-data)
    (define-values (new-stx gs c-parseds loc form-id tail)
      (parse-annotation-of/one stx ctx sub-n kws))
    (values
     (cond
       [(and predicate-maker
             (syntax-parse c-parseds
               [(c::annotation-predicate-form ...)
                (define c-predicates (syntax->list #'(c.predicate ...)))
                (define c-static-infoss (syntax->list #'(c.static-infos ...)))
                (transfer-origins
                 c-parseds
                 (relocate+reraw
                  loc
                  (annotation-predicate-form #`(let ([immed-pred #,predicate-stx]
                                                     [pred #,(predicate-maker c-predicates)])
                                                 (lambda (v)
                                                   (and (immed-pred v)
                                                        (pred v))))
                                             #`(#,@(compound-static-infos info-maker-id info-maker-data
                                                                          c-static-infoss)
                                                . #,static-infos))))]
               [_ #f]))]
       [else
        (unless (identifier? binding-maker-id)
          (raise-syntax-error #f
                              (or binding-maker-id
                                  "argument converter annotations are not supported")
                              new-stx))
        (syntax-parse c-parseds
          [(c::annotation-binding-form ...)
           (define c-static-infoss (syntax->list #'(c.static-infos ...)))
           (transfer-origins
            c-parseds
            (relocate+reraw
             loc
             (annotation-binding-form
              (binding-form #'annotation-of-infoer
                            #`[#,(shrubbery-tail->string new-stx)
                               #,predicate-stx #,binding-maker-id #,binding-maker-data
                               ([c.binding c.body] ...) #,static-infos result
                               #,kws])
              #'result
              #`(#,@(compound-static-infos info-maker-id info-maker-data
                                           c-static-infoss)
                 . #,static-infos))))])])
     tail))

  ;; This one is for converters that produce chaperones/impersonators.
  ;; The predicate case also produces a converter, because it has to
  ;; chaperone the original value.  Also, the makers receive the raw
  ;; text of subannotations so that they can compose a better error
  ;; message.
  (define (parse-annotation-of/chaperone stx ctx predicate-stx static-infos
                                         sub-n kws
                                         predicate-maker info-maker-id info-maker-data
                                         binding-maker-id binding-maker-data)
    (define-values (new-stx gs c-parseds loc form-id tail)
      (parse-annotation-of/one stx ctx sub-n kws))
    (define annot-strs (map shrubbery-tail->string gs))
    (values
     (syntax-parse c-parseds
       [(c::annotation-predicate-form ...)
        (define c-predicates (syntax->list #'(c.predicate ...)))
        (define c-static-infoss (syntax->list #'(c.static-infos ...)))
        (transfer-origins
         c-parseds
         (relocate+reraw
          loc
          (annotation-binding-form
           (binding-form #'annotation-of-infoer/chaperone
                         #`[#,(shrubbery-tail->string new-stx)
                            (let ([immed-pred #,predicate-stx]
                                  [pred #,(predicate-maker c-predicates annot-strs)])
                              (lambda (val-in)
                                (and (immed-pred val-in)
                                     (pred val-in))))
                            #,static-infos
                            result])
           #'result
           #`(#,@(compound-static-infos info-maker-id info-maker-data c-static-infoss)
              . #,static-infos))))]
       [(c::annotation-binding-form ...)
        (unless (identifier? binding-maker-id)
          (raise-syntax-error #f
                              (or binding-maker-id
                                  "argument converter annotations are not supported")
                              new-stx))
        (define c-static-infoss (syntax->list #'(c.static-infos ...)))
        (transfer-origins
         c-parseds
         (relocate+reraw
          loc
          (annotation-binding-form
           (binding-form #'annotation-of-infoer
                         #`[#,(shrubbery-tail->string new-stx)
                            #,predicate-stx #,binding-maker-id [#,annot-strs #,binding-maker-data]
                            ([c.binding c.body] ...) #,static-infos result
                            #,kws])
           #'result
           #`(#,@(compound-static-infos info-maker-id info-maker-data c-static-infoss)
              . #,static-infos))))])
     tail))

  (define (annotation-constructor predicate-stx get-static-infos
                                  sub-n kws predicate-maker info-maker-id info-maker-data
                                  binding-maker-id binding-maker-data
                                  parse-annotation-of)
    (define root
      (annotation-prefix-operator
       #f
       '((default . stronger))
       'macro
       (lambda (stx ctx)
         (syntax-parse stx
           [(form-id . tail)
            (values (relocate+reraw
                     #'form-id
                     (annotation-predicate-form predicate-stx
                                                (get-static-infos)))
                    #'tail)]))))
    (values
     ;; root
     root
     ;; `of`:
     (annotation-prefix-operator
      #f
      '((default . stronger))
      'macro
      (lambda (stx ctx)
        (parse-annotation-of stx ctx
                             predicate-stx (get-static-infos)
                             sub-n kws
                             predicate-maker info-maker-id info-maker-data
                             binding-maker-id binding-maker-data)))))

  (define (compound-static-infos info-maker-id info-maker-data
                                 c-static-infoss)
    (cond
      [(for/or ([c-static-infos (in-list c-static-infoss)])
         (static-info-lookup c-static-infos #'#%dependent-result #:no-indirect? #t))
       #`((#%dependent-result (dependent-compound-static-infos (#,info-maker-id
                                                                #,info-maker-data
                                                                #,c-static-infoss))))]
      [else
       (define info-maker (syntax-local-value info-maker-id))
       (info-maker info-maker-data c-static-infoss)])))

(define-syntax (dependent-compound-static-infos data deps)
  (syntax-parse data
    [(info-maker-id info-maker-data c-static-infoss-stx)
     (define c-static-infoss
       (for/list ([c-static-infos (in-list (syntax->list #'c-static-infoss-stx))])
         (cond
           [(static-info-lookup c-static-infos #'#%dependent-result)
            => (lambda (dep)
                 (syntax-parse dep
                   [(id data)
                    (define proc (syntax-local-value #'id))
                    (static-infos-and (proc #'data deps)
                                      (static-infos-remove c-static-infos #'#%dependent-result))]))]
           [else c-static-infos])))
     (define info-maker (syntax-local-value #'info-maker-id))
     (info-maker #'info-maker-data c-static-infoss)]))

(define-syntax (define-annotation-constructor stx)
  (syntax-parse stx
    [(_ (name of-name)
        binds
        predicate-stx static-infos
        sub-n kws predicate-maker info-maker-id info-maker-data
        binding-maker-id binding-maker-data
        (~optional (~seq #:parse-of parse-annotation-of-id)
                   #:defaults ([parse-annotation-of-id #'parse-annotation-of]))
        (~optional (~seq #:extends name-extends)
                   #:defaults ([name-extends #'#f])))
     #:with annot-name (in-annotation-space #'name)
     (define extra-names (list #'of-name))
     (define defs
       ;; usually `(define-syntaxes (annot-name of-name) ....)`:
       (build-syntax-definitions/maybe-extension
        (list 'rhombus/annot) #'name #:extra-names extra-names #'name-extends
        #'(let binds
              (annotation-constructor predicate-stx (lambda () #`static-infos)
                                      sub-n 'kws
                                      predicate-maker info-maker-id info-maker-data
                                      binding-maker-id binding-maker-data
                                      parse-annotation-of-id))))
     (if (and (pair? defs) (null? (cdr defs)))
         (car defs)
         #`(begin #,@defs))]))

(define-for-syntax (make-annotation-apply-expression-operator checked?)
  (expression-infix-operator
   #f
   `((default . weaker))
   'macro
   (lambda (form tail)
     (syntax-parse tail
       [(op::name . (~var t (:annotation-seq #'::)))
        (define op-name (string->symbol (shrubbery-syntax->string #'op.name)))
        (values
         (build-annotated-expression #'op.name #'t
                                     checked? (discard-static-infos form) (extract-static-infos form) #'t.parsed
                                     (lambda (tmp-id)
                                       #`(raise-::-annotation-failure '#,op-name #,tmp-id '#,(shrubbery-syntax->string #'t.parsed)))
                                     wrap-static-info*)
         #'t.tail)]))
   'none))

(define-for-syntax (make-annotation-apply-repetition-operator checked?)
  (repetition-infix-operator
   #f
   `((default . weaker))
   'macro
   (lambda (form tail)
     (syntax-parse tail
       [(op::name . (~var t (:annotation-seq #'::)))
        (define op-name (string->symbol (shrubbery-syntax->string #'op.name)))
        (syntax-parse form
          [rep::repetition-info
           (build-annotated-expression
            #'op.name #'t
            checked? #'rep.body #'rep.element-static-infos #'t.parsed
            (lambda (tmp-id)
              #`(raise-::-annotation-failure '#,op-name #,tmp-id '#,(shrubbery-syntax->string #'t.parsed)))
            (lambda (body static-infos)
              (values
               (make-repetition-info #'op
                                     #'rep.for-clausess
                                     body
                                     static-infos
                                     #'rep.used-depth)
               #'t.tail)))])]))
   'none))

(define-for-syntax (build-annotated-expression form-name orig-stx
                                               checked? form static-infos t-parsed
                                               build-fail
                                               k)
  (syntax-parse t-parsed
    [c-parsed::annotation-predicate-form
     (k
      (transfer-origin
       t-parsed
       (if checked?
           #`(let ([val #,form])
               (if (c-parsed.predicate val)
                   val
                   #,(build-fail #'val)))
           form))
      (static-infos-and #'c-parsed.static-infos static-infos))]
    [c-parsed::annotation-binding-form
     #:do [(unless checked?
             (raise-unchecked-disallowed form-name orig-stx))]
     #:with arg-parsed::binding-form #'c-parsed.binding
     #:with arg-impl::binding-impl #`(arg-parsed.infoer-id #,static-infos arg-parsed.data)
     #:with arg-info::binding-info #'arg-impl.info
     #:with ((bind-id bind-use . bind-static-infos) ...) #'arg-info.bind-infos
     (k
      (transfer-origin
       t-parsed
       #`(let* ([tmp-id (let ([arg-info.name-id #,(discard-static-infos form)])
                          arg-info.name-id)]
                [fail-k (lambda () #,(build-fail #'tmp-id))])
           (arg-info.oncer-id arg-info.data)
           (arg-info.matcher-id tmp-id
                                arg-info.data
                                if/blocked
                                (begin
                                  (arg-info.committer-id tmp-id arg-info.evidence-ids arg-info.data)
                                  (arg-info.binder-id tmp-id arg-info.evidence-ids arg-info.data)
                                  (define-static-info-syntax/maybe bind-id . bind-static-infos)
                                  ...
                                  c-parsed.body)
                                (fail-k))))
      #'c-parsed.static-infos)]))

(define-for-syntax (raise-unchecked-disallowed in at)
  (raise-syntax-error #f
                      "converter annotation not allowed in a non-checked position"
                      in
                      at))

(define-for-syntax (make-annotation-apply-binding-operator checked?)
  (binding-infix-operator
   #f
   `((default . weaker))
   'macro
   (lambda (form tail)
     (syntax-parse tail
       [(op::name . (~var t (:annotation-seq #'::)))
        #:with left::binding-form form
        (values
         (transfer-origins
          (list #'t.parsed form)
          (syntax-parse #'t.parsed
            [c-parsed::annotation-predicate-form
             (binding-form
              #'annotation-predicate-infoer
              #`(#,(shrubbery-syntax->string #'t.parsed)
                 #,(and checked? #'c-parsed.predicate)
                 c-parsed.static-infos
                 left.infoer-id
                 left.data))]
            [c-parsed::annotation-binding-form
             #:do [(unless checked?
                     (raise-unchecked-disallowed #'op.name #'t))]
             (binding-form
              #'annotation-binding-infoer
              #`(#,(shrubbery-syntax->string #'t.parsed)
                 c-parsed.binding
                 c-parsed.body
                 c-parsed.static-infos
                 left.infoer-id
                 left.data))]))
         #'t.tail)]))
   'none))

(define-syntax ::
  (make-annotation-apply-expression-operator #t))
(define-repetition-syntax ::
  (make-annotation-apply-repetition-operator #t))
(define-binding-syntax ::
  (make-annotation-apply-binding-operator #t))

(define-syntax :~
  (make-annotation-apply-expression-operator #f))
(define-repetition-syntax :~
  (make-annotation-apply-repetition-operator #f))
(define-binding-syntax :~
  (make-annotation-apply-binding-operator #f))

(define-for-syntax (parse-is_a form tail mode)
  (syntax-parse tail
    [(op . (~var t (:annotation-seq #'is_a)))
     (values
      (transfer-origin
       #'t.parsed
       (syntax-parse #'t.parsed
         [c-parsed::annotation-predicate-form
          (let ([r #`(let ([val #,(discard-static-infos form)])
                       (c-parsed.predicate val))])
            (if (eq? mode 'invert)
                #`(not #,r)
                r))]
         [c-parsed::annotation-binding-form
          #:with arg-parsed::binding-form #'c-parsed.binding
          #:with arg-impl::binding-impl #'(arg-parsed.infoer-id () arg-parsed.data)
          #:with arg-info::binding-info #'arg-impl.info
          #`(let ([val-in (let ([arg-info.name-id #,(discard-static-infos form)])
                            arg-info.name-id)])
              (arg-info.oncer-id arg-info.data)
              (arg-info.matcher-id val-in
                                   arg-info.data
                                   if/blocked
                                   #,(eq? mode 'normal)
                                   #,(not (eq? mode 'normal))))]))
       #'t.tail)]))

(define-syntax is_a
  (expression-infix-operator
   (lambda () (order-quote equivalence))
   '()
   'macro
   (lambda (form tail [mode 'normal])
     (parse-is_a form tail mode))
   'none))

(define-repetition-syntax is_a
  (repetition-infix-operator
   (lambda () (order-quote equivalence))
   '()
   'macro
   (lambda (form tail [mode 'normal])
     (syntax-parse tail
       [(self . _)
        (syntax-parse form
          [rep::repetition-info
           (define-values (body new-tail) (parse-is_a #'rep.body tail mode))
           (values
            (make-repetition-info #'self
                                  #'rep.for-clausess
                                  body
                                  #'()
                                  #'rep.used-depth)
            new-tail)])]))
   'none))

(define-syntax (annotation-predicate-infoer stx)
  (syntax-parse stx
    [(_ static-infos (annotation-str predicate implied-static-infos left-infoer-id left-data))
     #:with left-impl::binding-impl #`(left-infoer-id #,(static-infos-and #'implied-static-infos #'static-infos) left-data)
     #:with left::binding-info #'left-impl.info
     (if (syntax-e #'predicate)
         (binding-info (annotation-string-and (syntax-e #'annotation-str) (syntax-e #'left.annotation-str))
                       #'left.name-id
                       #'left.static-infos ; presumably includes `implied-static-infos` as passed to `left-infoer-id`
                       #'left.bind-infos
                       #'left-oncer
                       #'check-predicate-matcher
                       #'left.evidence-ids
                       #'commit-nothing-new
                       #'bind-nothing-new
                       #'(predicate predicate-id left.oncer-id left.matcher-id left.committer-id left.binder-id left.data))
         #'left)]))

(define-syntax (left-oncer stx)
  (syntax-parse stx
    [(_ (predicate predicate-id left-oncer-id left-matcher-id left-committer-id left-binder-id left-data))
     #'(begin
         (left-oncer-id left-data)
         (define predicate-id predicate))]))

(define-syntax (check-predicate-matcher stx)
  (syntax-parse stx
    [(_ arg-id (predicate predicate-id left-oncer-id left-matcher-id left-committer-id left-binder-id left-data) IF success fail)
     #'(IF (predicate-id arg-id)
           (left-matcher-id
            arg-id
            left-data
            IF
            success
            fail)
           fail)]))

(define-syntax (commit-nothing-new stx)
  (syntax-parse stx
    [(_ arg-id evidence-ids (predicate predicate-id left-oncer-id left-matcher-id left-committer-id left-binder-id left-data))
     #'(left-committer-id arg-id evidence-ids left-data)]))

(define-syntax (bind-nothing-new stx)
  (syntax-parse stx
    [(_ arg-id evidence-ids (predicate predicate-id left-oncer-id left-matcher-id left-committer-id left-binder-id left-data))
     #'(left-binder-id arg-id evidence-ids left-data)]))

(define-syntax (annotation-binding-infoer stx)
  (syntax-parse stx
    [(_ static-infos (annotation-str binding body body-static-infos left-infoer-id left-data))
     #:with arg-parsed::binding-form #'binding
     #:with arg-impl::binding-impl #'(arg-parsed.infoer-id static-infos arg-parsed.data)
     #:with arg-info::binding-info #'arg-impl.info
     #:with left-impl::binding-impl #'(left-infoer-id body-static-infos left-data)
     #:with left::binding-info #'left-impl.info
     #:with converted-id ((make-syntax-introducer) (datum->syntax #f (syntax-e #'left.name-id)))
     (define (build-binding-info matcher-id evidence-ids committer-id binder-id converted-as-evidence?)
       (binding-info (annotation-string-and (syntax-e #'annotation-str) (syntax-e #'left.annotation-str))
                     #'left.name-id
                     #'arg-info.static-infos ; this is about the value coming in, not the converted value
                     #'left.bind-infos
                     #'binding-oncer
                     matcher-id
                     (if converted-as-evidence?
                         #`(converted-id #,evidence-ids)
                         evidence-ids)
                     committer-id
                     binder-id
                     #'([left.oncer-id left.matcher-id left.committer-id left.binder-id left.data]
                        converted-id
                        [arg-info.oncer-id arg-info.matcher-id arg-info.evidence-ids arg-info.committer-id arg-info.binder-id arg-info.data
                                           arg-info.bind-infos body])))
     (cond
       [(free-identifier=? #'always-succeed #'left.matcher-id)
        ;; in this case, we can commit and bind lazily for the annotation's binding
        (build-binding-info #'check-binding-check-convert
                            #'arg-info.evidence-ids
                            #'commit-convert-then-via-converted
                            #'bind-after-converted
                            #f)]
       [else
        ;; in this case, we have to apply the annotation's binding's conversion before
        ;; we can apply the left-hand's matcher
        (build-binding-info #'check-binding-convert
                            #'left.evidence-ids
                            #'commit-via-converted
                            #'bind-via-converted
                            #t)])]))

(define-syntax (binding-oncer stx)
  (syntax-parse stx
    [(_ ([left-oncer-id
          left-matcher-id left-committer-id left-binder-id left-data]
         converted-id
         [arg-oncer-id
          arg-matcher-id arg-evidence-ids arg-committer-id arg-binder-id arg-data . _]))
     #'(begin
         (left-oncer-id left-data)
         (arg-oncer-id arg-data))]))

(define-syntax (check-binding-check-convert stx)
  (syntax-parse stx
    [(_ arg-id ([left-oncer-id left-matcher-id left-committer-id left-binder-id left-data]
                converted-id
                [arg-oncer-id arg-matcher-id arg-evidence-ids arg-committer-id arg-binder-id arg-data . _])
        IF success fail)
     ;; If we get here, then `left-matcher-id` is `always-succeed`
     #'(arg-matcher-id arg-id
                       arg-data
                       IF
                       success
                       fail)]))

(define-syntax (commit-convert-then-via-converted stx)
  (syntax-parse stx
    [(_ arg-id evidence-ids ([left-oncer-id left-matcher-id left-committer-id left-binder-id left-data]
                             converted-id
                             [arg-oncer-id arg-matcher-id arg-evidence-ids arg-committer-id arg-binder-id arg-data
                                           arg-bind-infos body]))
     #:with ((bind-id bind-use . bind-static-infos) ...) #'arg-bind-infos
     #'(begin
         (define converted-id
           (let ()
             (arg-committer-id arg-id evidence-ids arg-data)
             (arg-binder-id arg-id evidence-ids arg-data)
             (define-static-info-syntax/maybe bind-id . bind-static-infos)
             ...
             body))
         (left-committer-id converted-id () left-data))]))

(define-syntax (bind-after-converted stx)
  (syntax-parse stx
    [(_ arg-id evidence-ids ([left-oncer-id left-matcher-id left-committer-id left-binder-id left-data]
                             converted-id
                             arg/ignored-because-done))
     ;; If we get here, then `left-matcher-id` is `always-succeed`
     #'(left-binder-id converted-id () left-data)]))

(define-syntax (check-binding-convert stx)
  (syntax-parse stx
    [(_ arg-id ([left-oncer-id left-matcher-id left-committer-id left-binder-id left-data]
                converted-id
                [arg-oncer-id arg-matcher-id arg-evidence-ids arg-committer-id arg-binder-id arg-data
                              arg-bind-infos body])
        IF success fail)
     #:with ((bind-id bind-use . bind-static-infos) ...) #'arg-bind-infos
     ;; Since we need to use `left-matcher-id` on the converted value, we
     ;; cannot delay `arg-committer-id` and `arg-binder-id` if `arg-match-id`
     ;; branches to success
     #'(arg-matcher-id arg-id
                       arg-data
                       IF
                       (begin
                         (define converted-id
                           (let ()
                             (arg-committer-id arg-id arg-evidence-ids arg-data)
                             (arg-binder-id arg-id arg-evidence-ids arg-data)
                             (define-static-info-syntax/maybe bind-id . bind-static-infos)
                             ...
                             body))
                         (left-matcher-id converted-id
                                          left-data
                                          IF
                                          success
                                          fail))
                       fail)]))

(define-syntax (commit-via-converted stx)
  (syntax-parse stx
    [(_ arg-id (converted-id evidence-ids) ([left-oncer-id left-matcher-id left-committer-id left-binder-id left-data]
                                            _
                                            arg-ignored-because-done))
     #'(left-committer-id converted-id evidence-ids left-data)]))

(define-syntax (bind-via-converted stx)
  (syntax-parse stx
    [(_ arg-id (converted-id evidence-ids) ([left-oncer-id left-matcher-id left-committer-id left-binder-id left-data]
                                            _
                                            arg/ignored-because-done))
     #'(left-binder-id converted-id evidence-ids left-data)]))

(define-syntax (annotation-of-infoer stx)
  (syntax-parse stx
    [(_ rhs-static-infos [annotation-str
                          predicate-stx binding-maker-id binding-maker-data
                          ([c-binding c-body] ...) static-infos result-id kws])
     (define binding-maker (syntax-local-value #'binding-maker-id))
     (define converter
       #`(let ([pred predicate-stx])
           (lambda (val-in)
             (and (pred val-in)
                  #,(binding-maker
                     #'val-in
                     (for/list ([c-binding (in-list (syntax->list #'(c-binding ...)))]
                                [c-body (in-list (syntax->list #'(c-body ...)))])
                       (syntax-parse c-binding
                         [arg-parsed::binding-form
                          #:with arg-impl::binding-impl #'(arg-parsed.infoer-id () arg-parsed.data)
                          #:with arg-info::binding-info #'arg-impl.info
                          #:with ((bind-id bind-use . bind-static-infos) ...) #'arg-info.bind-infos
                          #`(let ()
                              (arg-info.oncer-id arg-info.data)
                              (lambda (val-in success-k fail-k)
                                (arg-info.matcher-id val-in
                                                     arg-info.data
                                                     if/blocked
                                                     (begin
                                                       (arg-info.committer-id val-in arg-info.evidence-ids arg-info.data)
                                                       (arg-info.binder-id val-in arg-info.evidence-ids arg-info.data)
                                                       (define-static-info-syntax/maybe bind-id . bind-static-infos)
                                                       ...
                                                       (success-k #,c-body))
                                                     (fail-k))))]))
                     (syntax->datum #'kws)
                     #'binding-maker-data)))))
     (binding-info #'annotation-str
                   #'composite
                   #'static-infos
                   #'((result-id ([#:repet ()])))
                   #'annotation-of-oncer
                   #'annotation-of-matcher
                   #'(temp)
                   #'annotation-of-committer
                   #'annotation-of-binder
                   #`[result-id temp converter-id #,converter])]))

(define-syntax (annotation-of-infoer/chaperone stx)
  (syntax-parse stx
    [(_ rhs-static-infos [annotation-str convert static-infos result-id])
     (binding-info #'annotation-str
                   #'composite
                   #'static-infos
                   #'((result-id ([#:repet ()])))
                   #'annotation-of-oncer
                   #'annotation-of-matcher
                   #'(temp)
                   #'annotation-of-committer
                   #'annotation-of-binder
                   #'[result-id temp converter-id convert])]))

(define-syntax (annotation-of-oncer stx)
  (syntax-parse stx
    [(_ [bind-id temp-id convert-id convert])
     #'(define convert-id convert)]))

(define-syntax (annotation-of-matcher stx)
  (syntax-parse stx
    [(_ arg-id [bind-id temp-id convert-id convert]
        IF success fail)
     #'(begin
         (define temp-id (convert-id arg-id))
         (IF temp-id success fail))]))

(define-syntax (annotation-of-committer stx)
  (syntax-parse stx
    [(_ arg-id (temp/evidence-id) [bind-id temp-id convert-id convert])
     #'(begin)]))

(define-syntax (annotation-of-binder stx)
  (syntax-parse stx
    [(_ arg-id (temp/evidence-id) [bind-id temp-id convert-id convert])
     #'(define bind-id temp/evidence-id)]))

(define-syntax (annotation-predicate-binding-infoer stx)
  (syntax-parse stx
    [(_ left-static-infos (arg predicate static-infos))
     (define all-static-infos (static-infos-and #'static-infos #'left-static-infos))
     (binding-info #'"<converted predicate>"
                   #'arg
                   all-static-infos
                   #`((arg ([#:repet ()]) . #,all-static-infos))
                   #'empty-oncer
                   (syntax-parse #'predicate
                     [(lam (_) #t) ;; matches `Any` and maybe more
                      #:when (or (free-identifier=? #'lam #'lambda)
                                 (free-identifier=? #'lam #'#%plain-lambda))
                      #'always-succeed]
                     [else
                      #'predicate-binding-matcher])
                   #'()
                   #'predicate-binding-committer
                   #'predicate-binding-binder
                   #`[arg predicate])]))

(define-syntax (predicate-binding-matcher stx)
  (syntax-parse stx
    [(_ arg-id [bind-id predicate]
        IF success fail)
     #'(IF (predicate arg-id) success fail)]))


(define-syntax (predicate-binding-committer stx)
  (syntax-parse stx
    [(_ arg-id () [bind-id predicate])
     #'(begin)]))

(define-syntax (predicate-binding-binder stx)
  (syntax-parse stx
    [(_ arg-id () [bind-id predicate])
     #'(define bind-id arg-id)]))

;; ----------------------------------------

(define-syntax (define-annotation-syntax stx)
  (syntax-parse stx
    [(_ id:identifier rhs)
     #`(define-syntax #,(in-annotation-space #'id)
         rhs)]))

(define (exact-number? n) (and (number? n) (exact? n)))
(define (inexact-number? n) (and (number? n) (inexact? n)))
(define (exact-negative-integer? n) (and (integer? n) (exact? n) (negative? n)))

(void (set-primitive-contract! 'exact-integer? "Int"))
(void (set-primitive-contract! 'exact-nonnegative-integer? "Nat"))
(void (set-primitive-contract! 'number? "Number"))
(void (set-primitive-contract! 'integer? "Integral"))
(void (set-primitive-contract! 'real? "Real"))
(void (set-primitive-contract! 'flonum? "Flonum"))
(void (set-primitive-contract! 'fixnum? "Fixnum"))
(define-annotation-syntax Any (identifier-annotation (lambda (x) #t) ()))
(define-annotation-syntax None (identifier-annotation (lambda (x) #f) ((#%none #t))))
(define-annotation-syntax Boolean (identifier-annotation boolean? ()))
(define-annotation-syntax Int (identifier-annotation exact-integer? #,(get-int-static-infos)))
(define-annotation-syntax PosInt (identifier-annotation exact-positive-integer? #,(get-int-static-infos)))
(define-annotation-syntax NegInt (identifier-annotation exact-negative-integer? #,(get-int-static-infos)))
(define-annotation-syntax NonnegInt (identifier-annotation exact-nonnegative-integer? #,(get-int-static-infos)))
(define-annotation-syntax Nat (identifier-annotation exact-nonnegative-integer? #,(get-int-static-infos)))
(define-annotation-syntax Flonum (identifier-annotation flonum? #,(get-flonum-static-infos)))
(define-annotation-syntax Number (identifier-annotation number? #,(get-number-static-infos)))
(define-annotation-syntax Fixnum (identifier-annotation fixnum? #,(get-fixnum-static-infos)))
(define-annotation-syntax Integral (identifier-annotation integer? #,(get-rational-static-infos)))
(define-annotation-syntax Rational (identifier-annotation rational? #,(get-rational-static-infos)))
(define-annotation-syntax Exact (identifier-annotation exact-number? #,(get-rational-static-infos)))
(define-annotation-syntax Inexact (identifier-annotation inexact-number? #,(get-real-static-infos)))
(define-annotation-syntax Real (identifier-annotation real? #,(get-real-static-infos)))
(define-annotation-syntax PosReal (identifier-annotation positive-real? #,(get-real-static-infos)))
(define-annotation-syntax NegReal (identifier-annotation negative-real? #,(get-real-static-infos)))
(define-annotation-syntax NonnegReal (identifier-annotation nonnegative-real? #,(get-real-static-infos)))
(define-annotation-syntax Void (identifier-annotation void? ()))
(define-annotation-syntax False (identifier-annotation not ()))
(define-annotation-syntax True (identifier-annotation (lambda (x) (and x #t)) ()))

(define-name-root Any
  #:fields
  ([of Any.of]
   [to_boolean Any.to_boolean]
   [like Any.like]
   [like_element Any.like_element]
   [like_key Any.like_key]
   [like_value Any.like_value]
   [like_first Any.like_first]
   [like_rest Any.like_rest]
   [like_result Any.like_result]
   [like_field Any.like_field]))

(define-name-root Int
  #:fields
  ([in Int.in]))

(define-name-root Real
  #:fields
  ([above Real.above]
   [at_least Real.at_least]
   [below Real.below]
   [at_most Real.at_most]
   [in Real.in]))

(define (positive-real? r) (and (real? r) (r . > . 0.0)))
(define (negative-real? r) (and (real? r) (r . < . 0.0)))
(define (nonnegative-real? r) (and (real? r) (r . >= . 0.0)))

;; not exported, but referenced by `:annotation-seq` uses so that
;; annotation parsing terminates appropriately
(define-annotation-syntax ::
  (annotation-infix-operator
   #f
   `((default . weaker))
   'macro
   (lambda (form tail) (error "should not get here"))
   'none))
(define-annotation-syntax is_a
  (annotation-infix-operator
   #f
   `((default . stronger))
   'macro
   (lambda (form tail) (error "should not get here"))
   'none))

(define (raise-::-annotation-failure who val ctc)
  (raise-annotation-failure who val ctc))

(define-annotation-syntax matching
  (annotation-prefix-operator
   #f
   '((default . stronger))
   'macro
   (lambda (stx ctx)
     (syntax-parse stx
       [(form-id (~and args (_::parens arg::binding)) . tail)
        #:with arg-parsed::binding-form #'arg.parsed
        #:with arg-impl::binding-impl #'(arg-parsed.infoer-id () arg-parsed.data)
        #:with arg-info::binding-info #'arg-impl.info
        (disallow-binding-as-namespace-extension #'arg-impl.info)
        (values
         (relocate+reraw
          (datum->syntax #f (list #'form-id #'args))
          (annotation-predicate-form
           #`(let ()
               (arg-info.oncer-id arg-info.data)
               (lambda (val-in)
                 (arg-info.matcher-id val-in
                                      arg-info.data
                                      if/blocked
                                      #t
                                      #f)))
           #'arg-info.static-infos))
         #'tail)]))))

(define-annotation-syntax satisfying
  (annotation-prefix-operator
   #f
   '((default . stronger))
   'macro
   (lambda (stx ctx)
     (syntax-parse stx
       [(form-id (~and args (_::parens pred-g)) . tail)
        (values
         (relocate+reraw
          (datum->syntax #f (list #'form-id #'args))
          (annotation-predicate-form
           ;; use `((lambda ....) ....)` to avoid inferred name
           #'((lambda (pred)
                (unless (and (procedure? pred)
                             (procedure-arity-includes? pred 1))
                  (raise-predicate-error 'satisfying pred))
                (lambda (v)
                  (and (pred v) #t)))
              (rhombus-expression pred-g))
           #'()))
         #'tail)]))))

(define (raise-predicate-error who val)
  (raise-annotation-failure who val "Function.of_arity(1)"))

;; `#%parens` is defined in "arrow-annotation.rkt"

(define-annotation-syntax #%literal
  (annotation-prefix-operator
   #f
   '((default . stronger))
   'macro
   (lambda (stxes ctx)
     (syntax-parse stxes
       [(_ kw:keyword . tail)
        (raise-syntax-error #f
                            "misplaced keyword as an annotation"
                            #'kw)]
       [(_ . tail)
        (raise-syntax-error #f
                            "literal not allowed as an annotation"
                            #'tail)]))))

(define-for-syntax (make-unary-real-annotation id comp-stx)
  (annotation-prefix-operator
   #f
   '((default . stronger))
   'macro
   (lambda (stxes ctx)
     (syntax-parse stxes
       [(_ (_::parens n-g) . tail)
        (values (annotation-predicate-form
                 #`(let ([n (rhombus-expression n-g)])
                     (unless (real? n)
                       (raise-annotation-failure '#,id n "Real"))
                     (lambda (v)
                       (and (real? v)
                            (#,comp-stx v n))))
                 #'())
                #'tail)]))))

(define-annotation-syntax Real.above (make-unary-real-annotation (annot-quote Real.above) #'>))
(define-annotation-syntax Real.at_least (make-unary-real-annotation (annot-quote Real.at_least) #'>=))
(define-annotation-syntax Real.below (make-unary-real-annotation (annot-quote Real.below) #'<))
(define-annotation-syntax Real.at_most (make-unary-real-annotation (annot-quote Real.at_most) #'<=))

(begin-for-syntax
  (define-syntax-class :incl-group
    #:attributes (g comp)
    #:datum-literals (group)
    (pattern (group t ... #:exclusive)
             #:with comp #'<
             #:with g #`(#,group-tag t ...))
    (pattern (group t ... #:inclusive)
             #:with comp #'<=
             #:with g #`(#,group-tag t ...))
    (pattern g
             #:with comp #'<=))

  (define is-range?-id #f)
  (define range-contains?-id #f)
  (define explode-range/inline (lambda (e) (values #f #f #f #f #f)))
  (define (install-range is-range? range-contains? explode/inline)
    (set! is-range?-id is-range?)
    (set! range-contains?-id range-contains?)
    (set! explode-range/inline explode/inline)))

(define-for-syntax (build-in-predicate check-who pred-stx annot-str lo-e lo-comp hi-e hi-comp
                                       #:check-lo-hi-stx [check-lo-hi-stx #f])
  #`(let (#,@(if lo-e
                 (list #`[lo-v #,lo-e])
                 null)
          #,@(if hi-e
                 (list #`[hi-v #,hi-e])
                 null))
      #,@(if lo-e
             (list #`(unless (#,pred-stx lo-v)
                       (raise-annotation-failure '#,check-who lo-v '#,annot-str)))
             null)
      #,@(if hi-e
             (list #`(unless (#,pred-stx hi-v)
                       (raise-annotation-failure '#,check-who hi-v '#,annot-str)))
             null)
      #,@(if check-lo-hi-stx
             (list #`(#,check-lo-hi-stx '#,check-who lo-v hi-v))
             null)
      (lambda (v)
        (and (#,pred-stx v)
             #,@(if lo-e
                    (list #`(lo-v . #,lo-comp . v))
                    null)
             #,@(if hi-e
                    (list #`(v . #,hi-comp . hi-v))
                    null)))))

(define-for-syntax (make-in-annotation pred-stx annot-str get-static-infos allow-range?)
  (define (gen-in form-id args-stx lo-e lo-comp hi-e hi-comp)
    (relocate+reraw
     (datum->syntax #f (list form-id args-stx))
     (annotation-predicate-form
      (build-in-predicate form-id pred-stx annot-str lo-e lo-comp hi-e hi-comp)
      (get-static-infos))))
  (annotation-prefix-operator
   #f
   '((default . stronger))
   'macro
   (lambda (stxes ctx)
     (cond
       [(not allow-range?)
        (syntax-parse stxes
          #:datum-literals (group)
          [(form-id (~and args (_::parens lo::incl-group hi::incl-group))
                    . tail)
           (values (gen-in #'form-id #'args
                           #'(rhombus-expression lo.g) #'lo.comp
                           #'(rhombus-expression hi.g) #'hi.comp)
                   #'tail)])]
       [else
        (syntax-parse stxes
          #:datum-literals (group)
          [(form-id (~and args (_::parens lo::incl-group hi::incl-group))
                    . tail)
           (values (gen-in #'form-id #'args
                           #'(rhombus-expression lo.g) #'lo.comp
                           #'(rhombus-expression hi.g) #'hi.comp)
                   #'tail)]
          [(form-id (~and args (_::parens g))
                    . tail)
           (values (relocate+reraw
                    (datum->syntax #f (list #'form-id #'args))
                    (annotation-predicate-form
                     #`(optimize-range-predicate form-id #,pred-stx #,annot-str g)
                     (get-static-infos)))
                   #'tail)])]))))

(define-syntax (optimize-range-predicate stx)
  (syntax-parse stx
    [(_ form-id pred annot-str e::expression)
     (define unwrapped-e (unwrap-static-infos #'e.parsed))
     (define-values (range-who lo lo-type hi hi-type rev? check-lo-hi-stx)
       (explode-range/inline unwrapped-e))
     (cond
       [range-who
        (when rev?
          (error "shouldn't get `rev?` ranges"))
        (define (get-comp type)
          (case type
            [(exclusive) #'<]
            [(inclusive) #'<=]
            [(infinite) #f]
            [else (error "cannot happen")]))
        (build-in-predicate range-who #'pred #'annot-str lo (get-comp lo-type) hi (get-comp hi-type)
                            #:check-lo-hi-stx check-lo-hi-stx)]
       [else
        #`(let ([rng #,unwrapped-e])
            (unless (#,is-range?-id rng)
              (raise-annotation-failure 'form-id rng "Range"))
            (lambda (v)
              (and (pred v)
                   (#,range-contains?-id rng v))))])]))

(define-annotation-syntax Real.in
  (make-in-annotation
   #'real?
   "Real"
   get-real-static-infos
   #f))

(define (handle-integer-in form)
  (and (pair? (cdr form))
       (exact-integer? (cadr form))
       (pair? (cddr form))
       (exact-integer? (caddr form))
       (null? (cdddr form))
       (string-append
        "Int.in("
        (number->string (cadr form)) " ..= " (number->string (caddr form))
        ")")))

(void (set-primitive-contract-combinator! 'integer-in handle-integer-in))
(define-annotation-syntax Int.in
  (make-in-annotation
   #'exact-integer?
   "Int"
   get-real-static-infos
   #t))

(define-annotation-syntax Any.of
  (annotation-prefix-operator
   #f
   '((default . stronger))
   'macro
   (lambda (stxes ctx)
     (syntax-parse stxes
       #:datum-literals (group)
       [(_ (_::parens g ...)
           . tail)
        (with-syntax ([(lit ...) (generate-temporaries #'(g ...))])
          (values (annotation-predicate-form
                   #'(let ([lit (rhombus-expression g)]
                           ...)
                       (lambda (v)
                         (or (equal-always? lit v)
                             ...)))
                   (if (null? (syntax->list #'(lit ...)))
                       #'((#%none #true))
                       #'()))
                  #'tail))]))))

(define-annotation-syntax Any.to_boolean
  (identifier-binding-annotation #,(binding-form #'to_boolean-infoer
                                                 #'val)
                                 val
                                 ()))

(define-for-syntax (select-like accessor-id id form ctx
                                #:make-data [make-data (lambda (x) x)])
  (define v (or (hash-ref (annotation-context-argument-names ctx)
                          (syntax-local-introduce id)
                          #f)
                (and (free-identifier=? id #'this)
                     (annotation-context-this-pos ctx))))
  (unless v
    (raise-syntax-error #f
                        "cannot find argument by name"
                        (respan form)
                        id))
  (define data (if (treelist? v) (treelist->list v) v))
  (annotation-predicate-form
   #'(lambda (x) #t)
   #`((#%dependent-result (#,accessor-id #,(make-data data))))))

(define-for-syntax (make-like accessor-id)
  (annotation-prefix-operator
   #f
   '((default . stronger))
   'macro
   (lambda (stxes ctx)
     (syntax-parse stxes
       #:datum-literals (group)
       [(form-id (~and args (_::parens (group id:identifier)))
                 . tail)
        (values (select-like accessor-id #'id #'(form-id args) ctx)
                #'tail)]))))

(define-for-syntax (get-argument-static-infos data deps)
  (define v (syntax->datum data))
  (or (cond
        [(integer? v)
         (define args (annotation-dependencies-args deps))
         (and (v . < . (length args))
              (list-ref args v))]
        [(keyword? v)
         (hash-ref (annotation-dependencies-kw-args deps) v #f)]
        [(and (pair? v)
              (or (eq? (car v) 'repet)
                  (eq? (car v) 'splice))
              (not (annotation-dependencies-rest? deps)))
         (define i (cadr v))
         (define args (annotation-dependencies-args deps))
         (cond
           [(i . < . (length args))
            (define si (for/fold ([si (car (list-tail args i))])
                                 ([new-si (list-tail args (add1 i))])
                         (static-infos-or si new-si)))
            (if (eq? (car v) 'splice)
                #`((#%index-result #,si)
                   #,@(indirect-get-treelist-static-infos))
                si)]
           [else
            (if (eq? (car v) 'splice)
                (indirect-get-treelist-static-infos)
                #f)])]
        [(and (pair? v)
              (eq? (car v) 'keyword_splice)
              (not (annotation-dependencies-kw-rest? deps)))
         (define kw-args
           (for/fold ([kw-args (annotation-dependencies-kw-args deps)]) ([kw (in-list (cdr v))])
             (hash-remove kw-args kw)))
         (define sis (hash-values kw-args))
         (define si
           (cond
             [(null? sis) #'()]
             [else (for/fold ([si (car sis)])
                             ([new-si (cdr sis)])
                     (static-infos-or si new-si))]))
         #`((#%sequence-element ((#%values (#,(indirect-get-keyword-static-infos)
                                            #,si))))
            #,@(indirect-get-map-static-infos))]
        [else #f])
      #'()))

(define-syntax (like-accessor data deps)
  (define si (get-argument-static-infos data deps))
  si)

(define-annotation-syntax Any.like
  (make-like #'like-accessor))

(define-syntax (like-element-accessor data deps)
  (define si (get-argument-static-infos data deps))
  (or (static-info-lookup si #'#%sequence-element)
      (extract-index-uniform-result (static-info-lookup si #'#%index-result))
      #'()))

(define-annotation-syntax Any.like_element
  (make-like #'like-element-accessor))

(define-syntax (like-first-accessor data deps)
  (static-info-lookup (get-argument-static-infos data deps) #'car))

(define-syntax (like-rest-accessor data deps)
  (static-info-lookup (get-argument-static-infos data deps) #'cdr))

(define-annotation-syntax Any.like_first
  (make-like #'like-first-accessor))

(define-annotation-syntax Any.like_rest
  (make-like #'like-rest-accessor))

(define-syntax (like-result-accessor data deps)
  (extract-call-result (get-argument-static-infos data deps)))

(define-for-syntax (extract-call-result statinfos)
  (define results (static-info-lookup statinfos #'#%call-result))
  (and results
       (find-call-result-at results #f #f #f
                            (lambda () (annotation-dependencies null #hasheq() #f #f)))))

(define-annotation-syntax Any.like_result
  (make-like #'like-result-accessor))

(define-for-syntax (like-sequence-values-accessor data deps key?)
  (define si (get-argument-static-infos data deps))
  (define se (static-info-lookup si #'#%sequence-element))
  (or (and se
           (syntax-parse (static-info-lookup se #'#%values)
             [(k v) (if key? #'k #'v)]
             [_ #f]))
      #'()))

(define-syntax (like-key-accessor data deps)
  (like-sequence-values-accessor data deps #t))

(define-annotation-syntax Any.like_key
  (make-like #'like-key-accessor))

(define-syntax (like-value-accessor data deps)
  (like-sequence-values-accessor data deps #f))

(define-annotation-syntax Any.like_value
  (make-like #'like-value-accessor))

(define-syntax (like-field data deps)
  (syntax-parse data
    [(accessor-id data)
     (define si (get-argument-static-infos #'data deps))
     (or (static-info-lookup si #'accessor-id)
         #'())]
    [_ #'()]))

(begin-for-syntax
  (define-syntax-class :dot
    #:description "dot operator"
    #:opaque
    #:datum-literals (op |.|)
    (pattern (op |.|))))

(define-annotation-syntax Any.like_field
  (annotation-prefix-operator
   #f
   '((default . stronger))
   'macro
   (lambda (stxes ctx)
     (syntax-parse stxes
       #:datum-literals (group)
       [(form-id (~and args (_::parens
                             (group class-name-seq ...+
                                    _::dot
                                    field-id:identifier
                                    (~and parens (_::parens . _)))))
                 . tail)
        (syntax-parse #'(class-name-seq ...)
          [(~var class-name (:hier-name-seq in-name-root-space in-class-desc-space name-path-op name-root-ref))
           (syntax-parse #'parens
             #:context (respan #'(form-id args))
             [(_parens (group id:identifier))
              (syntax-parse #'class-name.tail
                [(t . _) (raise-syntax-error #f
                                             "unexpected term after accessor name"
                                             (respan #'(form-id args))
                                             #'t)]
                [_ (void)])
              (define cls (or (syntax-local-value* (in-class-desc-space #'class-name.name) class-desc-ref)
                              (raise-syntax-error #f
                                                  "cannot find class"
                                                  (respan #'(form-id args))
                                                  #'class-name.name)))
              (define accessor-id (or (for/or ([f (in-list (class-desc-fields cls))])
                                        (and (eq? (field-desc-name f) (syntax-e #'field-id))
                                             (field-desc-accessor-id f)))
                                      (raise-syntax-error #f
                                                          "no such field in class"
                                                          (respan #'(form-id args))
                                                          #'field-id)))
              (values (select-like #'like-field #'id #'(form-id args) ctx
                                   #:make-data (lambda (data) #`(#,accessor-id #,data)))
                      #'tail)])]
          [_ (raise-syntax-error #f
                                 "expected a class name"
                                 (respan #'(form-id args))
                                 (respan #'(class-name-seq ...)))])]))))

(define-syntax (to_boolean-infoer stx)
  (syntax-parse stx
    [(_ static-infos val)
     (binding-info "to_boolean"
                   #'val
                   #'()
                   #'((val ([#:repet ()])))
                   #'empty-oncer
                   #'to_boolean-matcher
                   #'()
                   #'to_boolean-committer
                   #'to_boolean-binder
                   #'(converted-val val))]))

(define-syntax (to_boolean-matcher stx)
  (syntax-parse stx
    [(_ arg-id (converted-val val) IF success fail)
     #'(IF #t success fail)]))

(define-syntax (to_boolean-committer stx)
  (syntax-parse stx
    [(_ arg-id () (converted-val val))
     #'(define converted-val (and arg-id #t))]))

(define-syntax (to_boolean-binder stx)
  (syntax-parse stx
    [(_ arg-id () (converted-val val))
     #'(define val converted-val)]))

(begin-for-syntax
  (define find-call-result-at (lambda (results arity kws kw-rest? get-arg-static-infos) #f))
  (define (set-find-call-result-at! proc)
    (set! find-call-result-at proc)))
