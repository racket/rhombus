#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     shrubbery/print
                     enforest/property
                     enforest/proc-name
                     enforest/name-parse
                     "srcloc.rkt"
                     "introducer.rkt"
                     "annotation-string.rkt"
                     "keyword-sort.rkt"
                     "macro-result.rkt"
                     "tag.rkt"
                     (for-syntax racket/base))
         "provide.rkt"
         "enforest.rkt"
         "annotation-operator.rkt"
         "expression.rkt"
         "binding.rkt"
         "name-root.rkt"
         "name-root-ref.rkt"
         "dotted-sequence-parse.rkt"
         "static-info.rkt"
         "parse.rkt"
         "parens.rkt"
         "if-blocked.rkt"
         "number.rkt"
         "is-static.rkt"
         "rhombus-primitive.rkt"
         "annotation-failure.rkt")

(provide is_a
         (for-spaces (#f
                      rhombus/bind)
                     ::
                     :~)
         (for-space rhombus/annot

                    None
                    Boolean
                    PosInt
                    NegInt
                    NonnegInt
                    PosReal
                    NegReal
                    NonnegReal
                    Integral
                    Rational
                    Exact
                    Inexact
                    Flonum
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

             in-annotation-space
             annot-quote

             check-annotation-result

             :annotation
             :annotation-predicate-form
             :annotation-binding-form
             :inline-annotation
             :unparsed-inline-annotation
             :annotation-infix-op+form+tail
             :annotate-op

             annotation-predicate-form
             annotation-binding-form

             parse-annotation-of
             parse-annotation-of/chaperone

             annotation-relative-precedence
             build-annotated-expression
             raise-unchecked-disallowed))

  (provide define-annotation-syntax
           define-annotation-constructor

           raise-annotation-failure))

(module+ for-arrow
  (provide (for-space rhombus/annot
                      ::)))

(begin-for-syntax
  ;; see also "annotation-operator.rkt"

  (define in-annotation-space (make-interned-syntax-introducer/add 'rhombus/annot))

  (define-syntax (annot-quote stx)
    (syntax-case stx ()
      [(_ id) #`(quote-syntax #,((make-interned-syntax-introducer 'rhombus/annot) #'id))]))

  (define (raise-not-a-annotation id)
    (raise-syntax-error #f
                        "not bound as an annotation"
                        id))

  (define (check-annotation-result form proc)
    (syntax-parse (if (syntax? form) form #'#f)
      [(~or* _::annotation-predicate-form _::annotation-binding-form) form]
      [_ (raise-bad-macro-result (proc-name proc) "annotation" form)]))

  (define (shrubbery-tail->string tail) (shrubbery-syntax->string #`(group . #,tail)))

  (define-rhombus-enforest
    #:enforest enforest-annotation
    #:syntax-class :annotation
    #:infix-more-syntax-class :annotation-infix-op+form+tail
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
    #:attributes (converter annotation-str static-infos)
    (pattern (~seq op::annotate-op ctc ...)
             #:with c::annotation (no-srcloc #`(#,group-tag ctc ...))
             #:with (~var ca (:annotation-converted (attribute op.check?))) #'c.parsed
             #:do [(when (and (not (attribute op.check?))
                              (syntax-e #'ca.converter))
                     (raise-unchecked-disallowed #'op.name (respan #'(ctc ...))))]
             #:with converter #'ca.converter
             #:with annotation-str (datum->syntax #f (shrubbery-tail->string #'(ctc ...)))
             #:with static-infos #'ca.static-infos))

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
              #:with converter #'(lambda (tmp-id who fail-k)
                                   (arg-info.matcher-id tmp-id
                                                        arg-info.data
                                                        if/blocked
                                                        (begin
                                                          (arg-info.committer-id tmp-id arg-info.data)
                                                          (arg-info.binder-id tmp-id arg-info.data)
                                                          (define-static-info-syntax/maybe bind-id . bind-static-infos)
                                                          ...
                                                          c-parsed.body)
                                                        (fail-k tmp-id who)))
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
     '((default . stronger))
     'macro
     (lambda (stx)
       (when static-only? (check-static stx))
       (define-values (predicate-stx static-infos) (get))
       (define packed (annotation-predicate-form predicate-stx static-infos))
       (syntax-parse stx
         [(self . tail)
          (values (relocate+reraw #'self packed) #'tail)]
         [_ 'does-not-happen]))))

  (define-syntax (identifier-binding-annotation stx)
    (syntax-case stx ()
      [(_ binding body static-infos)
       #'(make-identifier-binding-annotation (lambda () (values #`binding #`body #`static-infos)))]
      [(_ binding body static-infos #:static-only)
       #'(make-identifier-binding-annotation (lambda () (values #`binding #`body #`static-infos)) #t)]))

  (define (make-identifier-binding-annotation get [static-only? #f])
    (annotation-prefix-operator
     '((default . stronger))
     'macro
     (lambda (stx)
       (when static-only? (check-static stx))
       (define-values (binding-stx body-stx static-infos) (get))
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

  (define (parse-annotation-of/one stx sub-n kws)
    (syntax-parse stx
      [(form-id (~and subs (_::parens g ...)) . tail)
       (define new-stx #'(form-id subs))
       (define unsorted-gs (syntax->list #'(g ...)))
       (unless (eqv? (length unsorted-gs) sub-n)
         (raise-syntax-error #f
                             "wrong number of subannotations in parentheses"
                             new-stx))
       (define gs (sort-with-respect-to-keywords kws unsorted-gs new-stx))
       (values new-stx
               gs
               (for/list ([g (in-list gs)])
                 (syntax-parse g
                   [c::annotation #'c.parsed]))
               (datum->syntax #f (list #'form-id #'subs))
               #'tail)]))

  (define (parse-annotation-of stx predicate-stx static-infos
                               sub-n kws
                               ;; predicate-maker can be #f if only a converter is supported
                               predicate-maker info-maker
                               ;; binding-maker-id can be #f or an error string if a converter is not supported
                               binding-maker-id binding-maker-data)
    (define-values (new-stx gs c-parseds loc tail)
      (parse-annotation-of/one stx sub-n kws))
    (values
     (cond
       [(and predicate-maker
             (syntax-parse c-parseds
               [(c::annotation-predicate-form ...)
                (define c-predicates (syntax->list #'(c.predicate ...)))
                (define c-static-infoss (syntax->list #'(c.static-infos ...)))
                (relocate+reraw
                 loc
                 (annotation-predicate-form #`(lambda (v)
                                                (and (#,predicate-stx v)
                                                     #,(predicate-maker #'v c-predicates)))
                                            #`(#,@(info-maker c-static-infoss)
                                               . #,static-infos)))]
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
           (relocate+reraw
            loc
            (annotation-binding-form
             (binding-form #'annotation-of-infoer
                           #`[#,(shrubbery-tail->string new-stx)
                              #,predicate-stx #,binding-maker-id #,binding-maker-data
                              ([c.binding c.body] ...) #,static-infos result
                              #,kws])
             #'result
             #`(#,@(info-maker c-static-infoss)
                . #,static-infos)))])])
     tail))

  ;; This one is for converters that produce chaperones/impersonators.
  ;; The predicate case also produces a converter, because it has to
  ;; chaperone the original value.  Also, the makers receive the raw
  ;; text of subannotations so that they can compose a better error
  ;; message.
  (define (parse-annotation-of/chaperone stx predicate-stx static-infos
                                         sub-n kws
                                         predicate-maker info-maker
                                         binding-maker-id binding-maker-data)
    (define-values (new-stx gs c-parseds loc tail)
      (parse-annotation-of/one stx sub-n kws))
    (define annot-strs (map shrubbery-tail->string gs))
    (values
     (syntax-parse c-parseds
       [(c::annotation-predicate-form ...)
        (define c-predicates (syntax->list #'(c.predicate ...)))
        (define c-static-infoss (syntax->list #'(c.static-infos ...)))
        (relocate+reraw
         loc
         (annotation-binding-form
          (binding-form #'annotation-of-infoer/chaperone
                        #`[#,(shrubbery-tail->string new-stx)
                           (lambda (val-in)
                             (and (#,predicate-stx val-in)
                                  (#,(predicate-maker c-predicates annot-strs) val-in)))
                           #,static-infos
                           result])
          #'result
          #`(#,@(info-maker c-static-infoss)
             . #,static-infos)))]
       [(c::annotation-binding-form ...)
        (unless (identifier? binding-maker-id)
          (raise-syntax-error #f
                              (or binding-maker-id
                                  "argument converter annotations are not supported")
                              new-stx))
        (define c-static-infoss (syntax->list #'(c.static-infos ...)))
        (relocate+reraw
         loc
         (annotation-binding-form
          (binding-form #'annotation-of-infoer
                        #`[#,(shrubbery-tail->string new-stx)
                           #,predicate-stx #,binding-maker-id [#,annot-strs #,binding-maker-data]
                           ([c.binding c.body] ...) #,static-infos result
                           #,kws])
          #'result
          #`(#,@(info-maker c-static-infoss)
             . #,static-infos)))])
     tail))

  (define (annotation-constructor predicate-stx get-static-infos
                                  sub-n kws predicate-maker info-maker
                                  binding-maker-id binding-maker-data
                                  parse-annotation-of)
    (define root
      (annotation-prefix-operator
       '((default . stronger))
       'macro
       (lambda (stx)
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
      '((default . stronger))
      'macro
      (lambda (stx)
        (parse-annotation-of (replace-head-dotted-name stx)
                             predicate-stx (get-static-infos)
                             sub-n kws
                             predicate-maker info-maker
                             binding-maker-id binding-maker-data))))))

(define-syntax (define-annotation-constructor stx)
  (syntax-parse stx
    [(_ (name of-name)
        binds
        predicate-stx static-infos
        sub-n kws predicate-maker info-maker
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
                                    predicate-maker info-maker
                                    binding-maker-id binding-maker-data
                                    parse-annotation-of-id))))
     (if (and (pair? defs) (null? (cdr defs)))
         (car defs)
         #`(begin #,@defs))]))

(define-for-syntax (make-annotation-apply-expression-operator checked?)
  (expression-infix-operator
   `((default . weaker))
   'macro
   (lambda (form tail)
     (syntax-parse tail
       [(op::name . (~var t (:annotation-seq #'::)))
        (define op-name (string->symbol (shrubbery-syntax->string #'op.name)))
        (values
         (build-annotated-expression #'op.name #'t
                                     checked? form #'t.parsed
                                     (lambda (tmp-id)
                                       #`(raise-::-annotation-failure '#,op-name #,tmp-id '#,(shrubbery-syntax->string #'t.parsed)))
                                     wrap-static-info*)
         #'t.tail)]))
   'none))

(define-for-syntax (build-annotated-expression form-name orig-stx checked? form t-parsed build-fail k)
  (syntax-parse t-parsed
    [c-parsed::annotation-predicate-form
     (k
      (if checked?
          #`(let ([val #,form])
              (if (c-parsed.predicate val)
                  val
                  #,(build-fail #'val)))
          form)
      #'c-parsed.static-infos)]
    [c-parsed::annotation-binding-form
     #:do [(unless checked?
             (raise-unchecked-disallowed form-name orig-stx))]
     #:with arg-parsed::binding-form #'c-parsed.binding
     #:with arg-impl::binding-impl #'(arg-parsed.infoer-id () arg-parsed.data)
     #:with arg-info::binding-info #'arg-impl.info
     #:with ((bind-id bind-use . bind-static-infos) ...) #'arg-info.bind-infos
     (k
      #`(let* ([tmp-id (let ([arg-info.name-id #,form])
                         arg-info.name-id)]
               [fail-k (lambda () #,(build-fail #'tmp-id))])
          (arg-info.matcher-id tmp-id
                               arg-info.data
                               if/blocked
                               (begin
                                 (arg-info.committer-id tmp-id arg-info.data)
                                 (arg-info.binder-id tmp-id arg-info.data)
                                 (define-static-info-syntax/maybe bind-id . bind-static-infos)
                                 ...
                                 c-parsed.body)
                               (fail-k)))
      #'c-parsed.static-infos)]))

(define-for-syntax (raise-unchecked-disallowed in at)
  (raise-syntax-error #f
                      "converter annotation not allowed in a non-checked position"
                      in
                      at))

(define-for-syntax (make-annotation-apply-binding-operator checked?)
  (binding-infix-operator
   `((default . weaker))
   'macro
   (lambda (form tail)
     (syntax-parse tail
       [(op::name . (~var t (:annotation-seq #'::)))
        #:with left::binding-form form
        (values
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
                left.data))])
         #'t.tail)]))
   'none))

(define-syntax ::
  (make-annotation-apply-expression-operator #t))
(define-binding-syntax ::
  (make-annotation-apply-binding-operator #t))

(define-syntax :~
  (make-annotation-apply-expression-operator #f))
(define-binding-syntax :~
  (make-annotation-apply-binding-operator #f))

(define-syntax is_a
  (expression-infix-operator
   '((default . weaker))
   'macro
   (lambda (form tail)
     (syntax-parse tail
       [(op . (~var t (:annotation-seq #'is_a)))
        (values
         (syntax-parse #'t.parsed
           [c-parsed::annotation-predicate-form
            #`(c-parsed.predicate #,form)]
           [c-parsed::annotation-binding-form
            #:with arg-parsed::binding-form #'c-parsed.binding
            #:with arg-impl::binding-impl #'(arg-parsed.infoer-id () arg-parsed.data)
            #:with arg-info::binding-info #'arg-impl.info
            #`(let ([val-in (let ([arg-info.name-id #,form])
                              arg-info.name-id)])
                (arg-info.matcher-id val-in
                                     arg-info.data
                                     if/blocked
                                     #t
                                     #f))])
         #'t.tail)]))
   'none))

(define-syntax (annotation-predicate-infoer stx)
  (syntax-parse stx
    [(_ static-infos (annotation-str predicate implied-static-infos left-infoer-id left-data))
     #:with left-impl::binding-impl #`(left-infoer-id #,(static-infos-union #'implied-static-infos #'static-infos) left-data)
     #:with left::binding-info #'left-impl.info
     (if (syntax-e #'predicate)
         (binding-info (annotation-string-and (syntax-e #'annotation-str) (syntax-e #'left.annotation-str))
                       #'left.name-id
                       #'left.static-infos ; presumably includes `implied-static-infos` as passed to `left-infoer-id`
                       #'left.bind-infos
                       #'check-predicate-matcher
                       #'commit-nothing-new
                       #'bind-nothing-new
                       #'(predicate left.matcher-id left.committer-id left.binder-id left.data))
         #'left)]))

(define-syntax (check-predicate-matcher stx)
  (syntax-parse stx
    [(_ arg-id (predicate left-matcher-id left-committer-id left-binder-id left-data) IF success fail)
     #'(IF (predicate arg-id)
           (left-matcher-id
            arg-id
            left-data
            IF
            success
            fail)
           fail)]))

(define-syntax (commit-nothing-new stx)
  (syntax-parse stx
    [(_ arg-id (predicate left-matcher-id left-committer-id left-binder-id left-data))
     #'(left-committer-id arg-id left-data)]))

(define-syntax (bind-nothing-new stx)
  (syntax-parse stx
    [(_ arg-id (predicate left-matcher-id left-committer-id left-binder-id left-data))
     #'(left-binder-id arg-id left-data)]))

(define-syntax (annotation-binding-infoer stx)
  (syntax-parse stx
    [(_ static-infos (annotation-str binding body body-static-infos left-infoer-id left-data))
     #:with arg-parsed::binding-form #'binding
     #:with arg-impl::binding-impl #'(arg-parsed.infoer-id static-infos arg-parsed.data)
     #:with arg-info::binding-info #'arg-impl.info
     #:with left-impl::binding-impl #'(left-infoer-id body-static-infos left-data)
     #:with left::binding-info #'left-impl.info
     #:with converted-id ((make-syntax-introducer) (datum->syntax #f (syntax-e #'left.name-id)))
     (define (build-binding-info matcher-id committer-id)
       (binding-info (annotation-string-and (syntax-e #'annotation-str) (syntax-e #'left.annotation-str))
                     #'left.name-id
                     #'arg-info.static-infos ; this is about the value coming in, not the converted value
                     #'left.bind-infos
                     matcher-id
                     committer-id
                     #'bind-via-converted
                     #'([left.matcher-id left.committer-id left.binder-id left.data]
                        converted-id
                        [arg-info.matcher-id arg-info.committer-id arg-info.binder-id arg-info.data
                                             arg-info.bind-infos body])))
     (cond
       [(free-identifier=? #'always-succeed #'left.matcher-id)
        ;; in this case, we can commit and bind lazily for the annotation's binding
        (build-binding-info #'check-binding-check-convert
                            #'commit-convert-then-via-converted)]
       [else
        ;; in this case, we have to apply the annotation's binding's conversion before
        ;; we can apply the left-hand's matcher
        (build-binding-info #'check-binding-convert
                            #'commit-via-converted)])]))

(define-syntax (check-binding-check-convert stx)
  (syntax-parse stx
    [(_ arg-id ([left-matcher-id left-committer-id left-binder-id left-data]
                converted-id
                [arg-matcher-id arg-committer-id arg-binder-id arg-data . _])
        IF success fail)
     ;; If we get here, then `left-matcher-id` is `always-succeed`
     #'(arg-matcher-id arg-id
                       arg-data
                       IF
                       success
                       fail)]))

(define-syntax (commit-convert-then-via-converted stx)
  (syntax-parse stx
    [(_ arg-id ([left-matcher-id left-committer-id left-binder-id left-data]
                converted-id
                [arg-matcher-id arg-committer-id arg-binder-id arg-data
                                arg-bind-infos body]))
     #:with ((bind-id bind-use . bind-static-infos) ...) #'arg-bind-infos
     #'(begin
         (arg-committer-id arg-id arg-data)
         (arg-binder-id arg-id arg-data)
         (define-static-info-syntax/maybe bind-id . bind-static-infos)
         ...
         (define converted-id body)
         (left-committer-id converted-id left-data))]))

(define-syntax (bind-via-converted stx)
  (syntax-parse stx
    [(_ arg-id ([left-matcher-id left-committer-id left-binder-id left-data]
                converted-id
                arg/ignored-because-done))
     #'(left-binder-id converted-id left-data)]))

(define-syntax (check-binding-convert stx)
  (syntax-parse stx
    [(_ arg-id ([left-matcher-id left-committer-id left-binder-id left-data]
                converted-id
                [arg-matcher-id arg-committer-id arg-binder-id arg-data
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
                         (arg-committer-id arg-id arg-data)
                         (arg-binder-id arg-id arg-data)
                         (define-static-info-syntax/maybe bind-id . bind-static-infos)
                         ...
                         (define converted-id body)
                         (left-matcher-id converted-id
                                          left-data
                                          IF
                                          success
                                          fail))
                       fail)]))

(define-syntax (commit-via-converted stx)
  (syntax-parse stx
    [(_ arg-id ([left-matcher-id left-committer-id left-binder-id left-data]
                converted-id
                arg-ignored-because-done))
     ;; If we get here, then `left-matcher-id` is `always-succeed`
     #'(left-committer-id converted-id left-data)]))

(define-syntax (annotation-of-infoer stx)
  (syntax-parse stx
    [(_ rhs-static-infos [annotation-str
                          predicate-stx binding-maker-id binding-maker-data
                          ([c-binding c-body] ...) static-infos result-id kws])
     (define binding-maker (syntax-local-value #'binding-maker-id))
     (define converter
       #`(lambda (val-in)
           (and (predicate-stx val-in)
                #,(binding-maker
                   #'val-in
                   (for/list ([c-binding (in-list (syntax->list #'(c-binding ...)))]
                              [c-body (in-list (syntax->list #'(c-body ...)))])
                     (syntax-parse c-binding
                       [arg-parsed::binding-form
                        #:with arg-impl::binding-impl #'(arg-parsed.infoer-id () arg-parsed.data)
                        #:with arg-info::binding-info #'arg-impl.info
                        #:with ((bind-id bind-use . bind-static-infos) ...) #'arg-info.bind-infos
                        #`(lambda (val-in success-k fail-k)
                            (arg-info.matcher-id val-in
                                                 arg-info.data
                                                 if/blocked
                                                 (begin
                                                   (arg-info.committer-id val-in arg-info.data)
                                                   (arg-info.binder-id val-in arg-info.data)
                                                   (define-static-info-syntax/maybe bind-id . bind-static-infos)
                                                   ...
                                                   (success-k #,c-body))
                                                 (fail-k)))]))
                   (syntax->datum #'kws)
                   #'binding-maker-data))))
     (binding-info #'annotation-str
                   #'composite
                   #'static-infos
                   #'((result-id (0)))
                   #'annotation-of-matcher
                   #'annotation-of-committer
                   #'annotation-of-binder
                   #`[result-id temp #,converter])]))

(define-syntax (annotation-of-infoer/chaperone stx)
  (syntax-parse stx
    [(_ rhs-static-infos [annotation-str convert static-infos result-id])
     (binding-info #'annotation-str
                   #'composite
                   #'static-infos
                   #'((result-id (0)))
                   #'annotation-of-matcher
                   #'annotation-of-committer
                   #'annotation-of-binder
                   #'[result-id temp convert])]))

(define-syntax (annotation-of-matcher stx)
  (syntax-parse stx
    [(_ arg-id [bind-id temp-id convert]
        IF success fail)
     #'(begin
         (define temp-id (convert arg-id))
         (IF temp-id success fail))]))

(define-syntax (annotation-of-committer stx)
  (syntax-parse stx
    [(_ arg-id [bind-id temp-id convert])
     #'(begin)]))

(define-syntax (annotation-of-binder stx)
  (syntax-parse stx
    [(_ arg-id [bind-id temp-id convert])
     #'(define bind-id temp-id)]))

(define-syntax (annotation-predicate-binding-infoer stx)
  (syntax-parse stx
    [(_ left-static-infos (arg predicate static-infos))
     (define all-static-infos (static-infos-union #'static-infos #'left-static-infos))
     (binding-info #'"<converted predicate>"
                   #'arg
                   all-static-infos
                   #`((arg (0) . #,all-static-infos))
                   (syntax-parse #'predicate
                     [(lam (_) #t) ;; matches `Any` and maybe more
                      #:when (or (free-identifier=? #'lam #'lambda)
                                 (free-identifier=? #'lam #'#%plain-lambda))
                      #'always-succeed]
                     [else
                      #'predicate-binding-matcher])
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
    [(_ arg-id [bind-id predicate])
     #'(begin)]))

(define-syntax (predicate-binding-binder stx)
  (syntax-parse stx
    [(_ arg-id [bind-id predicate])
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
(void (set-primitive-contract! 'exact-nonnegative-integer? "NonnegInt"))
(void (set-primitive-contract! 'number? "Number"))
(void (set-primitive-contract! 'integer? "Integral"))
(void (set-primitive-contract! 'real? "Real"))
(define-annotation-syntax Any (identifier-annotation (lambda (x) #t) ()))
(define-annotation-syntax None (identifier-annotation (lambda (x) #f) ()))
(define-annotation-syntax Boolean (identifier-annotation boolean? ()))
(define-annotation-syntax Int (identifier-annotation exact-integer? #,(get-int-static-infos)))
(define-annotation-syntax PosInt (identifier-annotation exact-positive-integer? #,(get-int-static-infos)))
(define-annotation-syntax NegInt (identifier-annotation exact-negative-integer? #,(get-int-static-infos)))
(define-annotation-syntax NonnegInt (identifier-annotation exact-nonnegative-integer? #,(get-int-static-infos)))
(define-annotation-syntax Flonum (identifier-annotation flonum? #,(get-flonum-static-infos)))
(define-annotation-syntax Number (identifier-annotation number? #,(get-number-static-infos)))
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
   [to_boolean Any.to_boolean]))

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
   `((default . weaker))
   'macro
   (lambda (form tail) (error "should not get here"))
   'none))
(define-annotation-syntax is_a
  (annotation-infix-operator
   `((default . stronger))
   'macro
   (lambda (form tail) (error "should not get here"))
   'none))

(define (raise-::-annotation-failure who val ctc)
  (raise-annotation-failure who val ctc))

(define-annotation-syntax matching
  (annotation-prefix-operator
   '((default . stronger))
   'macro
   (lambda (stx)
     (syntax-parse stx
       [(form-id (~and args (_::parens arg::binding)) . tail)
        #:with arg-parsed::binding-form #'arg.parsed
        #:with arg-impl::binding-impl #'(arg-parsed.infoer-id () arg-parsed.data)
        #:with arg-info::binding-info #'arg-impl.info
        (values
         (relocate+reraw
          (datum->syntax #f (list #'form-id #'args))
          (annotation-predicate-form
           #`(lambda (val-in)
               (arg-info.matcher-id val-in
                                    arg-info.data
                                    if/blocked
                                    #t
                                    #f))
           #'arg-info.static-infos))
         #'tail)]))))

(define-annotation-syntax satisfying
  (annotation-prefix-operator
   '((default . stronger))
   'macro
   (lambda (stx)
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
   '((default . stronger))
   'macro
   (lambda (stxes)
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
   '((default . stronger))
   'macro
   (lambda (stxes)
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
             #:with comp #'<=)))

(define-for-syntax (make-in-annotation pred-stx annot-str)
  (annotation-prefix-operator
   '((default . stronger))
   'macro
   (lambda (stxes)
     (syntax-parse stxes
       #:datum-literals (group)
       [(form-id (_::parens lo::incl-group hi::incl-group)
                 . tail)
        (values (annotation-predicate-form
                 #`(let ([lo-v (rhombus-expression lo.g)]
                         [hi-v (rhombus-expression hi.g)])
                     (unless (#,pred-stx lo-v)
                       (raise-annotation-failure 'form-id lo-v '#,annot-str))
                     (unless (#,pred-stx hi-v)
                       (raise-annotation-failure 'form-id hi-v '#,annot-str))
                     (lambda (v)
                       (and (#,pred-stx v)
                            (lo.comp lo-v v)
                            (hi.comp v hi-v))))
                 #'())
                #'tail)]))))

(define-annotation-syntax Real.in
  (make-in-annotation
   #'real?
   "Real"))

(define (handle-integer-in form)
  (and (pair? (cdr form))
       (exact-integer? (cadr form))
       (pair? (cddr form))
       (exact-integer? (caddr form))
       (null? (cdddr form))
       (string-append "Int.in(" (number->string (cadr form))
                      ", " (number->string (caddr form))
                      ")")))

(void (set-primitive-contract-combinator! 'integer-in handle-integer-in))
(define-annotation-syntax Int.in
  (make-in-annotation
   #'exact-integer?
   "Int"))

(define-annotation-syntax Any.of
  (annotation-prefix-operator
   '((default . stronger))
   'macro
   (lambda (stxes)
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
                   #'())
                  #'tail))]))))

(define-annotation-syntax Any.to_boolean
  (identifier-binding-annotation #,(binding-form #'to_boolean-infoer
                                                 #'val)
                                 val
                                 ()))

(define-syntax (to_boolean-infoer stx)
  (syntax-parse stx
    [(_ static-infos val)
     (binding-info "to_boolean"
                   #'val
                   #'()
                   #'((val (0)))
                   #'to_boolean-matcher
                   #'to_boolean-committer
                   #'to_boolean-binder
                   #'(converted-val val))]))

(define-syntax (to_boolean-matcher stx)
  (syntax-parse stx
    [(_ arg-id (converted-val val) IF success fail)
     #'(IF #t success fail)]))

(define-syntax (to_boolean-committer stx)
  (syntax-parse stx
    [(_ arg-id (converted-val val))
     #'(define converted-val (and arg-id #t))]))

(define-syntax (to_boolean-binder stx)
  (syntax-parse stx
    [(_ arg-id (converted-val val))
     #'(define val converted-val)]))
