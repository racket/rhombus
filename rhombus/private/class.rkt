#lang racket/base
(require (for-syntax racket/base
                     racket/symbol
                     syntax/parse/pre
                     enforest/syntax-local
                     "class-parse.rkt"
                     (submod "class-meta.rkt" for-class)
                     "class-field-parse.rkt"
                     "interface-parse.rkt")
         "provide.rkt"
         "forwarding-sequence.rkt"
         "definition.rkt"
         (submod "dot.rkt" for-dot-provider)
         "space.rkt"
         "class-clause.rkt"
         "class-clause-parse.rkt"
         "class-clause-tag.rkt"
         "class-step.rkt"
         "class-constructor.rkt"
         "class-binding.rkt"
         "class-annotation.rkt"
         "class-dot.rkt"
         "class-reconstructor.rkt"
         "class-static-info.rkt"
         "class-field.rkt"
         "class-method.rkt"
         "class-desc.rkt"
         "class-top-level.rkt"
         "dotted-sequence-parse.rkt"
         "parens.rkt"
         (submod "namespace.rkt" for-exports)
         (submod "print.rkt" for-class)
         "class-able.rkt"
         "index-property.rkt"
         "append-property.rkt"
         "reconstructor.rkt")

(provide this
         super
         (for-spaces (rhombus/space
                      rhombus/defn)
                     class))

(define-space-syntax class
  (space-syntax rhombus/class))

(define-defn-syntax class
  (definition-transformer
    (lambda (stxes)
      (parse-class stxes))))

(define-for-syntax (parse-class stxes)
  (syntax-parse stxes
    #:datum-literals (group)
    [(_ name-seq::dotted-identifier-sequence (tag::parens field::constructor-field ...)
        options::options-block)
     #:with full-name::dotted-identifier #'name-seq
     #:with name #'full-name.name
     #:with name-extends #'full-name.extends
     #:with tail-name #'full-name.tail-name
     #:with orig-stx stxes
     (define body #'(options.form ...))
     (define intro (make-syntax-introducer #t))
     ;; The shape of `finish-data` is recognzied in `class-annotation+finish`
     ;; and "class-meta.rkt"
     (define finish-data #`([orig-stx base-stx #,(intro #'scope-stx)
                                      name name-extends tail-name
                                      (field.name ...)
                                      (field.keyword ...)
                                      (field.default ...)
                                      (field.mutable ...)
                                      (field.private ...)
                                      (field.ann-seq ...)]
                            ;; data accumulated from parsed clauses:
                            ()))
     #`(#,(cond
            [(null? (syntax-e body))
             #`(class-annotation+finish #,finish-data [#:ctx base-stx base-stx] ())]
            [else
             #`(rhombus-mixed-nested-forwarding-sequence
                (class-annotation+finish #,finish-data) rhombus-class
                (class-body-step #,finish-data . #,(intro body)))]))]))

(define-class-body-step class-body-step
  :class-clause
  class-expand-data
  class-clause-accum)

;; First phase of `class` output: bind the annotation form, so it can be used
;; in field declarations
(define-syntax class-annotation+finish
  (lambda (stx)
    (syntax-parse stx
      [(_ ([orig-stx base-stx init-scope-stx
                     name name-extends tail-name
                     constructor-field-names
                     constructor-field-keywords
                     constructor-field-defaults
                     constructor-field-mutables
                     constructor-field-privates
                     constructor-field-ann-seqs]
           . _)
          [#:ctx forward-base-ctx forward-ctx]
          exports
          [option stx-params] ...)
       #:with scope-stx ((make-syntax-delta-introducer #'forward-ctx #'forward-base-ctx) #'init-scope-stx)
       (define options (parse-annotation-options #'orig-stx #'(option ...) #'(stx-params ...)))
       (define parent-name (hash-ref options 'extends #f))
       (define super (and parent-name
                          (or (syntax-local-value* (in-class-desc-space parent-name) class-desc-ref)
                              (raise-syntax-error #f "not a class name" #'orig-stx parent-name))))
       (define-values (super-constructor-fields super-keywords super-defaults)
         (extract-super-constructor-fields super))

       (define interface-names (reverse (hash-ref options 'implements '())))
       (define interfaces (interface-names->interfaces #'orig-stx interface-names))
       (define private-interfaces (interface-set-diff
                                   (interface-names->interfaces #'orig-stx (hash-ref options 'private-implements '()))
                                   (interface-names->interfaces #'orig-stx (hash-ref options 'public-implements '()))))

       (define-values (internal-id exposed-internal-id extra-exposed-internal-ids)
         (extract-internal-ids options
                               #'scope-stx #'base-stx
                               #'orig-stx))
       (define internal-of-id (and internal-id
                                   (car (generate-temporaries '(internal-of)))))
       (define make-converted-internal (and internal-id
                                            (car (generate-temporaries '(make-converted-internal)))))

       (define annotation-rhs (hash-ref options 'annotation-rhs #f))
       (define expression-macro-rhs (hash-ref options 'expression-rhs #f))

       (define prefab? (hash-ref options 'prefab? #f))
       (define final? (hash-ref options 'final? (not prefab?)))
       (define has-mutable-field? (or (hash-ref options 'has-mutable-field? #f)
                                      (for/or ([mut (in-list (syntax->list #'constructor-field-mutables))])
                                        (syntax-e mut))
                                      (and super
                                           (super-has-mutable-field? super))))

       (define intro (make-syntax-introducer))
       (define constructor-name-fields
         (make-accessor-names #'name
                              (syntax->list #'constructor-field-names)
                              intro))

       (define-values (call-statinfo-indirect-id
                       index-statinfo-indirect-id
                       index-set-statinfo-indirect-id
                       append-statinfo-indirect-id
                       compare-statinfo-indirect-id

                       super-call-statinfo-indirect-id

                       static-infos-id
                       static-infos-exprs
                       instance-static-infos

                       indirect-static-infos
                       internal-indirect-static-infos)
         (extract-instance-static-infoss #'name options super interfaces private-interfaces intro))

       (with-syntax ([constructor-name-fields constructor-name-fields]
                     [((constructor-public-name-field constructor-public-field-keyword) ...)
                      (for/list ([priv?-stx (in-list (syntax->list #'constructor-field-privates))]
                                 [name-field (in-list constructor-name-fields)]
                                 [field-keyword (in-list (syntax->list #'constructor-field-keywords))]
                                 #:unless (syntax-e priv?-stx))
                        (list name-field field-keyword))]
                     [name-instance (intro (datum->syntax #'name (string->symbol (format "~a.instance" (syntax-e #'name))) #'name))]
                     [internal-name-instance (and internal-id
                                                  (intro (datum->syntax #f (string->symbol
                                                                            (format "~a-internal-instance" (syntax-e #'name))))))]
                     [internal-of internal-of-id]
                     [name? (datum->syntax #'name (string->symbol (format "~a?" (syntax-e #'name))) #'name)]
                     [name-of (intro (datum->syntax #'name (string->symbol (format "~a-of" (syntax-e #'name))) #'name))]
                     [make-converted-name (and (not expression-macro-rhs)
                                               final?
                                               (not has-mutable-field?)
                                               (intro (datum->syntax #'name
                                                                     (string->symbol (format "make-converted-~a" (syntax-e #'name)))
                                                                     #'name)))]
                     [make-converted-internal make-converted-internal]
                     [call-statinfo-indirect call-statinfo-indirect-id]
                     [index-statinfo-indirect index-statinfo-indirect-id]
                     [index-set-statinfo-indirect index-set-statinfo-indirect-id]
                     [append-statinfo-indirect append-statinfo-indirect-id]
                     [compare-statinfo-indirect compare-statinfo-indirect-id]
                     [super-call-statinfo-indirect super-call-statinfo-indirect-id]
                     [(super-field-keyword ...) super-keywords]
                     [((super-field-name super-name-field . _) ...) (if super
                                                                        (class-desc-fields super)
                                                                        '())]
                     [((super-public-name-field super-public-field-keyword) ...) (if super
                                                                                     (for/list ([fld (in-list (class-desc-fields super))]
                                                                                                #:unless (identifier? (field-desc-constructor-arg fld)))
                                                                                       (list
                                                                                        (field-desc-accessor-id fld)
                                                                                        (let ([arg (field-desc-constructor-arg fld)])
                                                                                          (cond
                                                                                            [(keyword? (syntax-e arg)) arg]
                                                                                            [(box? (syntax-e arg))
                                                                                             (define c (unbox (syntax-e arg)))
                                                                                             (and (keyword? (syntax-e c)) c)]
                                                                                            [else #f]))))
                                                                                     '())]
                     [indirect-static-infos indirect-static-infos]
                     [internal-indirect-static-infos internal-indirect-static-infos]
                     [instance-static-infos instance-static-infos])
         (values
          #`(begin
              #,@(top-level-declare #'(name? . constructor-name-fields))
              #,@(build-instance-static-infos-defs static-infos-id static-infos-exprs)
              #,@(with-syntax ([dot-providers (add-super-dot-providers #'name-instance super interfaces)]
                               [internal-dot-providers (and exposed-internal-id
                                                            (add-super-dot-providers #'internal-name-instance super interfaces))])
                   (build-class-annotation-form super annotation-rhs
                                                super-constructor-fields
                                                exposed-internal-id internal-of-id intro
                                                #'(name name-extends tail-name
                                                        name-instance name? name-of
                                                        internal-name-instance indirect-static-infos internal-indirect-static-infos
                                                        dot-providers internal-dot-providers
                                                        make-converted-name make-converted-internal
                                                        constructor-name-fields [constructor-public-name-field ...]
                                                        [super-name-field ...] [super-public-name-field ...]
                                                        constructor-field-keywords [constructor-public-field-keyword ...]
                                                        [super-field-keyword ...] [super-public-field-keyword ...])))
              #,@(build-extra-internal-id-aliases exposed-internal-id extra-exposed-internal-ids)
              (class-finish
               [orig-stx base-stx scope-stx
                         name name-extends tail-name
                         name? name-of make-converted-name
                         name-instance internal-name-instance internal-of make-converted-internal
                         call-statinfo-indirect index-statinfo-indirect index-set-statinfo-indirect
                         append-statinfo-indirect compare-statinfo-indirect
                         super-call-statinfo-indirect
                         indirect-static-infos
                         instance-static-infos
                         constructor-field-names
                         constructor-field-keywords
                         constructor-field-defaults
                         constructor-field-mutables
                         constructor-field-privates
                         constructor-field-ann-seqs
                         constructor-name-fields]
               exports
               [option stx-params] ...))))])))

(define-syntax class-finish
  (lambda (stx)
    (syntax-parse stx
      [(_ [orig-stx base-stx scope-stx
                    name name-extends tail-name
                    name? name-of make-converted-name
                    name-instance internal-name-instance internal-of make-converted-internal
                    call-statinfo-indirect index-statinfo-indirect index-set-statinfo-indirect
                    append-statinfo-indirect compare-statinfo-indirect
                    super-call-statinfo-indirect
                    indirect-static-infos
                    instance-static-infos
                    (constructor-field-name ...)
                    (constructor-field-keyword ...) ; #f or keyword
                    (constructor-field-default ...) ; #f or (parsed)
                    (constructor-field-mutable ...)
                    (constructor-field-private ...)
                    (constructor-field-ann-seq ...)
                    (constructor-name-field ...)]
          exports
          [option stx-params] ...)
       #:with [(constructor-field-converter constructor-field-annotation-str constructor-field-static-infos)
               ...] (parse-field-annotations #'(constructor-field-ann-seq ...))
       (define stxes #'orig-stx)
       (define options (parse-options #'orig-stx #'(option ...) #'(stx-params ...)))
       (define parent-name (hash-ref options 'extends #f))
       (define super (and parent-name
                          (syntax-local-value* (in-class-desc-space parent-name) class-desc-ref)))
       (define interface-names (reverse (hash-ref options 'implements '())))
       (define-values (all-interfaces interfaces) (interface-names->interfaces stxes interface-names
                                                                               #:results values))
       (define private-interfaces (interface-set-diff
                                   (interface-names->interfaces stxes (hash-ref options 'private-implements '()))
                                   (interface-names->interfaces stxes (hash-ref options 'public-implements '()))))
       (define authentic? (hash-ref options 'authentic? #f))
       (define prefab? (hash-ref options 'prefab? #f))
       (define final? (hash-ref options 'final? (not prefab?)))
       (define opaque? (hash-ref options 'opaque? #f))
       (define primitive-properties (hash-ref options 'primitive-properties '()))
       (define given-constructor-rhs (hash-ref options 'constructor-rhs #f))
       (define given-constructor-stx-params (hash-ref options 'constructor-stx-params #f))
       (define given-constructor-name (hash-ref options 'constructor-name #f))
       (define expression-macro-rhs (hash-ref options 'expression-rhs #f))
       (define binding-rhs (hash-ref options 'binding-rhs #f))
       (define annotation-rhs (hash-ref options 'annotation-rhs #f))
       (define-values (internal-id exposed-internal-id extra-exposed-internal-ids)
         (extract-internal-ids options
                               #'scope-stx #'base-stx
                               stxes))
       (when super
         (check-consistent-subclass super options stxes parent-name))
       (define constructor-fields (syntax->list #'(constructor-field-name ...)))
       (define added-fields (reverse (hash-ref options 'fields '())))
       (define extra-fields (map added-field-id added-fields))
       (define has-extra-fields? (pair? extra-fields))
       (define fields (append constructor-fields extra-fields))
       (define mutables (append (syntax->list #'(constructor-field-mutable ...))
                                (for/list ([f (in-list added-fields)])
                                  #'#t)))
       (define constructor-private?s (map syntax-e (syntax->list #'(constructor-field-private ...))))
       (define has-private-constructor-fields? (for/or ([priv (in-list constructor-private?s)])
                                                 priv))
       (define private?s (append constructor-private?s
                                 (for/list ([a (in-list added-fields)])
                                   (eq? 'private (added-field-mode a)))))
       (define has-private-fields? (for/or ([priv (in-list private?s)])
                                     priv))
       (define (partition-fields l [private?s private?s] #:result [result list])
         (for/fold ([pub '()] [priv '()] #:result (result (reverse pub) (reverse priv)))
                   ([e (in-list (if (syntax? l) (syntax->list l) l))]
                    [p? (in-list private?s)])
           (if p?
               (values pub (cons e priv))
               (values (cons e pub) priv))))

       (define constructor-keywords (syntax->list #'(constructor-field-keyword ...)))
       (define constructor-defaults (syntax->list #'(constructor-field-default ...)))
       (define-values (constructor-public-fields constructor-private-fields)
         (partition-fields constructor-fields constructor-private?s #:result values))
       (define-values (constructor-public-keywords constructor-private-keywords)
         (partition-fields constructor-keywords constructor-private?s #:result values))
       (define-values (constructor-public-defaults constructor-private-defaults)
         (partition-fields constructor-defaults constructor-private?s #:result values))

       (define expose (make-expose #'scope-stx #'base-stx))
       (check-consistent-construction stxes mutables private?s constructor-defaults options
                                      #'name given-constructor-rhs
                                      (and given-constructor-name
                                           (expose given-constructor-name))
                                      expression-macro-rhs)

       (define has-defaults? (any-stx? constructor-defaults))
       (define has-keywords? (any-stx? constructor-keywords))
       (define-values (super-constructor-fields super-keywords super-defaults)
         (extract-super-constructor-fields super))
       (define-values (super-constructor+-fields super-constructor+-keywords super-constructor+-defaults)
         (extract-super-internal-constructor-fields super super-constructor-fields super-keywords super-defaults))
       (define super-has-keywords? (any-stx? super-keywords))
       (define super-has-defaults? (any-stx? super-constructor+-defaults))
       (define super-has-by-position-default? (for/or ([kw (in-list super-keywords)]
                                                       [df (in-list super-defaults)])
                                                (and (not (syntax-e kw))
                                                     (syntax-e df))))
       (define (to-keyword f) (datum->syntax f (string->keyword (symbol->immutable-string (syntax-e f))) f f))
       (define field-ht (check-duplicate-field-names stxes fields super (map objects-desc-dots interfaces)))
       (check-field-defaults stxes super-has-by-position-default? constructor-fields constructor-defaults constructor-keywords)
       (define intro (make-syntax-introducer))
       (define all-name-fields
         (append
          (syntax->list #'(constructor-name-field ...))
          (for/list ([field (in-list extra-fields)])
            (intro
             (datum->syntax field
                            (string->symbol (format "~a.~a"
                                                    (syntax-e #'name)
                                                    (syntax-e field)))
                            field)))))
       (define maybe-set-name-fields
         (for/list ([field (in-list fields)]
                    [mutable (in-list mutables)])
           (and (syntax-e mutable)
                (intro
                 (datum->syntax field
                                (string->symbol (format "set-~a.~a!"
                                                        (syntax-e #'name)
                                                        (syntax-e field)))
                                field)))))

       (define constructor-rhs
         (or given-constructor-rhs
             (and (or has-private-constructor-fields?
                      (and super
                           (class-desc-constructor-makers super)))
                  'synthesize)))

       (define dots (hash-ref options 'dots '()))
       (define dot-provider-rhss (map cdr dots))
       (define parent-dot-providers
         (for/list ([parent (in-list (cons super interfaces))]
                    #:do [(define dp (and parent (objects-desc-dot-provider parent)))]
                    #:when dp)
           dp))

       (define added-methods (reverse (hash-ref options 'methods '())))
       (define-values (method-mindex   ; symbol -> mindex
                       method-names    ; index -> symbol-or-identifier
                       method-vtable   ; index -> function-identifier or '#:abstract
                       method-results  ; symbol -> nonempty list of identifiers; first one implies others
                       method-private  ; symbol -> identifier or (list identifier)
                       method-decls    ; symbol -> identifier, intended for checking distinct
                       abstract-name)  ; #f or identifier for a still-abstract method
         (extract-method-tables stxes added-methods super interfaces private-interfaces final? prefab?))

       (check-fields-methods-dots-distinct stxes field-ht method-mindex method-names method-decls dots)
       (check-consistent-unimmplemented stxes final? abstract-name #'name)

       (define exs (parse-exports #'(combine-out . exports) expose))
       (check-exports-distinct stxes exs fields method-mindex dots)

       (define reconstructor-rhs
         (or (hash-ref options 'reconstructor-rhs #f)
             (and (not (or (hash-ref options 'expression-rhs #f)
                           (hash-ref options 'constructor-rhs #f)
                           abstract-name))
                  'default)))
       (define reconstructor-stx-params
         (hash-ref options 'reconstructor-stx-params #f))

       (define need-constructor-wrapper?
         (need-class-constructor-wrapper? extra-fields constructor-keywords constructor-defaults constructor-rhs
                                          has-private-constructor-fields?
                                          super-has-keywords? super-has-defaults? abstract-name super))

       (define has-private?
         (or has-private-fields?
             ((hash-count method-private) . > . 0)))

       (define has-mutable-constructor-arg?
         (or (for/or ([mut (in-list (syntax->list #'(constructor-field-mutable ...)))]
                      [priv (in-list (syntax->list #'(constructor-field-private ...)))])
               (and (syntax-e mut)
                    (not (syntax-e priv))))
             (and super
                  (super-has-mutable-constructor-field? super))))
       (define has-mutable-internal-constructor-arg?
         (for/or ([mut (in-list (syntax->list #'(constructor-field-mutable ...)))])
           (syntax-e mut)))

       (define-values (callable? here-callable? public-callable?)
         (able-method-status 'call super interfaces method-mindex method-vtable method-private))
       (define-values (indexable? here-indexable? public-indexable?)
         (able-method-status 'get super interfaces method-mindex method-vtable method-private))
       (define-values (setable? here-setable? public-setable?)
         (able-method-status 'set super interfaces method-mindex method-vtable method-private))
       (define-values (appendable? here-appendable? public-appendable?)
         (able-method-status 'append super interfaces method-mindex method-vtable method-private))
       (define-values (comparable? here-comparable? public-comparable?)
         (able-method-status 'compare super interfaces method-mindex method-vtable method-private
                             #:name 'compare_to))

       (define (temporary template)
         ((make-syntax-introducer) (datum->syntax #f (string->symbol (format template (syntax-e #'name))))))

       (with-syntax ([class:name (temporary "class:~a")]
                     [make-name (temporary "make-~a")]
                     [name-ref (temporary "~a-ref")]
                     [name-defaults (and (or super-has-defaults? (and has-defaults? (not final?)))
                                         (temporary "~a-defaults"))]
                     [(name-field ...) all-name-fields]
                     [(set-name-field! ...) (for/list ([id (in-list maybe-set-name-fields)]
                                                       #:when id)
                                              id)]
                     [(maybe-set-name-field! ...) maybe-set-name-fields]
                     [(field-name ...) fields]
                     [(field-static-infos ...) (append (syntax->list #'(constructor-field-static-infos ...))
                                                       (for/list ([f (in-list added-fields)])
                                                         (added-field-static-infos f)))]
                     [(field-argument ...) (append (for/list ([kw (in-list constructor-keywords)]
                                                              [df (in-list constructor-defaults)])
                                                     (if (syntax-e df)
                                                         (box kw)
                                                         kw))
                                                   (for/list ([f (in-list added-fields)])
                                                     (added-field-arg-id f)))]
                     [(field-converter ...) (append (syntax->list #'(constructor-field-converter ...))
                                                    (map added-field-converter added-fields))]
                     [(field-annotation-str ...) (append (syntax->list #'(constructor-field-annotation-str ...))
                                                         (map added-field-annotation-str added-fields))]
                     [super-name (and super (class-desc-id super))]
                     [((super-field-name
                        super-name-field
                        super-maybe-set-name-field!
                        super-field-static-infos
                        super-field-argument)
                       ...)
                      (if super
                          (class-desc-fields super)
                          '())]
                     [(super-field-keyword ...) super-keywords]
                     [(export ...) exs])
         (define-values (public-field-names private-field-names) (partition-fields #'(field-name ...) #:result values))
         (define-values (public-field-arguments private-field-arguments) (partition-fields #'(field-argument ...) #:result values))
         (define-values (public-name-fields private-name-fields) (partition-fields #'(name-field ...) #:result values))
         (define-values (recon-field-names recon-field-args recon-field-accs recon-field-rhss)
           (extract-reconstructor-fields stxes options super reconstructor-rhs
                                         public-field-names public-field-arguments public-name-fields))
         (with-syntax ([constructor-name (if (or constructor-rhs
                                                 expression-macro-rhs)
                                             (or (and given-constructor-name
                                                      (not (bound-identifier=? #'name (expose given-constructor-name)))
                                                      given-constructor-name)
                                                 (temporary "~a-ctr"))
                                             #'make-name)]
                       [constructor-visible-name (or given-constructor-name
                                                     #'name)]
                       [constructor-maker-name (and (or (not final?)
                                                        super)
                                                    (or constructor-rhs
                                                        has-private-constructor-fields?)
                                                    (temporary "~a-maker"))]
                       [make-all-name (if need-constructor-wrapper?
                                          (temporary "~a-make")
                                          #'make-name)]
                       [((public-field-name ...) (private-field-name ...)) (list public-field-names private-field-names)]
                       [((public-name-field ...) (private-name-field ...)) (list public-name-fields private-name-fields)]
                       [((public-maybe-set-name-field! ...) (private-maybe-set-name-field! ...)) (partition-fields #'(maybe-set-name-field! ...))]
                       [(public-name-field/mutate ...) (let-values ([(maybe-sets _) (partition-fields #'(maybe-set-name-field! ...) #:result values)])
                                                         (for/list ([maybe-set (in-list maybe-sets)]
                                                                    [name-field (in-list public-name-fields)])
                                                           (or (and maybe-set (car (generate-temporaries (list name-field))))
                                                               name-field)))]
                       [((public-field-static-infos ...) (private-field-static-infos ...)) (partition-fields #'(field-static-infos ...))]
                       [((public-field-argument ...) (private-field-argument ...)) (list public-field-arguments private-field-arguments)]
                       [(constructor-public-name-field ...) (partition-fields all-name-fields constructor-private?s
                                                                              #:result (lambda (pub priv) pub))]
                       [(constructor-public-field-static-infos ...) (partition-fields #'(constructor-field-static-infos ...)
                                                                                      constructor-private?s
                                                                                      #:result (lambda (pub priv) pub))]
                       [(constructor-public-field-keyword ...) constructor-public-keywords]
                       [(super-name* ...) (if super #'(super-name) '())]
                       [(interface-name ...) interface-names]
                       [make-internal-name (and exposed-internal-id
                                                #'make-converted-internal
                                                #;
                                                (temporary "make-internal-~a"))]
                       [(dot-id ...) (map car dots)]
                       [dot-provider-name (or (and (or (pair? dot-provider-rhss)
                                                       (and (pair? parent-dot-providers)
                                                            (pair? (cdr parent-dot-providers))))
                                                   (temporary "dot-provider-~a"))
                                              (and (pair? parent-dot-providers)
                                                   (car parent-dot-providers)))]
                       [reconstructor-name (and reconstructor-rhs
                                                (temporary "~a-reconstructor"))]
                       [(recon-field-name ...) recon-field-names]
                       [(recon-field-arg ...) recon-field-args]
                       [(recon-field-acc ...) recon-field-accs]
                       [(recon-field-rhs ...) recon-field-rhss]
                       [prefab-guard-name (and prefab?
                                               (or (and super
                                                        (class-desc-prefab-guard-id super))
                                                   (for/or ([converter-stx (in-list (syntax->list #'(field-converter ...)))])
                                                     (syntax-e converter-stx)))
                                               (temporary "prefab-guard-~a"))]
                       [dot-providers (add-super-dot-providers #'name-instance super interfaces)]
                       [internal-dot-providers (and exposed-internal-id
                                                    (add-super-dot-providers #'internal-name-instance super interfaces))])
           (define defns
             (reorder-for-top-level
              (append
               (build-methods method-results
                              added-methods method-mindex method-names method-private
                              reconstructor-rhs reconstructor-stx-params
                              #'(name name-instance name? #f reconstructor-name
                                      prop-methods-ref
                                      indirect-static-infos
                                      [(field-name) ... super-field-name ...]
                                      [field-static-infos ... super-field-static-infos ...]
                                      [name-field ... super-name-field ...]
                                      [maybe-set-name-field! ... super-maybe-set-name-field! ...]
                                      [private-field-name ...]
                                      [(list 'private-field-name
                                             (quote-syntax private-name-field)
                                             (quote-syntax private-maybe-set-name-field!)
                                             (quote-syntax private-field-static-infos)
                                             (quote-syntax private-field-argument))
                                       ...]
                                      [super-name* ... interface-name ...]
                                      [(recon-field-acc recon-field-rhs)
                                       ...]))
               (build-class-reconstructor super final?
                                          reconstructor-rhs method-private
                                          #'(name name? constructor-name name-instance
                                                  indirect-static-infos dot-providers reconstructor-name
                                                  [(recon-field-name recon-field-arg recon-field-acc)
                                                   ...]
                                                  [private-field-name ...]
                                                  [(list 'private-field-name
                                                         (quote-syntax private-name-field)
                                                         (quote-syntax private-maybe-set-name-field!)
                                                         (quote-syntax private-field-static-infos)
                                                         (quote-syntax private-field-argument))
                                                   ...]
                                                  [super-name* ... interface-name ...]))
               (build-class-struct super
                                   fields mutables constructor-keywords private?s final? authentic? prefab? opaque?
                                   method-mindex method-names method-vtable method-private
                                   abstract-name
                                   interfaces private-interfaces
                                   has-extra-fields? here-callable? here-indexable? here-setable?
                                   here-appendable? here-comparable?
                                   primitive-properties
                                   #'(name class:name make-all-name name? name-ref prefab-guard-name
                                           reconstructor-name
                                           [public-field-name ...]
                                           [public-name-field ...]
                                           [public-maybe-set-name-field! ...]
                                           [public-name-field/mutate ...]
                                           [field-name ...]
                                           [name-field ...]
                                           [set-name-field! ...]
                                           [field-converter ...]
                                           [field-annotation-str ...]
                                           [super-field-name ...]
                                           [super-name-field ...]
                                           [dot-id ...]
                                           ((recon-field-name recon-field-acc)
                                            ...)))
               (build-added-field-arg-definitions added-fields)
               ;; note: class name as expression is bound via `build-class-dot-handling`
               (build-class-constructor super constructor-rhs given-constructor-stx-params
                                        added-fields constructor-private?s
                                        constructor-fields super-constructor-fields super-constructor+-fields
                                        constructor-keywords super-keywords super-constructor+-keywords
                                        constructor-defaults super-defaults super-constructor+-defaults
                                        method-private
                                        need-constructor-wrapper?
                                        abstract-name
                                        has-defaults? super-has-defaults?
                                        final?
                                        #'(name make-name make-all-name constructor-name constructor-maker-name
                                                constructor-visible-name
                                                make-converted-name make-converted-internal
                                                name?
                                                name-defaults
                                                make-internal-name
                                                name-instance
                                                indirect-static-infos dot-providers
                                                [private-field-name ...]
                                                [(list 'private-field-name
                                                       (quote-syntax private-name-field)
                                                       (quote-syntax private-maybe-set-name-field!)
                                                       (quote-syntax private-field-static-infos)
                                                       (quote-syntax private-field-argument))
                                                 ...]))
               (build-class-binding-form super binding-rhs
                                         exposed-internal-id intro
                                         #'(name name-extends tail-name
                                                 name-instance name?
                                                 indirect-static-infos dot-providers
                                                 [constructor-name-field ...] [constructor-public-name-field ...] [super-name-field ...]
                                                 [constructor-field-static-infos ...] [constructor-public-field-static-infos ...] [super-field-static-infos ...]
                                                 [constructor-field-keyword ...] [constructor-public-field-keyword ...] [super-field-keyword ...]))
               ;; includes defining the namespace and constructor name:
               (build-class-dot-handling method-mindex method-vtable method-results final?
                                         has-private? method-private exposed-internal-id #'internal-of
                                         expression-macro-rhs intro (hash-ref options 'constructor-name #f)
                                         (and (not annotation-rhs)
                                              (if has-mutable-constructor-arg?
                                                  #'now_of
                                                  #'of))
                                         (if has-mutable-internal-constructor-arg?
                                             #'now_of
                                             #'of)
                                         dot-provider-rhss parent-dot-providers
                                         #'(name name-extends tail-name
                                                 name? #f constructor-name name-instance name-ref name-of
                                                 make-internal-name internal-name-instance dot-provider-name
                                                 indirect-static-infos dot-providers
                                                 [public-field-name ...] [private-field-name ...] [field-name ...]
                                                 [public-name-field ...] [name-field ...]
                                                 [public-name-field/mutate ...]
                                                 [dot-id ...]
                                                 [(list 'private-field-name
                                                        (quote-syntax private-name-field)
                                                        (quote-syntax private-maybe-set-name-field!)
                                                        (quote-syntax private-field-static-infos)
                                                        (quote-syntax private-field-argument))
                                                  ...]
                                                 [export ...]
                                                 base-stx scope-stx))
               (build-class-static-infos exposed-internal-id
                                         super
                                         given-constructor-rhs
                                         (append super-keywords constructor-public-keywords)
                                         (append super-defaults constructor-public-defaults)
                                         (append super-keywords constructor-private-keywords)
                                         (append super-defaults constructor-private-defaults)
                                         #'(name constructor-name name-instance
                                                 internal-name-instance make-internal-name
                                                 indirect-static-infos
                                                 dot-providers internal-dot-providers
                                                 [name-field ...]
                                                 [field-static-infos ...]
                                                 [public-name-field/mutate ...]
                                                 [public-maybe-set-name-field! ...]
                                                 [public-field-static-infos ...]))
               (build-class-desc exposed-internal-id super options
                                 constructor-public-keywords super-keywords ; public field for constructor
                                 constructor-public-defaults super-defaults
                                 constructor-keywords super-constructor+-keywords ; includes private fields for internal constructor
                                 constructor-defaults super-constructor+-defaults
                                 final? has-private-fields? private?s
                                 parent-name interface-names all-interfaces private-interfaces
                                 method-mindex method-names method-vtable method-results method-private dots
                                 authentic? prefab? (not (syntax-e #'reconstructor-name))
                                 here-callable? public-callable?
                                 here-indexable? public-indexable?
                                 here-setable? public-setable?
                                 here-appendable? public-appendable?
                                 here-comparable? public-comparable?
                                 (or (hash-ref options 'reconstructor-fields #f)
                                     (and super (class-desc-reconstructor-fields super)))
                                 #'(name name-extends class:name constructor-maker-name name-defaults name-ref
                                         dot-provider-name prefab-guard-name
                                         instance-static-infos dot-providers
                                         super-call-statinfo-indirect call-statinfo-indirect
                                         (list (list 'super-field-name
                                                     (quote-syntax super-name-field)
                                                     (quote-syntax super-maybe-set-name-field!)
                                                     (quote-syntax super-field-static-infos)
                                                     (quote-syntax super-field-argument))
                                               ...
                                               (list 'public-field-name
                                                     (quote-syntax public-name-field)
                                                     (quote-syntax public-maybe-set-name-field!)
                                                     (quote-syntax public-field-static-infos)
                                                     (quote-syntax public-field-argument))
                                               ...)
                                         ([field-name field-argument maybe-set-name-field!] ...)
                                         [(recon-field-name recon-field-acc) ...]))
               (build-method-results added-methods
                                     method-mindex method-vtable method-private
                                     method-results
                                     final?
                                     #'prop-methods-ref
                                     #'call-statinfo-indirect callable?
                                     #'index-statinfo-indirect indexable?
                                     #'index-set-statinfo-indirect setable?
                                     #'append-statinfo-indirect appendable?
                                     #'compare-statinfo-indirect comparable?
                                     #'super-call-statinfo-indirect))))
           #`(begin . #,defns)))])))

(define-for-syntax (build-class-struct super
                                       fields mutables constructor-keywords private?s final? authentic? prefab? opaque?
                                       method-mindex method-names method-vtable method-private
                                       abstract-name
                                       interfaces private-interfaces
                                       has-extra-fields? here-callable? here-indexable? here-setable?
                                       here-appendable? here-comparable?
                                       primitive-properties
                                       names)
  (with-syntax ([(name class:name make-all-name name? name-ref prefab-guard-name
                       reconstructor-name
                       [public-field-name ...]
                       [public-name-field ...]
                       [public-maybe-set-name-field! ...]
                       [public-name-field/mutate ...]
                       [field-name ...]
                       [name-field ...]
                       [set-name-field! ...]
                       [field-converter ...]
                       [field-annotation-str ...]
                       [super-field-name ...]
                       [super-name-field ...]
                       [dot-id ...]
                       [(recon-field-name recon-field-acc) ...])
                 names]
                [(mutable-field ...) (for/list ([field (in-list fields)]
                                                [mutable (in-list mutables)]
                                                #:when (syntax-e mutable))
                                       field)]
                [(field-index ...) (for/list ([field (in-list fields)]
                                              [i (in-naturals)])
                                     i)]
                [(immutable-field-index ...) (for/list ([mutable (in-list mutables)]
                                                        [i (in-naturals)]
                                                        #:when (not (syntax-e mutable)))
                                               i)]
                [(mutable-field-index ...) (for/list ([mutable (in-list mutables)]
                                                      [i (in-naturals)]
                                                      #:when (syntax-e mutable))
                                             i)])
    (with-syntax ([((mutable-field-converter mutable-field-annotation-str) ...)
                   (for/list ([converter (in-list (syntax->list #'(field-converter ...)))]
                              [ann-str (in-list (syntax->list #'(field-annotation-str ...)))]
                              [mutable (in-list mutables)]
                              #:when (syntax-e mutable))
                     (list converter ann-str))]
                  [(maybe-public-mutable-field-name ...)
                   (for/list ([name (in-list (syntax->list #'(public-field-name ...)))]
                              [mutator (in-list (syntax->list #'(public-maybe-set-name-field! ...)))])
                     (and (syntax-e mutator) name))]
                  [(((method-name method-proc) ...)
                    ((property-name property-proc) ...))
                   (for/fold ([ms '()] [ps '()] #:result (list (reverse ms) (reverse ps)))
                             ([v (in-vector method-vtable)]
                              [i (in-naturals)])
                     (define name (hash-ref method-names i))
                     (if (mindex-property? (hash-ref method-mindex (if (syntax? name) (syntax-e name) name)))
                         (values ms (cons (list name v) ps))
                         (values (cons (list name v) ms) ps)))]
                  [(all-dot-name ...) (extract-all-dot-names #'(dot-id ...) (cons super interfaces))]
                  [primitive-make-name (if (syntax-e #'prefab-guard-name)
                                           ((make-syntax-introducer)
                                            (datum->syntax #f (string->symbol (format "make-prefab-~a" (syntax-e #'name)))))
                                           #'make-all-name)])
      (define guard-expr
        (build-guard-expr (let ([fields (and super (class-desc-all-fields super))])
                            (if fields
                                (generate-temporaries fields)
                                #'(super-field-name ...)))
                          fields
                          (syntax->list #'(field-converter ...))
                          (map syntax-e
                               (syntax->list #'(field-annotation-str ...)))
                          #:super (and prefab?
                                       super
                                       (class-desc-prefab-guard-id super))))
      (define all-interfaces
        (close-interfaces-over-superinterfaces
         (if abstract-name
             ;; for interface-implementing properties, an abstract class defers to
             ;; subclasses for public interfaces
             null
             ;; otherwise, always implement interface properties from superclasses,
             ;; because it's a vtable that needs to refer to this class's implementations
             (append (if super
                         (interface-names->interfaces
                          #f
                          (let ([l (objects-desc-interface-ids super)])
                            (if (null? l)
                                null
                                (syntax->list l))))
                         null)
                     interfaces))
         private-interfaces))
      (define all-prim-prop-interfaces
        ;; for primitive properties, we only need to cover immediately
        ;; implemented interfaces; values can be inherited from superclasses
        (close-interfaces-over-superinterfaces interfaces
                                               private-interfaces))
      (append
       (list
        #`(define-values (class:name primitive-make-name name? name-field ... set-name-field! ...)
            (let-values ([(class:name name name? name-ref name-set!)
                          (make-struct-type 'name
                                            #,(and super (class-desc-class:id super))
                                            #,(length fields) 0 #f
                                            #,(if prefab?
                                                  #'null
                                                  #`(list #,@(if abstract-name
                                                                 null
                                                                 #`((cons prop:field-name->accessor
                                                                          (list* '(public-field-name ...)
                                                                                 (hasheq (~@ 'super-field-name super-name-field)
                                                                                         ...
                                                                                         (~@ 'property-name property-proc)
                                                                                         ...
                                                                                         (~@ 'all-dot-name (no-dynamic-dot-syntax 'all-dot-name))
                                                                                         ...)
                                                                                 (hasheq (~@ 'method-name method-proc)
                                                                                         ...)))))
                                                          #,@(if (or (syntax-e #'reconstructor-name)
                                                                     (and super
                                                                          (not (memq 'no-recon (objects-desc-flags super)))))
                                                                 #`((cons prop:reconstructor
                                                                          #,(and (syntax-e #'reconstructor-name)
                                                                                 #`(cons (list
                                                                                          #,@(for/list ([name (in-list (syntax->list #'(recon-field-name ...)))]
                                                                                                        [acc (in-list (syntax->list #'(recon-field-acc ...)))])
                                                                                               #`(cons '#,name #,acc)))
                                                                                         reconstructor-name))))
                                                                 null)
                                                          #,@(able-method-as-property 'call #'prop:procedure here-callable?
                                                                                      method-mindex method-vtable method-private)
                                                          #,@(able-method-as-property 'get #'prop:indexable here-indexable?
                                                                                      method-mindex method-vtable method-private)
                                                          #,@(able-method-as-property 'set #'prop:setable here-setable?
                                                                                      method-mindex method-vtable method-private)
                                                          #,@(able-method-as-property 'append #'prop:appendable here-appendable?
                                                                                      method-mindex method-vtable method-private)
                                                          #,@(if (or abstract-name
                                                                     (and (for/and ([maybe-name (in-list (syntax->list #'(maybe-public-mutable-field-name ...)))])
                                                                            (not (syntax-e maybe-name)))
                                                                          (null? (syntax-e #'(property-proc ...)))))
                                                                 null
                                                                 #`((cons prop:field-name->mutator
                                                                          (list* '(maybe-public-mutable-field-name ...)
                                                                                 (hasheq (~@ 'property-name property-proc)
                                                                                         ...)))))
                                                          #,@(cond
                                                               [opaque? #`((cons prop:print-field-shapes 'opaque))]
                                                               [else
                                                                (define field-print-shapes
                                                                  (print-field-shapes super fields constructor-keywords private?s))
                                                                (if (or abstract-name
                                                                        (and (andmap symbol? field-print-shapes)
                                                                             (not has-extra-fields?)))
                                                                    null
                                                                    #`((cons prop:print-field-shapes
                                                                             '#,field-print-shapes)))])
                                                          #,@(if final?
                                                                 (list #'(cons prop:sealed #t))
                                                                 '())
                                                          #,@(if authentic?
                                                                 (list #'(cons prop:authentic #t))
                                                                 '())
                                                          #,@(if (or (zero? (vector-length method-vtable))
                                                                     abstract-name)
                                                                 '()
                                                                 (list #`(cons prop:methods
                                                                               (vector #,@(vector->list method-vtable)))))
                                                          #,@(for/list ([pp (in-list primitive-properties)])
                                                               #`(cons #,(car pp) #,(cdr pp)))
                                                          #,@(for*/list ([intf (in-list all-prim-prop-interfaces)]
                                                                         [pp (in-list (interface-desc-primitive-properties intf))])
                                                               #`(cons #,(car pp) #,(cdr pp)))
                                                          #,@(for/list ([intf (in-list all-interfaces)])
                                                               #`(cons #,(interface-desc-prop:id intf)
                                                                       (vector #,@(build-interface-vtable intf
                                                                                                          method-mindex method-vtable method-names
                                                                                                          method-private))))))
                                            #,(if prefab? (quote-syntax 'prefab) #f)
                                            #f
                                            '(immutable-field-index ...)
                                            #,(and (not prefab?)
                                                   guard-expr)
                                            'name)])
              (values class:name name name?
                      (make-struct-field-accessor name-ref field-index 'name-field 'name 'rhombus)
                      ...
                      (compose-annotation-check
                       (make-struct-field-mutator name-set! mutable-field-index 'set-name-field! 'name 'rhombus)
                       mutable-field
                       mutable-field-converter
                       mutable-field-annotation-str)
                      ...)))
        #`(define (name-ref v)
            (define vtable (prop-methods-ref v #f))
            (or vtable
                (raise-not-an-instance 'name v))))
       (for/list ([def (in-list (syntax->list
                                 #'((define public-name-field/mutate
                                      (let ([public-name-field
                                             (case-lambda
                                               [(v) (public-name-field v)]
                                               [(v val) (public-maybe-set-name-field! v val)])])
                                        public-name-field))
                                    ...)))]
                  #:when (syntax-parse def
                           [(_ n (_ ([n2 . _]) . _)) (not (free-identifier=? #'n #'n2))]
                           [_ #t]))
         def)
       (if (syntax-e #'prefab-guard-name)
           (list
            #`(define prefab-guard-name #,guard-expr)
            #`(define make-all-name
                (let ([name (lambda (super-field-name ... field-name ...)
                              (let-values ([(super-field-name ... field-name ...)
                                            (prefab-guard-name super-field-name ... field-name ... 'name)])
                                (primitive-make-name super-field-name ... field-name ...)))])
                  name)))
           null)))))

(define-for-syntax (build-class-desc exposed-internal-id super options
                                     constructor-public-keywords super-keywords
                                     constructor-public-defaults super-defaults
                                     constructor-keywords super-constructor+-keywords
                                     constructor-defaults super-constructor+-defaults
                                     final? has-private-fields? private?s
                                     parent-name interface-names all-interfaces private-interfaces
                                     method-mindex method-names method-vtable method-results method-private dots
                                     authentic? prefab? no-recon?
                                     here-callable? public-callable?
                                     here-indexable? public-indexable?
                                     here-setable? public-setable?
                                     here-appendable? public-appendable?
                                     here-comparable? public-comparable?
                                     force-custom-recon?
                                     names)
  (with-syntax ([(name name-extends class:name constructor-maker-name name-defaults name-ref
                       dot-provider-name prefab-guard-name
                       instance-static-infos dot-providers
                       super-call-statinfo-indirect call-statinfo-indirect
                       fields
                       ([field-name field-argument maybe-set-name-field!] ...)
                       [(recon-field-name recon-field-acc) ...])
                 names])
    (append
     (list
      (build-syntax-definition/maybe-extension
       'rhombus/class #'name #'name-extends
       #`(class-desc-maker
          (lambda ()
            (class-desc (quote-syntax #,(interface-names->quoted-list interface-names all-interfaces private-interfaces 'public))
                        '#,(build-quoted-method-shapes method-vtable method-names method-mindex)
                        (quote-syntax #,method-vtable)
                        '#,(build-quoted-method-map method-mindex)
                        #,(build-method-result-expression method-results)
                        '#,(map car dots)
                        #,(and (syntax-e #'dot-provider-name)
                               #'(quote-syntax dot-provider-name))
                        (#,(quote-syntax quasisyntax) instance-static-infos)
                        '(#,@(if authentic? '(authentic) null)
                          #,@(if prefab? '(prefab) null)
                          #,@(if no-recon? '(no-recon) null)
                          #,@(if public-callable? '(call) null)
                          #,@(if public-indexable? '(get) null)
                          #,@(if public-setable? '(set) null)
                          #,@(if public-appendable? '(append) null)
                          #,@(if public-comparable? '(compare) null))
                        ;; ----------------------------------------
                        #,final?
                        (quote-syntax name)
                        #,(and parent-name #`(quote-syntax #,parent-name))
                        (quote-syntax class:name)
                        #,(if final? #'#f #`(quote-syntax dot-providers))
                        (quote-syntax name-ref)
                        fields
                        #,(and (or has-private-fields?
                                   (and super (class-desc-all-fields super)))
                               #`(list #,@(map (lambda (i)
                                                 (define (wrap i)
                                                   (if (identifier? i) #`(quote-syntax #,i) #`(quote #,i)))
                                                 (if (pair? i)
                                                     #`(cons (quote #,(car i)) #,(wrap (cdr i)))
                                                     (wrap i)))
                                               (append (if super
                                                           (or (class-desc-all-fields super)
                                                               ;; symbol means "inherited from superclass"
                                                               (for/list ([f (in-list (class-desc-fields super))])
                                                                 (field-desc-name f)))
                                                           '())
                                                       (for/list ([name (in-list (syntax->list #'(field-name ...)))]
                                                                  [arg (in-list (syntax->list #'(field-argument ...)))]
                                                                  [mutator (in-list (syntax->list #'(maybe-set-name-field! ...)))]
                                                                  [private? (in-list private?s)])
                                                         (cond
                                                           [private? (cons (syntax-e name)
                                                                           (if (and (not (identifier? arg))
                                                                                    (syntax-e mutator))
                                                                               (vector arg)
                                                                               arg))]
                                                           [else (syntax-e name)]))))))
                        #,(if super
                              (if (class-desc-all-fields super)
                                  (length (class-desc-all-fields super))
                                  (length (class-desc-fields super)))
                              0)
                        #,(cond
                            [(syntax-e #'constructor-maker-name)
                             #`(quote-syntax ([#,(encode-protocol constructor-public-keywords constructor-public-defaults
                                                                  constructor-keywords constructor-defaults)
                                               constructor-maker-name]
                                              #,@(if super
                                                     (or (class-desc-constructor-makers super)
                                                         (list (list (encode-protocol super-keywords super-defaults
                                                                                      super-constructor+-keywords super-constructor+-defaults)
                                                                     #f)))
                                                     '())))]
                            [else #'#f])
                        #,(and (or (hash-ref options 'expression-rhs #f)
                                   (hash-ref options 'constructor-rhs #f))
                               #t)
                        #,(and (hash-ref options 'binding-rhs #f) #t)
                        #,(and (hash-ref options 'annotation-rhs #f) #t)
                        #,(if force-custom-recon?
                              #`(list (cons 'recon-field-name (quote-syntax recon-field-acc)) ...)
                              #f)
                        #,(and (syntax-e #'name-defaults)
                               #'(quote-syntax name-defaults))
                        #,(able-method-for-class-desc 'call here-callable? public-callable?
                                                      super
                                                      method-mindex method-vtable method-private)
                        #,(able-method-for-class-desc 'get here-indexable? public-indexable?
                                                      super
                                                      method-mindex method-vtable method-private)
                        #,(able-method-for-class-desc 'set here-setable? public-setable?
                                                      super
                                                      method-mindex method-vtable method-private)
                        #,(able-method-for-class-desc 'append here-appendable? public-appendable?
                                                      super
                                                      method-mindex method-vtable method-private)
                        #,(able-method-for-class-desc 'compare here-comparable? public-comparable?
                                                      super
                                                      method-mindex method-vtable method-private
                                                      #:const '#:method)
                        #,(let ([id (if (syntax-e #'call-statinfo-indirect)
                                        #'call-statinfo-indirect
                                        #'super-call-statinfo-indirect)])
                            (if (and id (syntax-e id))
                                #`(quote-syntax #,id)
                                #f))
                        #,(and (syntax-e #'prefab-guard-name)
                               #`(quote-syntax prefab-guard-name)))))))
     (if exposed-internal-id
         (list
          #`(define-class-desc-syntax #,exposed-internal-id
              (class-internal-desc (quote-syntax name)
                                   (quote #,(build-quoted-private-method-list 'method method-private))
                                   (quote #,(build-quoted-private-method-list 'property method-private))
                                   (quote-syntax #,(interface-names->quoted-list interface-names all-interfaces private-interfaces 'private)))))
         null))))
