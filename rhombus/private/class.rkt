#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest/syntax-local
                     enforest/hier-name-parse
                     "srcloc.rkt"
                     "name-path-op.rkt"
                     "introducer.rkt"
                     "tag.rkt"
                     "class-parse.rkt"
                     "class-field-parse.rkt"
                     "interface-parse.rkt")
         racket/unsafe/undefined
         "forwarding-sequence.rkt"
         "definition.rkt"
         "expression.rkt"
         (submod "dot.rkt" for-dot-provider)
         "call-result-key.rkt"
         "class-clause.rkt"
         "class-clause-parse.rkt"
         "class-constructor.rkt"
         "class-binding.rkt"
         "class-annotation.rkt"
         "class-dot.rkt"
         "class-static-info.rkt"
         "class-method.rkt"
         "class-desc.rkt"
         "class-top-level.rkt"
         "dotted-sequence-parse.rkt"
         "parens.rkt"
         "parse.rkt"
         "error.rkt"
         (submod "namespace.rkt" for-exports)
         (submod "print.rkt" for-class))

(provide (rename-out [rhombus-class class])
         this
         super
         extends
         implements
         internal
         constructor
         binding
         annotation
         final
         nonfinal
         authentic
         field
         method
         property
         override
         private
         abstract)

(define-syntax rhombus-class
  (definition-transformer
   (lambda (stxes)
     (syntax-parse stxes
       #:datum-literals (group block)
       [(_ name-seq::dotted-identifier-sequence (tag::parens field::field ...)
           options::options-block)
        #:with full-name::dotted-identifier #'name-seq
        #:with name #'full-name.name
        #:with orig-stx stxes
        (define body #'(options.form ...))
        (define finish-data #`[orig-stx base-stx #,(syntax-local-introduce #'scope-stx)
                                        full-name name
                                        (field.name ...)
                                        (field.keyword ...)
                                        (field.default ...)
                                        (field.mutable ...)
                                        (field.private ...)
                                        (field.predicate ...)
                                        (field.annotation-str ...)
                                        (field.static-infos ...)])
        (cond
          [(null? (syntax-e body))
           #`((class-finish #,finish-data ()))]
          [else
           #`((rhombus-mixed-nested-forwarding-sequence (class-finish #,finish-data) rhombus-class
                                                        (class-body-step . #,(syntax-local-introduce body))))])]))))

(define-syntax class-body-step
  (lambda (stx)
    ;; parse the first form as a class clause, if possible, otherwise assume
    ;; an expression or definition
    (syntax-parse stx
      [(_ form . rest)
       #:with clause::class-clause (syntax-local-introduce #'form)
       (syntax-parse (syntax-local-introduce #'clause.parsed)
         #:datum-literals (group parsed)
         [((group (parsed p)) ...)
          #`(begin p ... (class-body-step . rest))]
         [(form ...)
          #`(class-body-step form ... . rest)])]
      [(_ form . rest)
       #`(rhombus-top-step
          class-body-step
          #f
          form . rest)]
      [(_) #'(begin)])))

(define-syntax class-finish
  (lambda (stx)
    (syntax-parse stx
      [(_ [orig-stx base-stx scope-stx
                    full-name name
                    (constructor-field-name ...)
                    (constructor-field-keyword ...) ; #f or keyword
                    (constructor-field-default ...) ; #f or (parsed)
                    (constructor-field-mutable ...)
                    (constructor-field-private ...)
                    (constructor-field-predicate ...)
                    (constructor-field-annotation-str ...)
                    (constructor-field-static-infos ...)]
          exports
          option ...)
       (define stxes #'orig-stx)
       (define options (parse-options #'orig-stx #'(option ...)))
       (define parent-name (hash-ref options 'extends #f))
       (define super (and parent-name
                          (or (syntax-local-value* (in-class-desc-space parent-name) class-desc-ref)
                              (raise-syntax-error #f "not a class name" stxes parent-name))))
       (define interfaces (interface-names->interfaces stxes (reverse (hash-ref options 'implements '()))))
       (define private-interfaces (interface-set-diff
                                   (interface-names->interfaces stxes (hash-ref options 'private-implements '()))
                                   (interface-names->interfaces stxes (hash-ref options 'public-implements '()))))
       (define final? (hash-ref options 'final? #t))
       (define authentic? (hash-ref options 'authentic? #f))
       (define given-constructor-rhs (hash-ref options 'constructor-rhs #f))
       (define binding-rhs (hash-ref options 'binding-rhs #f))
       (define annotation-rhs (hash-ref options 'annotation-rhs #f))
       (define-values (internal-id exposed-internal-id)
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

       (check-consistent-construction stxes mutables private?s constructor-defaults options)

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
       (define (to-keyword f) (datum->syntax f (string->keyword (symbol->string (syntax-e f))) f f))
       (define field-ht (check-duplicate-field-names stxes fields super))
       (check-field-defaults stxes super-has-by-position-default? constructor-fields constructor-defaults constructor-keywords)
       (define intro (make-syntax-introducer))
       (define all-name-fields
         (for/list ([field (in-list fields)])
           (intro
            (datum->syntax field
                           (string->symbol (format "~a.~a"
                                                   (syntax-e #'name)
                                                   (syntax-e field)))
                           field))))
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

       (define added-methods (reverse (hash-ref options 'methods '())))
       (define-values (method-mindex   ; symbol -> mindex
                       method-names    ; index -> symbol-or-identifier
                       method-vtable   ; index -> function-identifier or '#:abstract
                       method-results  ; symbol -> nonempty list of identifiers; first one implies others
                       method-private  ; symbol -> identifier
                       method-decls    ; symbol -> identifier, intended for checking distinct
                       abstract-name)  ; #f or identifier for a still-abstract method
         (extract-method-tables stxes added-methods super interfaces private-interfaces final?))

       (check-fields-methods-distinct stxes field-ht method-mindex method-names method-decls)
       (check-consistent-unimmplemented stxes final? abstract-name)

       (define exs (parse-exports #'(combine-out . exports)))
       (check-exports-distinct stxes exs fields method-mindex)

       (define need-constructor-wrapper?
         (need-class-constructor-wrapper? extra-fields constructor-keywords constructor-defaults constructor-rhs
                                          has-private-constructor-fields?
                                          super-has-keywords? super-has-defaults? abstract-name super))

       (define has-private?
         (or has-private-fields?
             ((hash-count method-private) . > . 0)))

       (define (temporary template)
         ((make-syntax-introducer) (datum->syntax #f (string->symbol (format template (syntax-e #'name))))))

       (with-syntax ([name? (datum->syntax #'name (string->symbol (format "~a?" (syntax-e #'name))) #'name)]
                     [class:name (temporary "class:~a")]
                     [make-name (temporary "make-~a")]
                     [name-ref (temporary "~a-ref")]
                     [name-defaults (and (or super-has-defaults? (and has-defaults? (not final?)))
                                         (temporary "~a-defaults"))]
                     [name-instance (intro (datum->syntax #'name (string->symbol (format "~a.instance" (syntax-e #'name))) #'name))]
                     [(name-field ...) all-name-fields]
                     [(constructor-name-field ...) (for/list ([c (in-list constructor-fields)]
                                                              [n-f (in-list all-name-fields)])
                                                     n-f)]
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
                     [(field-predicate ...) (append (syntax->list #'(constructor-field-predicate ...))
                                                    (map added-field-predicate added-fields))]
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
         (with-syntax ([constructor-name (if constructor-rhs
                                             (temporary "~a-ctr")
                                             #'make-name)]
                       [constructor-maker-name (and (or (not final?)
                                                        super)
                                                    (or constructor-rhs
                                                        has-private-constructor-fields?)
                                                    (temporary "~a-maker"))]
                       [make-all-name (if need-constructor-wrapper?
                                          (temporary "~a-make")
                                          #'make-name)]
                       [((public-field-name ...) (private-field-name ...)) (partition-fields #'(field-name ...))]
                       [((public-name-field ...) (private-name-field ...)) (partition-fields #'(name-field ...))]
                       [((public-maybe-set-name-field! ...) (private-maybe-set-name-field! ...)) (partition-fields #'(maybe-set-name-field! ...))]
                       [((public-field-static-infos ...) (private-field-static-infos ...)) (partition-fields #'(field-static-infos ...))]
                       [((public-field-argument ...) (private-field-argument ...)) (partition-fields #'(field-argument ...))]
                       [(constructor-public-name-field ...) (partition-fields all-name-fields constructor-private?s
                                                                              #:result (lambda (pub priv) pub))]
                       [(constructor-public-field-static-infos ...) (partition-fields #'(constructor-field-static-infos ...)
                                                                                      constructor-private?s
                                                                                      #:result (lambda (pub priv) pub))]
                       [(constructor-public-field-keyword ...) constructor-public-keywords]
                       [(super-name* ...) (if super #'(super-name) '())]
                       [make-internal-name (and exposed-internal-id
                                                (temporary "make-internal-~a"))]
                       [internal-name-instance (if exposed-internal-id
                                                   (temporary "~a-internal-instance")
                                                   #'name-instance)])
           (define defns
             (reorder-for-top-level
              (append
               (build-methods method-results
                              added-methods method-mindex method-names method-private
                              #'(name name-instance name?
                                      [field-name ... super-field-name ...]
                                      [name-field ... super-name-field ...]
                                      [maybe-set-name-field! ... super-maybe-set-name-field! ...]
                                      [private-field-name ...]
                                      [(list 'private-field-name
                                             (quote-syntax private-name-field)
                                             (quote-syntax private-maybe-set-name-field!)
                                             (quote-syntax private-field-static-infos)
                                             (quote-syntax private-field-argument))
                                       ...]
                                      [super-name* ...]))
               (build-class-struct super
                                   fields mutables constructor-keywords private?s final? authentic?
                                   method-mindex method-names method-vtable method-private
                                   abstract-name
                                   interfaces private-interfaces
                                   has-extra-fields?
                                   #'(name class:name make-all-name name? name-ref
                                           [public-field-name ...]
                                           [field-name ...]
                                           [name-field ...]
                                           [set-name-field! ...]
                                           [field-predicate ...]
                                           [field-annotation-str ...]
                                           [super-field-name ...]
                                           [super-name-field ...]))
               (build-class-constructor super constructor-rhs
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
                                                name?
                                                name-defaults
                                                make-internal-name
                                                name-instance
                                                [private-field-name ...]
                                                [(list 'private-field-name
                                                       (quote-syntax private-name-field)
                                                       (quote-syntax private-maybe-set-name-field!)
                                                       (quote-syntax private-field-static-infos)
                                                       (quote-syntax private-field-argument))
                                                 ...]))
               (build-class-binding-form super binding-rhs
                                         exposed-internal-id intro
                                         #'(name name-instance name?
                                                 [constructor-name-field ...] [constructor-public-name-field ...] [super-name-field ...]
                                                 [constructor-field-static-infos ...] [constructor-public-field-static-infos ...] [super-field-static-infos ...]
                                                 [constructor-field-keyword ...] [constructor-public-field-keyword ...] [super-field-keyword ...]))
               (build-class-annotation-form super annotation-rhs
                                            super-constructor-fields
                                            exposed-internal-id intro
                                            #'(name name-instance name?
                                                    internal-name-instance
                                                    [constructor-name-field ...] [constructor-public-name-field ...] [super-name-field ...]
                                                    [constructor-field-keyword ...] [constructor-public-field-keyword ...] [super-field-keyword ...]))
               (build-class-dot-handling method-mindex method-vtable final?
                                         has-private? method-private exposed-internal-id
                                         #'(name constructor-name name-instance name-ref
                                                 make-internal-name internal-name-instance
                                                 [public-field-name ...] [private-field-name ...] [field-name ...]
                                                 [public-name-field ...] [name-field ...]
                                                 [(list 'private-field-name
                                                        (quote-syntax private-name-field)
                                                        (quote-syntax private-maybe-set-name-field!)
                                                        (quote-syntax private-field-static-infos)
                                                        (quote-syntax private-field-argument))
                                                  ...]
                                                 [export ...]))
               (build-class-static-infos exposed-internal-id
                                         super
                                         #'(name constructor-name name-instance
                                                 internal-name-instance make-internal-name
                                                 [name-field ...]
                                                 [field-static-infos ...]))
               (build-class-desc super options
                                 constructor-public-keywords super-keywords ; public field for constructor
                                 constructor-public-defaults super-defaults
                                 constructor-keywords super-constructor+-keywords ; includes private fields for internal constructor
                                 constructor-defaults super-constructor+-defaults
                                 final? has-private-fields? private?s
                                 parent-name
                                 method-mindex method-names method-vtable method-results
                                 #'(name class:name constructor-maker-name name-defaults name-ref
                                         (list (list 'super-field-name
                                                     (quote-syntax super-name-field)
                                                     (quote-syntax super-make-set-name-field!)
                                                     (quote-syntax super-field-static-infos)
                                                     (quote-syntax super-field-argument))
                                               ...
                                               (list 'public-field-name
                                                     (quote-syntax public-name-field)
                                                     (quote-syntax public-maybe-set-name-field!)
                                                     (quote-syntax public-field-static-infos)
                                                     (quote-syntax public-field-argument))
                                               ...)
                                         ([field-name field-argument] ...)))
               (build-method-results added-methods
                                     method-mindex method-vtable method-private
                                     method-results))))
           #`(begin . #,defns)))])))

(define-for-syntax (build-class-struct super
                                       fields mutables constructor-keywords private?s final? authentic?
                                       method-mindex method-names method-vtable method-private
                                       abstract-name
                                       interfaces private-interfaces
                                       has-extra-fields?
                                       names)
  (with-syntax ([(name class:name make-all-name name? name-ref
                       [public-field-name ...]
                       [field-name ...]
                       [name-field ...]
                       [set-name-field! ...]
                       [field-predicate ...]
                       [field-annotation-str ...]
                       [super-field-name ...]
                       [super-name-field ...])
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
    (with-syntax ([((mutable-field-predicate mutable-field-annotation-str) ...)
                   (for/list ([pred (in-list (syntax->list #'(field-predicate ...)))]
                              [ann-str (in-list (syntax->list #'(field-annotation-str ...)))]
                              [mutable (in-list mutables)]
                              #:when (syntax-e mutable))
                     (list pred ann-str))]
                  [(((method-name method-proc) ...)
                    ((property-name property-proc) ...))
                   (for/fold ([ms '()] [ps '()] #:result (list (reverse ms) (reverse ps)))
                             ([v (in-vector method-vtable)]
                              [i (in-naturals)])
                     (define name (hash-ref method-names i))
                     (if (mindex-property? (hash-ref method-mindex (if (syntax? name) (syntax-e name) name)))
                         (values ms (cons (list name v) ps))
                         (values (cons (list name v) ms) ps)))])
      (list
       #`(define-values (class:name make-all-name name? name-field ... set-name-field! ...)
           (let-values ([(class:name name name? name-ref name-set!)
                         (make-struct-type 'name
                                           #,(and super (class-desc-class:id super))
                                           #,(length fields) 0 #f
                                           (list #,@(if abstract-name
                                                        null
                                                        #`((cons prop:field-name->accessor
                                                                 (list* '(public-field-name ...)
                                                                        (hasheq (~@ 'super-field-name super-name-field)
                                                                                ...
                                                                                (~@ 'property-name property-proc)
                                                                                ...)
                                                                        (hasheq (~@ 'method-name method-proc)
                                                                                ...)))))
                                                 #,@ (let ([field-print-shapes (print-field-shapes
                                                                                super
                                                                                fields constructor-keywords private?s)])
                                                       (if (or abstract-name
                                                               (and (andmap symbol? field-print-shapes)
                                                                    (not has-extra-fields?)))
                                                           null
                                                           #`((cons prop:print-field-shapes
                                                                    '#,field-print-shapes))))
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
                                                 #,@(if abstract-name
                                                        null
                                                        (for/list ([intf (in-list (close-interfaces-over-superinterfaces interfaces
                                                                                                                         private-interfaces))])
                                                          #`(cons #,(interface-desc-prop:id intf)
                                                                  (vector #,@(build-interface-vtable intf
                                                                                                     method-mindex method-vtable method-names
                                                                                                     method-private))))))
                                           #f #f
                                           '(immutable-field-index ...)
                                           #,(build-guard-expr fields
                                                               (syntax->list #'(field-predicate ...))
                                                               (map syntax-e
                                                                    (syntax->list #'(field-annotation-str ...)))))])
             (values class:name name name?
                     (make-struct-field-accessor name-ref field-index 'name-field 'name 'rhombus)
                     ...
                     (compose-annotation-check
                      (make-struct-field-mutator name-set! mutable-field-index 'set-name-field! 'name 'rhombus)
                      mutable-field
                      mutable-field-predicate
                      mutable-field-annotation-str)
                     ...)))
       #`(define (name-ref v)
           (if (name? v)
               (prop-methods-ref v)
               (raise-not-an-instance 'name v)))))))

(define-for-syntax (build-class-desc super options
                                     constructor-public-keywords super-keywords
                                     constructor-public-defaults super-defaults
                                     constructor-keywords super-constructor+-keywords
                                     constructor-defaults super-constructor+-defaults
                                     final? has-private-fields? private?s
                                     parent-name
                                     method-mindex method-names method-vtable method-results
                                     names)
  (with-syntax ([(name class:name constructor-maker-name name-defaults name-ref
                       fields
                       ([field-name field-argument] ...))
                 names])
    (list
     #`(define-class-desc-syntax name
         (class-desc #,final?
                     (quote-syntax name)
                     #,(and parent-name #`(quote-syntax #,parent-name))
                     (quote-syntax class:name)
                     (quote-syntax name-ref)
                     fields
                     #,(and (or has-private-fields?
                                (and super (class-desc-all-fields super)))
                            #`(list #,@(map (lambda (i)
                                              (if (identifier? i) #`(quote-syntax #,i) #`(quote #,i)))
                                            (append (if super
                                                        (or (class-desc-all-fields super)
                                                            (for/list ([f (in-list (class-desc-fields super))])
                                                              (field-desc-name f)))
                                                        '())
                                                    (for/list ([name (in-list (syntax->list #'(field-name ...)))]
                                                               [arg (in-list (syntax->list #'(field-argument ...)))]
                                                               [private? (in-list private?s)])
                                                      (cond
                                                        [(identifier? arg) arg]
                                                        [private? arg] ; #f, keyword, or identifier
                                                        [else (syntax-e name)]))))))
                     '#,(build-quoted-method-shapes method-vtable method-names method-mindex)
                     (quote-syntax #,method-vtable)
                     '#,(build-quoted-method-map method-mindex)
                     #,(build-method-result-expression method-results)
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
                     #,(and (hash-ref options 'binding-rhs #f) #t)
                     #,(and (hash-ref options 'annotation-rhs #f) #t)
                     #,(and (syntax-e #'name-defaults)
                            #'(quote-syntax name-defaults)))))))
