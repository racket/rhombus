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
         "dotted-sequence-parse.rkt"
         "parens.rkt"
         "parse.rkt"
         "error.rkt"
         (submod "namespace.rkt" for-exports))

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
       #:with (parsed ...) (syntax-local-introduce #'clause.parsed)
       #`(begin
           parsed
           ...
           (class-body-step . rest))]
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
       (define final? (hash-ref options 'final? (not super)))
       (define authentic? (hash-ref options 'authentic? #f))
       (define-values (internal-id exposed-internal-id constructor-id binding-id annotation-id)
         (extract-internal-ids options
                               #'scope-stx #'base-stx
                               stxes))
       (when super
         (check-consistent-subclass super options stxes parent-name))
       (define constructor-fields (syntax->list #'(constructor-field-name ...)))
       (define added-fields (reverse (hash-ref options 'fields '())))
       (define extra-fields (map added-field-id added-fields))
       (define fields (append constructor-fields extra-fields))
       (define mutables (append (syntax->list #'(constructor-field-mutable ...))
                                (for/list ([f (in-list added-fields)])
                                  #'#t)))
       (define has-private-fields? (for/or ([a (in-list added-fields)])
                                     (eq? 'private (added-field-mode a))))
       (define (partition-fields l)
         (for/fold ([pub '()] [priv '()] #:result (list (reverse pub) (reverse priv)))
                   ([e (in-list (syntax->list l))]
                    [a (append constructor-fields added-fields)])
           (if (or (not (added-field? a))
                   (eq? (added-field-mode a) 'public))
               (values (cons e pub) priv)
               (values pub (cons e priv)))))
          
       (define constructor-keywords (syntax->list #'(constructor-field-keyword ...)))
       (define constructor-defaults (syntax->list #'(constructor-field-default ...)))
       (define has-defaults? (any-stx? constructor-defaults))
       (define-values (super-constructor-fields super-keywords super-defaults)
         (for/lists (fs ls ds) ([f (in-list (if super
                                                (class-desc-fields super)
                                                '()))]
                                #:do [(define arg (field-desc-constructor-arg f))]
                                #:unless (identifier? arg))
           (values (field-desc-name f)
                   (if (box? (syntax-e arg))
                       (unbox (syntax-e arg))
                       arg)
                   (if (box? (syntax-e arg))
                       #'(unsafe-undefined)
                       #'#f))))
       (define super-has-keywords? (any-stx? super-keywords))
       (define super-has-defaults? (any-stx? super-defaults))
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

       (define added-methods (reverse (hash-ref options 'methods '())))
       (define-values (method-map      ; symbol -> index (final) or box-of-index (non-final)
                       method-names    ; index -> symbol-or-identifier
                       method-vtable   ; index -> accessor-identifier or '#:abstract
                       method-private  ; symbol -> identifier
                       method-decls    ; symbol -> identifier, intended for checking distinct
                       abstract-name)  ; #f or identifier for a still-abstract method
         (build-method-map stxes added-methods super interfaces private-interfaces))

       (check-fields-methods-distinct stxes field-ht method-map method-names method-decls)
       (check-consistent-unimmplemented stxes final? abstract-name)

       (define exs (parse-exports #'(combine-out . exports)))
       (check-exports-distinct stxes exs fields method-map)

       (define need-constructor-wrapper?
         (need-class-constructor-wrapper? extra-fields constructor-keywords constructor-defaults constructor-id
                                          super-has-keywords? super-has-defaults? abstract-name super))

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
         (with-syntax ([constructor-name (if constructor-id
                                             (temporary "~a-ctr")
                                             #'make-name)]
                       [constructor-maker-name (and (or (not final?)
                                                        super)
                                                    constructor-id
                                                    (temporary "~a-maker"))]
                       [make-all-name (if need-constructor-wrapper?
                                          (temporary "~a-make")
                                          #'make-name)]
                       [((public-field-name ...) (private-field-name ...)) (partition-fields #'(field-name ...))]
                       [((public-name-field ...) (private-name-field ...)) (partition-fields #'(name-field ...))]
                       [((public-maybe-set-name-field! ...) (private-maybe-set-name-field! ...)) (partition-fields #'(maybe-set-name-field! ...))]
                       [((public-field-static-infos ...) (private-field-static-infos ...)) (partition-fields #'(field-static-infos ...))]
                       [((public-field-argument ...) (private-field-argument ...)) (partition-fields #'(field-argument ...))]
                       [(super-name* ...) (if super #'(super-name) '())])
           (define defns
             (append
              (if (eq? (syntax-local-context) 'top-level)
                  ;; forward declaration for methods:
                  (list #'(define-syntaxes (name?) (values)))
                  null)
              (build-methods added-methods method-map method-names method-private
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
                                  fields mutables final? authentic?
                                  method-map method-names method-vtable method-private
                                  abstract-name
                                  interfaces private-interfaces
                                  #'(name class:name make-all-name name? name-ref
                                          [public-field-name ...]
                                          [field-name ...]
                                          [name-field ...]
                                          [set-name-field! ...]
                                          [field-predicate ...]
                                          [field-annotation-str ...]
                                          [super-field-name ...]
                                          [super-name-field ...]))
              (build-class-constructor super constructor-id options
                                       constructor-fields super-constructor-fields added-fields
                                       constructor-keywords super-keywords
                                       constructor-defaults super-defaults
                                       need-constructor-wrapper?
                                       abstract-name
                                       has-defaults? super-has-defaults?
                                       final?
                                       exposed-internal-id
                                       #'(name make-name make-all-name constructor-name constructor-maker-name
                                               name?
                                               name-defaults))
              (build-class-binding-form super binding-id options
                                        exposed-internal-id intro
                                        #'(name name-instance name?
                                                [constructor-name-field ...] [super-name-field ...]
                                                [constructor-field-static-infos ...] [super-field-static-infos ...]
                                                [constructor-field-keyword ...] [super-field-keyword ...]))
              (build-class-annotation-form super annotation-id options
                                           constructor-fields super-constructor-fields
                                           exposed-internal-id intro
                                           #'(name name-instance name?
                                                   [constructor-name-field ...] [super-name-field ...]
                                                   [constructor-field-keyword ...] [super-field-keyword ...]))
              (build-class-dot-handling method-map method-vtable final?
                                        #'(name constructor-name name-instance name-ref
                                                [public-field-name ...]
                                                [public-name-field ...]
                                                [export ...]))
              (build-class-static-infos exposed-internal-id
                                        #'(name constructor-name name-instance
                                                [name-field ...]
                                                [field-static-infos ...]))
              (build-class-desc super options
                                constructor-keywords super-keywords
                                constructor-defaults super-defaults
                                final? has-private-fields?
                                parent-name
                                method-map method-names method-vtable
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
                                        ([field-name field-argument] ...)))))
           #`(begin . #,defns)))])))

(define-for-syntax (build-class-struct super
                                       fields mutables final? authentic?
                                       method-map method-names method-vtable method-private
                                       abstract-name
                                       interfaces private-interfaces
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
                  [((method-name method-proc) ...) (for/list ([v (in-vector method-vtable)]
                                                              [i (in-naturals)])
                                                     (list (hash-ref method-names i) v))])
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
                                                                                ...)
                                                                        (hasheq (~@ 'method-name method-proc)
                                                                                ...)))))
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
                                                                                                     method-map method-vtable method-names
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

(define-syntax (define-class-desc-syntax stx)
  (syntax-parse stx
    [(_ id:identifier rhs)
     #`(define-syntax #,(in-class-desc-space #'id)
         rhs)]))

(define-for-syntax (build-class-desc super options
                                     constructor-keywords super-keywords
                                     constructor-defaults super-defaults
                                     final? has-private-fields?
                                     parent-name
                                     method-map method-names method-vtable
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
                                                        (class-desc-all-fields super)
                                                        '())
                                                    (for/list ([name (in-list (syntax->list #'(field-name ...)))]
                                                               [arg (in-list (syntax->list #'(field-argument ...)))])
                                                      (if (identifier? arg)
                                                          arg
                                                          (syntax-e name)))))))
                     '#,(for/vector ([i (in-range (vector-length method-vtable))])
                          (define name (hash-ref method-names i))
                          (if (box? (hash-ref method-map (if (syntax? name) (syntax-e name) name)))
                              (box name)
                              name))
                     (quote-syntax #,method-vtable)
                     '#,method-map
                     #,(cond
                         [(syntax-e #'constructor-maker-name)
                          #`(quote-syntax ([#,(encode-protocol constructor-keywords constructor-defaults) constructor-maker-name]
                                           #,@(if super
                                                  (or (class-desc-constructor-makers super)
                                                      (list (list (encode-protocol super-keywords super-defaults) #f)))
                                                  '())))]
                         [else #'#f])
                     #,(and (hash-ref options 'binding #f) #t)
                     #,(and (hash-ref options 'annotation #f) #t)
                     #,(and (syntax-e #'name-defaults)
                            #'(quote-syntax name-defaults)))))))
