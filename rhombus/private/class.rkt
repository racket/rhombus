#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest/syntax-local
                     enforest/hier-name-parse
                     (only-in enforest/operator operator-proc)
                     "srcloc.rkt"
                     "name-path-op.rkt"
                     "introducer.rkt"
                     "tag.rkt"
                     "with-syntax.rkt")
         racket/unsafe/undefined
         "forwarding-sequence.rkt"
         "definition.rkt"
         "expression.rkt"
         "binding.rkt"
         (submod "binding-syntax.rkt" for-class)
         (for-syntax "class-transformer.rkt")
         "annotation.rkt"
         (submod "annotation.rkt" for-class)
         (submod "annotation-syntax.rkt" for-class)
         (submod "dot.rkt" for-dot-provider)
         "call-result-key.rkt"
         "composite.rkt"
         "assign.rkt"
         "static-info.rkt"
         "name-root.rkt"
         "name-root-ref.rkt"
         "dotted-sequence-parse.rkt"
         "realm.rkt"
         "parens.rkt"
         "parse.rkt"
         "implicit.rkt"
         "error.rkt"
         "class-clause.rkt"
         "entry-point.rkt"
         (submod "boolean-pattern.rkt" for-class)
         (rename-in "equal.rkt"
                    [= rhombus=]))

(provide (rename-out [rhombus-class class])
         extends
         internal
         constructor
         binding
         annotation
         final
         nonfinal
         authentic
         field)

(begin-for-syntax
  (struct class-desc (final?
                      class:id
                      constructor-id
                      binding-id
                      annotation-id
                      fields ; (list (list id accessor-id mutator-id static-infos constructor-mode) ...)
                      constructor-makers  ; (list constructor-maker ... maybe-default-constuctor-desc)
                      custom-binding?
                      custom-annotation?
                      defaults-id)) ; #f if no arguments with defaults
  (define (class-desc-ref v) (and (class-desc? v) v))

  (define (field-desc-name f) (car f))
  (define (field-desc-accessor-id f) (cadr f))
  (define (field-desc-mutator-id f) (list-ref f 2))
  (define (field-desc-static-infos f) (list-ref f 3))
  (define (field-desc-constructor-arg f) (list-ref f 4)) ; syntax of #f (by-position), keyword, or identifier (not in constructor)

  (define (field-spec-arg-id fs) (cadr fs))
  (define (field-spec-static-infos fs) (list-ref fs 2))
  (define (field-spec-mutable fs) (list-ref fs 3))

  (define in-class-desc-space (make-interned-syntax-introducer/add 'rhombus/class))

  (define-syntax-class :not-equal
    #:description "an annotation term"
    #:datum-literals (op)
    #:literals (rhombus=)
    (pattern (~not (op rhombus=))))

  (define-syntax-class :id-field
    #:datum-literals (group op)
    #:literals (mutable rhombus=)
    (pattern (group (~optional (~and mutable (~var mutable))
                               #:defaults ([mutable #'#f]))
                    name:identifier
                    ann::not-equal ...
                    (op rhombus=)
                    default-form ...+)
             #:with ((~optional c::inline-annotation)) #'(ann ...)
             #:attr predicate (if (attribute c)
                                  #'c.predicate
                                  #'#f)
             #:attr annotation-str (if (attribute c)
                                       #'c.annotation-str
                                       #'#f)
             #:attr static-infos (if (attribute c)
                                     #'c.static-infos
                                     #'())
             #:attr default #`((rhombus-expression (#,group-tag default-form ...))))
    (pattern (group (~optional (~and mutable (~var mutable))
                               #:defaults ([mutable #'#f]))
                    name:identifier
                    (~optional c::inline-annotation))
             #:attr predicate (if (attribute c)
                                  #'c.predicate
                                  #'#f)
             #:attr annotation-str (if (attribute c)
                                       #'c.annotation-str
                                       #'#f)
             #:attr static-infos (if (attribute c)
                                     #'c.static-infos
                                     #'())
             #:attr default #'#f))
  
  (define-syntax-class :field
    #:datum-literals (group op)
    #:literals (mutable rhombus=)
    (pattern idf::id-field
             #:attr predicate #'idf.predicate
             #:attr annotation-str #'idf.annotation-str
             #:attr static-infos #'idf.static-infos
             #:attr name #'idf.name
             #:attr keyword #'#f
             #:attr default #'idf.default
             #:attr mutable #'idf.mutable)
    (pattern (group kw:keyword (::block idf::id-field))
             #:attr predicate #'idf.predicate
             #:attr annotation-str #'idf.annotation-str
             #:attr static-infos #'idf.static-infos
             #:attr name #'idf.name
             #:attr keyword #'kw
             #:attr default #'idf.default
             #:attr mutable #'idf.mutable)
    (pattern (group kw:keyword)
             #:attr predicate #'#f
             #:attr annotation-str #'#f
             #:attr static-infos #'()
             #:attr name (datum->syntax #'kw (string->symbol (keyword->string (syntax-e #'kw))) #'kw #'kw)
             #:attr keyword #'kw
             #:attr default #'#f
             #:attr mutable #'#f)
    (pattern (group kw:keyword (op rhombus=) default-form ...+)
             #:attr predicate #'#f
             #:attr annotation-str #'#f
             #:attr static-infos #'()
             #:attr name (datum->syntax #'kw (string->symbol (keyword->string (syntax-e #'kw))) #'kw #'kw)
             #:attr keyword #'kw
             #:attr default #`((rhombus-expression (#,group-tag default-form ...)))
             #:attr mutable #'#f))

  (define-syntax-class :not-parens
    #:datum-literals (parens)
    (pattern (~not (parens . _))))

  (define-splicing-syntax-class :options-block
    #:datum-literals (block group parens)
    (pattern (~seq)
             #:attr (form 1) '())
    (pattern (~seq (_::block form ...))))

  (define (check-duplicate-field-names stxes fields super)
    (let ([ht (for/hasheq ([field (in-list (if super (class-desc-fields super) '()))])
                (values (field-desc-name field) 'super))])
      (for/fold ([ht ht]) ([field (in-list fields)])
        (define prev (hash-ref ht (syntax-e field) #f))
        (when prev
          (raise-syntax-error #f
                              (if (eq? prev 'super)
                                  "field name already exists in superclass"
                                  "duplicate field name")
                              stxes
                              field))
        (hash-set ht (syntax-e field) #t))))

  (define (check-field-defaults stxes super-has-defaults? constructor-fields defaults keywords)
    (for/fold ([need-default? #f]) ([f (in-list constructor-fields)]
                                    [df (in-list defaults)]
                                    [kw (in-list keywords)])
      (cond
        [(syntax-e kw) need-default?]
        [(syntax-e df) #t]
        [need-default? (raise-syntax-error #f
                                           "by-position field without default after by-position field with a default"
                                           stxes
                                           f)]
        [super-has-defaults? (raise-syntax-error #f
                                                 (string-append "field needs a default,"
                                                                " because a superclass field has a default")
                                                 stxes
                                                 f)]
        [else #f]))))

(define-syntax rhombus-class
  (definition-transformer
   (lambda (stxes)
     (syntax-parse stxes
       #:datum-literals (group block)
       [(_ name-seq::dotted-identifier-sequence ((~datum parens) field::field ...)
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
           #`((class-finish #,finish-data))]
          [else
           #`((rhombus-mixed-forwarding-sequence (class-finish #,finish-data) rhombus-class
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
       #`(begin
           (rhombus-definition form)
           (class-body-step . rest))]
      [(_) #'(begin)])))

(define-syntax class-finish
  (lambda (stx)
    (syntax-parse stx
      [(_ [orig-stx base-stx scope-stx
                    full-name name
                    (constructor-field-name ...)
                    (field-keyword ...) ; #f or keyword
                    (field-default ...) ; #f or (parsed)
                    (field-mutable ...)
                    (field-predicate ...)
                    (field-annotation-str ...)
                    (constructor-field-static-infos ...)]
          option ...)
       (define stxes #'orig-stx)
       (define options (parse-options #'orig-stx #'(option ...)))
       (define parent-name (hash-ref options 'extends #f))
       (define super (and parent-name
                          (or (syntax-local-value* (in-class-desc-space parent-name) class-desc-ref)
                              (raise-syntax-error #f "not a class name" stxes parent-name))))
       (define final? (hash-ref options 'final? (not super)))
       (define authentic? (hash-ref options 'authentic? #f))
       (define internal-id (hash-ref options 'internal #f))
       (define expose (if internal-id
                          (let ([intro (make-syntax-delta-introducer #'scope-stx #'base-stx)])
                            (lambda (stx)
                              (intro stx 'remove)))
                          (lambda (stx) stx)))
       (define (maybe-use-internal-id id clause-name)
         (cond
           [(syntax-e id) id]
           [internal-id internal-id]
           [else (raise-syntax-error #f
                                     (format "no `internal` clause, and no maker name in `~a` clause"
                                             clause-name)
                                     stxes)]))
       (define constructor-id (let ([id (hash-ref options 'constructor-id #f)])
                                (and id
                                     (maybe-use-internal-id id 'constructor))))
       (define binding-id (let ([b (hash-ref options 'binding #f)])
                            (and b
                                 (maybe-use-internal-id (car b) 'binding))))
       (define annotation-id (let ([b (hash-ref options 'annotation #f)])
                               (and b
                                    (maybe-use-internal-id (car b) 'annotation))))
       (when super
         (when (class-desc-final? super)
           (raise-syntax-error #f
                               "superclass is final and cannot be extended"
                               stxes
                               parent-name))
         (define (check-consistent-custom p? id what)
           (when (and p? (not id))
             (raise-syntax-error #f
                                 (format "superclass has a custom ~a, so a subclass needs a custom ~a" what what)
                                 stxes
                                 parent-name)))
         (check-consistent-custom (class-desc-constructor-makers super) constructor-id "constructor")
         (check-consistent-custom (class-desc-custom-binding? super) binding-id "binding")
         (check-consistent-custom (class-desc-custom-annotation? super) annotation-id "annotation"))
       (define constructor-fields (syntax->list #'(constructor-field-name ...)))
       (define more-field-specs (reverse (hash-ref options 'fields '())))
       (define extra-fields (map car more-field-specs))
       (define fields (append constructor-fields extra-fields))
       (define mutables (append (syntax->list #'(field-mutable ...))
                                (for/list ([f (in-list more-field-specs)])
                                  (if (syntax-e (field-spec-mutable f)) #'#t #'#f))))
       (define keywords (syntax->list #'(field-keyword ...)))
       (define defaults (syntax->list #'(field-default ...)))
       (define has-defaults? (for/or ([df (in-list defaults)]) (syntax-e df)))
       (define super-constructor-fields
         (if super
             (for/list ([f (in-list (class-desc-fields super))]
                        #:do [(define arg (field-desc-constructor-arg f))]
                        #:unless (identifier? arg))
               (field-desc-name f))
             '()))
       (define super-keywords
         (if super
             (for/list ([f (in-list (class-desc-fields super))]
                        #:do [(define arg (field-desc-constructor-arg f))]
                        #:unless (identifier? arg))
               (if (box? (syntax-e arg))
                   (unbox (syntax-e arg))
                   arg))
             '()))
       (define super-defaults
         (if super
             (for/list ([f (in-list (class-desc-fields super))]
                        #:do [(define arg (field-desc-constructor-arg f))]
                        #:unless (identifier? arg))
               (if (box? (syntax-e arg))
                   #'(unsafe-undefined)
                   #'#f))
             '()))
       (define super-has-keywords? (for/or ([kw (in-list super-keywords)])
                                     (syntax-e kw)))
       (define super-has-defaults? (for/or ([df (in-list super-defaults)])
                                     (syntax-e df)))
       (define super-has-by-position-default? (for/or ([kw (in-list super-keywords)]
                                                       [df (in-list super-defaults)])
                                                (and (not (syntax-e kw))
                                                     (syntax-e df))))
       (define need-constructor-wrapper?
         (need-class-constructor-wrapper? extra-fields keywords defaults constructor-id
                                          super-has-keywords? super-has-defaults? super))
       (define (to-keyword f) (datum->syntax f (string->keyword (symbol->string (syntax-e f))) f f))
       (check-duplicate-field-names stxes fields super)
       (check-field-defaults stxes super-has-by-position-default? constructor-fields defaults keywords)
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
       (with-syntax ([name? (datum->syntax #'name (string->symbol (format "~a?" (syntax-e #'name))) #'name)]
                     [(class:name) (generate-temporaries #'(name))]
                     [(make-name) (generate-temporaries #'(name))]
                     [name-defaults (and (or super-has-defaults? (and has-defaults? (not final?)))
                                         (car (generate-temporaries (list (format "~a-defaults" (syntax-e #'name))))))]
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
                                                       (for/list ([f (in-list more-field-specs)])
                                                         (field-spec-static-infos f)))]
                     [(field-argument ...) (append (for/list ([kw (in-list keywords)]
                                                              [df (in-list defaults)])
                                                     (if (syntax-e df)
                                                         (box kw)
                                                         kw))
                                                   (for/list ([f (in-list more-field-specs)])
                                                     (field-spec-arg-id f)))]
                     [((super-field-name
                        super-name-field
                        super-maybe-set-name-field!
                        super-field-static-infos
                        super-field-argument)
                       ...)
                      (if super
                          (class-desc-fields super)
                          '())]
                     [(super-field-keyword ...) super-keywords])
         (with-syntax ([constructor-name (if constructor-id
                                             (car (generate-temporaries #'(name)))
                                             #'make-name)]
                       [constructor-maker-name (and (or (not final?)
                                                        super)
                                                    constructor-id
                                                    (car (generate-temporaries #'(maker))))]
                       [make-all-name (if need-constructor-wrapper?
                                          (car (generate-temporaries #'(name)))
                                          #'make-name)])
           (define defns
             (append
              (build-class-struct super
                                  fields mutables final? authentic?
                                  #'(name class:name make-all-name name?
                                          [field-name ...]
                                          [name-field ...]
                                          [set-name-field! ...]
                                          [field-predicate ...]
                                          [field-annotation-str ...]
                                          [super-field-name ...]
                                          [super-name-field ...]))
              (build-class-constructor super constructor-id options
                                       constructor-fields super-constructor-fields more-field-specs
                                       keywords super-keywords
                                       defaults super-defaults
                                       need-constructor-wrapper?
                                       has-defaults? super-has-defaults?
                                       final?
                                       (expose internal-id)
                                       #'(make-name make-all-name constructor-name constructor-maker-name
                                                    name?
                                                    name-defaults))
              (build-class-binding-form super binding-id options
                                        (expose internal-id) intro
                                        #'(name name-instance name?
                                                [constructor-name-field ...] [super-name-field ...]
                                                [constructor-field-static-infos ...] [super-field-static-infos ...]
                                                [field-keyword ...] [super-field-keyword ...]))
              (build-class-annotation-form super annotation-id options
                                           constructor-fields super-constructor-fields
                                           (expose internal-id) intro
                                           #'(name name-instance name?
                                                   [constructor-name-field ...] [super-name-field ...]
                                                   [field-keyword ...] [super-field-keyword ...]))
              (build-class-dot-handling #'(name constructor-name name-instance
                                                [field-name ...]
                                                [name-field ...]))
              (build-class-static-infos (expose internal-id)
                                        #'(name constructor-name name-instance
                                                [name-field ...]
                                                [field-static-infos ...]))
              (build-class-desc super options
                                keywords super-keywords
                                defaults super-defaults
                                final?
                                #'(name class:name constructor-maker-name name-defaults
                                        (list (list 'super-field-name
                                                    (quote-syntax super-name-field)
                                                    (quote-syntax super-make-set-name-field!)
                                                    (quote-syntax super-field-static-infos)
                                                    (quote-syntax super-field-argument))
                                              ...
                                              (list 'field-name
                                                    (quote-syntax name-field)
                                                    (quote-syntax maybe-set-name-field!)
                                                    (quote-syntax field-static-infos)
                                                    (quote-syntax field-argument))
                                              ...)))))
           #`(begin . #,defns)))])))

(define-for-syntax (class-expression-transformer id make-id)
  (expression-transformer
   id
   (lambda (stx)
     (syntax-parse stx
       [(_ . tail) (values make-id #'tail)]))))

(define-for-syntax (build-class-struct super
                                       fields mutables final? authentic?
                                       names)
  (with-syntax ([(name class:name make-all-name name?
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
                     (list pred ann-str))])
      (list
       #`(define-values (class:name make-all-name name? name-field ... set-name-field! ...)
           (let-values ([(class:name name name? name-ref name-set!)
                         (make-struct-type 'name
                                           #,(and super (class-desc-class:id super))
                                           #,(length fields) 0 #f
                                           (list (cons prop:field-name->accessor
                                                       (cons '(field-name ...)
                                                             (hasheq (~@ 'super-field-name super-name-field)
                                                                     ...)))
                                                 #,@(if final?
                                                        (list #'(cons prop:sealed #t))
                                                        '())
                                                 #,@(if authentic?
                                                        (list #'(cons prop:authentic #t))
                                                        '()))
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
                     ...)))))))

(define-for-syntax (build-guard-expr fields predicates annotation-strs)
  (and (for/or ([predicate (in-list predicates)])
         (syntax-e predicate))
       #`(lambda (#,@fields who)
           (values #,@(for/list ([field (in-list fields)]
                                 [predicate (in-list predicates)]
                                 [annotation-str (in-list annotation-strs)])
                        (cond
                          [(not (syntax-e predicate)) field]
                          [else #`(if (#,predicate #,field)
                                      #,field
                                      (raise-annotation-failure who
                                                                #,field
                                                                '#,annotation-str))]))))))

(define-for-syntax (make-class-instance-predicate accessors)
  (lambda (arg predicate-stxs)
    #`(and #,@(for/list ([acc (in-list accessors)]
                         [pred (in-list predicate-stxs)])
                #`(#,pred (#,acc #,arg))))))

(define-for-syntax (make-class-instance-static-infos accessors)
  (lambda (static-infoss)
    (for/list ([acc (in-list accessors)]
               [static-infos (in-list static-infoss)])
      #`(#,acc #,static-infos))))

;; dot provider for a class instance used before a `.`
(define-for-syntax ((make-handle-class-instance-dot name) form1 dot field-id
                                                          tail more-static?
                                                          success failure)
  (define desc (syntax-local-value* (in-class-desc-space name) class-desc-ref))
  (unless desc (error "cannot find annotation binding for instance dot provider"))
  (define fld
    (for/or ([field+acc (in-list (class-desc-fields desc))])
      (and (eq? (field-desc-name field+acc) (syntax-e field-id))
           field+acc)))
  (cond
    [fld
     (define accessor-id (field-desc-accessor-id fld))
     (define-values (op-id assign-rhs new-tail)
       (syntax-parse tail
         #:datum-literals (op)
         #:literals (:=)
         [((op :=) rhs ...)
          #:when (syntax-e (field-desc-mutator-id fld))
          (values (field-desc-mutator-id fld)
                  #`(rhombus-expression (#,group-tag rhs ...))
                  #'())]
         [_
          (values accessor-id
                  #f
                  tail)]))
     (define e (datum->syntax (quote-syntax here)
                              (append (list (relocate field-id op-id) form1)
                                      (if assign-rhs
                                          (list field-id)
                                          null))
                              (span-srcloc form1 field-id)
                              #'dot))
     (define full-e
       (cond
         [assign-rhs
          (success #`(let ([#,field-id #,assign-rhs])
                       #,e
                       #,field-id))]
         [else e]))
     
     (define static-infos (field-desc-static-infos fld))
     (define more-static-infos (syntax-local-static-info form1 accessor-id))
     (define all-static-infos (if more-static-infos
                                  (datum->syntax #f
                                                 (append (syntax->list more-static-infos)
                                                         static-infos))
                                  static-infos))
     (success (wrap-static-info* full-e all-static-infos)
              new-tail)]
    [else (failure)]))

(define-syntax (define-class-desc-syntax stx)
  (syntax-parse stx
    [(_ id:identifier rhs)
     #`(define-syntax #,(in-class-desc-space #'id)
         rhs)]))

(define-syntax (define-static-info-syntax/maybe* stx)
  (syntax-parse stx
    [(_ id (_)) #'(begin)]
    [(_ id rhs ...) #'(define-static-info-syntax id rhs ...)]))

;; ----------------------------------------

(define-for-syntax (build-class-constructor super constructor-id options
                                            constructor-fields super-constructor-fields more-field-specs
                                            keywords super-keywords
                                            defaults super-defaults
                                            need-constructor-wrapper?
                                            has-defaults? super-has-defaults?
                                            final?
                                            exposed-internal-id
                                            names)
  (with-syntax ([(make-name make-all-name constructor-name constructor-maker-name
                            name?
                            name-defaults)
                 names])
    (append
     (if (syntax-e #'name-defaults)
         (list
          ;; default-value expressions should see only earlier fields
          ;; from `constructor-fields`, so use some temporary names
          ;; to make sure they can't be referenced
          (let ([super-tmps (generate-temporaries super-constructor-fields)]
                [tmps (generate-temporaries constructor-fields)])
            #`(define (name-defaults #,@(append super-tmps tmps))
                (let-values #,(cond
                                [super-has-defaults?
                                 #`([#,super-tmps
                                     (#,(class-desc-defaults-id super) . #,super-tmps)])]
                                [else '()])
                  (let* #,(for/list ([f (in-list constructor-fields)]
                                     [tmp (in-list tmps)]
                                     [df (in-list defaults)])
                            (cond
                              [(syntax-e df)
                               #`[#,f (if (eq? #,tmp unsafe-undefined)
                                          (let ([#,f . #,df]) #,f)
                                          #,tmp)]]
                              [else
                               #`[#,f #,tmp]]))
                    (values #,@super-tmps #,@constructor-fields))))))
         null)
     (if need-constructor-wrapper?
         (list
          #`(define make-name
              (lambda #,(apply append (for/list ([f (in-list (append super-constructor-fields constructor-fields))]
                                                 [kw (in-list (append super-keywords keywords))]
                                                 [df (in-list (append super-defaults defaults))])
                                        (let ([arg (if (syntax-e df)
                                                       (if final?
                                                           #`[#,f . #,df]
                                                           #`[#,f unsafe-undefined])
                                                       f)])
                                          (if (keyword? (syntax-e kw))
                                              (list kw arg)
                                              (list arg)))))
                (let-values #,(cond
                                [(and super-has-defaults? (or final? (not has-defaults?)))
                                 #`([#,super-constructor-fields
                                     (#,(class-desc-defaults-id super) . #,super-constructor-fields)])]
                                [(and has-defaults? (not final?))
                                 (define fields (append super-constructor-fields constructor-fields))
                                 #`([#,fields (name-defaults . #,fields)])]
                                [else '()])
                  (make-all-name #,@(for/list ([f (in-list (if super (class-desc-fields super) null))])
                                      (if (identifier? (field-desc-constructor-arg f))
                                          (field-desc-constructor-arg f)
                                          (field-desc-name f)))
                                 #,@constructor-fields
                                 #,@(map field-spec-arg-id more-field-specs))))))
         null)
     (if exposed-internal-id
         (list
          #`(define-syntax #,exposed-internal-id (make-rename-transformer (quote-syntax make-name))))
         null)
     (if constructor-id
         (cond
           [(and final?
                 (not super))
            (list
             #`(define constructor-name
                 (let-syntax ([#,constructor-id (make-rename-transformer (quote-syntax make-name))])
                   (let ([name (wrap-constructor name name?
                                                 #,(hash-ref options 'constructor-rhs))])
                     name))))]
           [else
            (list
             #`(define constructor-maker-name
                 (lambda (#,constructor-id)
                   (let ([name (wrap-constructor name name?
                                                 #,(hash-ref options 'constructor-rhs))])
                     name)))
             #`(define constructor-name
                 #,(cond
                     [super
                      (compose-constructor
                       #'make-name
                       #`([#,(encode-protocol keywords defaults) constructor-maker-name]
                          #,@(or (class-desc-constructor-makers super)
                                 (list (list (encode-protocol super-keywords super-defaults) #f)))))]
                     [else #'(constructor-maker-name make-name)])))])
         null))))

(define-for-syntax (need-class-constructor-wrapper? extra-fields keywords defaults constructor-id
                                                    super-has-keywords? super-has-defaults?
                                                    super)
  (or (pair? extra-fields)
      (for/or ([kw (in-list keywords)])
        (syntax-e kw))
      (for/or ([df (in-list defaults)])
        (syntax-e df))
      (and (or super-has-keywords?
               super-has-defaults?)
           (or (not (class-desc-constructor-makers super))
               constructor-id))
      (and super
           (for/or ([f (in-list (class-desc-fields super))])
             (identifier? (field-desc-constructor-arg f))))))

(define-syntax (wrap-constructor stx)
  (syntax-parse stx
    [(_ name predicate-id g)
     #:do [(define adjustments (entry-point-adjustments
                                '()
                                (lambda (body)
                                  #`(let ([r #,body])
                                      (if (predicate-id r)
                                          r
                                          #,(quasisyntax/loc #'g
                                              (raise-constructor-result-error 'name r)))))
                                #f))]
     #:with (~var lam (:entry-point adjustments)) #'g
     #'lam.parsed]))

(define (raise-constructor-result-error who val)
  (raise-contract-error who
                        (string-append "constructor result does not match annotation\n"
                                       "  result: ~v")
                        val))

(define-for-syntax (makers->pair-expressions stx)
  (and stx
       (syntax-parse stx
         [() '()]
         [([count id] . rest)
          (cons #'(cons count id) (makers->pair-expressions #'rest))]
         [(count) #'count])))

;; Beware that this function generates code that is quadratic in the
;; length of `makers` (i.e., in the depth of subclassing with custom
;; constructors)
(define-for-syntax (compose-constructor real-make makers)
  (define argss
    (let loop ([makers makers])
      (syntax-parse makers
        [([proto _])
         (list (generate-protocol-formal-arguments #'proto))]
        [([proto _] . rest)
         (define super-args (loop #'rest))
         (cons (append (car super-args) (generate-protocol-formal-arguments #'proto))
               super-args)])))
  (let loop ([makers makers]
             [argss argss]
             [final-make real-make])
    (syntax-parse makers
      [([proto id:identifier])
       ;; root ancestor has a custom protocol
       #`(id #,final-make)]
      [([_ #f])
       ;; represents some number of ancestors that use the default protocol;
       ;; we don't expect to get here, though, since the last case below
       ;; looks ahead to cover this case
       (error "should not get here")]
      [([proto id] . rest)
       ;; look ahead by one in `rest` so we can get the arity right
       (define formal-args (generate-protocol-formal-arguments #'proto))
       (define ancestor-formal-args (cadr argss))
       (define args (protocol-formal->actual formal-args))
       (define ancestor-args (protocol-formal->actual ancestor-formal-args))
       (syntax-parse #'rest
         [([ancestor-proto #f])
          #`(id
             (lambda #,ancestor-formal-args
               (lambda #,args
                 (#,final-make #,@ancestor-args #,@args))))]
         [([ancestor-proto ancestor-id] . _)
          #`(id
             ;; The position after `constructor` is syntactically constrainted, but
             ;; not so much that we can statically extract its signature. So, we
             ;; dynamically adjust a generic wrapper to match the constructor's
             ;; signature
             (make-keyword-procedure-like
              ancestor-id
              (lambda (kws kw-args . next-args)
                (lambda #,formal-args
                  (keyword-apply #,(loop #'rest
                                         (cdr argss)
                                         #`(lambda #,ancestor-formal-args
                                             (#,final-make #,@ancestor-args #,@args)))
                                 kws kw-args
                                 next-args)))))])])))

(define-for-syntax (encode-protocol keywords defaults)
  (if (for/and ([kw (in-list keywords)]
                [df (in-list defaults)])
        (and (not (syntax-e kw))
             (not (syntax-e df))))
      (length keywords)
      (for/list ([kw (in-list keywords)]
                 [df (in-list defaults)])
        (if (syntax-e df)
            (box kw)
            kw))))

(define-for-syntax (generate-protocol-formal-arguments proto)
  (syntax-parse proto
    [count:exact-integer
     (generate-temporaries (for/list ([i (syntax-e #'count)]) i))]
    [(arg-desc ...)
     (apply append
            (for/list ([desc (syntax->list #'(arg-desc ...))])
              (define id (car (generate-temporaries '(arg))))
              (define arg (if (box? (syntax-e desc))
                              #`[#,id unsafe-undefined]
                              id))
              (if (or (keyword? (syntax-e desc))
                      (and (box? (syntax-e desc))
                           (keyword? (syntax-e (unbox (syntax-e desc))))))
                  (list desc arg)
                  (list arg))))]))

(define-for-syntax (protocol-formal->actual args)
  (for/list ([arg (in-list args)])
    (syntax-parse arg
      [(id . _) #'id]
      [_ arg])))

(define (make-keyword-procedure-like make-proc gen-proc)
  (define proc (make-proc void)) ; we know that `make-proc` immediately returns a procedure
  (define-values (allow-kws req-kws) (procedure-keywords proc))
  (procedure-reduce-keyword-arity-mask (make-keyword-procedure gen-proc)
                                       (procedure-arity-mask proc)
                                       allow-kws
                                       req-kws))

(define-syntax (compose-annotation-check stx)
  (syntax-parse stx
    [(_ mutator who #f _) #'mutator]
    [(_ mutator who predicate annotation-str)
     #`(let ([m mutator])
         (lambda (obj val)
           (unless (predicate val)
             (raise-annotation-failure 'who
                                       val
                                       'annotation-str))
           (m obj val)))]))

;; ----------------------------------------

(define-for-syntax (build-class-binding-form super binding-id options
                                             exposed-internal-id intro
                                             names)
  (with-syntax ([(name name-instance name?
                       [constructor-name-field ...] [super-name-field ...]
                       [constructor-field-static-infos ...] [super-field-static-infos ...]
                       [field-keyword ...] [super-field-keyword ...])
                 names])
    (with-syntax ([core-bind-name (if (hash-ref options 'binding #f)
                                      (car (generate-temporaries #'(name)))
                                      #'name)])
      (append
       (list
        #`(define-binding-syntax core-bind-name
            (binding-transformer
             (quote-syntax name)
             #,(if (and super
                        binding-id)
                   #`(make-curried-binding-transformer (quote-syntax #,(class-desc-binding-id super))
                                                       #,(symbol->string (syntax-e #'name))
                                                       (quote-syntax name?)
                                                       #:static-infos (quote-syntax ((#%dot-provider name-instance)))
                                                       (list (quote-syntax constructor-name-field) ...)
                                                       #:keywords '(field-keyword ...)
                                                       (list (quote-syntax constructor-field-static-infos) ...))
                   #`(make-composite-binding-transformer #,(symbol->string (syntax-e #'name))
                                                         (quote-syntax name?)
                                                         #:static-infos (quote-syntax ((#%dot-provider name-instance)))
                                                         (list (quote-syntax super-name-field) ...
                                                               (quote-syntax constructor-name-field) ...)
                                                         #:keywords '(super-field-keyword ... field-keyword ...)
                                                         (list (quote-syntax super-field-static-infos) ...
                                                               (quote-syntax constructor-field-static-infos) ...)
                                                         #:accessor->info? #t)))))
       (if exposed-internal-id
           (list
            #`(define-binding-syntax #,exposed-internal-id (make-rename-transformer (quote-syntax core-bind-name))))
           null)
       (cond
         [(hash-ref options 'binding #f)
          => (lambda (bind)
               (list
                #`(define-binding-syntax #,(intro binding-id) (make-rename-transformer
                                                               (quote-syntax #,(in-binding-space #'core-bind-name))))
                #`(define-binding-syntax name
                    (wrap-class-transformer name #,(intro (cadr bind)) make-binding-prefix-operator))))]
         [else null])))))

(define-for-syntax (make-curried-binding-transformer super-binding-id
                                                     constructor-str predicate accessors static-infoss
                                                     #:static-infos static-infos
                                                     #:keywords keywords)
  (define t
    (make-composite-binding-transformer constructor-str predicate accessors static-infoss
                                        #:static-infos static-infos
                                        #:keywords keywords
                                        #:accessor->info? #t))
  (cond
    [super-binding-id
     (define p-t (operator-proc
                  (syntax-local-value* (in-binding-space super-binding-id) binding-prefix-operator-ref)))
     (lambda (tail)
       (syntax-parse tail
         [(form-id p-term (tag::parens g ...) . new-tail)
          (define stx (no-srcloc #'(form-id p-term (tag g ...))))
          (define-values (p-binding p-tail) (p-t #'(form-id p-term)))
          (define-values (binding c-tail) (t #'(form-id (tag g ...)) #f stx))
          (values (make-and-binding p-binding binding)
                  #'new-tail)]))]
    [else t]))

;; ----------------------------------------

(define-for-syntax (build-class-annotation-form super annotation-id options
                                                constructor-fields super-constructor-fields
                                                exposed-internal-id intro
                                                names)
  (with-syntax ([(name name-instance name?
                       [constructor-name-field ...] [super-name-field ...]
                       [field-keyword ...] [super-field-keyword ...])
                 names])
    (with-syntax ([core-ann-name (if (hash-ref options 'annotation #f)
                                     (car (generate-temporaries #'(name)))
                                     #'name)]
                  [parse-name-of (and super
                                      annotation-id
                                      (car (generate-temporaries #'(name))))])
      (append
       (list
        #`(define-annotation-constructor core-ann-name
            ([accessors #,(if (syntax-e #'parse-name-of)
                              #'(list (quote-syntax constructor-name-field) ...)
                              #'(list (quote-syntax super-name-field) ...
                                      (quote-syntax constructor-name-field) ...))]
             #,@(if (syntax-e #'parse-name-of)
                    #`([parse-name-of
                        (make-curried-annotation-of-tranformer (quote-syntax #,(class-desc-annotation-id super)))])
                    null))
            (quote-syntax name?)
            (quote-syntax ((#%dot-provider name-instance)))
            #,(if (syntax-e #'parse-name-of)
                  (length constructor-fields)
                  #`(quote #,(+ (length constructor-fields)
                                (length super-constructor-fields))))
            #,(if (syntax-e #'parse-name-of)
                  #'(field-keyword ...)
                  #'(super-field-keyword ... field-keyword ...))
            (make-class-instance-predicate accessors)
            (make-class-instance-static-infos accessors)
            #:parse-of #,(if (syntax-e #'parse-name-of)
                             #'parse-name-of
                             #'parse-annotation-of)))
       (if exposed-internal-id
           (list
            #`(define-annotation-syntax #,exposed-internal-id (make-rename-transformer (quote-syntax core-ann-name))))
           null)
       (cond
         [(hash-ref options 'annotation #f)
          => (lambda (ann)
               (list
                #`(define-annotation-syntax #,(intro annotation-id) (make-rename-transformer
                                                                     (quote-syntax #,(in-annotation-space #'core-ann-name))))
                #`(define-annotation-syntax name
                    (wrap-class-transformer name #,(intro (cadr ann)) make-annotation-prefix-operator))))]
         [else null])))))

(define-for-syntax (make-curried-annotation-of-tranformer super-annotation-id)
  (lambda (tail predicate-stx static-infos
                sub-n kws predicate-maker info-maker)
    (syntax-parse tail
      [(form-id p-term (tag::parens g ...) . new-tail)
       #:with p::annotation #`(#,group-tag #,super-annotation-id (op |.|) of p-term)
       (define-values (ann c-tail) (parse-annotation-of #'(form-id (tag g ...))
                                                        predicate-stx static-infos
                                                        sub-n kws predicate-maker info-maker))
       (with-syntax-parse ([p::annotation-form #'p.parsed]
                           [c::annotation-form ann])
         (values (annotation-form
                  #`(let ([p? p.predicate]
                          [c? c.predicate])
                      (lambda (v) (and (p? v) (c? v))))
                  (append (syntax->list #'p.static-infos)
                          #'c.static-infos))
                 #'new-tail))])))

;; ----------------------------------------

(define-for-syntax (build-class-dot-handling names)
  (with-syntax ([(name constructor-name name-instance
                       [field-name ...]
                       [name-field ...])
                 names])
    (list
     #'(define-name-root name
         #:root (class-expression-transformer (quote-syntax name) (quote-syntax constructor-name))
         #:fields ([field-name name-field] ...))
     #'(define-dot-provider-syntax name-instance
         (dot-provider-more-static (make-handle-class-instance-dot (quote-syntax name)))))))

;; ----------------------------------------

(define-for-syntax (build-class-static-infos exposed-internal-id
                                             names)
  (with-syntax ([(name constructor-name name-instance
                       [name-field ...]
                       [field-static-infos ...])
                 names])
    (append
     (list
      #'(define-static-info-syntax constructor-name (#%call-result ((#%dot-provider name-instance)))))
     (if exposed-internal-id
         (list
          #`(define-static-info-syntax #,exposed-internal-id (#%call-result ((#%dot-provider name-instance)))))
         '())
     (list
      #'(begin
          (define-static-info-syntax/maybe* name-field (#%call-result field-static-infos))
          ...)))))

;; ----------------------------------------

(define-for-syntax (build-class-desc super options
                                     keywords super-keywords
                                     defaults super-defaults
                                     final?
                                     names)
  (with-syntax ([(name class:name constructor-maker-name name-defaults
                       fields)
                 names])
    (list
     #`(define-class-desc-syntax name
         (class-desc #,final?
                     (quote-syntax class:name)
                     (quote-syntax name)
                     (quote-syntax name)
                     (quote-syntax name)
                     fields
                     #,(cond
                         [(syntax-e #'constructor-maker-name)
                          #`(quote-syntax ([#,(encode-protocol keywords defaults) constructor-maker-name]
                                           #,@(if super
                                                  (or (class-desc-constructor-makers super)
                                                      (list (list (encode-protocol super-keywords super-defaults) #f)))
                                                  '())))]
                         [else #'#f])
                     #,(and (hash-ref options 'binding #f) #t)
                     #,(and (hash-ref options 'annotation #f) #t)
                     #,(and (syntax-e #'name-defaults)
                            #'(quote-syntax name-defaults)))))))

;; ----------------------------------------

(define-for-syntax (parse-options orig-stx forms)
  (syntax-parse forms
    #:context orig-stx
    [((_ clause-parsed) ...)
     (define clauses (syntax->list #'(clause-parsed ...)))
     (define (extract-rhs b)
       (syntax-parse b
         [(_::block g) #'g]
         [else
          (raise-syntax-error #f
                              "expected a single entry point in block body"
                              b)]))
     (let loop ([clauses clauses] [options #hasheq()])
       (cond
         [(null? clauses) options]
         [else
          (define clause (car clauses))
          (define new-options
            (syntax-parse clause
              #:literals (extends constructor final nonfinal authentic binding annotation)
              [(extends id)
               (when (hash-has-key? options 'extends)
                 (raise-syntax-error #f "redundant superclass clause" orig-stx clause))
               (hash-set options 'extends #'id)]
              [(internal id)
               (when (hash-has-key? options 'internal)
                 (raise-syntax-error #f "redundant internal-name clause" orig-stx clause))
               (hash-set options 'internal #'id)]
              [(constructor id block)
               (when (hash-has-key? options 'constructor-id)
                 (raise-syntax-error #f "redundant constructor clause" orig-stx clause))
               (hash-set (hash-set options 'constructor-id #'id)
                         'constructor-rhs
                         (extract-rhs #'block))]
              [(binding core-name block)
               (when (hash-has-key? options 'binding)
                 (raise-syntax-error #f "redundant binding clause" orig-stx clause))
               (hash-set options 'binding (list #'core-name (extract-rhs #'block)))]
              [(annotation core-name block)
               (when (hash-has-key? options 'annotation)
                 (raise-syntax-error #f "redundant annotation clause" orig-stx clause))
               (hash-set options 'annotation (list #'core-name (extract-rhs #'block)))]
              [(final)
               (when (hash-has-key? options 'final?)
                 (raise-syntax-error #f "redundant finality clause" orig-stx clause))
               (hash-set options 'final? #t)]
              [(nonfinal)
               (when (hash-has-key? options 'final?)
                 (raise-syntax-error #f "redundant finality clause" orig-stx clause))
               (hash-set options 'final? #f)]
              [(authentic)
               (when (hash-has-key? options 'authentic?)
                 (raise-syntax-error #f "redundant authenticity clause" orig-stx clause))
               (hash-set options 'authentic? #t)]
              [(field id rhs-id static-infos predicate annotation-str)
               (hash-set options 'fields (cons (list #'id
                                                     #'rhs-id
                                                     #'static-infos
                                                     #'predicate
                                                     #'annotation-str)
                                               (hash-ref options 'fields null)))]
              [_
               (raise-syntax-error #f "unrecognized clause" orig-stx clause)]))
          (loop (cdr clauses) new-options)]))]))

(define-for-syntax (wrap-class-clause parsed)
  #`[(quote-syntax (rhombus-class #,parsed) #:local)]) ; `quote-syntax` + `rhombus-class` wrapper => clause

(define-syntax extends
  (class-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ (~seq form ...))
        #:with (~var id (:hier-name-seq in-class-desc-space name-path-op name-root-ref)) #'(form ...)
        #:with () #'id.tail
        (wrap-class-clause #'(extends id.name))]))))

(define-syntax internal
  (class-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ name:identifier)
        (wrap-class-clause #'(internal name))]))))

(define-syntax constructor
  (class-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (group)
       [(_ (_::parens (group make:identifier))
           (~and (_::block . _)
                 constructor-block))
        (wrap-class-clause #`(constructor make constructor-block))]
       [(_ (~and (_::block . _)
                 constructor-block))
        (wrap-class-clause #`(constructor #f constructor-block))]))))

(define-syntax binding
  (class-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (group)
       [(_ (_::parens (group core:identifier))
           (~and (_::block . _)
                 binding-block))
        (wrap-class-clause #`(binding core binding-block))]
       [(_ (~and (_::block . _)
                 binding-block))
        (wrap-class-clause #`(binding #f binding-block))]))))

(define-syntax annotation
  (class-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (group)
       [(_ (_::parens (group core:identifier))
           (~and (_::block . _)
                 annotation-block))
        (wrap-class-clause #`(annotation core annotation-block))]
       [(_ (~and (_::block . _)
                 annotation-block))
        (wrap-class-clause #`(annotation #f annotation-block))]))))

(define-syntax nonfinal
  (class-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_) (wrap-class-clause #`(nonfinal))]))))

(define-syntax final
  (class-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_) (wrap-class-clause #`(final))]))))

(define-syntax authentic
  (class-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_) (wrap-class-clause #`(authentic))]))))

(define-syntax field
  (class-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       [(form-id bind ...
                 (~and blk (_::block . _)))
        (syntax-parse #'(bind ...)
          [(id:identifier (~optional c::inline-annotation))
           #:attr predicate (if (attribute c)
                                #'c.predicate
                                #'#f)
           #:attr annotation-str (if (attribute c)
                                     #'c.annotation-str
                                     #'#f)
           #:attr static-infos (if (attribute c)
                                   #'c.static-infos
                                   #'())
           #`[(define tmp-id (let ([f-info.name-id (rhombus-body-at . blk)])
                               {~? (if (c.predicate f-info.name-id)
                                       f-info.name-id
                                       (raise-binding-failure 'form-id "value" f-info.name-id 'c.annotation-str))
                                   f-info.name-id}))
              #,@(wrap-class-clause #`(field id
                                             tmp-id
                                             static-infos
                                             predicate
                                             annotation-str))]]
          [_
           (raise-syntax-error #f
                               "field identifier with optional annotation"
                               stx)])]
       [_
        (raise-syntax-error #f
                            "expected a field specification followed by a block"
                            stx)]))))

(define-syntax-rule (if/blocked tst thn els)
  (if tst (let () thn) els))
