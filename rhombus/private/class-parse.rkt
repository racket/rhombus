#lang racket/base
(require syntax/parse
         "tag.rkt"
         "introducer.rkt"
         (for-template
          (submod "annotation.rkt" for-class)
          "parens.rkt"
          "assign.rkt"
          (rename-in "equal.rkt"
                     [= rhombus=])
          "parse.rkt"))

(provide in-class-desc-space

         (struct-out class-desc)
         class-desc-ref

         field-desc-name
         field-desc-accessor-id
         field-desc-mutator-id
         field-desc-static-infos
         field-desc-constructor-arg

         (struct-out added-field)
         (struct-out added-method)
         
         any-stx?

         :field
         :options-block

         check-duplicate-field-names
         check-fields-methods-distinct
         check-consistent-subclass
         check-field-defaults)

(define in-class-desc-space (make-interned-syntax-introducer/add 'rhombus/class))

(struct class-desc (final?
                    id
                    super-id
                    class:id
                    fields ; (list (list id accessor-id mutator-id static-infos constructor-arg) ...)
                    all-fields ; #f or (list symbol-or-id ...), includes private fields
                    method-names  ; list of symbol or boxed symbol; plain symbol means final
                    method-vtable ; syntax-object vector of accessor identifiers
                    method-map    ; hash of name -> index or boxed index; the inverse of `method-names`
                    constructor-makers  ; (list constructor-maker ... maybe-default-constuctor-desc)
                    custom-binding?
                    custom-annotation?
                    defaults-id)) ; #f if no arguments with defaults
(define (class-desc-ref v) (and (class-desc? v) v))

;; quoted as a list in a `class-desc` construction
(define (field-desc-name f) (car f))
(define (field-desc-accessor-id f) (cadr f))
(define (field-desc-mutator-id f) (list-ref f 2))
(define (field-desc-static-infos f) (list-ref f 3))
(define (field-desc-constructor-arg f) (list-ref f 4)) ; syntax of #f (by-position), keyword, or identifier (not in constructor)

;; quoted as a list in a `class-desc` construction
(define (method-desc-name f) (car f))

(struct added-field (id arg-id static-infos predicate annotation-str mode))
(struct added-method (id rhs-id rhs mode))

(define (any-stx? l) (for/or ([x (in-list l)]) (syntax-e x)))

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

(define-splicing-syntax-class :options-block
  #:datum-literals (block group parens)
  (pattern (~seq)
           #:attr (form 1) '())
  (pattern (~seq (_::block form ...))))

(define (check-duplicate-field-names stxes ids super)
  (define super-ids (map field-desc-name (if super (class-desc-fields super) '())))
  (let ([ht (for/hasheq ([id (in-list super-ids)])
              (values id 'super))])
    (for/fold ([ht ht]) ([id (in-list ids)])
      (define prev (hash-ref ht (syntax-e id) #f))
      (when prev
        (raise-syntax-error #f
                            (if (eq? prev 'super)
                                "field name already exists in superclass"
                                "duplicate field name")
                            stxes
                            id))
      (hash-set ht (syntax-e id) id))))
  
(define (check-fields-methods-distinct stxes field-ht method-map method-names method-decls)
  (for ([k (in-hash-keys field-ht)])
    (define id-or-sym (or (hash-ref method-decls k #f)
                          (let ([i (hash-ref method-map k #f)])
                            (and i
                                 (hash-ref method-names (if (box? i) (unbox i) i))))))
    (when id-or-sym
      (define id (if (symbol? id-or-sym)
                     (hash-ref field-ht k #f)
                     id-or-sym))
      (raise-syntax-error #f
                          "identifier used as both a field name and method name"
                          stxes
                          id))))

(define (check-consistent-subclass super options stxes parent-name)
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
  (check-consistent-custom (class-desc-constructor-makers super) (hash-ref options 'constructor-id #f) "constructor")
  (check-consistent-custom (class-desc-custom-binding? super) (hash-ref options 'binding #f) "binding")
  (check-consistent-custom (class-desc-custom-annotation? super) (hash-ref options 'annotation #f) "annotation"))

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
      [else #f])))
