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
         (struct-out mindex)
         
         any-stx?

         :options-block

         check-duplicate-field-names
         check-fields-methods-distinct
         check-consistent-subclass
         check-consistent-construction
         check-consistent-unimmplemented
         check-field-defaults
         check-exports-distinct

         extract-super-constructor-fields
         extract-super-internal-constructor-fields

         print-field-shapes)

(define in-class-desc-space (make-interned-syntax-introducer/add 'rhombus/class))

(struct class-desc (final?
                    id
                    super-id
                    class:id
                    ref-id
                    fields ; (list (list id accessor-id mutator-id static-infos constructor-arg) ...)
                    all-fields ; #f or (list symbol-or-id-or-arg ...), includes private fields; arg means omitted from public constructor
                    method-shapes ; vector of shaped-symbol; see below
                    method-vtable ; syntax-object vector of function identifiers or #'#:abstract
                    method-map    ; hash of symbol -> index; inverse of `method-names`, could be computed on demand
                    method-result ; hash of symbol -> identifier-or-#f; identifier has compile-time binding to predicate and static infos
                    constructor-makers  ; (list constructor-maker ... maybe-default-constuctor-desc)
                    custom-binding?
                    custom-annotation?
                    defaults-id)) ; #f if no arguments with defaults
(define (class-desc-ref v) (and (class-desc? v) v))

;; A shaped-symbol is one of the following, where a symbol is the method's external name:
;;  - symbol: final method
;;  - #&symbol: method (can be overridden)
;;  - (symbol): final property
;;  - (#&symbol): property (can be overridden)

;; quoted as a list in a `class-desc` construction
(define (field-desc-name f) (car f))
(define (field-desc-accessor-id f) (cadr f))
(define (field-desc-mutator-id f) (list-ref f 2))
(define (field-desc-static-infos f) (list-ref f 3))
(define (field-desc-constructor-arg f) (list-ref f 4)) ; syntax of #f (by-position), keyword, or identifier
;;                                                       where identifier =>  not in constructor,
;;                                                       but also not private from body

;; quoted as a list in a `class-desc` construction
(define (method-desc-name f) (car f))

(struct added-field (id arg-id static-infos predicate annotation-str mode))
(struct added-method (id rhs-id rhs maybe-ret result-id
                         mode        ; 'method, 'override, or 'abstract
                         disposition ; 'abstract, 'final, 'private
                         kind))      ; 'method, 'property

;; used for a table produced by `extract-method-tables`
(struct mindex (index final? property?))

(define (any-stx? l) (for/or ([x (in-list l)]) (syntax-e x)))

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
  
(define (check-fields-methods-distinct stxes field-ht method-mindex method-names method-decls)
  (for ([k (in-hash-keys field-ht)])
    (define id-or-sym (or (hash-ref method-decls k #f)
                          (let ([mix (hash-ref method-mindex k #f)])
                            (and mix
                                 (hash-ref method-names (mindex-index mix))))))
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
  (check-consistent-custom (class-desc-custom-binding? super) (hash-ref options 'binding-rhs #f) "binding")
  (check-consistent-custom (class-desc-custom-annotation? super) (hash-ref options 'annotation-rhs #f) "annotation"))

(define (check-consistent-construction stxes mutables private?s defaults options)
  (when (for/or ([m (in-list mutables)]
                 [p? (in-list private?s)]
                 [d (in-list defaults)])
          (and (not (syntax-e m))
               p?
               (not (syntax-e d))))
    (unless (hash-ref options 'constructor-rhs #f)
      (raise-syntax-error #f
                          "class needs a custom constructor to initialize private immutable fields"
                          stxes))))

(define (check-consistent-unimmplemented stxes final? abstract-name)
  (when (and final? abstract-name)
    (raise-syntax-error #f
                        "final class cannot have abstract methods"
                        stxes
                        abstract-name)))

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

(define (check-exports-distinct stxes exports-stx fields method-mindex)
  (define exports (for/list ([ex (in-list exports-stx)])
                    (syntax-parse ex
                      [(id ext-id) #'ext-id]
                      [_ ex])))
  (define ht (for/hasheq ([field (in-list fields)])
               (values (syntax-e field) #t)))
  (for ([ex (in-list exports)])
    (define name (syntax-e ex))
    (when (hash-ref ht name #f)
      (raise-syntax-error #f
                          "exported name conflicts with field name"
                          ex))
    (let ([mix (hash-ref method-mindex name #f)])
      (when mix
        (raise-syntax-error #f
                            (format "exported name conflicts with ~a name"
                                    (if (mindex-property? mix) "property" "method"))
                            ex)))))

(define (field-to-field+keyword+default f arg)
  (values (field-desc-name f)
          (if (box? (syntax-e arg))
              (unbox (syntax-e arg))
              arg)
          (if (box? (syntax-e arg))
              #'(unsafe-undefined)
              #'#f)))

(define (extract-super-constructor-fields super)
  (for/lists (fs ls ds) ([f (in-list (if super
                                         (class-desc-fields super)
                                         '()))]
                         #:do [(define arg (field-desc-constructor-arg f))]
                         #:unless (identifier? arg))
    (field-to-field+keyword+default f arg)))

(define (extract-super-internal-constructor-fields super super-constructor-fields super-keywords super-defaults)
  (cond
    [(and super (class-desc-all-fields super))
     (let loop ([all-fields (class-desc-all-fields super)]
                [fields (class-desc-fields super)]
                [rev-fields '()]
                [rev-keywords '()]
                [rev-defaults '()])
       (cond
         [(null? all-fields) (values (reverse rev-fields) (reverse rev-keywords) (reverse rev-defaults))]
         [(identifier? (car all-fields)) ; not in constructor
          (loop (cdr all-fields) fields rev-fields rev-keywords rev-defaults)]
         [(and (pair? fields) (identifier? (field-desc-constructor-arg (car fields)))) ; not in constructor
          (loop all-fields (cdr fields) rev-fields rev-keywords rev-defaults)]
         [(symbol? (car all-fields)) ; public field in constructor
          (define-values (f k d) (field-to-field+keyword+default (car fields) (field-desc-constructor-arg (car fields))))
          (loop (cdr all-fields) (cdr fields) (cons f rev-fields) (cons k rev-keywords) (cons d rev-defaults))]
         [else ; private field in internal constructor, only
          (define f (car (generate-temporaries '(field))))
          (define k (datum->syntax #f (if (box? (car all-fields)) (unbox (car all-fields)) (car all-fields))))
          (define d (if (box? (car all-fields)) #'(unsafe-undefined) #'#f))
          (loop (cdr all-fields) fields (cons f rev-fields) (cons k rev-keywords) (cons d rev-defaults))]))]
    [else
     (values super-constructor-fields super-keywords super-defaults)]))

(define (print-field-shapes super fields keywords private?s)
  (append
   (if super
       (let ([shapes (for/list ([fld (in-list (class-desc-fields super))]
                                #:do [(define arg (field-desc-constructor-arg fld))]
                                #:when (not (identifier? arg)))
                       (if (keyword? (syntax-e arg))
                           (syntax-e arg)
                           (field-desc-name fld)))])
         (define all-fields (class-desc-all-fields super))
         (if all-fields
             ;; insert `#f`s for private fields
             (let loop ([all-fields all-fields] [shapes shapes])
               (cond
                 [(null? all-fields) null]
                 [(null? shapes)
                  ;; only private fields left
                  (cons #f (loop (cdr all-fields) shapes))]
                 [(symbol? (car all-fields))
                  ;; public, maybe keyword
                  (cons (car shapes)
                        (loop (cdr all-fields) (cdr shapes)))]
                 [else
                  ;; private
                  (cons #f
                        (loop (cdr all-fields) shapes))]))
             shapes))
       null)
   (let loop ([fields fields] [keywords keywords] [private?s private?s])
     (cond
       [(null? keywords) '()]
       [(car private?s) (cons #f (loop (cdr fields) (cdr keywords) (cdr private?s)))]
       [else
        (cons (or (syntax-e (car keywords))
                  (syntax-e (car fields)))
              (loop (cdr fields) (cdr keywords) (cdr private?s)))]))))
