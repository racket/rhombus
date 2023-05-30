#lang racket/base
(require syntax/parse/pre
         "tag.rkt"
         "introducer.rkt"
         (for-template
          racket/unsafe/undefined
          (submod "annotation.rkt" for-class)
          "parens.rkt"
          "assign.rkt"
          (rename-in "equal.rkt"
                     [= rhombus=])
          "parse.rkt"))

(provide in-class-desc-space

         (struct-out class-desc)
         class-desc-ref

         (struct-out class-internal-desc)
         class-internal-desc-ref

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
         check-fields-methods-dots-distinct
         check-consistent-subclass
         check-consistent-construction
         check-consistent-unimmplemented
         check-field-defaults
         check-exports-distinct

         extract-super-constructor-fields
         extract-super-internal-constructor-fields

         print-field-shapes

         make-accessor-names)

(define in-class-desc-space (make-interned-syntax-introducer/add 'rhombus/class))

(struct class-desc (final?
                    id
                    super-id
                    interface-ids
                    class:id
                    ref-id
                    fields ; (list (list symbol accessor-id mutator-id static-infos constructor-arg) ...)
                    all-fields ; #f or (list a-field ...), includes private fields; see below for a-field
                    inherited-field-count ; number of fields that are inherited
                    method-shapes ; vector of shape; see below
                    method-vtable ; syntax-object vector of function identifiers or #'#:abstract
                    method-map    ; hash of symbol -> index; inverse of `method-names`, could be computed on demand
                    method-result ; hash of symbol -> identifier-or-#f; identifier has compile-time binding to predicate and static infos
                    constructor-makers  ; (list constructor-maker ... maybe-default-constuctor-desc)
                    custom-constructor?
                    custom-binding?
                    custom-annotation?
                    dots ; list of symbols for dot syntax
                    dot-provider  ; #f or compile-time identifier
                    defaults-id   ; #f if no arguments with defaults
                    static-infos  ; syntax object for additional instance static-infos
                    call-method-id ; #f or identifier as private `call` (for Callable) whose static info is relevant
                    index-method-id ; for `get`
                    index-set-method-id ; for `set`
                    append-method-id ; for `append`
                    prefab-guard-id
                    flags))       ; list with 'authentic, 'prefab, and/or 'call (=> public `call` is for Callable), 'get, 'set, 'append
(define (class-desc-ref v) (and (class-desc? v) v))

(struct class-internal-desc (id                   ; identifier of non-internal class
                             private-methods      ; (list symbol ...)
                             private-properties   ; (list symbol ...)
                             private-interfaces)) ; (list identifier ...)

(define (class-internal-desc-ref v) (and (class-internal-desc? v) v))

;; A shaped is either
;;  - shaped-symbol
;;  - (vector shaped-symbol arity) where arity can be a list that includes keyword info

;; A shaped-symbol is one of the following, where a symbol is the method's external name:
;;  - symbol: final method
;;  - #&symbol: method (can be overridden)
;;  - (symbol): final property
;;  - (#&symbol): property (can be overridden)

;; An a-field is one of the following:
;;  - symbol: public (so `class-desc-fields` describes constructor arg)
;;  - (cons sym arg): private as internal constructor argument; sym is name, and see below for arg
;;  - (cons sym (vector arg)): like the previous case, but mutable
;;  - (cons sym identifier): private and not in constructor; sym is name, and calling identifier supplies default

;; An arg is one of the following:
;;  - #f: by-position, required
;;  - &#f: by-position, optional
;;  - keyword: by-position, required
;;  - &keyword: by-position, optional
;;  - identifier: not in constructor, call identifier to get the default value

;; quoted as a list in a `class-desc` construction
(define (field-desc-name f) (car f))
(define (field-desc-accessor-id f) (cadr f))
(define (field-desc-mutator-id f) (list-ref f 2))
(define (field-desc-static-infos f) (list-ref f 3))
(define (field-desc-constructor-arg f) (list-ref f 4)) ; see above for arg

;; quoted as a list in a `class-desc` construction
(define (method-desc-name f) (car f))

(struct added-field (id arg-id arg-blk form-id static-infos converter annotation-str mode))
(struct added-method (id rhs-id rhs maybe-ret result-id
                         body        ; 'method, 'abstract
                         replace     ; 'method, 'override
                         disposition ; 'abstract, 'final, 'private
                         kind        ; 'method, 'property
                         arity))     ; #f, integer, or (list integer required-list allowed-list)

;; used for a table produced by `extract-method-tables`
(struct mindex (index final? property? arity inherited?))

(define (any-stx? l) (for/or ([x (in-list l)]) (syntax-e x)))

(define-splicing-syntax-class :options-block
  #:datum-literals (block group parens)
  (pattern (~seq)
           #:attr (form 1) '())
  (pattern (~seq (_::block form ...))))

(define (check-duplicate-field-names stxes ids super interface-dotss)
  (define super-ids (map field-desc-name (if super (class-desc-fields super) '())))
  (define dots-ht (let ([ht (if (not super)
                                #hasheq()
                                (for/fold ([ht #hasheq()]) ([dot-name (in-list (class-desc-dots super))])
                                  (hash-set ht dot-name "superclass")))])
                    (for*/fold ([ht ht]) ([dots (in-list interface-dotss)]
                                          [dot-name (in-list dots)])
                      (hash-set ht dot-name "interface"))))
  (let ([ht (for/hasheq ([id (in-list super-ids)])
              (values id 'super))])
    (for/fold ([ht ht]) ([id (in-list ids)])
      (cond
        [(hash-ref ht (syntax-e id) #f)
         => (lambda (prev)
              (raise-syntax-error #f
                                  (if (eq? prev 'super)
                                      "field name already exists in superclass"
                                      "duplicate field name")
                                  stxes
                                  id))]
        [(hash-ref dots-ht (syntax-e id) #f)
         => (lambda (what)
              (raise-syntax-error #f
                                  (format "field name already in ~a as dot syntax" what)
                                  stxes
                                  id))]
        [else
         (hash-set ht (syntax-e id) id)]))))
  
(define (check-fields-methods-dots-distinct stxes field-ht method-mindex method-names method-decls dots)
  (define dots-ht (for/hasheq ([dot (in-list dots)])
                    (values (syntax-e (car dot)) (car dot))))
  (define (check-method k what)
    (define id-or-sym (or (hash-ref method-decls k #f)
                          (let ([mix (hash-ref method-mindex k #f)])
                            (and mix
                                 (hash-ref method-names (mindex-index mix))))))
    (when id-or-sym
      (define id (if (symbol? id-or-sym)
                     (hash-ref field-ht k #f)
                     id-or-sym))
      (raise-syntax-error #f
                          (format "identifier used as both a ~a name and method name" what)
                          stxes
                          id)))
  (for ([k (in-hash-keys field-ht)])
    (check-method k "field"))
  (for ([(k id) (in-hash dots-ht)])
    (check-method k "dot-syntax")
    (when (hash-ref field-ht k #f)
      (raise-syntax-error #f
                          "identifier used as both a field name and dot-syntax name"
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
  (check-consistent-custom (class-desc-custom-annotation? super) (hash-ref options 'annotation-rhs #f) "annotation")
  (when (hash-ref options 'prefab? #f)
    (unless (memq 'prefab (class-desc-flags super))
      (raise-syntax-error #f
                          "superclass must be prefab for a prefab subclass"
                          stxes
                          parent-name)))
  (unless (eq? (and (memq 'authentic (class-desc-flags super)) #t)
               (hash-ref options 'authentic? #f))
    (raise-syntax-error #f
                        (if (hash-ref options 'authentic? #f)
                            "cannot create a non-authenic subclass of an authentic superclass"
                            "cannot create an authenic subclass of a non-authentic superclass")
                        stxes
                        parent-name)))

(define (check-consistent-construction stxes mutables private?s defaults options
                                       name given-constructor-rhs given-constructor-name expression-macro-rhs)
  (when (for/or ([m (in-list mutables)]
                 [p? (in-list private?s)]
                 [d (in-list defaults)])
          (and (not (syntax-e m))
               p?
               (not (syntax-e d))))
    (unless (hash-ref options 'constructor-rhs #f)
      (raise-syntax-error #f
                          "class needs a custom constructor to initialize private immutable fields"
                          stxes)))
  (when (and given-constructor-rhs
             expression-macro-rhs)
    (cond
      [(not given-constructor-name)
       (raise-syntax-error #f
                           "unnamed constructor inaccessible due to expression macro"
                           stxes)]
      [(bound-identifier=? given-constructor-name name)
       (raise-syntax-error #f
                           "constructor name conflicts with expression macro"
                           stxes)])))

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

(define (check-exports-distinct stxes exports-stx fields method-mindex dots)
  (define dots-ht (for/hasheq ([dot (in-list dots)])
                    (values (syntax-e (car dot)) (car dot))))
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
                            ex)))
    (when (hash-ref ht dots-ht #f)
      (raise-syntax-error #f
                          "exported name conflicts with dot-syntax name"
                          ex))))

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
         [(and (pair? (car all-fields)) (identifier? (cdar all-fields))) ; not in constructor
          (loop (cdr all-fields) fields rev-fields rev-keywords rev-defaults)]
         [(pair? (car all-fields)) ; private field in internal constructor, only
          (define f (car (generate-temporaries (list (caar all-fields)))))
          (define arg (cdar all-fields))
          (define k (datum->syntax #f (if (box? arg) (unbox arg) arg)))
          (define d (if (box? arg) #'(unsafe-undefined) #'#f))
          (loop (cdr all-fields) fields (cons f rev-fields) (cons k rev-keywords) (cons d rev-defaults))]
         [(and (pair? fields) (identifier? (field-desc-constructor-arg (car fields)))) ; not in constructor
          (loop (cdr all-fields) (cdr fields) rev-fields rev-keywords rev-defaults)]
         [else ; public field in constructor
          (define-values (f k d) (field-to-field+keyword+default (car fields) (field-desc-constructor-arg (car fields))))
          (loop (cdr all-fields) (cdr fields) (cons f rev-fields) (cons k rev-keywords) (cons d rev-defaults))]))]
    [else
     (values super-constructor-fields super-keywords super-defaults)]))

(define (print-field-shapes super fields keywords private?s)
  (append
   (if super
       (let ([shapes (for/list ([fld (in-list (class-desc-fields super))]
                                #:do [(define arg (field-desc-constructor-arg fld))])
                       (if (identifier? arg)
                           #f
                           (if (keyword? (syntax-e arg))
                               (syntax-e arg)
                               (field-desc-name fld))))])
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

(define (make-accessor-names name field-ids intro)
  (for/list ([field-id (in-list field-ids)])
    (intro
     (datum->syntax field-id
                    (string->symbol (format "~a.~a"
                                            (syntax-e name)
                                            (syntax-e field-id)))
                    field-id))))

