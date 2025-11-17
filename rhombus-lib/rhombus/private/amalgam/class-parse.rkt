#lang racket/base
(require syntax/parse/pre
         "introducer.rkt"
         (for-template racket/unsafe/undefined
                       "parens.rkt"
                       "name-prefix.rkt")
         "dotted-sequence.rkt"
         "namespace-options-block.rkt")

(provide in-class-desc-space

         (struct-out objects-desc)
         (struct-out class-desc)
         (rename-out [class-desc-maker/cache class-desc-maker])
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
         (struct-out protect)

         unpack-intf-ref

         any-stx?

         :options-block
         class-reflect-name

         check-duplicate-field-names
         check-fields-methods-dots-distinct
         check-consistent-subclass
         check-consistent-construction
         check-consistent-unimmplemented
         check-field-defaults
         check-exports-distinct

         constructor-as-expression?

         extract-super-constructor-fields
         extract-super-internal-constructor-fields

         super-has-mutable-field?
         super-has-mutable-constructor-field?

         extract-has-mutable-constructor-arguments

         print-field-shapes

         make-accessor-names)

(define in-class-desc-space (make-interned-syntax-introducer/add 'rhombus/class))

;; common fields for `class`, `interface`, and `veneer` descriptions
(struct objects-desc
  (interface-ids ; syntax list of identifiers for implemented/extended interfaces
   method-shapes ; vector of shape; see below
   method-vtable ; syntax-object vector of function identifiers or #'#:abstract
   method-map    ; hash of symbol -> index; inverse of `method-vtable`, could be computed on demand
   method-result ; hash of symbol -> identifier-or-#f; identifier has compile-time binding to predicate and static infos
   dots          ; list of symbols for dot syntax
   dot-provider  ; #f or compile-time identifier
   static-infos  ; syntax object for additional instance static-infos
   flags))       ; 'call (=> public `call` is Callable), 'get, 'set, 'append, 'compare, 'contains; others specific to class/interface/veneer

(struct class-desc objects-desc
  ;; `flags` from `objects-desc` can include 'authentic, 'prefab, 'no-recon
  (final?
   id
   super-id
   protected-interface-ids ; needed for `.` resolution in some cases
   class:id
   instance-dot-providers ; `#%dot-provider` value, only for non-final
   ref-id
   fields                ; (list (list symbol accessor-id mutator-id static-infos constructor-arg) ...)
   all-fields            ; #f or (list a-field ...), includes private and protected fields; see below for a-field
   inherited-field-count ; number of fields that are inherited
   constructor-makers    ; (list constructor-maker ... maybe-default-constr-desc); see "About the constuctor protocol" in "class-constructor.rkt"
   custom-constructor-maybe-arity ; #f, #t, or arity
   custom-binding?
   custom-annotation?
   reconstructor-fields  ; #f or (list (cons field-sym accessor-id) ...)
   defaults-id           ; #f if no arguments with defaults
   call-method-id        ; #f or identifier as private `call` (for Callable) whose static info is relevant
   index-method-id       ; for `get`
   index-set-method-id   ; for `set`
   append-method-id      ; for `append`
   compare-method-id     ; for `compare`
   contains-method-id ; for `contains`
   indirect-call-method-id ; #f or identifier for `call`
   prefab-guard-id))

(struct class-desc-maker (proc))
(define (class-desc-maker/cache proc)
  (let ([desc #f])
    (class-desc-maker
     (lambda ()
       (or desc (begin
                  (set! desc (proc))
                  desc))))))

(define (class-desc-ref v) (or (and (class-desc? v) v)
                               (and (class-desc-maker? v)
                                    ((class-desc-maker-proc v)))))

(struct class-internal-desc (id                   ; identifier of non-internal class
                             private-methods      ; (list symbol ...)
                             private-properties   ; (list symbol ...)
                             private-interfaces)) ; (list identifier ...), includes protected interfaces

(define (class-internal-desc-ref v) (and (class-internal-desc? v) v))

;; A shaped is either
;;  - shaped-expose-symbol
;;  - (vector shaped-expose-symbol arity) where arity can be a list that includes keyword info

;; A shaped-expose-symbol is either
;;  - #s(protect shaped-symbol)
;;  - shaped-symbol
(struct protect (v) #:prefab)

;; A shaped-symbol is one of the following, where a symbol is the method's external name:
;;  - symbol: final method
;;  - #&symbol: method (can be overridden)
;;  - (symbol): final property
;;  - (#&symbol): property (can be overridden)

;; An a-field is one of the following:
;;  - symbol: public (so `class-desc-fields` describes constructor arg)
;;  - (protect field): protected; `field` content is like an element in `fields` of `class-desc`
;;  - (cons sym arg): private as internal constructor argument; sym is name, and see below for arg
;;  - (cons sym (vector arg)): like the previous case, but mutable
;;  - (cons sym identifier): private and not in constructor; sym is name, and calling identifier supplies default
;;  - (cons sym (vector identifier)): like the previous case, but *im*mutable

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

(struct added-field (id
                     arg-id
                     arg-default
                     arg-stx-params
                     form-id
                     static-infos
                     converter
                     annotation-str
                     annot-origins
                     exposure ; 'public, 'private, or 'protected
                     mutability))
(struct added-method (id rhs-id
                         rhs
                         stx-params
                         ret-forwards ; #f or #`([maybe-id ret] ...) to represent lifted result annotations
                         maybe-ret   ; #`[args ret-seq] for overall method
                         result-id
                         body        ; 'method, 'abstract
                         replace     ; 'method, 'override
                         disposition ; 'abstract, 'final, 'private
                         exposure    ; 'private, 'public, 'protected
                         kind        ; 'method, 'property
                         arity       ; #f, integer, or (list integer required-list allowed-list)
                         reflect-name)) ; #f or symbol

;; used for a table produced by `extract-method-tables`
(struct mindex (index final? protected? property? arity inherited? reflect-name))

;; When a private method or property overrides one from a privately
;; implemented interface, and when the enclosing class is not final,
;; then a subclass might privately (again) override the method or
;; property. Even though the subclass does not (in principle) know
;; that the superclass implements the method, it must override the
;; superclass's method. So, calling the private method via the
;; superclass needs to be dynamic through the interface's vtable. To
;; record this indirection, we package the relevant interface's
;; internal vtable accessor, an index into that vtable, and an
;; identifier for the method's result.
(define (unpack-intf-ref vec-stx)
  (define vec (syntax-e vec-stx))
  (define maybe-id (vector-ref vec 2))
  (values (vector-ref vec 0)
          (syntax-e (vector-ref vec 1))
          (and (syntax-e maybe-id) maybe-id)))

(define (any-stx? l) (for/or ([x (in-list l)]) (syntax-e x)))

(define (class-reflect-name name name-prefix bind-name)
   (or (and (syntax-e name) name)
       (add-name-prefix name-prefix bind-name)))

(define (check-duplicate-field-names stxes ids super interface-dotss)
  (define super-ids (if super
                        (append (map field-desc-name (class-desc-fields super))
                                (if (class-desc-all-fields super)
                                    (for/list ([a-field (in-list (class-desc-all-fields super))]
                                               #:when (protect? a-field))
                                      (car (protect-v a-field)))
                                    null))
                        null))
  (define dots-ht (let ([ht (if (not super)
                                #hasheq()
                                (for/fold ([ht #hasheq()]) ([dot-name (in-list (objects-desc-dots super))])
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
    (unless (memq 'prefab (objects-desc-flags super))
      (raise-syntax-error #f
                          "superclass must be prefab for a prefab subclass"
                          stxes
                          parent-name)))
  (unless (eq? (and (memq 'authentic (objects-desc-flags super)) #t)
               (hash-ref options 'authentic? #f))
    (raise-syntax-error #f
                        (if (hash-ref options 'authentic? #f)
                            "cannot create a non-authenic subclass of an authentic superclass"
                            "cannot create an authenic subclass of a non-authentic superclass")
                        stxes
                        parent-name)))

(define (check-consistent-construction stxes mutables exposures defaults name
                                       given-constructor-rhs given-constructor-name given-expression-macro-rhs)
  (when (for/or ([m (in-list mutables)]
                 [exposure (in-list exposures)]
                 [d (in-list defaults)])
          (and (not (syntax-e m))
               (not (eq? exposure 'public))
               (not (syntax-e d))))
    (unless (or given-constructor-rhs
                (disabled-constructor? given-expression-macro-rhs))
      (raise-syntax-error #f
                          (format (string-append "class needs a custom constructor to initialize ~a fields;"
                                                 "\n alternatively, constructor or expression macro must be disabled")
                                  (for/or ([exposure (in-list exposures)]
                                           #:when (not (eq? 'public exposure)))
                                    exposure))
                          stxes)))
  (when (and given-constructor-rhs
             given-expression-macro-rhs)
    (cond
      [(and (disabled-constructor? given-constructor-rhs)
            (disabled-constructor? given-expression-macro-rhs))
       (unless (eq? (syntax-e given-constructor-rhs)
                    (syntax-e given-expression-macro-rhs))
         (raise-syntax-error #f
                             "constructor has a different disable mode than expression macro"
                             stxes))]
      [(disabled-constructor? given-constructor-rhs)
       (raise-syntax-error #f
                           "expression macro is given, but unnamed constructor is disabled"
                           stxes)]
      [(not given-constructor-name)
       (raise-syntax-error #f
                           "unnamed constructor inaccessible due to expression macro"
                           stxes)]
      [(bound-identifier=? given-constructor-name name)
       (raise-syntax-error #f
                           "constructor name conflicts with expression macro"
                           stxes)])))

(define (check-consistent-unimmplemented stxes final? abstract-name name)
  (when (and final? abstract-name)
    (raise-syntax-error #f
                        "final class cannot have abstract methods"
                        stxes
                        abstract-name
                        (list name))))

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

;; Returns a table of method/field names that should not be added to a
;; namespace, because they are replaced by explciit exports
(define (check-exports-distinct stxes exports-stx fields method-mindex dots)
  (define dots-ht (for/hasheq ([dot (in-list dots)])
                    (values (syntax-e (car dot)) (car dot))))
  (define exports (for/list ([ex (in-list exports-stx)])
                    (syntax-parse ex
                      [(ext-id id) #'ext-id]
                      [_ ex])))
  (define ht (for/hasheq ([field (in-list fields)])
               (values (syntax-e field) #t)))
  (for/fold ([replaced-ht #hasheq()]) ([ex (in-list exports)])
    (define name (syntax-e ex))
    (define field-replaced-ht
      (cond
        [(hash-ref ht name #f)
         #;
         (raise-syntax-error #f
                             "exported name conflicts with field name"
                             ex)
         (hash-set replaced-ht name #t)]
        [else replaced-ht]))
    (define new-replaced-ht
      (let ([mix (hash-ref method-mindex name #f)])
        (cond
          [mix
           #;
           (raise-syntax-error #f
                               (format "exported name conflicts with ~a name"
                                       (if (mindex-property? mix) "property" "method"))
                               ex)
           (hash-set field-replaced-ht name #t)]
          [else field-replaced-ht])))
    (when (hash-ref ht dots-ht #f)
      (raise-syntax-error #f
                          "exported name conflicts with dot-syntax name"
                          ex))
    new-replaced-ht))

(define (disabled-constructor? given-constructor-or-expression-macro-rhs)
  (and given-constructor-or-expression-macro-rhs
       (memq (syntax-e given-constructor-or-expression-macro-rhs) '(#:none #:error))))

(define (constructor-as-expression? given-constructor-rhs)
  (disabled-constructor? given-constructor-rhs))

(define (field-to-field+keyword+default f arg)
  (values (field-desc-name f)
          (field-desc-accessor-id f)
          (field-desc-mutator-id f)
          (if (box? (syntax-e arg))
              (unbox (syntax-e arg))
              arg)
          (if (box? (syntax-e arg))
              #'unsafe-undefined
              #'#f)))

(define (extract-super-constructor-fields super)
  (for/lists (fs as ms ls ds) ([f (in-list (if super
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
         [(or (and (pair? (car all-fields))
                   ;; not in constructor?
                   (or (identifier? (cdar all-fields))
                       (and (vector? (cdar all-fields))
                            (identifier? (vector-ref (cdar all-fields) 0)))))
              (and (protect? (car all-fields))
                   ;; not in constructor?
                   (identifier? (field-desc-constructor-arg (protect-v (car all-fields))))))
          (loop (cdr all-fields) fields rev-fields rev-keywords rev-defaults)]
         [(or (pair? (car all-fields)) ; private field in internal constructor, only
              (protect? (car all-fields)))
          (define pr-protect (car all-fields))
          (define pr (if (protect? pr-protect) (protect-v pr-protect) pr-protect))
          (define f (car (generate-temporaries (list (car pr)))))
          (define arg/v (if (protect? pr-protect) (field-desc-constructor-arg pr) (cdr pr)))
          (define arg (if (vector? arg/v) (vector-ref arg/v 0) arg/v))
          (define k (datum->syntax #f (if (box? arg) (unbox arg) arg)))
          (define d (if (box? arg) #'unsafe-undefined #'#f))
          (loop (cdr all-fields) fields (cons f rev-fields) (cons k rev-keywords) (cons d rev-defaults))]
         [(and (pair? fields) (identifier? (field-desc-constructor-arg (car fields)))) ; not in constructor
          (loop (cdr all-fields) (cdr fields) rev-fields rev-keywords rev-defaults)]
         [else ; public field in constructor
          (define-values (f a m k d) (field-to-field+keyword+default (car fields) (field-desc-constructor-arg (car fields))))
          (loop (cdr all-fields) (cdr fields) (cons f rev-fields) (cons k rev-keywords) (cons d rev-defaults))]))]
    [else
     (values super-constructor-fields super-keywords super-defaults)]))

(define (super-has-mutable-field? super)
  (or (for/or ([fld (in-list (class-desc-fields super))])
        (and (field-desc-mutator-id fld)
             (syntax-e (field-desc-mutator-id fld))))
      (and (class-desc-all-fields super)
           (for/or ([af (in-list (class-desc-all-fields super))])
             (and (pair? af)
                  (if (vector? (cdr af))
                      (let ([v (vector-ref (cdr af) 0)])
                        (not (identifier? v)))
                      (identifier? (cdr af))))))))

(define (super-has-mutable-constructor-field? super)
  (for/or ([fld (in-list (class-desc-fields super))])
    (and (field-desc-mutator-id fld)
         (syntax-e (field-desc-mutator-id fld))
         (not (identifier? (field-desc-constructor-arg fld))))))


(define (extract-has-mutable-constructor-arguments constructor-field-mutables
                                                   constructor-field-exposures
                                                   super)
  (values
   ;; has-mutable-constructor-arg?
   (or (for/or ([mut (in-list (syntax->list constructor-field-mutables))]
                [ex (in-list (syntax->list constructor-field-exposures))])
         (and (syntax-e mut)
              (eq? 'public (syntax-e ex))))
       (and super
            (super-has-mutable-constructor-field? super)))
   ;; has-mutable-internal-constructor-arg?
   (for/or ([mut (in-list (syntax->list constructor-field-mutables))])
     (syntax-e mut))))

(define (print-field-shapes super fields keywords exposures)
  (append
   (if super
       (let ([shapes (for/list ([fld (in-list (class-desc-fields super))]
                                #:do [(define arg (field-desc-constructor-arg fld))])
                       (if (identifier? arg)
                           #f
                           (cond
                             [(keyword? (syntax-e arg))
                              (syntax-e arg)]
                             [(and (box? (syntax-e arg))
                                   (keyword? (syntax-e (unbox (syntax-e arg)))))
                              (syntax-e (unbox (syntax-e arg)))]
                             [else
                              (field-desc-name fld)])))])
         (define all-fields (class-desc-all-fields super))
         (if all-fields
             ;; insert `#f`s for private and protected fields
             (let loop ([all-fields all-fields] [shapes shapes])
               (cond
                 [(null? all-fields) null]
                 [(null? shapes)
                  ;; only private/protected fields left
                  (cons #f (loop (cdr all-fields) shapes))]
                 [(symbol? (car all-fields))
                  ;; public, maybe keyword
                  (cons (car shapes)
                        (loop (cdr all-fields) (cdr shapes)))]
                 [else
                  ;; private/protected
                  (cons #f
                        (loop (cdr all-fields) shapes))]))
             shapes))
       null)
   (let loop ([fields fields] [keywords keywords] [exposures exposures])
     (cond
       [(null? keywords) '()]
       [(not (eq? (car exposures) 'public))
        (cons #f (loop (cdr fields) (cdr keywords) (cdr exposures)))]
       [else
        (cons (or (syntax-e (car keywords))
                  (syntax-e (car fields)))
              (loop (cdr fields) (cdr keywords) (cdr exposures)))]))))

(define (make-accessor-names name field-ids intro)
  (for/list ([field-id (in-list field-ids)])
    (intro
     (datum->syntax field-id
                    (string->symbol (format "~a.~a"
                                            (syntax-e name)
                                            (syntax-e field-id)))
                    field-id))))
