#lang racket/base
(require (for-syntax racket/base
                     racket/symbol
                     syntax/parse/pre
                     enforest/syntax-local
                     enforest/deprecated
                     "srcloc.rkt"
                     "class-parse.rkt")
         "name-root.rkt"
         "expression.rkt"
         "binding.rkt"
         (submod "annotation.rkt" for-class)
         (submod "dot.rkt" for-dot-provider)
         "static-info.rkt"
         "dot-parse.rkt"
         "function-arity-key.rkt"
         "call-result-key.rkt"
         "dot-provider-key.rkt"
         "indirect-static-info-key.rkt"
         "composite.rkt"
         "class-desc.rkt"
         "define-arity.rkt"
         (submod "define-arity.rkt" for-info)
         "rhombus-primitive.rkt")

(provide define-primitive-class)

(define-syntax (define-primitive-class stx)
  (syntax-parse stx
    [(_ Name:id name:id (~optional name/rkt:id
                                   #:defaults ([name/rkt #'name]))
        (~optional (~and #:lift-declaration
                         (~bind [lift-declaration? #t])))
        (~optional (~or* (~and #:no-constructor-static-info
                               (~bind [constructor-static-infos #f]))
                         (~seq #:constructor-static-info constructor-static-infos))
                   #:defaults ([constructor-static-infos #'()]))
        (~optional (~seq #:constructor-arity constructor-arity))
        (~optional (~seq #:instance-static-info instance-static-infos)
                   #:defaults ([instance-static-infos #'()]))
        (~and creation (~or* #:new #:existing))
        (~optional (~and actual-class (~or* #:class)))
        (~and mode (~or* #:transparent #:translucent
                         #:just-annot #:just-constructor+binding #:just-constructor
                         #:opaque))
        (~optional (~and no-primitive #:no-primitive))
        (~optional (~seq #:parent Parent parent)
                   #:defaults ([parent #'#f]
                               [Parent #'#f]))
        #:fields
        ;; only for binding pattern in translucent mode
        ((~or* (~and [(~or* ((~and field _field)) (field _field)) (~optional field-static-infos)]
                     (~parse Name.field
                             (datum->syntax
                              #'Name
                              (string->symbol (format "~a.~a" (syntax-e #'Name) (syntax-e #'field)))))
                     (~parse name-field
                             (datum->syntax
                              #'name/rkt
                              (string->symbol (format "~a-~a" (syntax-e #'name/rkt) (syntax-e #'_field)))))
                     (~parse Name.field-def
                             #'(define/arity (Name.field obj)
                                 (~? (~@ #:static-infos ((#%call-result field-static-infos))))
                                 (name-field obj))))
               (~and [field Name.field (~optional field-static-infos)]
                     (~bind [Name.field-def #f])))
         ...)
        (~optional (~seq #:namespace-fields
                         (~or* (ns-field ... (~and no-methods #:no-methods))
                               (ns-field ...)))
                   #:defaults ([(ns-field 1) '()]))
        #:properties
        ([property property-accessor
                   (~optional (~seq #:mutator property-mutator))
                   (~optional property-extract
                              #:defaults ([property-extract #'extract-empty-statinfo]))]
         ...)
        #:methods
        ((~or* (~and [method mask name-method-proc:identifier method-proc:identifier]
                     (~parse method-dispatch
                             #'(nary 'mask (quote-syntax name-method-proc) (quote-syntax method-proc)))
                     (~parse method-depr #'()))
               (~and (~or* (~and [method name-method-proc #:deprecate method-depr-date]
                                 (~parse method-depr #'(#:deprecate (#f rhombus/statinfo) method-depr-date)))
                           (~and [method name-method-proc]
                                 (~parse method-depr #'()))
                           (~and method
                                 (~parse method-depr #'())
                                 (~parse name-method-proc
                                         (datum->syntax
                                          #'method
                                          (string->symbol (format "~a.~a" (syntax-e #'Name) (syntax-e #'method)))))))
                     (~parse method-proc
                             (datum->syntax
                              #'method
                              (string->symbol (format "~a/method" (syntax-e #'name-method-proc)))))
                     (~parse method-dispatch
                             #`(#,(datum->syntax
                                   #'method
                                   (string->symbol (format "~a/dispatch" (syntax-e #'name-method-proc))))
                                nary))))
         ...)
        (~optional (~seq #:dot-methods
                         ((~and [dot-method name-dot-method-proc]
                                (~parse dot-method-proc
                                        (datum->syntax
                                         #'dot-method
                                         (string->symbol (format "~a/method" (syntax-e #'name-dot-method-proc)))))
                                (~parse dot-method-dispatch
                                        #`(#,(datum->syntax
                                              #'dot-method
                                              (string->symbol (format "~a/dispatch" (syntax-e #'name-dot-method-proc))))
                                           nary)))
                          ...))
                   #:defaults ([(dot-method 1) null]
                               [(name-dot-method-proc 1) null]
                               [(dot-method-proc 1) null]
                               [(dot-method-dispatch 1) null]))
        )
     #:do [(define transparent? (eq? '#:transparent (syntax-e #'mode)))
           (define translucent? (eq? '#:translucent (syntax-e #'mode)))
           (define just-annot? (eq? '#:just-annot (syntax-e #'mode)))
           (define just-constructor+binding? (eq? '#:just-constructor+binding (syntax-e #'mode)))
           (define just-constructor? (eq? '#:just-constructor (syntax-e #'mode)))
           (define new? (eq? '#:new (syntax-e #'creation)))]
     #:with name? (datum->syntax #'name/rkt (string->symbol (format "~a?" (syntax-e #'name/rkt))))
     #:with (~var struct:name) (datum->syntax #'name/rkt (string->symbol (format "struct:~a" (syntax-e #'name/rkt))))
     #:with ([prop prop-accessor (~optional prop-mutator) prop-extract] ...)
     #`(#,@(if transparent?
               #`([field Name.field (~? (lambda (lhs-si)
                                          #`field-static-infos)
                                        extract-empty-statinfo)]
                  ...)
               '())
        [property property-accessor (~? property-mutator) property-extract] ...)
     #:with name-dot-dispatch (datum->syntax #'name (string->symbol
                                                     (format "~a-dot-dispatch" (syntax-e #'name))))
     #:attr parent-dot-dispatch (and (syntax-e #'parent)
                                     (datum->syntax #'parent (string->symbol
                                                              (format "~a-dot-dispatch" (syntax-e #'parent)))))
     #:attr get-parent-instances (and (syntax-e #'parent)
                                      (datum->syntax #'parent (string->symbol
                                                               (format "get-~a-instances" (syntax-e #'parent)))))
     #:with name-method-table (datum->syntax #'name (string->symbol (format "~a-method-table" (syntax-e #'name))))
     #:attr parent-method-table (and (syntax-e #'parent)
                                     (datum->syntax #'parent (string->symbol (format "~a-method-table" (syntax-e #'parent)))))
     #:with name-mutator-table (datum->syntax #'name (string->symbol (format "~a-mutator-method-table" (syntax-e #'name))))
     #:attr parent-mutator-table (and (syntax-e #'parent)
                                      (datum->syntax #'parent (string->symbol (format "~a-mutator-method-table" (syntax-e #'parent)))))
     #:with get-name-field-list (datum->syntax #'name (string->symbol (format "get-~a-field-list" (syntax-e #'name))))
     #:with ((parent-Name.field parent-field-static-infos) ...)
     (if (syntax-e #'parent)
         ((syntax-local-value
           (datum->syntax #'parent (string->symbol (format "get-~a-field-list" (syntax-e #'parent))))))
         null)
     #:with name-static-infos (datum->syntax #'name (string->symbol (format "~a-static-infos" (syntax-e #'name))))
     #:with get-name-static-infos (datum->syntax #'name (string->symbol (format "get-~a-static-infos" (syntax-e #'name))))
     #:with Name-str (datum->syntax #'here (symbol->immutable-string (syntax-e #'Name)))
     #:with name-instance (datum->syntax #'here (string->symbol (format "~a-instance" (syntax-e #'name))))
     #:with get-name-instances (datum->syntax #'name (string->symbol (format "get-~a-instances" (syntax-e #'name))))
     #:with field-list #'((parent-Name.field parent-field-static-infos)
                          ...
                          (Name.field (~? field-static-infos ()))
                          ...)
     #:do [(define super (and (syntax-e #'Parent)
                              (syntax-local-value* (in-class-desc-space #'Parent) class-desc-ref)))
           (define inherited-field-count (if super
                                             (+ (class-desc-inherited-field-count super)
                                                (length (class-desc-fields super)))
                                             0))]
     #:with ((super-field-name
              super-name-field
              super-name-field-set!
              super-field-static-infos
              super-field-constructor)
             ...) (if super
                      (class-desc-fields super)
                      null)
     #:with (method-dispatch/maybe-depr ...) (for/list ([method (syntax->list #'(method ...))]
                                                        [method-dispatch (syntax->list #'(method-dispatch ...))]
                                                        [method-depr (syntax->list #'(method-depr ...))])
                                               (syntax-parse method-depr
                                                 [(#:deprecate spaces date)
                                                  (define name-method
                                                    (string->symbol (format "~a.~a" (syntax-e #'Name) (syntax-e method))))
                                                  #`(deprecated-nary '#,name-method date . #,method-dispatch)]
                                                 [_ method-dispatch]))

     (define declaration
       (let ([mutator-pairs #'((~? (~@ 'prop prop-mutator)) ...)])
         (if new?
             #`(struct name/rkt (field ...)
                 #:property prop:field-name->accessor
                 (list* '()
                        (hasheq (~@ 'prop prop-accessor)
                                ...
                                (~@ 'method method-proc)
                                ...
                                (~@ 'dot-method dot-method-proc)
                                ...)
                        #hasheq())
                 #,@(if (null? (syntax-e mutator-pairs))
                        '()
                        #`(#:property prop:field-name->mutator
                           (list* '()
                                  (hasheq . #,mutator-pairs)))))
             #`(begin
                 (define name-method-table
                   (hash-add* (~? parent-method-table '#hasheq())
                              (~@ 'prop prop-accessor)
                              ...
                              (~@ 'method method-proc)
                              ...
                              (~@ 'dot-method dot-method-proc)
                              ...))
                 #,@(if (null? (syntax-e mutator-pairs))
                        '()
                        (list #`(define name-mutator-table
                                  (hash-add* (~? parent-mutator-table '#hasheq())
                                             . #,mutator-pairs))))))))

     #`(begin
         ;; must be before the creation of method table
         (~? Name.field-def) ...

         #,@(if (attribute lift-declaration?)
                (begin
                  (syntax-local-lift-module-end-declaration declaration)
                  '())
                (list declaration))

         (define-for-syntax (get-name-instances)
           (~? (add-dot-providers (quote-syntax name-instance)
                                  (get-parent-instances))
               (quote-syntax name-instance)))

         (define-static-info-syntax name-static-infos
           #,#'(#%dot-provider #,(get-name-instances))
           . instance-static-infos)

         (define-static-info-getter get-name-static-infos
           (#%indirect-static-info name-static-infos))

         #,@(if (attribute constructor-static-infos)
                (with-syntax ([arity-mask
                               (or (attribute constructor-arity)
                                   (arithmetic-shift 1 (length (syntax->list #'field-list))))]
                              [(si ...)
                               #'constructor-static-infos])
                  (list #'(define-static-info-syntax name/rkt
                            si ...
                            (#%call-result #,(get-name-static-infos))
                            (#%function-arity arity-mask)
                            . #,(indirect-get-function-static-infos))))
                '())

         #,@(if (and (not new?)
                     (not (attribute no-primitive)))
                (list
                 #'(void (set-primitive-contract! 'name? Name-str)))
                '())
         #,@(if (or transparent? translucent?
                    just-annot?)
                (list
                 #'(define-annotation-syntax Name
                     (identifier-annotation name? #,(get-name-static-infos))))
                '())
         #,@(if (or transparent? translucent?
                    just-constructor+binding?)
                (list
                 #`(define-binding-syntax Name
                     (binding-transformer
                      (lambda (tail)
                        (composite-binding-transformer tail
                                                       Name-str
                                                       (quote-syntax name?)
                                                       (list (quote-syntax parent-Name.field)
                                                             ...
                                                             (quote-syntax Name.field)
                                                             ...)
                                                       (list (quote-syntax parent-field-static-infos)
                                                             ...
                                                             (~? #`field-static-infos #'())
                                                             ...)
                                                       #:static-infos (get-name-static-infos))))))
                '())
         #,@(if (or transparent? translucent?
                    just-constructor+binding? just-constructor?)
                (list
                 #`(define-syntax Name
                     (expression-repeatable-transformer
                      (lambda (stx)
                        (syntax-parse stx
                          [(head . tail)
                           (values (relocate-id #'head (quote-syntax name/rkt)) #'tail)])))))
                '())

         (define-name-root Name
           #:fields
           #,(if (attribute no-methods)
                 #'(ns-field ...)
                 #'(ns-field ... [prop prop-accessor] ... [method name-method-proc . method-depr] ...)))

         (define-syntax (get-name-field-list) #`field-list)

         #,@(if (attribute actual-class)
                #`((define-class-desc-syntax Name
                     (class-desc-maker
                      (lambda ()
                        (class-desc null
                                    #() ; no methods
                                    (quote-syntax #()) ; no methods
                                    '#hasheq() ; empty method map
                                    '#hasheq() ; empty method results
                                    null ; no dot syntax
                                    #f   ; no dot-provider
                                    #'() ; no additional instance static-infos
                                    null ; no flags
                                    ;; ----------------------------------------
                                    #f ; not final
                                    (quote-syntax Name)
                                    #,(and (syntax-e #'Parent) #'(quote-syntax Parent))
                                    #f
                                    (quote-syntax struct:name)
                                    (get-name-instances)
                                    #f ; `ref-id` would only used by the normal class dot provider
                                    (list (list 'super-field-name
                                                (quote-syntax super-name-field)
                                                #f
                                                (quote-syntax super-field-static-infos)
                                                (quote-syntax super-field-constructor))
                                          ...
                                          (list 'field
                                                (quote-syntax Name.field)
                                                #f
                                                (~? #`field-static-infos #'())
                                                (quote-syntax #f))
                                          ...)
                                    #f ; no private fields
                                    #,inherited-field-count
                                    #f ; constructor-makers
                                    #f ; not custom constructor
                                    #f ; not custom binding
                                    #f ; not custom annotation
                                    #f ; no functional-update specialization
                                    #f ; no arguments with defaults
                                    #f ; not callable
                                    #f ; not indexable
                                    #f ; not mutable indexable
                                    #f ; not appendable
                                    #f ; not comparable
                                    #f ; not container
                                    #f ; not callable (again)
                                    #f ; not prefab
                                    )))))
                null)

         (define-for-syntax name-dot-dispatch
           (lambda (field-sym field-proc ary nary repetition? fail-k)
             (case field-sym
               [(prop)
                (cond
                  [repetition?
                   ;; let dot-provider dispatcher handle repetition construction:
                   (fail-k)]
                  [else
                   (field-proc (lambda (lhs lhs-si reloc)
                                 (wrap-static-info*
                                  (reloc #`(prop-accessor #,lhs))
                                  (prop-extract lhs-si)))
                               (~? (lambda (lhs rhs reloc)
                                     (reloc #`(prop-mutator #,lhs #,rhs)))))])]
               ...
               [(method) method-dispatch/maybe-depr]
               ...
               [(dot-method) dot-method-dispatch]
               ...
               [else (~? (parent-dot-dispatch field-sym field-proc ary nary repetition? fail-k)
                         (fail-k))])))

         (define-syntax name-instance
           (dot-provider (dot-parse-dispatch name-dot-dispatch)))
         )]))

(define-for-syntax (extract-empty-statinfo lhs-si)
  #'())

(define-for-syntax (add-dot-providers dot-provider dot-providers)
  (cons dot-provider
        (if (identifier? dot-providers)
            (list dot-providers)
            dot-providers)))

(define (hash-add* ht . kvs)
  (let loop ([ht ht] [kvs kvs])
    (if (null? kvs)
        ht
        (loop (hash-set ht (car kvs) (cadr kvs))
              (cddr kvs)))))

(define-for-syntax (deprecated-nary name date-str dispatch nary)
  (warn-deprecated! name date-str)
  (dispatch nary))
