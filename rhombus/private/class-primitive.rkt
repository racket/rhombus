#lang racket/base
(require (for-syntax racket/base
                     racket/symbol
                     syntax/parse/pre
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
         "composite.rkt"
         "class-desc.rkt"
         "define-arity.rkt"
         (submod "define-arity.rkt" for-info)
         "indirect-static-info-key.rkt"
         "rhombus-primitive.rkt")

(provide define-primitive-class)

(define-syntax (define-primitive-class stx)
  (syntax-parse stx
    [(_ Name name
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
        (~and mode (~or* #:transparent #:opaque #:translucent))
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
                              #'name
                              (string->symbol (format "~a-~a" (syntax-e #'name) (syntax-e #'_field)))))
                     (~parse Name.field-def
                             #'(define/arity (Name.field obj)
                                 #:inline
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
        ([property property-proc
                   (~optional (~seq #:mutator property-mutator))
                   (~optional property-si #:defaults ([property-si #'#f]))]
         ...)
        #:methods
        ((~or* (~and [method mask name-method-proc method-proc]
                     (~parse method-dispatch
                             #'(nary 'mask (quote-syntax name-method-proc) (quote-syntax method-proc))))
               (~and (~or* (~and [method name-method-proc])
                           (~and method
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
        )
     #:do [(define transparent? (eq? '#:transparent (syntax-e #'mode)))
           (define translucent? (eq? '#:translucent (syntax-e #'mode)))]
     #:with name? (datum->syntax #'name (string->symbol (format "~a?" (syntax-e #'name))))
     #:with struct_name (datum->syntax #'name (string->symbol (format "struct:~a" (syntax-e #'name))))
     #:with ([prop prop-proc (~optional prop-mutator) prop-static-infos] ...)
     #`(#,@(if transparent?
               #`([field Name.field (~? #`field-static-infos #f)] ...)
               '())
        [property property-proc (~? property-mutator) property-si] ...)
     #:with name-dot-dispatch (datum->syntax #'name (string->symbol
                                                     (format "~a-dot-dispatch" (syntax-e #'name))))
     #:with parent-dot-dispatch (and (syntax-e #'parent)
                                     (datum->syntax #'parent (string->symbol
                                                              (format "~a-dot-dispatch" (syntax-e #'parent)))))
     #:with name-method-table (datum->syntax #'name (string->symbol (format "~a-method-table" (syntax-e #'name))))
     #:with parent-method-table (datum->syntax #'parent (string->symbol (format "~a-method-table" (syntax-e #'parent))))
     #:with name-mutator-table (datum->syntax #'name (string->symbol (format "~a-mutator-method-table" (syntax-e #'name))))
     #:with parent-mutator-table (datum->syntax #'parent (string->symbol (format "~a-mutator-method-table" (syntax-e #'parent))))
     #:with name-field-list (datum->syntax #'name (string->symbol (format "~a-field-list" (syntax-e #'name))))
     #:with ((parent-Name.field parent-field-static-infos) ...)
     (if (syntax-e #'parent)
         (syntax-local-value
          (datum->syntax #'parent (string->symbol (format "~a-field-list" (syntax-e #'parent)))))
         null)
     #:with name-static-infos (datum->syntax #'name (string->symbol (format "~a-static-infos" (syntax-e #'name))))
     #:with Name-str (datum->syntax #'here (symbol->immutable-string (syntax-e #'Name)))
     #:with name-instance (datum->syntax #'here (string->symbol (format "~a-instance" (syntax-e #'name))))
     #:with field-list #'((parent-Name.field parent-field-static-infos)
                          ...
                          (Name.field (~? field-static-infos ()))
                          ...)
     #:do [(define super (and (syntax-e #'Parent)
                              (syntax-local-value (in-class-desc-space #'Parent))))
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

     (define declaration
       (let ([mutator-pairs #'((~? (~@ 'prop prop-mutator)) ...)])
         (if (eq? (syntax-e #'creation) '#:new)
             #`(struct name (field ...)
                 #:property prop:field-name->accessor
                 (list* '()
                        (hasheq (~@ 'prop prop-proc)
                                ...
                                (~@ 'method method-proc)
                                ...)
                        #hasheq())
                 #,@(if (null? (syntax-e mutator-pairs))
                        '()
                        #`(#:property prop:field-name->mutator
                           (list* '()
                                  (hasheq . #,mutator-pairs)))))
             #`(begin
                 (define name-method-table
                   (hash-add* #,(if (syntax-e #'parent)
                                    #'parent-method-table
                                    #'#hasheq())
                              (~@ 'prop prop-proc)
                              ...
                              (~@ 'method method-proc)
                              ...))
                 #,@(if (null? (syntax-e mutator-pairs))
                        '()
                        (list #`(define name-mutator-table
                                  (hash-add* #,(if (syntax-e #'parent)
                                                   #'parent-mutator-table
                                                   #'#hasheq())
                                             . #,mutator-pairs))))))))

     #`(begin
         ;; must be before the creation of method table
         (~? Name.field-def) ...

         #,@(if (attribute lift-declaration?)
                (begin
                  (syntax-local-lift-module-end-declaration declaration)
                  '())
                (list declaration))

         (define-for-syntax name-static-infos
           #`((#%dot-provider name-instance)
              . instance-static-infos))

         #,@(if (attribute constructor-static-infos)
                (with-syntax ([arity-mask
                               (or (attribute constructor-arity)
                                   (arithmetic-shift 1 (length (syntax->list #'field-list))))]
                              [(si ...)
                               #'constructor-static-infos])
                  (list #'(define-static-info-syntax name
                            si ...
                            (#%call-result #,name-static-infos)
                            (#%function-arity #,arity-mask)
                            (#%indirect-static-info indirect-function-static-info))))
                '())

         #,@(if (or transparent? translucent?)
                (list
                 #'(set-primitive-contract! 'name? Name-str)
                 #'(define-annotation-syntax Name
                     (identifier-annotation #'name? name-static-infos))
                 #`(define-syntax Name
                     (expression-transformer
                      (lambda (stx)
                        (syntax-parse stx
                          [(head . tail)
                           (values (relocate-id #'head #'name) #'tail)]))))
                 #`(define-binding-syntax Name
                     (binding-transformer
                      (make-composite-binding-transformer Name-str
                                                          #'name?
                                                          (list (quote-syntax parent-Name.field)
                                                                ...
                                                                (quote-syntax Name.field)
                                                                ...)
                                                          (list (quote-syntax parent-field-static-infos)
                                                                ...
                                                                (~? #`field-static-infos #'())
                                                                ...)
                                                          #:static-infos name-static-infos))))
                '())

         (define-name-root Name
           #:fields
           #,(if (attribute no-methods)
                 #'(ns-field ...)
                 #'(ns-field ... [prop prop-proc] ... [method name-method-proc] ...)))

         (define-syntax name-field-list #`field-list)

         #,@(if (attribute actual-class)
                #`((define-class-desc-syntax Name
                     (class-desc #f ; not final
                                 (quote-syntax Name)
                                 #,(and (syntax-e #'Parent) #'(quote-syntax Parent))
                                 null
                                 (quote-syntax struct_name)
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
                                 #() ; no methods
                                 (quote-syntax #()) ; no methods
                                 '#hasheq() ; empty method map
                                 '#hasheq() ; empty method results
                                 #f ; constructor-makers
                                 #f ; not custom constructor
                                 #f ; not custom binding
                                 #f ; not custom annotation
                                 #f ; no functional-update specialization
                                 null ; no dot syntax
                                 #f ; dot-provider
                                 #f ; no arguments with defaults
                                 #'() ; no additional instance static-infos
                                 #f ; not callable
                                 #f ; not indexable
                                 #f ; not mutable indexable
                                 #f ; not appendable
                                 #f ; not prefab
                                 null)))
                null)

         (define-for-syntax name-dot-dispatch
           (lambda (field-sym field-proc ary nary fail-k)
             (case field-sym
               [(prop) (field-proc (lambda (e reloc)
                                     (build-accessor-call #'prop-proc e reloc prop-static-infos))
                                   (~? (lambda (e rhs reloc)
                                         (build-mutator-call #'prop-mutator e rhs reloc))))]
               ...
               [(method) method-dispatch]
               ...
               [else #,(if (syntax-e #'parent)
                           #'(parent-dot-dispatch field-sym field-proc ary nary fail-k)
                           #'(fail-k))])))

         (define-syntax name-instance
           (dot-provider (dot-parse-dispatch name-dot-dispatch)))
         )]))

(define-for-syntax (build-accessor-call rator rand reloc static-infos)
  (define call (reloc #`(#,rator #,rand)))
  (define maybe-static-infos
    (if (procedure? static-infos)
        (static-infos rand)
        static-infos))
  (if maybe-static-infos
      (wrap-static-info* call maybe-static-infos)
      call))

(define-for-syntax (build-mutator-call rator rand rhs reloc)
  (reloc #`(#,rator #,rand #,rhs)))

(define (hash-add* ht . kvs)
  (let loop ([ht ht] [kvs kvs])
    (if (null? kvs)
        ht
        (loop (hash-set ht (car kvs) (cadr kvs))
              (cddr kvs)))))
