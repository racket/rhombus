#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "srcloc.rkt"
                     "class-parse.rkt")
         "name-root.rkt"
         "name-root-space.rkt"
         "expression.rkt"
         "binding.rkt"
         (submod "annotation.rkt" for-class)
         (submod "dot.rkt" for-dot-provider)
         "static-info.rkt"
         "dot-parse.rkt"
         "function-arity-key.rkt"
         "call-result-key.rkt"
         "composite.rkt"
         "class-desc.rkt")

(provide define-primitive-class)

(define-syntax (define-primitive-class stx)
  (syntax-parse stx
    [(_ Name name
        #:constructor-static-info (constructor-static-info ...)
        (~and creation (~or #:new #:existing))
        (~optional (~and actual-class (~or #:class)))
        (~and mode (~or #:transparent #:opaque #:translucent))
        (~optional (~seq #:parent Parent parent)
                   #:defaults ([parent #'#f]
                               [Parent #'#f]))
        #:fields
        ([field/rename field-static-info] ...) ; only for binding pattern in translucent mode
        (~optional
         (~seq #:namespace-fields
               (ns-field ...))
         #:defaults ([(ns-field 1) '()]))
        #:properties
        ([property property-proc] ...)
        #:methods
        ([method mask name-method-proc method-proc . method-si] ...))
     #:do [(define transparent? (eq? '#:transparent (syntax-e #'mode)))
           (define translucent? (eq? '#:translucent (syntax-e #'mode)))]
     #:with name? (datum->syntax #'name (string->symbol (format "~a?" (syntax-e #'name))))
     #:with struct_name (datum->syntax #'name (string->symbol (format "struct:~a" (syntax-e #'name))))
     #:with ((field _field) ...) (for/list ([f/r (in-list (syntax->list #'(field/rename ...)))])
                                   (syntax-parse f/r
                                     [id:identifier #'(id id)]
                                     [(f:id r:id) f/r]))
     #:with (name-field ...) (for/list ([field (in-list (syntax->list #'(field ...)))])
                               (datum->syntax #'name (string->symbol
                                                      (format "~a-~a" (syntax-e #'name)
                                                              (syntax-e field)))))
     #:with ([prop prop-proc] ...) (if transparent?
                                       #`([_field name-field]
                                          ...
                                          [property property-proc]
                                          ...)
                                       #`([property property-proc]
                                          ...))
     #:with name-dot-dispatch (datum->syntax #'name (string->symbol
                                                     (format "~a-dot-dispatch" (syntax-e #'name))))
     #:with parent-dot-dispatch (and (syntax-e #'parent)
                                     (datum->syntax #'parent (string->symbol
                                                              (format "~a-dot-dispatch" (syntax-e #'parent)))))
     #:with name-method-table (datum->syntax #'name (string->symbol (format "~a-method-table" (syntax-e #'name))))
     #:with parent-method-table (datum->syntax #'parent (string->symbol (format "~a-method-table" (syntax-e #'parent))))
     #:with name-field-list (datum->syntax #'name (string->symbol (format "~a-field-list" (syntax-e #'name))))
     #:with (parent-name-field ...) (if (syntax-e #'parent)
                                        (syntax-local-value
                                         (datum->syntax #'parent (string->symbol (format "~a-field-list" (syntax-e #'parent)))))
                                        null)
     #:with (parent-static-infos ...) (for/list ([proc-id (in-list (syntax->list #'(parent-name-field ...)))])
                                        (extract-static-infos #'proc-id))
     #:with name-static-infos (datum->syntax #'name (string->symbol (format "~a-static-infos" (syntax-e #'name))))
     #:with Name-str (datum->syntax #'here (symbol->string (syntax-e #'Name)))
     #:with arity-mask #`#,(arithmetic-shift 1 (length (syntax->list #'(parent-name-field ... name-field ...))))
     #:do [(define super (and (syntax-e #'Parent)
                              (syntax-local-value (in-class-desc-space #'Parent))))
           (define inherited-field-count (if super
                                             (+ (class-desc-inherited-field-count super)
                                                (length (class-desc-fields super)))
                                             0))]
     #:with ((super-field-name
              super-name-field
              super-name-field-set!
              super-field-static-info
              super-field-constructor)
             ...) (if super
                      (class-desc-fields super)
                      null)
     #`(begin
         #,(if (eq? (syntax-e #'creation) '#:new)
               #`(struct name (field ...)
                   #:property prop:field-name->accessor
                   (list* '()
                          (hasheq (~@ 'prop prop-proc)
                                  ...)
                          (hasheq (~@ 'method name-method-proc)
                                  ...)))
               #`(define name-method-table
                   (hash-add* #,(if (syntax-e #'parent)
                                    #'parent-method-table
                                    #'#hasheq())
                              (~@ 'prop prop-proc)
                              ...
                              (~@ 'method method-proc)
                              ...)))
         
         (define-for-syntax name-static-infos
           #'((#%dot-provider instance)))
         
         #,#'(define-static-info-syntax name
               (#%call-result #,name-static-infos)
               (#%function-arity #,arity-mask)
               constructor-static-info ...)

         #,(if (or transparent? translucent?)
               #`(define-annotation-syntax Name
                   (identifier-annotation #'name? name-static-infos))
               #'(begin))

         #,@(cond
              [(or transparent?
                   translucent?)
               #`((define-syntax Name
                    (expression-transformer
                     (lambda (stx)
                       (syntax-parse stx
                         [(head . tail)
                          (values (relocate-id #'head #'name) #'tail)]))))
                  (define-binding-syntax Name
                    (binding-transformer
                     (make-composite-binding-transformer Name-str
                                                         #'name?
                                                         (list (quote-syntax parent-name-field)
                                                               ...
                                                               (quote-syntax name-field)
                                                               ...)
                                                         (list (quote-syntax parent-static-infos)
                                                               ...
                                                               #`field-static-info
                                                               ...)
                                                         #:static-infos name-static-infos))))]
              [else null])

         #,@(for/list ([name-field (in-list (syntax->list #'(name-field ...)))]
                       [static-infos (in-list (syntax->list #'(field-static-info ...)))]
                       #:unless (null? (syntax-e static-infos)))
              #`(define-static-info-syntax #,name-field
                  (#%call-result #,static-infos)))

         (define-name-root Name
           #:fields
           (ns-field
            ...
            [prop prop-proc]
            ...
            [method name-method-proc]
            ...))

         (define-syntax name-field-list
           (quote-syntax (parent-name-field ... name-field ...)))

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
                                             (quote-syntax super-field-static-info)
                                             (quote-syntax super-field-constructor))
                                       ...
                                       (list 'field
                                             (quote-syntax name-field)
                                             #f
                                             (quote-syntax field-static-info)
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
           (lambda (field-sym field-proc ary 0ary nary fail-k)
             (case field-sym
               [(prop) (field-proc (lambda (e reloc) (build-accessor-call #'prop-proc e reloc)))]
               ...
               [(method) (nary #'method-proc mask #'name-method-proc . method-si)]
               ...
               [else
                #,(if (syntax-e #'parent)
                      #'(parent-dot-dispatch field-sym field-proc ary 0ary nary fail-k)
                      #'(fail-k))])))

         (define-syntax instance
           (dot-provider
            (dot-parse-dispatch
             name-dot-dispatch))))]))

(define-for-syntax (build-accessor-call rator rand reloc)
  (define call (reloc #`(#,rator #,rand)))
  (define static-infos (syntax-local-static-info rator #'#%call-result))
  (if static-infos
      (wrap-static-info* call static-infos)
      call))

(define (hash-add* ht . kvs)
  (let loop ([ht ht] [kvs kvs])
    (if (null? kvs)
        ht
        (loop (hash-set ht (car kvs) (cadr kvs))
              (cddr kvs)))))
