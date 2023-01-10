#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "name-root.rkt"
         "expression.rkt"
         "expression+binding.rkt"
         (submod "annotation.rkt" for-class)
         (submod "dot.rkt" for-dot-provider)
         "static-info.rkt"
         "dot-parse.rkt"
         "call-result-key.rkt"
         "composite.rkt")

(provide define-primitive-class)

(define-syntax (define-primitive-class stx)
  (syntax-parse stx
    [(_ Name name
        (~and creation (~or #:new #:existing))
        (~and mode (~or #:transparent #:opaque #:translucent))
        #:fields
        ([field field-static-info] ...) ; only for binding pattern in translucent mode
        #:properties
        ([property property-proc] ...)
        #:methods
        ([method n name-method-proc method-proc] ...))
     #:do [(define transparent? (eq? '#:transparent (syntax-e #'mode)))
           (define translucent? (eq? '#:translucent (syntax-e #'mode)))]
     #:with name? (datum->syntax #'name (string->symbol (format "~a?" (syntax-e #'name))))
     #:with (name-field ...) (for/list ([field (in-list (syntax->list #'(field ...)))])
                               (datum->syntax #'name (string->symbol
                                                      (format "~a-~a" (syntax-e #'name)
                                                              (syntax-e field)))))
     #:with ([prop prop-proc] ...) (if transparent?
                                       #`([field name-field]
                                          ...
                                          [property property-proc]
                                          ...)
                                       #`([property property-proc]
                                          ...))
     #:with name-method-table (datum->syntax #'name (string->symbol (format "~a-method-table" (syntax-e #'name))))
     #:with name-static-infos (datum->syntax #'name (string->symbol (format "~a-static-infos" (syntax-e #'name))))
     #:with Name-str (datum->syntax #'here (symbol->string (syntax-e #'Name)))
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
                   (hash (~@ 'prop prop-proc)
                         ...
                         (~@ 'method method-proc)
                         ...)))
         
         (define-for-syntax name-static-infos
           #'((#%dot-provider instance)))
         
         #,#'(define-static-info-syntax name
               (#%call-result #,name-static-infos))

         #,(if (or transparent? translucent?)
               #`(define-annotation-syntax Name
                   (identifier-annotation #'Name #'name? name-static-infos))
               #'(begin))

         (define-name-root Name
           #:root #,(cond
                      [(or transparent?
                           translucent?)
                       #`(make-expression+binding-prefix-operator
                          #'Name
                          '((default . stronger))
                          'macro
                          (lambda (stx)
                            (syntax-parse stx
                              [(_ . tail)
                               (values #'name #'tail)]))
                          (make-composite-binding-transformer Name-str
                                                              #'name?
                                                              (list #'name-field
                                                                    ...)
                                                              (list #'field-static-info
                                                                    ...)))]
                      [else
                       #`(identifier-annotation #'Name #'name? #'())])
           #:fields
           ([prop prop-proc]
            ...
            [method name-method-proc]
            ...))

         (define-syntax instance
           (dot-provider-more-static
            (dot-parse-dispatch
             (lambda (field-sym field-proc ary 0ary nary fail-k)
               (case field-sym
                 [(prop) (field-proc (lambda (e) #`(prop-proc #,e)))]
                 ...
                 [(method) (nary #'method-proc n #'name-method-proc)]
                 ...
                 [else (fail-k)]))))))]))
