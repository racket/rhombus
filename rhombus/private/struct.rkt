#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest/syntax-local
                     "srcloc.rkt")
         "definition.rkt"
         "expression.rkt"
         "binding.rkt"
         "contract.rkt"
         (submod "contract.rkt" for-struct)
         (submod "dot.rkt" for-dot-provider)
         "composite.rkt"
         "assign.rkt"
         "result.rkt"
         "static-info.rkt")

(provide (rename-out [rhombus-struct struct]))

(begin-for-syntax
  (struct struct-contract contract (constructor-id fields))
  (define (struct-contract-ref v) (and (struct-contract? v) v))

  (define-syntax-class :field
    #:datum-literals (group op)
    #:literals (:: mutable)
    (pattern (group (~optional mutable
                               #:defaults ([mutable #'#f]))
                    name:identifier
                    (~optional (~seq (op ::) contract::contract)))
             #:attr predicate #`(~? contract.predicate #f)
             #:attr static-infos #`(~? contract.static-infos ())
             #:attr contract-name #`(~? contract.name #f))))
                                 
(define-syntax rhombus-struct
  (definition-transformer
   (lambda (stxes)
     (syntax-parse stxes
       [(_ name:identifier ((~datum parens) field::field ...))
        (define fields (syntax->list #'(field.name ...)))
        (define-values (immutable-fields mutable-fields)
          (for/fold ([imm '()] [m '()] #:result (values (reverse imm) (reverse m)))
                    ([field (in-list fields)]
                     [mutable (syntax->list #'(field.mutable ...))])
            (if (syntax-e mutable)
                (values imm (cons field m))
                (values (cons field imm) m))))
        (with-syntax ([name? (datum->syntax #'name (string->symbol (format "~a?" (syntax-e #'name))) #'name)]
                      [(struct:name) (generate-temporaries #'(name))]
                      [name-instance (datum->syntax #'name (string->symbol (format "~a.instance" (syntax-e #'name))) #'name)]
                      [(name-field ...) (for/list ([field (in-list fields)])
                                          (datum->syntax field
                                                         (string->symbol (format "~a.~a"
                                                                                 (syntax-e #'name)
                                                                                 (syntax-e field)))
                                                         field))]
                      [(set-name-field! ...) (for/list ([field (in-list mutable-fields)])
                                               (datum->syntax field
                                                              (string->symbol (format "set-~a.~a!"
                                                                                      (syntax-e #'name)
                                                                                      (syntax-e field)))
                                                              field))]
                      [cnt (length fields)]
                      [(field-index ...) (for/list ([field (in-list fields)]
                                                    [i (in-naturals)])
                                           i)]
                      [(immutable-field-index ...) (for/list ([field (in-list immutable-fields)]
                                                              [i (in-naturals)])
                                                     i)]
                      [(mutable-field ...) mutable-fields]
                      [(mutable-field-index ...) (for/list ([field (in-list mutable-fields)]
                                                            [i (in-naturals)])
                                                   i)])
          (list
           #`(define-values (struct:name name name? name-field ... set-name-field! ...)
               (let-values ([(struct:name name name? name-ref name-set!)
                             (make-struct-type 'name #f cnt 0 #f null #f #f
                                               '(immutable-field-index ...)
                                               #,(build-guard-expr fields
                                                                   (syntax->list #'(field.predicate ...))))])
                 (values struct:name name name?
                         (make-struct-field-accessor name-ref field-index 'field.name)
                         ...
                         (make-struct-field-accessor name-set! mutable-field-index 'mutable-field)
                         ...)))
           #'(define-binding-syntax name
               (binding-transformer
                #'name
                (make-composite-binding-transformer (quote-syntax name?)
                                                    (list (quote-syntax name-field) ...)
                                                    (list (quote-syntax field.static-infos) ...))))
           #'(define-contract-syntax name
               (struct-contract (quote-syntax name?)
                                (quote-syntax ((#%dot-provider name-instance)))
                                (quote-syntax name)
                                (list (list 'field.name (quote-syntax name-field) (quote-syntax field.static-infos))
                                      ...)))
           #'(define-dot-provider-syntax name
               (dot-provider (make-handle-struct-type-dot (quote-syntax name))))
           #'(define-dot-provider-syntax name-instance
               (dot-provider (make-handle-struct-instance-dot (quote-syntax name))))
           #'(define-static-info-syntax name (#%result ((#%dot-provider name-instance))))
           #'(begin
               (define-static-info-syntax/maybe* name-field (#%result field.static-infos))
               ...)))]))))

(define-for-syntax (build-guard-expr fields predicates)
  (and (for/or ([predicate (in-list predicates)])
         (syntax-e predicate))
       #`(lambda (#,@fields who)
           (values #,@(for/list ([field (in-list fields)]
                                 [predicate (in-list predicates)])
                        (cond
                          [(not (syntax-e predicate)) field]
                          [else #`(if (#,predicate #,field)
                                      #,field
                                      (raise-argument-error who
                                                            #,(format "~a" (syntax-e predicate))
                                                            #,field))]))))))

;; dot provider for a structure name used before a `.`
(define-for-syntax ((make-handle-struct-type-dot name) form1 dot field-id)
  (define contract (syntax-local-value* (in-contract-space name) struct-contract-ref))
  (unless contract (error "cannot find contract binding for dot provider"))
  (define accessor-id
    (for/or ([field+acc (in-list (struct-contract-fields contract))])
      (and (eq? (car field+acc) (syntax-e field-id))
           (cadr field+acc))))
  (unless accessor-id
    (raise-syntax-error #f
                        "cannot find field in structure"
                        field-id))
  (relocate (span-srcloc form1 field-id) accessor-id))

;; dot provider for a structure instance used before a `.`
(define-for-syntax ((make-handle-struct-instance-dot name) form1 dot field-id)
  (define contract (syntax-local-value* (in-contract-space name) struct-contract-ref))
  (unless contract (error "cannot find contract binding for instance dot provider"))
  (define accessor-id+static-infos
    (for/or ([field+acc (in-list (struct-contract-fields contract))])
      (and (eq? (car field+acc) (syntax-e field-id))
           (cdr field+acc))))
  (unless accessor-id+static-infos
    (raise-syntax-error #f
                        "don't know how to access field"
                        field-id))
  (define accessor-id (car accessor-id+static-infos))
  (define e (datum->syntax (quote-syntax here)
                           (list accessor-id form1)
                           (span-srcloc form1 field-id)
                           #'dot))
  (wrap-static-info* e (cadr accessor-id+static-infos)))


(define-syntax (define-static-info-syntax/maybe* stx)
  (syntax-parse stx
    [(_ id (_)) #'(begin)]
    [(_ id rhs ...) #'(define-static-info-syntax id rhs ...)]))
