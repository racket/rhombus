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
         "composite.rkt"
         "assign.rkt")

(provide (rename-out [rhombus-struct struct])
         |.|)

(begin-for-syntax
  (struct struct-contract rhombus-contract (constructor-id fields))

  (define-syntax-class :field
    #:datum-literals (group op)
    #:literals (:: mutable)
    (pattern (group (~optional mutable
                               #:defaults ([mutable #'#f]))
                    name:identifier
                    (~optional (~seq (op ::) contract::contract)
                               #:defaults ([contract #'#f])))
             #:attr predicate #`(~? contract.predicate #f))))
                                 
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
                                                    (list (quote-syntax name-field) ...))))
           #'(define-contract-syntax name
               (struct-contract (quote-syntax name?)
                                (quote-syntax name)
                                (list (list 'field.name (quote-syntax name-field) (quote-syntax field.contract))
                                      ...)))))]))))

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

(define-syntax |.|
  (expression-infix-operator
   (quote-syntax |.|)
   '((default . stronger))
   'macro
   (lambda (form1 tail)
     (syntax-parse tail
       [(dot field:identifier . tail)
        (cond
          [(and (identifier? form1)
                ;; FIXME: we should check an expression-space binding
                ;; for `form1` and get from there to a struct-contract...
                (syntax-local-value* (in-contract-space form1) struct-contract?))
           => (lambda (contract)
                (define accessor-id
                  (for/or ([field+acc (in-list (struct-contract-fields contract))])
                    (and (eq? (car field+acc) (syntax-e #'field))
                         (cadr field+acc))))
                (unless accessor-id
                  (raise-syntax-error #f
                                      "cannot find field in structure"
                                      #'field))
                (values accessor-id
                        #'tail))]
          [else
           (define contract-id (rhombus-syntax-local-contract form1))
           (define contract (and (identifier? contract-id)
                                 (syntax-local-value* (in-contract-space contract-id) struct-contract?)))
           (define accessor-id+contract (and (struct-contract? contract)
                                             (for/or ([field+acc (in-list (struct-contract-fields contract))])
                                               (and (eq? (car field+acc) (syntax-e #'field))
                                                    (cdr field+acc)))))
           (unless accessor-id+contract
             (raise-syntax-error #f
                                 "don't know how to access field"
                                 #'field))
           (define accessor-id (car accessor-id+contract))
           (define e (datum->syntax (quote-syntax here)
                                    (list accessor-id form1)
                                    (span-srcloc form1 #'field)
                                    #'dot))
           (define maybe-contract-e (if (syntax-e (cadr accessor-id+contract))
                                        (syntax-property e
                                                         rhombus-contract-property
                                                         (cadr accessor-id+contract))
                                        e))
           (values maybe-contract-e 
                   #'tail)])]
       [(dot other . tail)
        (raise-syntax-error #f
                            "expected an identifier for a field name, but found something else"
                            #'dot
                            #f
                            (list #'other))]
       [(dot)
        (raise-syntax-error #f
                            "expected an identifier for a field name"
                            #'dot)]))
   'left))
