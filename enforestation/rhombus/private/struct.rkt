#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest/syntax-local
                     "srcloc.rkt")
         "definition.rkt"
         "expression.rkt"
         "binding.rkt"
         "type.rkt"
         "composite.rkt")

(provide (rename-out [rhombus-struct struct])
         |.|)

(begin-for-syntax
  (struct struct-type rhombus-type (constructor-id fields)))

(define-syntax rhombus-struct
  (definition-transformer
   (lambda (stxes)
     (syntax-parse stxes
       [(_ name:identifier ((~datum parens) ((~datum group) field:id) ...))
        (define fields (syntax->list #'(field ...)))
        (with-syntax ([name? (datum->syntax #'name (string->symbol (format "~a?" (syntax-e #'name))) #'name)]
                      [(struct:name) (generate-temporaries #'(name))]
                      [(name-field ...) (for/list ([field (in-list fields)])
                                          (datum->syntax field
                                                         (string->symbol (format "~a_~a"
                                                                                 (syntax-e #'name)
                                                                                 (syntax-e field)))
                                                         field))]
                      [cnt (length fields)]
                      [(field-index ...) (for/list ([field (in-list fields)]
                                                    [i (in-naturals)])
                                           i)])
          (list
           #'(define-values (struct:name name name? name-field ...)
               (let-values ([(struct:name name name? name-ref name-set!)
                             (make-struct-type 'name #f cnt 0 #f null #f #f
                                               '(field-index ...))])
                 (values struct:name name name?
                         (make-struct-field-accessor name-ref field-index 'field)
                         ...)))
           #'(define-binding-syntax name
               (binding-transformer
                #'name
                (make-composite-binding-transformer (quote-syntax name?)
                                                    (list (quote-syntax name-field) ...))))
           #'(define-type-syntax name
               (struct-type (quote-syntax name?)
                            (quote-syntax name)
                            (list (cons 'field (quote-syntax name-field)) ...)))))]))))

(define-syntax |.|
  (expression-infix-operator
   (quote-syntax |.|)
   '((default . stronger))
   'macro
   (lambda (form1 tail)
     (syntax-parse tail
       [(dot field:identifier . tail)
        (define type-id (rhombus-syntax-local-type form1))
        (define type (and (identifier? type-id)
                          (syntax-local-value* (in-type-space type-id) struct-type?)))
        (define accessor-id (and (struct-type? type)
                                 (for/or ([field+acc (in-list (struct-type-fields type))])
                                   (and (eq? (car field+acc) (syntax-e #'field))
                                        (cdr field+acc)))))
        (unless accessor-id
          (raise-syntax-error #f
                              "don't know how to access field"
                              #'field))
        (values (datum->syntax (quote-syntax here)
                               (list accessor-id form1)
                               (span-srcloc form1 #'field)
                               #'dot)
                #'tail)]
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
