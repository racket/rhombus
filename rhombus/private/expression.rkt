#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "op.rkt"
                     "transformer.rkt"
                     "property.rkt"
                     "check.rkt")
         "property-out.rkt")

(provide (for-syntax (property-out expression-prefix-operator)
                     (property-out expression-infix-operator)
                     expression-operator-ref
                     
                     (property-out expression-transformer)
                     
                     check-expression-result

                     in-expression-space)

         define-expression-syntax)

(begin-for-syntax
  (property expression-prefix-operator prefix-operator)
  (property expression-infix-operator infix-operator)

  (define (expression-operator-ref v)
    (or (expression-prefix-operator-ref v)
        (expression-infix-operator-ref v)
        (error #f "identifier is not mapped to an expression operator: ~e" v)))

  (property expression-transformer transformer)

  (define (check-expression-result form proc)
    (unless (syntax? form) (raise-result-error (proc-name proc) "syntax?" form))
    form)

  (define in-expression-space (make-interned-syntax-introducer 'rhombus/expression)))

(define-syntax (define-expression-syntax stx)
  (syntax-parse stx
    [(_ name:id rhs)
     (quasisyntax/loc stx
       (define-syntax #,(in-expression-space #'name) rhs))]))
