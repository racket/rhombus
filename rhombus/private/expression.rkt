#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest/operator
                     enforest/property
                     enforest/proc-name))

(begin-for-syntax
  (provide (property-out expression-prefix-operator)
           (property-out expression-infix-operator)

           expression-transformer

           make-identifier-expression
                     
           check-expression-result
           
           in-expression-space))

(provide define-expression-syntax)

(begin-for-syntax
  (property expression-prefix-operator prefix-operator)
  (property expression-infix-operator infix-operator)

  (define (expression-transformer name proc)
    (expression-prefix-operator name '((default . stronger)) 'macro proc))

  (define (make-identifier-expression id)
    id)

  (define (check-expression-result form proc)
    (unless (syntax? form) (raise-result-error (proc-name proc) "syntax?" form))
    form)

  (define in-expression-space (make-interned-syntax-introducer 'rhombus/expression)))

(define-syntax (define-expression-syntax stx)
  (syntax-parse stx
    [(_ name:id rhs)
     (quasisyntax/loc stx
       (define-syntax #,(in-expression-space #'name) rhs))]))
