#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "op.rkt"
                     "transformer.rkt"
                     "property.rkt"
                     "check.rkt")
         "property-out.rkt")

(provide (for-syntax (property-out binding-prefix-operator)
                     (property-out binding-infix-operator)
                     binding-operator-ref
                     
                     (property-out binding-transformer)
                     
                     :binding-form
                     check-binding-result

                     in-binding-space)

         define-binding-syntax)

(begin-for-syntax
  ;; To unpack a binding transformer result:
  (define-syntax-class :binding-form
    (pattern ((~and variable-ids (_:identifier ...))
              matcher-form
              (~and syntax-ids (_:identifier ...))
              syntax-form)))

  (property binding-prefix-operator prefix-operator)
  (property binding-infix-operator infix-operator)

  (define (binding-operator-ref v)
    (or (binding-prefix-operator-ref v)
        (binding-infix-operator-ref v)
        (error #f "identifier is not mapped to an binding operator: ~e" v)))

  (property binding-transformer transformer)

  (define (check-binding-result form proc)
    (syntax-parse (if (syntax? form) form #'#f)
      [_::binding-form form]
      [_ (raise-result-error (proc-name proc) "binding-result?" form)]))

  (define in-binding-space (make-interned-syntax-introducer 'rhombus/binding)))

(define-syntax (define-binding-syntax stx)
  (syntax-parse stx
    [(_ name:id rhs)
     (quasisyntax/loc stx
       (define-syntax #,(in-binding-space #'name) rhs))]))
