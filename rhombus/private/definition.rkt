#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     syntax/stx
                     enforest/transformer
                     enforest/sequence
                     enforest/property
                     enforest/proc-name
                     "introducer.rkt"
                     "expression-space.rkt"
                     "macro-result.rkt"))

(provide define-defn-syntax)

(begin-for-syntax
  (provide (property-out definition-transformer)
           (property-out definition-sequence-transformer)

           check-definition-result

           in-defn-space)

  (property definition-transformer transformer)
  (property definition-sequence-transformer sequence-transformer)

  (define (check-definition-result forms proc)
    (unless (stx-list? forms) (raise-bad-macro-result (proc-name proc) "definitions and expressions" forms))
    forms)

  (define in-defn-space (make-interned-syntax-introducer/add 'rhombus/defn)))

(define-syntax (define-defn-syntax stx)
  (syntax-parse stx
    [(_ id:identifier rhs)
     #`(define-syntax #,(in-defn-space #'id)
         rhs)]))
