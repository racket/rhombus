#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     syntax/stx
                     enforest/transformer
                     enforest/sequence
                     enforest/property
                     enforest/proc-name
                     "introducer.rkt"
                     "macro-result.rkt"
                     (for-syntax racket/base)))

(provide define-defn-syntax)

(begin-for-syntax
  (provide (property-out definition-transformer)
           (property-out definition-sequence-transformer)

           check-definition-result

           in-defn-space
           defn-quote)

  (property definition-transformer transformer)
  (property definition-sequence-transformer sequence-transformer)

  (define (check-definition-result forms proc)
    (unless (stx-list? forms) (raise-bad-macro-result (proc-name proc) "definitions and expressions" forms))
    forms)

  (define in-defn-space (make-interned-syntax-introducer/add 'rhombus/defn))
  (define-syntax (defn-quote stx)
    (syntax-case stx ()
      [(_ id) #`(quote-syntax #,((make-interned-syntax-introducer 'rhombus/defn) #'id))])))

(define-syntax (define-defn-syntax stx)
  (syntax-parse stx
    [(_ id:identifier rhs)
     #`(define-syntax #,(in-defn-space #'id)
         rhs)]))
