#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     syntax/stx
                     enforest/transformer
                     enforest/sequence
                     enforest/property
                     enforest/proc-name
                     "introducer.rkt"))

(begin-for-syntax
  (provide (property-out definition-transformer)
           (property-out definition-sequence-transformer)

           check-definition-result)

  (property definition-transformer transformer)
  (property definition-sequence-transformer sequence-transformer)

  (define (check-definition-result forms proc)
    (unless (stx-list? forms) (raise-result-error (proc-name proc) "stx-list?" forms))
    forms))
