#lang racket/base
(require (for-syntax racket/base
                     syntax/stx
                     enforest/transformer
                     enforest/property
                     enforest/proc-name))

(begin-for-syntax
  (provide (property-out definition-transformer)

           check-definition-result)

  (property definition-transformer transformer)

  (define (check-definition-result forms proc)
    (unless (stx-list? forms) (raise-result-error (proc-name proc) "stx-list?" forms))
    forms))
