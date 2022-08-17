#lang racket/base
(require (for-syntax racket/base
                     syntax/stx
                     enforest/transformer
                     enforest/property
                     enforest/proc-name))

(begin-for-syntax
  (provide (property-out nestable-declaration-transformer)

           check-nestable-declaration-result)

  (property nestable-declaration-transformer transformer)

  (define (check-nestable-declaration-result forms proc)
    (unless (stx-list? forms) (raise-result-error (proc-name proc) "stx-list?" forms))
    forms))
