#lang racket/base
(require (for-syntax racket/base
                     syntax/stx
                     enforest/transformer
                     enforest/property
                     enforest/proc-name
                     "macro-result.rkt"))

(begin-for-syntax
  (provide (property-out declaration-transformer)

           check-declaration-result)

  (property declaration-transformer transformer)

  (define (check-declaration-result forms proc)
    (unless (stx-list? forms) (raise-bad-macro-result (proc-name proc) "declarations" forms))
    forms))
