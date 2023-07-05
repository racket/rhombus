#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     syntax/stx
                     enforest/transformer
                     enforest/property
                     enforest/proc-name
                     "macro-result.rkt"
                     "introducer.rkt"))


(provide define-decl-syntax)

(begin-for-syntax
  (provide (property-out declaration-transformer)

           check-declaration-result

           in-decl-space)

  (property declaration-transformer transformer)

  (define (check-declaration-result forms proc)
    (unless (stx-list? forms) (raise-bad-macro-result (proc-name proc) "declarations" forms))
    forms)

  (define in-decl-space (make-interned-syntax-introducer/add 'rhombus/decl)))

(define-syntax (define-decl-syntax stx)
  (syntax-parse stx
    [(_ id:identifier rhs)
     #`(define-syntax #,(in-decl-space #'id)
         rhs)]))
