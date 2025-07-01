#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/transformer
                     enforest/property
                     enforest/proc-name
                     "introducer.rkt"
                     "macro-result.rkt"
                     "track-parsed.rkt")
         "enforest.rkt")

(provide define-interface-clause-syntax)

(module+ for-interface
  (provide (for-syntax in-interface-clause-space)))

(begin-for-syntax
  (provide (property-out interface-clause-transformer)
           :interface-clause
           :interface-clause-form
           interface-clause?)

  (property interface-clause-transformer transformer)

  (define-syntax-class :interface-clause-form
    (pattern [parsed ...]))

  (define (check-interface-clause-result form proc data)
    (syntax-parse (if (syntax? form) form #'#f)
      [_::interface-clause-form form]
      [_ (raise-bad-macro-result (proc-name proc) "`class` clause" form)]))

  (define in-interface-clause-space (make-interned-syntax-introducer/add 'rhombus/interface_clause))

  (define-rhombus-transform
    #:syntax-class (:interface-clause intf-data)
    #:predicate interface-clause?
    #:desc "interface clause"
    #:parsed-tag #:rhombus/class_clause
    #:in-space in-interface-clause-space
    #:transformer-ref interface-clause-transformer-ref
    #:check-result check-interface-clause-result
    #:track-origin track-parsed-sequence-origin))

(define-syntax (define-interface-clause-syntax stx)
  (syntax-parse stx
    [(_ id:identifier rhs)
     #`(define-syntax #,(in-interface-clause-space #'id)
         rhs)]))
