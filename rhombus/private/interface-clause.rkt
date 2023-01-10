#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/transformer
                     enforest/property
                     enforest/proc-name
                     "name-path-op.rkt"
                     "introducer.rkt"
                     "realm.rkt")
         "name-root-ref.rkt")

(provide define-interface-clause-syntax)

(module+ for-interface
  (provide (for-syntax in-interface-clause-space)))

(begin-for-syntax
  (provide (property-out interface-clause-transformer)
           :interface-clause
           :interface-clause-form)

  (property interface-clause-transformer transformer)

  (define-syntax-class :interface-clause-form
    (pattern [parsed ...]))

  (define (check-interface-clause-result form proc)
    (syntax-parse (if (syntax? form) form #'#f)
      [_::interface-clause-form form]
      [_ (raise-result-error* (proc-name proc) rhombus-realm "Interface_Clause_Syntax" form)]))

  (define in-interface-clause-space (make-interned-syntax-introducer/add 'rhombus/interface_clause))

  (define (make-interface-clause-transformer-ref class-data)
    ;; "accessor" closes over `class-data`:
    (lambda (v)
      (define cc (interface-clause-transformer-ref v))
      (and cc
           (transformer (lambda (stx)
                          ((transformer-proc cc) stx class-data))))))

  (define-transform
    #:syntax-class (:interface-clause intf-data)
    #:desc "interface clause"
    #:in-space in-interface-clause-space
    #:name-path-op name-path-op
    #:name-root-ref name-root-ref
    #:name-root-ref-root name-root-ref-root
    #:transformer-ref (make-interface-clause-transformer-ref intf-data)
    #:check-result check-interface-clause-result))

(define-syntax (define-interface-clause-syntax stx)
  (syntax-parse stx
    [(_ id:identifier rhs)
     #`(define-syntax #,(in-interface-clause-space #'id)
         rhs)]))
