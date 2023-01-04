#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest/transformer
                     enforest/property
                     enforest/proc-name
                     "name-path-op.rkt"
                     "introducer.rkt"
                     "realm.rkt")
         "name-root-ref.rkt")

(provide define-class-clause-syntax)

(module+ for-class
  (provide (for-syntax in-class-clause-space)))

(begin-for-syntax
  (provide (property-out class-clause-transformer)
           :class-clause
           :class-clause-form)

  (property class-clause-transformer transformer)

  (define-syntax-class :class-clause-form
    (pattern [parsed ...]))

  (define (check-class-clause-result form proc)
    (syntax-parse (if (syntax? form) form #'#f)
      [_::class-clause-form form]
      [_ (raise-result-error (proc-name proc) rhombus-realm "Class_Clause_Syntax" form)]))

  (define in-class-clause-space (make-interned-syntax-introducer/add 'rhombus/class_clause))

  (define (make-class-clause-transformer-ref class-data)
    ;; "accessor" closes over `class-data`:
    (lambda (v)
      (define cc (class-clause-transformer-ref v))
      (and cc
           (transformer (lambda (stx)
                          ((transformer-proc cc) stx class-data))))))

  (define-transform
    #:syntax-class (:class-clause class-data)
    #:desc "class clause"
    #:in-space in-class-clause-space
    #:name-path-op name-path-op
    #:name-root-ref name-root-ref
    #:name-root-ref-root name-root-ref-root
    #:transformer-ref (make-class-clause-transformer-ref class-data)
    #:check-result check-class-clause-result))

(define-syntax (define-class-clause-syntax stx)
  (syntax-parse stx
    [(_ id:identifier rhs)
     #`(define-syntax #,(in-class-clause-space #'id)
         rhs)]))