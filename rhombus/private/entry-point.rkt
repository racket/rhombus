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

(module+ for-class
  (provide (for-syntax in-entry-point-space)))

(begin-for-syntax
  (provide (property-out entry-point-transformer)
           :entry-point
           (struct-out entry-point-adjustments)
           no-adjustments)

  (property entry-point-transformer transformer)

  (define (check-entry-point-result form proc)
    (unless (syntax? form)
      (raise-result-error (proc-name proc) rhombus-realm "Entry_Point_Syntax" form))
    form)

  (define in-entry-point-space (make-interned-syntax-introducer/add 'rhombus/entry-point))

  (struct entry-point-adjustments (prefix-arguments wrap-body method?))
  (define no-adjustments (entry-point-adjustments '() (lambda (arity stx) stx) #f))
  
  (define-transform
    #:syntax-class (:entry-point adjustments)
    #:desc "entry-point form"
    #:in-space in-entry-point-space
    #:name-path-op name-path-op
    #:name-root-ref name-root-ref
    #:name-root-ref-root name-root-ref-root
    #:transformer-ref (lambda (v)
                        (define t (entry-point-transformer-ref v))
                        (and t (transformer
                                (lambda (stx)
                                  (define new-adjustments
                                    (struct-copy entry-point-adjustments adjustments
                                                 [prefix-arguments (map transform-in
                                                                        (entry-point-adjustments-prefix-arguments adjustments))]))
                                  ((transformer-proc t) stx new-adjustments)))))
    #:check-result check-entry-point-result))
