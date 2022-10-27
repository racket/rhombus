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
  (provide (for-syntax in-callable-space)))

(begin-for-syntax
  (provide (property-out callable-transformer)
           :callable
           :callable-form
           (struct-out callable-adjustments)
           no-adjustments)

  (property callable-transformer transformer)

  (define-syntax-class :callable-form
    (pattern [parsed ...]))

  (define (check-callable-result form proc)
    (unless (syntax? form)
      (raise-result-error (proc-name proc) rhombus-realm "Callable_Syntax" form))
    form)

  (define in-callable-space (make-interned-syntax-introducer/add 'rhombus/callable))

  (struct callable-adjustments (prefix-arguments wrap-body method?))
  (define no-adjustments (callable-adjustments '() (lambda (stx) stx) #f))
  
  (define-transform
    #:syntax-class (:callable adjustments)
    #:desc "callable form"
    #:in-space in-callable-space
    #:name-path-op name-path-op
    #:name-root-ref name-root-ref
    #:name-root-ref-root name-root-ref-root
    #:transformer-ref (lambda (v)
                        (define t (callable-transformer-ref v))
                        (and t (transformer
                                (lambda (stx)
                                  ((transformer-proc t) stx adjustments)))))
    #:check-result check-callable-result))
