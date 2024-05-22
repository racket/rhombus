#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/transformer
                     enforest/property
                     enforest/proc-name
                     "introducer.rkt"
                     "macro-result.rkt")
         "enforest.rkt")

(module+ for-class
  (provide (for-syntax in-immediate-callee-space)))

(provide define-immediate-callee-syntax)

(begin-for-syntax
  (provide (property-out immediate-callee-transformer)
           :immediate-callee
           pack-immediate-callee
           unpack-immediate-callee)

  (property immediate-callee-transformer transformer ())

  (define (check-immediate-callee-result form proc static-infoss op-mode op-stx)
    (unless (and (syntax? form)
                 (syntax-parse form
                   [(term . tail) #t]
                   [_ #f]))
      (raise-bad-macro-result (proc-name proc) "immediate callee form" form))
    form)

  (define (pack-immediate-callee stx tail)
    #`(#,stx . #,tail))
  (define (unpack-immediate-callee stx)
    (syntax-parse stx
      [(stx . tail) (values #'stx #'tail)]))

  (define in-immediate-callee-space (make-interned-syntax-introducer/add 'rhombus/immediate_callee))

  (define-rhombus-transform
    #:syntax-class (:immediate-callee static-infoss op-mode op-stx)
    #:desc "immediate-callee form"
    #:parsed-tag #:rhombus/immediate_callee
    #:in-space in-immediate-callee-space
    #:transformer-ref immediate-callee-transformer-ref
    #:check-result check-immediate-callee-result))

(define-syntax (define-immediate-callee-syntax stx)
  (syntax-parse stx
    [(_ name:id rhs)
     (quasisyntax/loc stx
       (define-syntax #,(in-immediate-callee-space #'name) rhs))]))
