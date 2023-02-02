#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/operator
                     enforest/property
                     enforest/proc-name
                     "introducer.rkt"
                     "expression-space.rkt"
                     (for-syntax racket/base)
                     "realm.rkt"))

(begin-for-syntax
  (provide (property-out expression-prefix-operator)
           (property-out expression-infix-operator)

           expression-transformer

           make-identifier-expression

           check-expression-result
           
           in-expression-space
           expr-quote
           out-of-expression-space

           (struct-out expression-prefix+infix-operator)))

(module+ for-top-expand
  (provide (for-syntax check-unbound-identifier-early!)))

(begin-for-syntax
  (property expression-prefix-operator prefix-operator)
  (property expression-infix-operator infix-operator)

  (define (expression-transformer proc)
    (expression-prefix-operator (quote-syntax unused) '((default . stronger)) 'macro proc))

  (define early-unbound? #f)
  (define (check-unbound-identifier-early!)
    (set! early-unbound? #t))

  (define (make-identifier-expression id)
    (when early-unbound?
      (unless (identifier-binding id)
        ;; within a module, report unbound identifiers early;
        ;; otherwise, enforestation may continue and assume an
        ;; expression where some other kind of form was intended,
        ;; leading to confusing error messages; the trade-off is
        ;; that we don't compile some things that could (would?)
        ;; end up reporting a use before definition
        (raise-syntax-error #f "unbound identifier" id)))
    id)

  (define (check-expression-result form proc)
    (unless (syntax? form) (raise-result-error* (proc-name proc) rhombus-realm "Syntax" form))
    form)

  (define-syntax (expr-quote stx)
    (syntax-case stx ()
      [(_ id) #`(quote-syntax id)])))

(begin-for-syntax
  (struct expression-prefix+infix-operator (prefix infix)
    #:property prop:expression-prefix-operator (lambda (self) (expression-prefix+infix-operator-prefix self))
    #:property prop:expression-infix-operator (lambda (self) (expression-prefix+infix-operator-infix self))))
