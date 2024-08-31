#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/operator
                     enforest/property
                     enforest/proc-name
                     enforest/syntax-local
                     "introducer.rkt"
                     "expression-space.rkt"
                     (for-syntax racket/base)
                     "macro-result.rkt")
         (only-in "definition.rkt"
                  in-defn-space
                  definition-transformer-ref
                  definition-sequence-transformer-ref)
         (only-in "declaration.rkt"
                  in-decl-space
                  declaration-transformer-ref)
         (only-in "nestable-declaration.rkt"
                  nestable-declaration-transformer-ref))

(begin-for-syntax
  (provide (property-out expression-prefix-operator)
           (property-out expression-infix-operator)

           expression-transformer

           expression-repeatable-prefix-operator?
           expression-repeatable-transformer
           
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
    (expression-prefix-operator '((default . stronger)) 'macro proc))

  ;; shortcut for an expression binding that can be used by itself like
  ;; an identifier in a repeition; this is a little bit of a cheat, so use
  ;; it only for things that are rename-transformer-like
  (struct expression-repeatable-prefix-operator expression-prefix-operator ())
  (define (expression-repeatable-transformer proc)
    (expression-repeatable-prefix-operator '((default . stronger)) 'macro proc))

  (define early-unbound? #f)
  (define (check-unbound-identifier-early!)
    (set! early-unbound? #t))

  (define (make-identifier-expression id)
    (unless (identifier-binding id)
      (when (or (syntax-local-value* (in-defn-space id)
                                     (lambda (v)
                                       (or (definition-transformer-ref v)
                                           (definition-sequence-transformer-ref v))))
                (syntax-local-value* (in-decl-space id)
                                     (lambda (v)
                                       (or (declaration-transformer-ref v)
                                           (nestable-declaration-transformer-ref v)))))
        (raise-syntax-error #f "misuse as an expression" id))
      (when early-unbound?
        ;; within a module, report unbound identifiers early;
        ;; otherwise, enforestation may continue and assume an
        ;; expression where some other kind of form was intended,
        ;; leading to confusing error messages; the trade-off is
        ;; that we don't compile some things that could (would?)
        ;; end up reporting a use before definition
        (raise-syntax-error #f "unbound identifier" id)))
    id)

  (define (check-expression-result form proc)
    (unless (syntax? form) (raise-bad-macro-result (proc-name proc) "expression" form))
    form)

  (define-syntax (expr-quote stx)
    (syntax-case stx ()
      [(_ id) #`(quote-syntax id)])))

(begin-for-syntax
  (struct expression-prefix+infix-operator (prefix infix)
    #:property prop:expression-prefix-operator (lambda (self) (expression-prefix+infix-operator-prefix self))
    #:property prop:expression-infix-operator (lambda (self) (expression-prefix+infix-operator-infix self))))
