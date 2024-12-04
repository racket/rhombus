#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/operator
                     enforest/property
                     enforest/syntax-local
                     "introducer.rkt"
                     (for-syntax racket/base))
         "enforest.rkt")

(begin-for-syntax
  (provide (property-out unquote-binding-prefix-operator)
           (property-out unquote-binding-infix-operator)

           unquote-binding-transformer

           :unquote-binding

           in-unquote-binding-space
           unquote-bind-quote

           current-unquote-binding-kind

           unquote-binding-prefix+infix-operator

           unquote-binding-id?))

(provide define-unquote-binding-syntax)

(begin-for-syntax
  (property unquote-binding-prefix-operator prefix-operator)
  (property unquote-binding-infix-operator infix-operator)

  (define (unquote-binding-transformer proc)
    (unquote-binding-prefix-operator '((default . stronger)) 'macro proc))

  (define (make-identifier-unquote-binding id)
    id)

  (define in-unquote-binding-space (make-interned-syntax-introducer/add 'rhombus/unquote_bind))
  (define-syntax (unquote-bind-quote stx)
    (syntax-case stx ()
      [(_ id) #`(quote-syntax #,((make-interned-syntax-introducer 'rhombus/unquote_bind) #'id))]))

  ;; Binding context kinds:
  ;;  'term1 - parse a term, but can splice multi
  ;;  'grouplet - parse a non-empty term sequence, allowed in designated pattern positions
  ;;  'group1 - parse a whole group; like 'grouplet, but where an id should be group mode
  ;;  'multi1 - parse a group sequence
  ;;  'block1 - parse a group sequence within a block
  ;; There's a close connection between pattern types and binding contexts, but we
  ;; use different symbols to reflect how pattern-type expands adapt to binding context
  (define current-unquote-binding-kind (make-parameter 'term1))

  (define-rhombus-enforest
    #:syntax-class :unquote-binding
    #:desc "unquote binding"
    #:operator-desc "unquote binding operator"
    #:parsed-tag #:rhombus/unquote_binding
    #:in-space in-unquote-binding-space
    #:prefix-operator-ref unquote-binding-prefix-operator-ref
    #:infix-operator-ref unquote-binding-infix-operator-ref
    #:make-identifier-form make-identifier-unquote-binding)

  (struct unquote-binding-prefix+infix-operator (prefix infix)
    #:property prop:unquote-binding-prefix-operator (lambda (self) (unquote-binding-prefix+infix-operator-prefix self))
    #:property prop:unquote-binding-infix-operator (lambda (self) (unquote-binding-prefix+infix-operator-infix self)))

  (define (unquote-binding-id? id)
    (syntax-local-value* (in-unquote-binding-space id)
                         (lambda (v)
                           (or (unquote-binding-prefix-operator-ref v)
                               (unquote-binding-infix-operator-ref v))))))

(define-syntax (define-unquote-binding-syntax stx)
  (syntax-parse stx
    [(_ name:id rhs)
     (quasisyntax/loc stx
       (define-syntax #,(in-unquote-binding-space #'name) rhs))]))
