#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest
                     enforest/operator
                     enforest/transformer
                     enforest/property
                     enforest/syntax-local
                     "introducer.rkt"
                     "name-path-op.rkt")
         "name-root-ref.rkt")

(begin-for-syntax
  (provide (property-out syntax-binding-prefix-operator)
           (property-out syntax-binding-infix-operator)

           syntax-binding-transformer
                     
           :syntax-binding

           in-syntax-binding-space

           current-syntax-binding-kind

           syntax-binding-prefix+infix-operator

           syntax-binding-id?))

(provide define-syntax-binding-syntax)

(begin-for-syntax
  (property syntax-binding-prefix-operator prefix-operator)
  (property syntax-binding-infix-operator infix-operator)

  (define (syntax-binding-transformer name proc)
    (syntax-binding-prefix-operator name '((default . stronger)) 'macro proc))

  (define (make-identifier-syntax-binding id)
    id)

  (define in-syntax-binding-space (make-interned-syntax-introducer/add 'rhombus/syntax_binding))

  (define current-syntax-binding-kind (make-parameter 'term))

  (define-enforest
    #:syntax-class :syntax-binding
    #:desc "syntax pattern binding"
    #:operator-desc "syntax pattern binding operator"
    #:in-space in-syntax-binding-space
    #:name-path-op name-path-op
    #:name-root-ref name-root-ref
    #:name-root-ref-root name-root-ref-root
    #:prefix-operator-ref syntax-binding-prefix-operator-ref
    #:infix-operator-ref syntax-binding-infix-operator-ref
    #:make-identifier-form make-identifier-syntax-binding)

  (struct syntax-binding-prefix+infix-operator (prefix infix)
    #:property prop:syntax-binding-prefix-operator (lambda (self) (syntax-binding-prefix+infix-operator-prefix self))
    #:property prop:syntax-binding-infix-operator (lambda (self) (syntax-binding-prefix+infix-operator-infix self)))

  (define (syntax-binding-id? id)
    (syntax-local-value* (in-syntax-binding-space id)
                         (lambda (v)
                           (or (syntax-binding-prefix-operator-ref v)
                               (syntax-binding-infix-operator-ref v))))))

(define-syntax (define-syntax-binding-syntax stx)
  (syntax-parse stx
    [(_ name:id rhs)
     (quasisyntax/loc stx
       (define-syntax #,(in-syntax-binding-space #'name) rhs))]))
