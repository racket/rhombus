#lang racket/base
(require (for-syntax racket/base
                     enforest
                     enforest/transformer
                     enforest/sequence                     
                     "name-path-op.rkt")
         "name-root-ref.rkt"
         "name-root-space.rkt")

(provide (for-syntax define-rhombus-transform
                     define-rhombus-sequence-transform
                     define-rhombus-enforest))

(begin-for-syntax
  (define-syntax define-rhombus-transform
    (syntax-rules ()
      [(_ option ... #:name-root-ref name-root-ref)
       (define-transform
         option ...
         #:name-path-op name-path-op
         #:in-name-root-space in-name-root-space
         #:name-root-ref name-root-ref)]
      [(_ option ...)
       (define-transform
         option ...
         #:name-path-op name-path-op
         #:in-name-root-space in-name-root-space
         #:name-root-ref name-root-ref/maybe)]))

  (define-syntax define-rhombus-sequence-transform
    (syntax-rules ()
      [(_ option ... #:name-root-ref name-root-ref)
       (define-sequence-transform
         option ...
         #:name-path-op name-path-op
         #:in-name-root-space in-name-root-space
         #:name-root-ref name-root-ref)]
      [(_ option ...)
       (define-sequence-transform
         option ...
         #:name-path-op name-path-op
         #:in-name-root-space in-name-root-space
         #:name-root-ref name-root-ref/maybe)]))

  (define-syntax define-rhombus-enforest
    (syntax-rules ()
      [(_ option ... #:name-root-ref name-root-ref)
       (define-enforest
         option ...
         #:name-path-op name-path-op
         #:in-name-root-space in-name-root-space
         #:name-root-ref name-root-ref
         #:lookup-space-description lookup-space-description)]
      [(_ option ...)
       (define-enforest
         option ...
         #:name-path-op name-path-op
         #:in-name-root-space in-name-root-space
         #:name-root-ref name-root-ref
         #:lookup-space-description lookup-space-description)])))

(define-for-syntax (lookup-space-description space-sym)
  (case space-sym
    [(#f) "expressions, definitions, and declarations"]
    [(rhombus/repet) "repetitions"]
    [(rhombus/annot) "annotations"]
    [(rhombus/bind) "bindings"]
    [(rhombus/stxclass) "syntax classes"]
    [(rhombus/unquote_binding) "unquote binding"]
    [(rhombus/namespace) "namespaces"]
    [(rhombus/class) "classes and interfaces"]
    [(rhombus/for_clause) "for clauses"]
    [(rhombus/class_clause) "class clauses"]
    [(rhombus/interface_clause) "interface clauses"]
    [else #f]))
