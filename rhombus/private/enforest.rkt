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
  (define-syntax-rule (define-rhombus-transform option ...)
    (define-transform
      option ...
      #:name-path-op name-path-op
      #:in-name-root-space in-name-root-space
      #:name-root-ref name-root-ref))

  (define-syntax-rule (define-rhombus-sequence-transform option ...)
    (define-sequence-transform
      option ...
      #:name-path-op name-path-op
      #:in-name-root-space in-name-root-space
      #:name-root-ref name-root-ref))

  (define-syntax define-rhombus-enforest
    (syntax-rules ()
      [(_ option ... #:name-root-ref name-root-ref)
       (define-enforest
         option ...
         #:name-path-op name-path-op
         #:in-name-root-space in-name-root-space
         #:name-root-ref name-root-ref)]
      [(_ option ...)
       (define-enforest
         option ...
         #:name-path-op name-path-op
         #:in-name-root-space in-name-root-space
         #:name-root-ref name-root-ref)])))
