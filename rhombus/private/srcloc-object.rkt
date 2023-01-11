#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "class-primitive.rkt"
         "function-arity-key.rkt")

(provide Srcloc
         (for-space rhombus/annot Srcloc))

(module+ for-builtin
  (provide srcloc-method-table))

(module+ for-static-info
  (provide (for-syntax srcloc-static-infos)))

(define-primitive-class Srcloc srcloc
  #:constructor-static-info (#%function-arity 32)
  #:existing
  #:transparent
  #:fields
  ([source ()]
   [line ()]
   [column ()]
   [position ()]
   [span ()])
  #:properties
  ()
  #:methods
  ())
