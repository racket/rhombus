#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "provide.rkt"
         "class-primitive.rkt"
         "function-arity-key.rkt")

(provide (for-spaces (rhombus/namespace
                      #f
                      rhombus/bind
                      rhombus/annot)
                     Srcloc))

(module+ for-builtin
  (provide srcloc-method-table))

(module+ for-static-info
  (provide (for-syntax srcloc-static-infos)))

(define-primitive-class Srcloc srcloc
  #:constructor-static-info ()
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
