#lang racket/base
(require (for-syntax racket/base
                     "annot-context.rkt")
         racket/mutability
         "provide.rkt"
         (submod "annotation.rkt" for-class)
         "static-info.rkt"
         "define-arity.rkt"
         "call-result-key.rkt"
         "class-primitive.rkt")

(provide (for-spaces (rhombus/namespace
                      #f
                      rhombus/annot
                      rhombus/statinfo)
                     Ephemeron))

(module+ for-builtin
  (provide ephemeron-method-table))

(define-primitive-class Ephemeron ephemeron
  #:lift-declaration
  #:existing
  #:opaque
  #:fields ()
  #:namespace-fields
  ()
  #:properties
  ([value Ephemeron.value])
  #:methods
  ())

(define/arity (Ephemeron k v)
  #:static-infos ((#%call-result #,(get-ephemeron-static-infos)))
  (make-ephemeron k v))

(define-annotation-syntax Ephemeron (identifier-annotation ephemeron? #,(get-ephemeron-static-infos)))

(define/arity (Ephemeron.value wb)
  #:primitive (ephemeron-value)
  (ephemeron-value wb))
