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
                     WeakBox))

(module+ for-builtin
  (provide weak-box-method-table))

(define-primitive-class WeakBox weak-box
  #:lift-declaration
  #:existing
  #:opaque
  #:fields ()
  #:namespace-fields
  ()
  #:properties
  ([value WeakBox.value])
  #:methods
  ())

(define/arity (WeakBox v)
  #:static-infos ((#%call-result #,(get-weak-box-static-infos)))
  (make-weak-box v))

(define-annotation-syntax WeakBox (identifier-annotation weak-box? #,(get-weak-box-static-infos)))

(define/arity (WeakBox.value wb)
  #:primitive (weak-box-value)
  (weak-box-value wb))
