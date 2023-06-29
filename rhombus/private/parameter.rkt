#lang racket/base
(require (for-syntax racket/base)
         "provide.rkt"
         (submod "annotation.rkt" for-class)
         "define-arity.rkt"
         "name-root.rkt"
         "function-arity-key.rkt"
         "call-result-key.rkt")

(provide (for-spaces (rhombus/namespace
                      rhombus/annot)
                     Parameter))

(define-for-syntax parameter-static-infos
  #`((#%function-arity 3)))

(define/arity (Parameter.make v
                              #:guard [guard #f]
                              #:name [name 'parameter])
  #:static-infos ((#%call-result #,parameter-static-infos))
  (make-parameter v guard name))

(define-name-root Parameter
  #:fields
  ([make Parameter.make]))

(define-annotation-syntax Parameter
  (identifier-annotation #'parameter? parameter-static-infos))
