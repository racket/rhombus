#lang racket/base
(require (for-syntax racket/base)
         "provide.rkt"
         "entry-point-adjustment.rkt"
         "class-primitive.rkt"
         "function-arity-key.rkt"
         "index-result-key.rkt"
         (submod "list.rkt" for-compound-repetition)
         (submod "syntax-object.rkt" for-quasiquote))

(provide (for-spaces (rhombus/namespace
                      #f
                      rhombus/bind
                      rhombus/annot)
                     entry_point_meta.Adjustment)
         (for-syntax get-entry-point-adjustment-static-infos))

(define-primitive-class entry_point_meta.Adjustment entry-point-adjustment
  #:existing
  #:transparent
  #:fields
  ([(prefix_arguments prefix-arguments) ((#%index-result #,(get-syntax-static-infos))
                                         . #,(get-treelist-static-infos))]
   [(wrap_body wrap-body) ((#%function-arity 4))]
   [(is_method method?)])
  #:properties
  ()
  #:methods
  ())
