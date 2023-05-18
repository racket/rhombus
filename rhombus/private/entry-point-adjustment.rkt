#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "provide.rkt"
         "class-primitive.rkt"
         "function-arity-key.rkt"
         "ref-result-key.rkt"
         (submod "list.rkt" for-compound-repetition)
         (submod "syntax-object.rkt" for-quasiquote)
         (prefix-in rkt: (for-template "entry-point.rkt")))

(provide (for-spaces (rhombus/namespace
                      #f
                      rhombus/bind
                      rhombus/annot)
                     entry_point_meta.Adjustment)
         (for-syntax (rename-out [rkt:entry_point_meta.Adjustment-static-infos
                                  entry_point_meta.Adjustment-static-infos])))

(define-primitive-class entry_point_meta.Adjustment rkt:entry_point_meta.Adjustment
  #:constructor-static-info ()
  #:existing
  #:transparent
  #:fields
  ([(prefix-arguments prefix_arguments) ((#%ref-result #,syntax-static-infos)
                                         #,@list-static-infos)]
   [(wrap-body wrap_body) ((#%function-arity 4))]
   [(method? is_method) ()])
  #:properties
  ()
  #:methods
  ())
