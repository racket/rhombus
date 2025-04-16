#lang racket/base
(require (for-syntax racket/base)
         "provide.rkt"
         "annot-context.rkt"
         "class-primitive.rkt"
         "function-arity-key.rkt"
         "index-result-key.rkt"
         (submod "list.rkt" for-compound-repetition)
         (submod "map.rkt" for-info))

(provide (for-spaces (rhombus/namespace
                      #f
                      rhombus/bind
                      rhombus/annot)
                     annot_meta.Context
                     annot_meta.Dependencies)
         (for-syntax get-annotation-context-static-infos))

(define-primitive-class annot_meta.Context annotation-context
  #:existing
  #:transparent #:no-primitive
  #:fields
  ([(argument_names argument-names) #,(get-map-static-infos)]
   [(this_position this-pos)])
  #:namespace-fields
  ([empty empty-annot-context])
  #:properties
  ()
  #:methods
  ())

(define-primitive-class annot_meta.Dependencies annotation-dependencies
  #:existing
  #:transparent #:no-primitive
  #:fields
  ([(arguments args) #,(get-list-static-infos)]
   [(keyword_arguments args) #,(get-map-static-infos)]
   [(has_more_arguments rest?)]
   [(has_more_keyword_arguments kw-rest?)])
  #:properties
  ()
  #:methods
  ())
