#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "srcloc.rkt")
         "provide.rkt"
         "expression.rkt"
         "binding.rkt"
         (submod "annotation.rkt" for-class)
         (submod "dot.rkt" for-dot-provider)
         "static-info.rkt"
         "define-arity.rkt"
         "index-key.rkt"
         "call-result-key.rkt"
         "index-result-key.rkt"
         "composite.rkt"
         "op-literal.rkt"
         "name-root.rkt"
         "dot-parse.rkt"
         "realm.rkt"
         "mutability.rkt")

(provide (for-spaces (rhombus/namespace
                      #f
                      rhombus/bind
                      rhombus/annot
                      rhombus/statinfo)
                     Box)
         (for-space rhombus/annot
                    MutableBox
                    ImmutableBox))

(module+ for-builtin
  (provide box-method-table
           box-mutator-method-table))

(define-for-syntax box-static-infos
  #'((#%dot-provider box-instance)))

(define-name-root Box
  #:fields
  (now_of
   later_of))

(define/arity (Box v)
  #:static-infos ((#%call-result #,box-static-infos))
  (box v))

(define-binding-syntax Box
  (binding-transformer
   (make-composite-binding-transformer "Box"
                                       #'box?
                                       #:static-infos box-static-infos
                                       (list #'unbox)
                                       (list #'()))))

(define-annotation-constructor (Box now_of)
  ()
  #'box? box-static-infos
  1
  #f
  (lambda (arg-id predicate-stxs)
    #`(#,(car predicate-stxs) (unbox #,arg-id)))
  (lambda (static-infoss)
    ;; no static info, since mutable and content is checked only initially
    #'())
  "converting annotation not supported for value;\n immediate checking needs a predicate annotation for the box content" #'())

(define-annotation-constructor (Box/again later_of)
  ()
  #'box? box-static-infos
  1
  #f
  #f ;; no predicate maker, so uses converter builder
  (lambda (static-infoss)
    #`((unbox #,(car static-infoss))))
  #'box-build-convert #'())

(define-syntax (box-build-convert arg-id build-convert-stxs kws data)
  #`(let ([cvt (lambda (v success-k fail-k)
                 (#,(car build-convert-stxs) v success-k fail-k))])
      (impersonate-box #,arg-id
                       (make-reboxer cvt "current ")
                       (make-reboxer cvt "new "))))

(define (make-reboxer cvt prefix)
  (lambda (bx val)
    (cvt
     val
     (lambda (v) val)
     (lambda ()
       (raise-arguments-error 'Box (string-append prefix "value does not satisfy the box's annotation")
                              "value" val)))))

(define-annotation-syntax MutableBox (identifier-annotation #'mutable-box? box-static-infos))
(define-annotation-syntax ImmutableBox (identifier-annotation #'immutable-box? box-static-infos))

(define box-method-table
  (hash 'value unbox))

(define box-mutator-method-table
  (hash 'value set-box!))

(define-syntax box-instance
  (dot-provider
   (dot-parse-dispatch
    (lambda (field-sym field ary 0ary nary fail-k)
      (case field-sym
        [(value) (field (lambda (x reloc) (add-info (reloc #`(unbox #,x)) x #'unbox))
                        (lambda (x v reloc) (reloc #`(set-box! #,x #,v))))]
        [else (fail-k)])))))

(define-for-syntax (add-info e on-e key)
  (define result-static-infos (syntax-local-static-info on-e key))
  (if result-static-infos
      (wrap-static-info* e result-static-infos)
      e))
