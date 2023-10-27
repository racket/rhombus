#lang racket/base
(require (for-syntax racket/base)
         "provide.rkt"
         "binding.rkt"
         (submod "annotation.rkt" for-class)
         "static-info.rkt"
         "define-arity.rkt"
         "call-result-key.rkt"
         "composite.rkt"
         "mutability.rkt"
         "class-primitive.rkt"
         "rhombus-primitive.rkt")

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

(define-primitive-class Box box
  #:lift-declaration
  #:no-constructor-static-info
  #:existing
  #:opaque
  #:fields ()
  #:namespace-fields
  (now_of
   later_of
   )
  #:properties
  ;; TODO undocumented (as function)
  ([value Box.value #:mutator Box.value
          (lambda (e)
            (syntax-local-static-info e #'unbox))]
   )
  #:methods
  ())

(set-primitive-contract! 'box? "Box")
(set-primitive-contract! '(and/c box? (not/c immutable?)) "MutableBox")

(define/arity (Box v)
  #:inline
  #:static-infos ((#%call-result #,box-static-infos))
  (box v))

(define/arity Box.value
  #:inline
  #:primitive (unbox set-box!)
  (case-lambda
    [(b) (unbox b)]
    [(b v) (set-box! b v)]))

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
