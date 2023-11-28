#lang racket/base
(require (for-syntax racket/base)
         "provide.rkt"
         "binding.rkt"
         (submod "annotation.rkt" for-class)
         "static-info.rkt"
         "define-arity.rkt"
         "call-result-key.rkt"
         "composite.rkt"
         "realm.rkt"
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
  "converter annotation not supported for value;\n immediate checking needs a predicate annotation for the box content" #'())

(define-annotation-constructor (Box/again later_of)
  ()
  #'box? box-static-infos
  1
  #f
  (lambda (predicate-stxes annot-strs)
    (define (make-reboxer what)
      #`(lambda (bx v)
          (unless (pred v)
            (raise-reboxer-error '#,what v '#,(car annot-strs)))
          v))
    #`(lambda (bx)
        (let ([pred #,(car predicate-stxes)])
          (chaperone-box bx
                         #,(make-reboxer "current")
                         #,(make-reboxer "new")))))
  (lambda (static-infoss)
    #`((unbox #,(car static-infoss))))
  #'box-build-convert #'()
  #:parse-of parse-annotation-of/chaperone)

(define-syntax (box-build-convert arg-id build-convert-stxs kws data)
  (with-syntax ([[(annot-str . _) _] data])
    (define (make-reboxer what)
      #`(lambda (bx val)
          (cvt
           val
           (lambda (v) v)
           (lambda ()
             (raise-reboxer-error '#,what val 'annot-str)))))
    #`(let ([cvt #,(car build-convert-stxs)])
        (impersonate-box #,arg-id
                         #,(make-reboxer "current")
                         #,(make-reboxer "new")))))

(define (raise-reboxer-error what v annot-str)
  (raise-arguments-error*
   'Box rhombus-realm
   (string-append what " value does not satisfy the box's annotation")
   "value" v
   "annotation" (unquoted-printing-string
                 (error-contract->adjusted-string
                  annot-str
                  rhombus-realm))))

(define-annotation-syntax MutableBox (identifier-annotation #'mutable-box? box-static-infos))
(define-annotation-syntax ImmutableBox (identifier-annotation #'immutable-box? box-static-infos))
