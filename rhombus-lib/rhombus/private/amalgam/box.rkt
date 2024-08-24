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
   later_of)
  #:properties
  ([value Box.value #:mutator Box.value
          (lambda (e)
            (syntax-local-static-info e #'unbox))])
  #:methods
  ())

(define/arity (Box v)
  #:inline
  #:static-infos ((#%call-result #,(get-box-static-infos)))
  (box v))

(define/arity Box.value
  #:inline
  #:primitive (unbox set-box!)
  (case-lambda
    [(b) (unbox b)]
    [(b v) (set-box! b v)]))

(define-binding-syntax Box
  (binding-transformer
   (lambda (stx)
     (composite-binding-transformer stx
                                    "Box"
                                    #'box?
                                    #:static-infos (get-box-static-infos)
                                    (list #'unbox)
                                    (list #'())))))

(define-annotation-constructor (Box now_of)
  ()
  #'box? #,(get-box-static-infos)
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
  #'box? #,(get-box-static-infos)
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
  (raise-binding-failure 'Box (string-append what " value") v annot-str))

(set-primitive-subcontract! '(box? (not/c immutable?)) 'mutable-box?)
(set-primitive-contract! 'mutable-box? "MutableBox")
(define-annotation-syntax MutableBox (identifier-annotation mutable-box? #,(get-box-static-infos)))
(define-annotation-syntax ImmutableBox (identifier-annotation immutable-box? #,(get-box-static-infos)))
