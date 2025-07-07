#lang racket/base
(require (for-syntax racket/base
                     "annot-context.rkt")
         racket/mutability
         "provide.rkt"
         "binding.rkt"
         (submod "annotation.rkt" for-class)
         "static-info.rkt"
         "define-arity.rkt"
         "call-result-key.rkt"
         "composite.rkt"
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
          (lambda (lhs-si)
            (or (static-info-lookup lhs-si #'unbox)
                #'()))])
  #:methods
  (copy
   snapshot))

(define/arity (Box v)
  #:static-infos ((#%call-result #,(get-box-static-infos)))
  (box v))

(define-syntax (select-value data deps)
  (define args (annotation-dependencies-args deps))
  (define bx-i 0)
  (define si
    (or (static-info-lookup (or (and (< bx-i (length args))
                                     (list-ref args bx-i))
                                #'())
                            #'unbox)
        #'()))
  (cond
    [(or (null? si)
         (and (syntax? si) (null? (syntax-e si))))
     #'()]
    [else
     (case (syntax-e data)
       [(box) #`((unbox #,si))]
       [else si])]))

(define/arity Box.value
  #:primitive (unbox set-box!)
  #:static-infos ((#%call-result (#:at_arities
                                  ([2 ((#%dependent-result (select-value value)))]))))
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

(define-syntax (no-of-static-infos data static-infoss)
  #`())

(define-annotation-constructor (Box now_of)
  ()
  #'box? #,(get-box-static-infos)
  1
  #f
  (lambda (predicate-stxs)
    #`(let ([pred #,(car predicate-stxs)])
        (lambda (arg)
          (pred (unbox arg)))))
  ;; no static info, since mutable and content is checked only initially
  #'no-of-static-infos #f
  "converter annotation not supported for value;\n immediate checking needs a predicate annotation for the box content" #'())

(define-syntax (box-later-of-static-infos data static-infoss)
  #`((unbox #,(car static-infoss))))

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
    #`(let ([pred #,(car predicate-stxes)])
        (lambda (bx)
          (chaperone-box bx
                         #,(make-reboxer "current")
                         #,(make-reboxer "new")))))
  #'box-later-of-static-infos #f
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

(void (set-primitive-contract! '(and/c box? (not/c immutable?)) "MutableBox"))
(void (set-primitive-contract! 'mutable-box? "MutableBox"))
(define-annotation-syntax MutableBox (identifier-annotation mutable-box? #,(get-box-static-infos)))
(define-annotation-syntax ImmutableBox (identifier-annotation immutable-box? #,(get-box-static-infos)))

(define (check-box who bx)
  (unless (box? bx)
    (raise-annotation-failure who bx "Box")))

(define/method (Box.copy bx)
  #:static-infos ((#%call-result #,(get-box-static-infos)))
  (check-box who bx)
  (box (unbox bx)))

(define/method (Box.snapshot bx)
  #:static-infos ((#%call-result ((#%dependent-result (select-value box))
                                  #,@(get-box-static-infos))))
  (check-box who bx)
  (if (immutable-box? bx)
      bx
      (box-immutable (unbox bx))))
