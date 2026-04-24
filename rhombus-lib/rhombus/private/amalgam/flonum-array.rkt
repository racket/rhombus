#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "srcloc.rkt"
                     "annot-context.rkt")
         racket/treelist
         racket/flonum
         "provide.rkt"
         "expression.rkt"
         (submod "annotation.rkt" for-class)
         "static-info.rkt"
         "define-arity.rkt"
         "index-key.rkt"
         "call-result-key.rkt"
         "index-result-key.rkt"
         "sequence-constructor-key.rkt"
         "sequence-element-key.rkt"
         "contains-key.rkt"
         "composite.rkt"
         "class-primitive.rkt"
         "rhombus-primitive.rkt"
         (submod "list.rkt" for-compound-repetition)
         (submod "arithmetic.rkt" static-infos))

(provide (for-spaces (rhombus/namespace
                      #f
                      rhombus/annot)
                     flonum.Array))

(module+ for-builtin
  (provide flvector-method-table))

(define-primitive-class flonum.Array flvector
  #:lift-declaration
  #:constructor-arity -1
  #:instance-static-info ((#%index-get flonum.Array.get)
                          (#%index-set flonum.Array.set)
                          (#%index-result #,(get-flonum-static-infos))
                          (#%contains flonum.Array.contains)
                          (#%sequence-constructor flonum.Array.to_sequence/optimize))
  #:existing
  #:opaque
  #:fields ()
  #:namespace-fields
  ([make flonum.Array.make])
  #:properties
  ()
  #:methods
  (length
   get
   set
   contains
   copy
   to_list
   to_sequence))

(define-syntax flonum.Array
  (expression-repeatable-transformer
   (lambda (stx)
     (syntax-parse stx
       [(form-id . tail) (values (relocate-id #'form-id #'flvector) #'tail)]))))

(define-annotation-syntax flonum.Array
  (identifier-annotation flvector? #,(get-flvector-static-infos)))

(define (check-flvector who v)
  (unless (flvector? v)
    (raise-annotation-failure who v "flonum.Array")))

(define/method (flonum.Array.get v i)
  #:primitive (flvector-ref)
  #:static-infos ((#%call-result #,(get-flonum-static-infos)))
  (flvector-ref v i))

(define/method (flonum.Array.set v i x)
  #:primitive (flvector-set!)
  (flvector-set! v i x))

(define/method (flonum.Array.contains v i)
  (check-flvector who v)
  (and (flonum? i)
       (for/or ([fl (in-flvector v)])
         (fl= fl i))))

(define/arity flonum.Array.make
  #:primitive (make-flvector)
  #:static-infos ((#%call-result #,(get-flvector-static-infos)))
  (case-lambda
    [(len) (make-flvector len)]
    [(len val) (make-flvector len val)]))

(define/method (flonum.Array.length v)
  #:primitive (flvector-length)
  (flvector-length v))

(define/method flonum.Array.copy
  #:primitive (flvector-copy)
  #:static-infos ((#%call-result #,(get-flvector-static-infos)))
  (case-lambda
    [(v) (flvector-copy v)]
    [(v start) (flvector-copy v start)]
    [(v start end) (flvector-copy v start end)]))

(define/method (flonum.Array.to_list v)
  #:static-infos ((#%call-result ((#%index-result #,(get-flonum-static-infos))
                                  #,@(get-treelist-static-infos))))
  (check-flvector who v)
  (for/treelist ([fl (in-flvector v)])
    fl))

(define-sequence-syntax flonum.Array.to_sequence/optimize
  (lambda () #'flonum.Array.to_sequence)
  (lambda (stx)
    (syntax-parse stx
      [[(id) (_ arr-expr)]
       #`[(id) (in-flvector #,(discard-static-infos #'arr-expr))]]
      [_ #f])))

(define/method (flonum.Array.to_sequence v)
  #:primitive (in-flvector)
  #:static-infos ((#%call-result ((#%seuence-element #,(get-flonum-static-infos))
                                  (#%sequence-constructor #t))))
  (in-flvector v))

(void (set-primitive-who! 'flvector 'flonum.Array))

