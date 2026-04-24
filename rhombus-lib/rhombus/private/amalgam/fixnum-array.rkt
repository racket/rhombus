#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "srcloc.rkt"
                     "annot-context.rkt")
         racket/treelist
         racket/fixnum
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
                     fixnum.Array))

(module+ for-builtin
  (provide fxvector-method-table))

(define-primitive-class fixnum.Array fxvector
  #:lift-declaration
  #:constructor-arity -1
  #:instance-static-info ((#%index-get fixnum.Array.get)
                          (#%index-set fixnum.Array.set)
                          (#%index-result #,(get-fixnum-static-infos))
                          (#%contains fixnum.Array.contains)
                          (#%sequence-constructor fixnum.Array.to_sequence/optimize))
  #:existing
  #:opaque
  #:fields ()
  #:namespace-fields
  ([make fixnum.Array.make])
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

(define-syntax fixnum.Array
  (expression-repeatable-transformer
   (lambda (stx)
     (syntax-parse stx
       [(form-id . tail) (values (relocate-id #'form-id #'fxvector) #'tail)]))))

(define-annotation-syntax fixnum.Array
  (identifier-annotation fxvector? #,(get-fxvector-static-infos)))

(define (check-fxvector who v)
  (unless (fxvector? v)
    (raise-annotation-failure who v "fixnum.Array")))

(define/method (fixnum.Array.get v i)
  #:primitive (fxvector-ref)
  #:static-infos ((#%call-result #,(get-fixnum-static-infos)))
  (fxvector-ref v i))

(define/method (fixnum.Array.set v i x)
  #:primitive (fxvector-set!)
  (fxvector-set! v i x))

(define/method (fixnum.Array.contains v i)
  (check-fxvector who v)
  (and (fixnum? i)
       (for/or ([fx (in-fxvector v)])
         (fx= fx i))))

(define/arity fixnum.Array.make
  #:primitive (make-fxvector)
  #:static-infos ((#%call-result #,(get-fxvector-static-infos)))
  (case-lambda
    [(len) (make-fxvector len)]
    [(len val) (make-fxvector len val)]))

(define/method (fixnum.Array.length v)
  #:primitive (fxvector-length)
  (fxvector-length v))

(define/method fixnum.Array.copy
  #:primitive (fxvector-copy)
  #:static-infos ((#%call-result #,(get-fxvector-static-infos)))
  (case-lambda
    [(v) (fxvector-copy v)]
    [(v start) (fxvector-copy v start)]
    [(v start end) (fxvector-copy v start end)]))

(define/method (fixnum.Array.to_list v)
  #:static-infos ((#%call-result ((#%index-result #,(get-fixnum-static-infos))
                                  #,@(get-treelist-static-infos))))
  (check-fxvector who v)
  (for/treelist ([fx (in-fxvector v)])
    fx))

(define-sequence-syntax fixnum.Array.to_sequence/optimize
  (lambda () #'fixnum.Array.to_sequence)
  (lambda (stx)
    (syntax-parse stx
      [[(id) (_ arr-expr)]
       #`[(id) (in-fxvector #,(discard-static-infos #'arr-expr))]]
      [_ #f])))

(define/method (fixnum.Array.to_sequence v)
  #:primitive (in-fxvector)
  #:static-infos ((#%call-result ((#%seuence-element #,(get-fixnum-static-infos))
                                  (#%sequence-constructor #t))))
  (in-fxvector v))

(void (set-primitive-who! 'fxvector 'fixnum.Array))

