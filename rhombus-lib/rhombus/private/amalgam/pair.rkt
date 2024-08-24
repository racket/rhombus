#lang racket/base
(require (for-syntax racket/base)
         "provide.rkt"
         "composite.rkt"
         "binding.rkt"
         (submod "annotation.rkt" for-class)
         "static-info.rkt"
         "call-result-key.rkt"
         "define-arity.rkt"
         "class-primitive.rkt"
         "rhombus-primitive.rkt")

(provide (for-spaces (rhombus/namespace
                      rhombus/annot)
                     Pair)
         (for-spaces (#f
                      rhombus/bind
                      rhombus/statinfo)
                     (rename-out [Pair.cons Pair])))

(module+ for-builtin
  (provide pair-method-table))

(define-primitive-class Pair pair
  #:lift-declaration
  #:no-constructor-static-info
  #:existing
  #:opaque
  #:fields ()
  #:namespace-fields
  ([cons Pair.cons]
   of)
  #:properties
  ([first Pair.first
          (lambda (e)
            (syntax-local-static-info e #'car))]
   [rest Pair.rest
         (lambda (e)
           (syntax-local-static-info e #'cdr))])
  #:methods
  ())

(define/arity #:name Pair (Pair.cons a d)
  #:inline
  #:static-infos ((#%call-result #,(get-pair-static-infos)))
  (cons a d))

(define/arity (Pair.first p)
  #:inline
  #:primitive (car)
  (car p))

(define/arity (Pair.rest p)
  #:inline
  #:primitive (cdr)
  (cdr p))

(define-binding-syntax Pair.cons
  (binding-transformer
   (lambda (tail)
     (composite-binding-transformer tail
                                    "Pair"
                                    #'pair?
                                    #:static-infos (get-pair-static-infos)
                                    (list #'car #'cdr)
                                    (list #'() #'())
                                    #:accessor->info? #t))))

(define-annotation-constructor (Pair of)
  ()
  #'pair? #,(get-pair-static-infos)
  2
  #f
  (lambda (arg-id predicate-stxs)
    #`(and (#,(car predicate-stxs) (car #,arg-id))
           (#,(cadr predicate-stxs) (cdr #,arg-id))))
  (lambda (static-infoss)
    #`((car #,(car static-infoss))
       (cdr #,(cadr static-infoss))))
  #'pair-build-convert #'())

(define-syntax (pair-build-convert arg-id build-convert-stxs kws data)
  #`(#,(car build-convert-stxs)
     (car #,arg-id)
     (lambda (a)
       (#,(cadr build-convert-stxs)
        (cdr #,arg-id)
        (lambda (d) (cons a d))
        (lambda () #f)))
     (lambda () #f)))
