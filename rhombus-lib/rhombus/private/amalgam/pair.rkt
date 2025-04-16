#lang racket/base
(require (for-syntax racket/base
                     "annot-context.rkt")
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

(module+ for-static-info
  (provide (for-syntax get-pair-static-infos)))

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
          (lambda (lhs-si)
            (or (static-info-lookup lhs-si #'car)
                #'()))]
   [rest Pair.rest
         (lambda (lhs-si)
           (or (static-info-lookup lhs-si #'cdr)
                #'()))])
  #:methods
  ())

(define-syntax (select-for-constructor data deps)
  (define args (annotation-dependencies-args deps))
  (define car-si (or (and (< 0 (length args))
                          (list-ref args 0))
                     #'()))
  (define cdr-si (or (and (< 1 (length args))
                          (list-ref args 1))
                     #'()))
  (append
   (if (static-infos-empty? car-si)
       null
       (list #`(car #,car-si)))
   (if (static-infos-empty? cdr-si)
       null
       (list #`(cdr #,cdr-si)))))

(define/arity #:name Pair (Pair.cons a d)
  #:static-infos ((#%call-result ((#%dependent-result (select-for-constructor #f))
                                  #,@(get-pair-static-infos))))
  (cons a d))

(define-syntax (select-field data deps)
  (define args (annotation-dependencies-args deps))
  (define pr-i 0)
  (or (static-info-lookup (or (and (< pr-i (length args))
                                   (list-ref args pr-i))
                              #'())
                          data)
      #'()))

(define/arity (Pair.first p)
  #:primitive (car)
  #:static-infos ((#%call-result ((#%dependent-result (select-field car)))))
  (car p))

(define/arity (Pair.rest p)
  #:primitive (cdr)
  #:static-infos ((#%call-result ((#%dependent-result (select-field cdr)))))
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

(define-syntax (pair-of-static-infos data static-infoss)
  #`((car #,(car static-infoss))
     (cdr #,(cadr static-infoss))))

(define-annotation-constructor (Pair of)
  ()
  #'pair? #,(get-pair-static-infos)
  2
  #f
  (lambda (predicate-stxs)
    #`(let ([car-pred #,(car predicate-stxs)]
            [cdr-pred #,(cadr predicate-stxs)])
        (lambda (arg)
          (and (car-pred (car arg))
               (cdr-pred (cdr arg))))))
  #'pair-of-static-infos #f
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
