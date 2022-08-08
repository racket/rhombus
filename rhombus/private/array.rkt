#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/stx)
         "binding.rkt"
         (submod "annotation.rkt" for-class)
         "static-info.rkt"
         "map-ref-set-key.rkt"
         "call-result-key.rkt"
         "composite.rkt")

(provide Array
         (for-space rhombus/binding Array)
         (for-space rhombus/annotation Array)
         (for-space rhombus/static-info Array))

(define Array vector)

(define-annotation-constructor Array
  () #'vector? #'((#%map-ref vector-ref)
                  (#%map-set! vector-set!))
  1
  (lambda (arg-id predicate-stxs)
    #`(for/and ([e (in-vector #,arg-id)])
        (#,(car predicate-stxs) e)))
  (lambda (static-infoss)
    #`((#%ref-result #,(car static-infoss)))))

(define-static-info-syntax Array
  (#%call-result ((#%map-ref vector-ref)
                  (#%map-set! vector-set!)
                  (#%sequence-constructor in-vector))))

(define-binding-syntax Array
  (binding-prefix-operator
   #'Array
   '((default . stronger))
   'macro
   (lambda (stx)
     (syntax-parse stx
       [(form-id ((~and tag (~datum parens)) arg ...) . tail)
        (define args (syntax->list #'(arg ...)))
        (define len (length args))
        (define pred #`(lambda (v)
                         (and (vector? v)
                              (= (vector-length v) #,len))))
        ((make-composite-binding-transformer "Array"
                                             pred
                                             (for/list ([arg (in-list args)]
                                                        [i (in-naturals)])
                                               #`(lambda (v) (vector-ref v #,i)))
                                             (for/list ([arg (in-list args)])
                                               #'())
                                             #:ref-result-info? #t)
         stx)]))))
