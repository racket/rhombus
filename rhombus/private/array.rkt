#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/stx)
         "expression.rkt"
         "binding.rkt"
         (submod "annotation.rkt" for-class)
         (submod "dot.rkt" for-dot-provider)
         "static-info.rkt"
         "map-ref-set-key.rkt"
         "call-result-key.rkt"
         "composite.rkt"
         "name-root.rkt"
         "dot-parse.rkt")

(provide Array
         (for-space rhombus/binding Array)
         (for-space rhombus/annotation Array))

(module+ for-builtin
  (provide array-method-table))

(define array-method-table
  (hash 'length (method1 vector-length)))

(define-for-syntax array-static-infos
  #'((#%map-ref vector-ref)
     (#%map-set! vector-set!)
     (#%sequence-constructor in-vector)
     (#%dot-provider array-instance)))

(define-name-root Array
  #:fields
  ([make make-vector]
   [length vector-length])
  #:root
  (expression-transformer
   #'Array
   (lambda (stx)
     (syntax-parse stx
       [(_ . tail) (values #'vector #'tail)]))))

(define-annotation-constructor Array
  () #'vector? array-static-infos
  1
  (lambda (arg-id predicate-stxs)
    #`(for/and ([e (in-vector #,arg-id)])
        (#,(car predicate-stxs) e)))
  (lambda (static-infoss)
    #`((#%ref-result #,(car static-infoss)))))

(define-static-info-syntax vector
  (#%call-result #,array-static-infos))

(define-syntax array-instance
  (dot-provider-more-static
   (dot-parse-dispatch
    (lambda (field-sym ary 0ary nary fail-k)
      (case field-sym
        [(length) (0ary #'vector-length)]
        [else #f])))))

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
