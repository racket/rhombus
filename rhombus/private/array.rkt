#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     syntax/stx)
         "provide.rkt"
         "expression.rkt"
         "binding.rkt"
         (submod "annotation.rkt" for-class)
         (submod "dot.rkt" for-dot-provider)
         "static-info.rkt"
         "map-ref-set-key.rkt"
         "call-result-key.rkt"
         "ref-result-key.rkt"
         "function-arity-key.rkt"
         "composite.rkt"
         "name-root.rkt"
         "dot-parse.rkt"
         "reducer.rkt"
         "parens.rkt"
         "parse.rkt")

(provide (for-spaces (rhombus/namespace
                      #f
                      rhombus/bind
                      rhombus/annot
                      rhombus/reducer)
                     Array))

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
   [length vector-length]
   of))

(define-syntax Array
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ . tail) (values #'vector #'tail)]))))

(define-annotation-constructor (Array of)
  () #'vector? array-static-infos
  1
  (#f)
  (lambda (arg-id predicate-stxs)
    #`(for/and ([e (in-vector #,arg-id)])
        (#,(car predicate-stxs) e)))
  (lambda (static-infoss)
    #`((#%ref-result #,(car static-infoss)))))

(define-static-info-syntax vector
  (#%call-result #,array-static-infos)
  (#%function-arity -1))

(define-reducer-syntax Array
  (reducer-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ #:length e ...+)
        #`[(array-reduce-wrap
            dest
            (rhombus-expression (group e ...)))
           ([i 0])
           ((lambda (v) (vector-set! dest i v) (add1 i)))
           #,array-static-infos]]
       [(_)
        #`[(reverse-list->vector)
           ([accum null])
           ((lambda (v) (cons v accum)))
           #,array-static-infos]]))))

(define (reverse-list->vector l)
  (list->vector (reverse l)))

(define-syntax array-reduce-wrap
  (lambda (stx)
    (syntax-parse stx
      [(_ dest-id len-expr body)
       #'(let ([dest-id (make-vector len-expr)])
           body
           dest-id)])))

(define-syntax array-instance
  (dot-provider-more-static
   (dot-parse-dispatch
    (lambda (field-sym field ary 0ary nary fail-k)
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
       [(form-id (tag::parens arg ...) . tail)
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

(define-static-info-syntax make-vector
  (#%call-result #,array-static-infos)
  (#%function-arity 6))

(define-static-info-syntax vector-length
  (#%function-arity 2))
