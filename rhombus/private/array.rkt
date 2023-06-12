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
         "define-arity.rkt"
         "index-key.rkt"
         "append-key.rkt"
         "call-result-key.rkt"
         "index-result-key.rkt"
         "function-arity-key.rkt"
         "sequence-constructor-key.rkt"
         "composite.rkt"
         "op-literal.rkt"
         "name-root.rkt"
         "dot-parse.rkt"
         "reducer.rkt"
         "parens.rkt"
         "parse.rkt"
         "realm.rkt"
         "mutability.rkt"
         "vector-append.rkt")

(provide (for-spaces (rhombus/namespace
                      #f
                      rhombus/bind
                      rhombus/annot
                      rhombus/reducer)
                     Array)
         (for-space rhombus/annot
                    MutableArray
                    ImmutableArray))

(module+ for-builtin
  (provide array-method-table))

(define-for-syntax array-static-infos
  #'((#%index-get vector-ref)
     (#%index-set vector-set!)
     (#%append vector-append)
     (#%sequence-constructor in-vector)
     (#%dot-provider array-instance)))

(define-name-root Array
  #:fields
  ([make make-vector]
   [length vector-length]
   copy
   copy_from
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
    #`((#%index-result #,(car static-infoss))))
  #'array-build-convert #'())

(define-syntax (array-build-convert arg-id build-convert-stxs kws data)
  #`(for/fold ([lst '()] #:result (and lst (apply vector (reverse lst))))
              ([v (in-list #,arg-id)])
      #:break (not lst)
      (#,(car build-convert-stxs)
       v
       (lambda (v) (cons v lst))
       (lambda () #f))))

(define-annotation-syntax MutableArray (identifier-annotation #'mutable-vector? array-static-infos))
(define-annotation-syntax ImmutableArray (identifier-annotation #'immutable-vector? array-static-infos))

(define-static-info-syntax vector
  (#%call-result #,array-static-infos)
  (#%function-arity -1))

(define-reducer-syntax Array
  (reducer-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ #:length e ...+)
        (values (reducer/no-break #'build-array-reduce
                                  #'([i 0])
                                  #'build-array-assign
                                  array-static-infos
                                  #'[dest
                                     (rhombus-expression (group e ...))
                                     i])
                '())]
       [(_ . tail)
        (values
         (reducer/no-break #'build-array-reduce-list
                           #'([accum null])
                           #'build-array-cons
                           array-static-infos
                           #'accum)
         #'tail)]))))

(define-syntax (build-array-reduce stx)
  (syntax-parse stx
    [(_ (dest-id len-expr i) body)
     (wrap-static-info* #'(let ([dest-id (make-vector len-expr)])
                            body
                            dest-id)
                        array-static-infos)]))

(define-syntax (build-array-assign stx)
  (syntax-parse stx
    [(_ (dest-id len-expr i) v)
     #'(begin (vector-set! dest-id i v) (add1 i))]))

(define-syntax (build-array-reduce-list stx)
  (syntax-parse stx
    [(_ accum body) #`(list->vector (reverse body))]))

(define-syntax (build-array-cons stx)
  (syntax-parse stx
    [(_ accum v) #'(cons v accum)]))

(define-syntax array-reduce-wrap
  (lambda (stx)
    (syntax-parse stx
      [(_ dest-id len-expr body)
       #'(let ([dest-id (make-vector len-expr)])
           body
           dest-id)])))

(define/arity (copy v)
  #:static-infos ((#%call-result #,array-static-infos))
  (unless (vector? v) (raise-argument-error* 'Vector.copy rhombus-realm "Array" v))
  (define len (vector-length v))
  (define new-v (make-vector len))
  (vector-copy! new-v 0 v 0 len)
  new-v)
(define/arity (copy_from v dest-start src [src-start 0] [src-end (and (vector? src) (vector-length src))])
  #:static-infos ((#%call-result #,array-static-infos))
  (vector-copy! v dest-start src src-start src-end))

(define array-method-table
  (hash 'length (method1 vector-length)
        'copy (method1 copy)
        'copy_from (lambda (vec)
                     (lambda (dest-start src [src-start 0] [src-end (and (vector? src) (vector-length src))])
                       (vector-copy! vec dest-start src src-start src-end)))))

(define-syntax array-instance
  (dot-provider
   (dot-parse-dispatch
    (lambda (field-sym field ary 0ary nary fail-k)
      (case field-sym
        [(length) (0ary #'vector-length)]
        [(copy) (0ary #'copy array-static-infos)]
        [(copy_from) (nary #'copy_from 28 #'copy_from #'())]
        [else (fail-k)])))))

(define-binding-syntax Array
  (binding-prefix-operator
   #'Array
   '((default . stronger))
   'macro
   (lambda (stx)
     (define (build args len pred rest-arg form-id tail)
       ((make-composite-binding-transformer "Array"
                                            pred
                                            (for/list ([arg (in-list args)]
                                                       [i (in-naturals)])
                                              #`(lambda (v) (vector-ref v #,i)))
                                            (for/list ([arg (in-list args)])
                                              #'())
                                            #:static-infos array-static-infos
                                            #:index-result-info? #t
                                            #:rest-accessor (and rest-arg
                                                                 #`(lambda (v) (vector-suffix->list v #,len)))
                                            #:rest-repetition? (and rest-arg #t))
        #`(#,form-id (parens . #,args) . #,tail)
        rest-arg))
     (syntax-parse stx
       [(form-id (tag::parens arg ... rest-arg (group _::...-bind)) . tail)
        (define args (syntax->list #'(arg ...)))
        (define len (length args))
        (define pred #`(lambda (v)
                         (and (vector? v)
                              (>= (vector-length v) #,len))))
        (build args len pred #'rest-arg #'form-id #'tail)]
       [(form-id (tag::parens arg ...) . tail)
        (define args (syntax->list #'(arg ...)))
        (define len (length args))
        (define pred #`(lambda (v)
                         (and (vector? v)
                              (= (vector-length v) #,len))))
        (build args len pred #f #'form-id #'tail)]))))

(define-static-info-syntax make-vector
  (#%call-result #,array-static-infos)
  (#%function-arity 6))

(define-static-info-syntax vector-length
  (#%function-arity 2))

(define-static-info-syntax vector-append
  (#%call-result #,array-static-infos)
  (#%function-arity 4))

(define (vector-suffix->list v start)
  (for/list ([i (in-range start (vector-length v))])
    (vector-ref v i)))
