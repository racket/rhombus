#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "srcloc.rkt")
         (only-in racket/base
                  [vector array])
         "provide.rkt"
         "expression.rkt"
         "binding.rkt"
         (submod "annotation.rkt" for-class)
         "static-info.rkt"
         "define-arity.rkt"
         "index-key.rkt"
         "append-key.rkt"
         "call-result-key.rkt"
         "index-result-key.rkt"
         "sequence-constructor-key.rkt"
         "composite.rkt"
         "op-literal.rkt"
         "reducer.rkt"
         "parens.rkt"
         "parse.rkt"
         "realm.rkt"
         "mutability.rkt"
         "vector-append.rkt"
         "class-primitive.rkt"
         "rhombus-primitive.rkt")

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

(define-primitive-class Array array
  #:lift-declaration
  #:constructor-arity -1
  #:instance-static-info ((#%index-get Array.get)
                          (#%index-set Array.set)
                          (#%append Array.append)
                          (#%sequence-constructor in-vector))
  #:existing
  #:opaque
  #:fields ()
  #:namespace-fields
  ([make Array.make]
   [of now_of] ;; TEMPORARY
   now_of
   later_of
   )
  #:properties
  ()
  #:methods
  (length
   copy
   copy_from
   ))

(define-syntax Array
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       [(form-id . tail) (values (relocate+reraw #'form-id #'array) #'tail)]))))

(define-annotation-constructor (Array now_of)
  () #'vector? array-static-infos
  1
  #f
  (lambda (arg-id predicate-stxs)
    #`(for/and ([e (in-vector #,arg-id)])
        (#,(car predicate-stxs) e)))
  (lambda (static-infoss)
    ;; no static info, since mutable and content is checked only initially
    #'())
  "converter annotation not supported for elements;\n immediate checking needs a predicate annotation for the array content" #'())

(define-annotation-constructor (Array/again later_of)
  () #'vector? array-static-infos
  1
  #f
  (lambda (predicate-stxes annot-strs)
    (define (make-reelementer what)
      #`(lambda (vec idx v)
          (unless (pred v)
            (raise-reelementer-error '#,what idx v '#,(car annot-strs)))
          v))
    #`(lambda (vec)
        (let ([pred #,(car predicate-stxes)])
          (chaperone-vector vec
                            #,(make-reelementer "current")
                            #,(make-reelementer "new")))))
  (lambda (static-infoss)
    #`((#%index-result #,(car static-infoss))))
  #'array-build-convert #'()
  #:parse-of parse-annotation-of/chaperone)

(define-syntax (array-build-convert arg-id build-convert-stxs kws data)
  (with-syntax ([[(annot-str . _) _] data])
    (define (make-reelementer what)
      #`(lambda (vec idx val)
          (cvt
           val
           (lambda (v) v)
           (lambda ()
             (raise-reelementer-error '#,what idx val 'annot-str)))))
    #`(let ([cvt #,(car build-convert-stxs)])
        (impersonate-vector #,arg-id
                            #,(make-reelementer "current")
                            #,(make-reelementer "new")))))

(define (raise-reelementer-error what idx v annot-str)
  (raise-binding-failure
   'Array (string-append what " element") v annot-str
   "position" (unquoted-printing-string (number->string idx))))

(define-annotation-syntax MutableArray (identifier-annotation #'mutable-vector? array-static-infos))
(define-annotation-syntax ImmutableArray (identifier-annotation #'immutable-vector? array-static-infos))

(define-reducer-syntax Array
  (reducer-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ #:length e ...+)
        ;; If `e` ends with parentheses after at least one term, then
        ;; let those parentheses be for the initial `each` shorthand,
        ;; instead of treating it as part of the length expression
        (define (finish len-g tail)
          (values (reducer/no-break #'build-array-reduce
                                    #'([i 0])
                                    #'build-array-assign
                                    array-static-infos
                                    #`[dest
                                       (rhombus-expression #,len-g)
                                       i])
                  tail))
        (syntax-parse #'(e ...)
          [(e ...+ (~and p (_::parens . _)))
           (finish #'(group e ...) #'(p))]
          [_
           (finish #'(group e ...) #'())])]
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

(set-primitive-contract! 'vector? "Array")
(set-primitive-contract! '(and/c vector? (not/c immutable?)) "MutableArray")

(define/arity (Array.get v i)
  #:inline
  #:primitive (vector-ref)
  (vector-ref v i))

(define/arity (Array.set v i x)
  #:inline
  #:primitive (vector-set!)
  (vector-set! v i x))

(define (check-array who v)
  (unless (vector? v)
    (raise-argument-error* who rhombus-realm "Array" v)))

(define/arity (Array.append v1 v2)
  #:static-infos ((#%call-result #,array-static-infos))
  (check-array who v1)
  (check-array who v2)
  (vector-append v1 v2))

(define/arity Array.make
  #:inline
  #:primitive (make-vector)
  #:static-infos ((#%call-result #,array-static-infos))
  (case-lambda
    [(len) (make-vector len)]
    [(len val) (make-vector len val)]))

(define/method (Array.length v)
  #:inline
  #:primitive (vector-length)
  (vector-length v))

(define/method (Array.copy v)
  #:static-infos ((#%call-result #,array-static-infos))
  (check-array who v)
  (define len (vector-length v))
  (define new-v (make-vector len))
  (vector-copy! new-v 0 v 0 len)
  new-v)

(define/method Array.copy_from
  #:inline
  #:primitive (vector-copy!)
  (case-lambda
    [(v dest-start src) (vector-copy! v dest-start src)]
    [(v dest-start src src-start) (vector-copy! v dest-start src src-start)]
    [(v dest-start src src-start src-end) (vector-copy! v dest-start src src-start src-end)]))

(define-binding-syntax Array
  (binding-prefix-operator
   (bind-quote Array)
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

(define (vector-suffix->list v start)
  (for/list ([i (in-range start (vector-length v))])
    (vector-ref v i)))
