#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "srcloc.rkt")
         (only-in racket/base
                  [vector array])
         racket/vector
         "treelist.rkt"
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
         "class-primitive.rkt"
         "rhombus-primitive.rkt"
         "version-case.rkt"
         (submod "list.rkt" for-compound-repetition))

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
                          (#%sequence-constructor Array.to_sequence/optimize))
  #:existing
  #:opaque
  #:fields ()
  #:namespace-fields
  ([make Array.make]
   [of now_of] ;; TEMPORARY
   now_of
   later_of
   of_length)
  #:properties
  ()
  #:methods
  (length
   get
   set
   append
   copy
   copy_from
   drop_left
   drop_right
   take_left
   take_right
   set_in_copy
   to_list
   to_sequence))

(define-syntax Array
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       [(form-id . tail) (values (relocate+reraw #'form-id #'array) #'tail)]))))

(define-annotation-constructor (Array now_of)
  () #'vector? #,(get-array-static-infos)
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
  () #'vector? #,(get-array-static-infos)
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

(define-annotation-syntax of_length
  (annotation-prefix-operator
   `((default . stronger))
   'macro
   (lambda (stx)
     (syntax-parse stx
       [(_ (_::parens len-g) . tail)
        (values
         (annotation-predicate-form
          #'(let ([n (rhombus-expression len-g)])
              (check-nonneg-int 'Array.of_length n)
              (lambda (v)
                (and (vector? v)
                     (= (vector-length v) n))))
          (get-array-static-infos))
         #'tail)]))))

(define (check-nonneg-int who v)
  (unless (exact-nonnegative-integer? v)
    (raise-argument-error* who rhombus-realm "NonnegInt" v)))

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

(define-annotation-syntax MutableArray (identifier-annotation mutable-vector? #,(get-array-static-infos)))
(define-annotation-syntax ImmutableArray (identifier-annotation immutable-vector? #,(get-array-static-infos)))

(define-reducer-syntax Array
  (reducer-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ . tail)
        (values
         (reducer/no-break #'build-array-reduce-list
                           #'([accum null])
                           #'build-array-cons
                           (get-array-static-infos)
                           #'accum)
         #'tail)]))))

(define-reducer-syntax of_length
  (reducer-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (group)
       [(_ (_::parens len-g
                      (~optional (group #:fill (_::block fill-g))))
           . tail)
        (values
         (reducer/no-break #'build-array-reduce
                           #'([i 0])
                           #'build-array-assign
                           (get-array-static-infos)
                           #'[dest
                              (rhombus-expression len-g)
                              (~? (rhombus-expression fill-g) 0)
                              i])
         #'tail)]))))

(define-syntax (build-array-reduce stx)
  (syntax-parse stx
    [(_ (dest-id len-expr fill-expr i) body)
     (wrap-static-info* #'(let ([dest-id (let ([len len-expr]
                                               [fill fill-expr])
                                           (check-nonneg-int 'Array.of_length len)
                                           (make-vector len fill))])
                            body
                            dest-id)
                        (get-array-static-infos))]))

(define-syntax (build-array-assign stx)
  (syntax-parse stx
    [(_ (dest-id len-expr fill-expr i) v)
     #'(begin (vector-set! dest-id i v) (add1 i))]))

(define-syntax (build-array-reduce-list stx)
  (syntax-parse stx
    [(_ accum body) #`(list->vector (reverse body))]))

(define-syntax (build-array-cons stx)
  (syntax-parse stx
    [(_ accum v) #'(cons v accum)]))

(set-primitive-contract! 'vector? "Array")
(set-primitive-contract! '(and/c vector? (not/c immutable?)) "MutableArray")

(define/method (Array.get v i)
  #:inline
  #:primitive (vector-ref)
  (vector-ref v i))

(define/method (Array.set v i x)
  #:inline
  #:primitive (vector-set!)
  (vector-set! v i x))

(define (check-array who v)
  (unless (vector? v)
    (raise-argument-error* who rhombus-realm "Array" v)))

(define/method Array.append
  #:inline
  #:primitive (vector-append)
  #:static-infos ((#%call-result #,(get-array-static-infos)))
  (case-lambda
    [() (vector)]
    [(v1) (vector-append v1)]
    [(v1 v2) (vector-append v1 v2)]
    [(v1 v2 v3) (vector-append v1 v2 v3)]
    [args (apply vector-append args)]))

(define/arity Array.make
  #:inline
  #:primitive (make-vector)
  #:static-infos ((#%call-result #,(get-array-static-infos)))
  (case-lambda
    [(len) (make-vector len)]
    [(len val) (make-vector len val)]))

(define/method (Array.length v)
  #:inline
  #:primitive (vector-length)
  (vector-length v))

(define/method Array.copy
  #:primitive (vector-copy)
  #:static-infos ((#%call-result #,(get-array-static-infos)))
  (case-lambda
    [(v) (vector-copy v)]
    [(v start) (vector-copy v start)]
    [(v start end) (vector-copy v start end)]))

(define/method Array.copy_from
  #:inline
  #:primitive (vector-copy!)
  (case-lambda
    [(v dest-start src) (vector-copy! v dest-start src)]
    [(v dest-start src src-start) (vector-copy! v dest-start src src-start)]
    [(v dest-start src src-start src-end) (vector-copy! v dest-start src src-start src-end)]))

(define/method (Array.take_left v n)
  #:primitive (vector-take)
  #:static-infos ((#%call-result #,(get-array-static-infos)))
  (vector-take v n))

(define/method (Array.take_right v n)
  #:primitive (vector-take-right)
  #:static-infos ((#%call-result #,(get-array-static-infos)))
  (vector-take-right v n))

(define/method (Array.drop_left v n)
  #:primitive (vector-drop)
  #:static-infos ((#%call-result #,(get-array-static-infos)))
  (vector-drop v n))

(define/method (Array.drop_right v n)
  #:primitive (vector-drop-right)
  #:static-infos ((#%call-result #,(get-array-static-infos)))
  (vector-drop-right v n))

(define/method (Array.set_in_copy v i val)
  #:primitive (vector-set/copy)
  #:static-infos ((#%call-result #,(get-array-static-infos)))
  (meta-if-version-at-least
   "8.11.1.10"
   (vector-set/copy v i val)
   (begin
     (define v2 (vector-copy v))
     (vector-set! v2 i val)
     v2)))

(define/method (Array.to_list v)
  #:static-infos ((#%call-result #,(get-treelist-static-infos)))
  (check-array who v)
  (vector->treelist v))

(define-sequence-syntax Array.to_sequence/optimize
  (lambda () #'Array.to_sequence)
  (lambda (stx)
    (syntax-parse stx
      [[(id) (_ arr-expr)] #'[(id) (in-vector arr-expr)]]
      [_ #f])))

(define/method (Array.to_sequence v)
  #:inline
  #:primitive (in-vector)
  #:static-infos ((#%call-result ((#%sequence-constructor #t))))
  (in-vector v))

(define-binding-syntax Array
  (binding-prefix-operator
   '((default . stronger))
   'macro
   (lambda (stx)
     (define (build args len pred rest-arg form-id tail)
       (composite-binding-transformer #`(#,form-id (parens . #,args) . #,tail)
                                      #:rest-arg rest-arg
                                      "Array"
                                      pred
                                      (for/list ([arg (in-list args)]
                                                 [i (in-naturals)])
                                        #`(lambda (v) (vector-ref v #,i)))
                                      (for/list ([arg (in-list args)])
                                        #'())
                                      #:static-infos (get-array-static-infos)
                                      #:index-result-info? #t
                                      #:rest-accessor (and rest-arg
                                                           #`(lambda (v) (vector-suffix->list v #,len)))
                                      #:rest-repetition? (and rest-arg #t)))
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
