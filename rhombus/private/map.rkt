#lang racket/base
(require racket/unsafe/undefined
         (for-syntax racket/base
                     racket/syntax
                     racket/symbol
                     syntax/parse/pre
                     "srcloc.rkt"
                     "tag.rkt"
                     shrubbery/print)
         "treelist.rkt"
         "to-list.rkt"
         "provide.rkt"
         "expression.rkt"
         "binding.rkt"
         "repetition.rkt"
         "compound-repetition.rkt"
         (submod "annotation.rkt" for-class)
         "literal.rkt"
         "static-info.rkt"
         "index-result-key.rkt"
         "index-key.rkt"
         "append-key.rkt"
         "call-result-key.rkt"
         "function-arity-key.rkt"
         "sequence-constructor-key.rkt"
         "sequence-element-key.rkt"
         "values-key.rkt"
         "composite.rkt"
         (submod "list.rkt" for-compound-repetition)
         "parse.rkt"
         "realm.rkt"
         "reducer.rkt"
         "setmap-parse.rkt"
         "parens.rkt"
         "op-literal.rkt"
         (only-in "pair.rkt"
                  Pair)
         "hash-snapshot.rkt"
         "mutability.rkt"
         "define-arity.rkt"
         (submod "define-arity.rkt" for-info)
         "indirect-static-info-key.rkt"
         "class-primitive.rkt"
         "rhombus-primitive.rkt"
         "rest-bind.rkt"
         "hash-remove.rkt"
         "key-comp.rkt"
         "key-comp-property.rkt"
         "number.rkt")

(provide (for-spaces (rhombus/namespace
                      #f
                      rhombus/bind
                      rhombus/repet
                      rhombus/annot
                      rhombus/reducer)
                     Map)
         (for-spaces (rhombus/namespace
                      #f
                      rhombus/repet
                      rhombus/annot)
                     MutableMap
                     WeakMutableMap)
         (for-spaces (rhombus/namespace
                      rhombus/bind
                      rhombus/annot)
                     ReadableMap
                     ;; temporary:
                     (rename-out [ReadableMap MapView])))

(module+ for-binding
  (provide (for-syntax parse-map-binding)))

(module+ for-info
  (provide (for-syntax map-static-infos)
           Map-build))

(module+ for-builtin
  (provide map-method-table
           mutable-map-method-table))

(module+ for-build
  (provide hash-append
           hash-extend*
           hash-assert
           list->map))

(module+ for-append
  (provide hash-append
           immutable-hash?))

(module+ for-key-comp
  (provide equal-always-hash?
           Map-build Map-pair-build list->map
           mutable-equal-always-hash?
           MutableMap-build
           weak-mutable-equal-always-hash?
           WeakMutableMap-build
           (for-space rhombus/statinfo
                      Map-pair-build
                      MutableMap-build
                      WeakMutableMap-build)

           object-hash?
           ObjectMap-build ObjectMap-pair-build list->object-map
           mutable-object-hash?
           MutableObjectMap-build
           weak-mutable-object-hash?
           WeakMutableObjectMap-build
           (for-space rhombus/statinfo
                      ObjectMap-pair-build
                      MutableObjectMap-build
                      WeakMutableObjectMap-build)

           now-hash?
           NowMap-build NowMap-pair-build list->now-map
           mutable-now-hash?
           MutableNowMap-build
           weak-mutable-now-hash?
           WeakMutableNowMap-build
           (for-space rhombus/statinfo
                      NowMap-pair-build
                      MutableNowMap-build
                      WeakMutableNowMap-build)

           number-or-object-hash?
           NumberOrObjectMap-build NumberOrObjectMap-pair-build list->number-or-object-map
           mutable-number-or-object-hash?
           MutableNumberOrObjectMap-build
           weak-mutable-number-or-object-hash?
           WeakMutableNumberOrObjectMap-build
           (for-space rhombus/statinfo
                      NumberOrObjectMap-pair-build
                      MutableNumberOrObjectMap-build
                      WeakMutableNumberOrObjectMap-build)))

(module+ for-key-comp-macro
  (provide build-map))

(define-for-syntax any-map-static-infos
  #'((#%index-get Map.get)
     (#%sequence-constructor Map.to_sequence/optimize)))

(define-primitive-class ReadableMap readable-map
  #:lift-declaration
  #:no-constructor-static-info
  #:instance-static-info #,any-map-static-infos
  #:existing
  #:opaque
  #:fields ()
  #:namespace-fields
  ([empty empty-readable-map]
   #:no-methods)
  #:properties
  ()
  #:methods
  ([length Map.length]
   [keys Map.keys]
   [values Map.values]
   [get Map.get]
   [has_key Map.has_key]
   [copy Map.copy]
   [snapshot Map.snapshot]
   [to_sequence Map.to_sequence]))

(define-primitive-class Map map
  #:lift-declaration
  #:no-constructor-static-info
  #:instance-static-info ((#%append Map.append/optimize)
                          . #,any-map-static-infos)
  #:existing
  #:opaque
  #:parent #f readable-map
  #:fields ()
  #:namespace-fields
  ([empty empty-map]
   [length Map.length]
   [values Map.values]
   [keys Map.keys]
   [get Map.get]
   [ref Map.get] ; temporary
   [has_key Map.has_key]
   [copy Map.copy]
   [snapshot Map.snapshot]
   [to_sequence Map.to_sequence]
   of
   [later_of Map.later_of]
   [by Map.by])
  #:properties
  ()
  #:methods
  (append
   remove))

(define-primitive-class MutableMap mutable-map
  #:lift-declaration
  #:no-constructor-static-info
  #:instance-static-info ((#%index-set MutableMap.set)
                          . #,any-map-static-infos)
  #:existing
  #:opaque
  #:parent #f readable-map
  #:fields ()
  #:namespace-fields
  ([by MutableMap.by]
   [now_of MutableMap.now_of]
   [later_of MutableMap.later_of])
  #:properties
  ()
  #:methods
  (set
   delete))

(define-primitive-class WeakMutableMap weak-mutable-map
  #:lift-declaration
  #:no-constructor-static-info
  #:instance-static-info #,mutable-map-static-infos
  #:existing
  #:opaque
  #:parent #f mutable-map
  #:fields ()
  #:namespace-fields
  ([by WeakMutableMap.by])
  #:properties
  ()
  #:methods
  ())

(define Map-build hashalw)
(define ObjectMap-build hasheq)
(define NowMap-build hash)
(define NumberOrObjectMap-build hasheqv)

(define (equal-always-hash? v) (and (immutable-hash? v) (hash-equal-always? v)))
(define (object-hash? v) (and (immutable-hash? v) (hash-eq? v)))
(define (now-hash? v) (and (immutable-hash? v) (and (not (custom-map-ref v #f)) (hash-equal? v))))
(define (number-or-object-hash? v) (and (immutable-hash? v) (hash-eqv? v)))

(define (mutable-equal-always-hash? v) (and (mutable-hash? v) (hash-equal-always? v)))
(define (mutable-object-hash? v) (and (mutable-hash? v) (hash-eq? v)))
(define (mutable-now-hash? v) (and (mutable-hash? v) (and (not (custom-map-ref v #f)) (hash-equal? v))))
(define (mutable-number-or-object-hash? v) (and (mutable-hash? v) (hash-eqv? v)))

(define (weak-mutable-equal-always-hash? v) (and (mutable-hash? v) (hash-ephemeron? v) (hash-equal-always? v)))
(define (weak-mutable-object-hash? v) (and (mutable-hash? v) (hash-ephemeron? v) (hash-eq? v)))
(define (weak-mutable-now-hash? v) (and (mutable-hash? v) (hash-ephemeron? v) (and (not (custom-map-ref v #f)) (hash-equal? v))))
(define (weak-mutable-number-or-object-hash? v) (and (mutable-hash? v) (hash-ephemeron? v) (hash-eqv? v)))

(define Map-pair-build
  (let ([Map (lambda args
               (build-map 'Map #hashalw() args))])
    Map))

(define ObjectMap-pair-build
  (let ([|Map.by(===)| (lambda args
                         (build-map '|Map.by(===)| #hasheq() args))])
    |Map.by(===)|))

(define NowMap-pair-build
  (let ([|Map.by(is_now)| (lambda args
                            (build-map '|Map.by(is_now)| #hash() args))])
    |Map.by(is_now)|))

(define NumberOrObjectMap-pair-build
  (let ([|Map.by(is_number_or_object)| (lambda args
                                         (build-map '|Map.by(is_number_or_object)| #hasheqv() args))])
    |Map.by(is_number_or_object)|))

(define (build-map who ht args)
  (for/fold ([ht ht]) ([arg (in-list args)])
    (cond
      [(and (pair? arg) (pair? (cdr arg)) (null? (cddr arg)))
       (hash-set ht (car arg) (cadr arg))]
      [(and (treelist? arg) (= 2 (treelist-length arg)))
       (hash-set ht (treelist-ref arg 0) (treelist-ref arg 1))]
      [(and (vector? arg) (= 2 (vector-length arg)))
       (hash-set ht (vector-ref arg 0) (vector-ref arg 1))]
      [else
       (define lst (to-list #f arg))
       (unless (and (pair? lst) (pair? (cdr lst)) (null? (cddr lst)))
         (raise-argument-error* who rhombus-realm "Listable.to_list && matching([_, _])" arg))
       (hash-set ht (car lst) (cadr lst))])))

(define (list->map key+vals)
  (for/hashalw ([key+val (in-list key+vals)])
    (values (car key+val) (cdr key+val))))

(define (list->object-map key+vals)
  (for/hasheq ([key+val (in-list key+vals)])
    (values (car key+val) (cdr key+val))))

(define (list->now-map key+vals)
  (for/hash ([key+val (in-list key+vals)])
    (values (car key+val) (cdr key+val))))

(define (list->number-or-object-map key+vals)
  (for/hasheqv ([key+val (in-list key+vals)])
    (values (car key+val) (cdr key+val))))

(define (hash-pairs ht)
  (for/list ([p (in-immutable-hash-pairs ht)]) p))

(define empty-map #hashalw())
(define-static-info-syntax empty-map
  #:defined map-static-infos)

(define empty-readable-map empty-map)
(define-static-info-syntax empty-readable-map
  #:defined readable-map-static-infos)

(define-for-syntax (make-empty-map-binding name+hash?-id+static-infos)
  (binding-transformer
   (lambda (stx)
     (syntax-parse stx
       [(form-id . tail)
        (values
         (binding-form #'empty-map-infoer name+hash?-id+static-infos)
         #'tail)]))))

(define-binding-syntax empty-map
  (make-empty-map-binding
   #`["Map.empty" immutable-hash? #,map-static-infos]))
(define-binding-syntax empty-readable-map
  (make-empty-map-binding
   #`["ReadableMap.empty" hash? #,readable-map-static-infos]))

(define-syntax (empty-map-infoer stx)
  (syntax-parse stx
    [(_ up-static-infos [name-str hash? static-infos])
     (binding-info #'name-str
                   #'empty
                   (static-infos-union #'static-infos #'up-static-infos)
                   #'()
                   #'empty-map-matcher
                   #'literal-bind-nothing
                   #'literal-commit-nothing
                   #'hash?)]))

(define-syntax (empty-map-matcher stx)
  (syntax-parse stx
    [(_ arg-id hash? IF success fail)
     #'(IF (and (hash? arg-id) (eqv? 0 (hash-count arg-id)))
           success
           fail)]))

(define-for-syntax (parse-map stx arg-stxes repetition? map-build-id map-pair-build-id list->map-id)
  (syntax-parse stx
    [(form-id (~and content (_::braces . _)) . tail)
     (define-values (shape argss) (parse-setmap-content #'content
                                                        #:shape 'map
                                                        #:who (syntax-e #'form-id)
                                                        #:repetition? repetition?
                                                        #:list->map list->map-id))
     (values (relocate-wrapped
              (respan (datum->syntax #f (append (list #'form-id) arg-stxes (list #'content))))
              (build-setmap stx argss
                            map-build-id
                            #'hash-extend*
                            #'hash-append
                            #'hash-assert
                            map-static-infos
                            #:repetition? repetition?
                            #:list->setmap #'list->map))
             #'tail)]
    [(form-id . tail) (values (cond
                                [repetition? (identifier-repetition-use map-pair-build-id)]
                                [else (relocate+reraw #'form-id map-pair-build-id)])
                              #'tail)]))

(define-syntax Map
  (expression-transformer
   (lambda (stx) (parse-map stx '() #f #'Map-build #'Map-pair-build #'list->map))))

(define-syntax Map.by
  (expression-transformer
   (lambda (stx)
     (parse-key-comp stx
                     (lambda (stx arg-stxes str mapper)
                       (parse-map stx arg-stxes #f
                                  (key-comp-map-build-id mapper)
                                  (key-comp-map-pair-build-id mapper)
                                  (key-comp-list->map-id mapper)))))))

(define-for-syntax (make-map-binding-transformer mode)
  (lambda (stx)
    (syntax-parse stx
      [(form-id (_::braces . _) . tail)
       (parse-map-binding (syntax-e #'form-id) stx "braces" mode)]
      [(form-id (_::parens arg ...) . tail)
       (let loop ([args (syntax->list #'(arg ...))] [keys '()] [vals '()])
         (cond
           [(null? args) (generate-map-binding (reverse keys) (reverse vals) #f #'tail mode)]
           [else
            (syntax-parse (car args)
              #:datum-literals (group)
              [(group (_::brackets key val))
               (loop (cdr args) (cons #'key keys) (cons #'val vals))]
              [_ (raise-syntax-error #f
                                     "expected [<key-expr>, <value-binding>]"
                                     stx
                                     (car args))])]))])))

(define-binding-syntax Map
  (binding-transformer
   (make-map-binding-transformer #'("Map" immutable-hash? values))))
(define-binding-syntax ReadableMap
  (binding-transformer
   (make-map-binding-transformer #'("ReadableMap" hash? hash-snapshot))))

(define-binding-syntax Map.by
  (binding-transformer
   (lambda (stx)
     (parse-key-comp stx
                     (lambda (stx arg-stxes str mapper)
                       (define mode #`(#,str #,(key-comp-map?-id mapper) values))
                       ((make-map-binding-transformer mode) stx))))))

(define-repetition-syntax Map
  (repetition-transformer
   (lambda (stx) (parse-map stx '() #t #'Map-build #'Map-pair-build #'list->map))))

(define-repetition-syntax Map.by
  (repetition-transformer
   (lambda (stx)
     (parse-key-comp stx
                     (lambda (stx arg-stxes str mapper)
                       (parse-map stx arg-stxes #t
                                  (key-comp-map-build-id mapper)
                                  (key-comp-map-pair-build-id mapper)
                                  (key-comp-list->map-id mapper)))))))

(define-for-syntax map-annotation-make-predicate
  (lambda (arg-id predicate-stxs)
    #`(for/and ([(k v) (in-immutable-hash #,arg-id)])
        (and (#,(car predicate-stxs) k)
             (#,(cadr predicate-stxs) v)))))

(define-for-syntax map-annotation-make-static-info
  (lambda (static-infoss)
    #`((#%index-result #,(cadr static-infoss))
       (#%sequence-element ((#%values (#,(car static-infoss)
                                       #,(cadr static-infoss))))))))

(define-annotation-constructor (Map of)
  ()
  #'immutable-hash? map-static-infos
  2
  #f
  map-annotation-make-predicate
  map-annotation-make-static-info
  #'map-build-convert #'(#hashalw()))

(define-for-syntax (make-map-later-chaperoner who)
  (lambda (predicate-stxes annot-strs)
    #`(lambda (ht)
        (let ([k-pred #,(car predicate-stxes)]
              [k-str #,(car annot-strs)]
              [v-pred #,(cadr predicate-stxes)]
              [v-str #,(cadr annot-strs)])
          (chaperone-hash ht
                          ;; ref
                          (lambda (ht k)
                            (values (check-map-key '#,who k k-pred k-str)
                                    (lambda (ht k v)
                                      (check-map-val '#,who v v-pred v-str))))
                          ;; set
                          (lambda (ht k v)
                            (values (check-map-key '#,who k k-pred k-str)
                                    (check-map-val '#,who v v-pred v-str)))
                          ;; remove
                          (lambda (ht k) (check-map-key '#,who k k-pred k-str))
                          ;; key
                          (lambda (ht k) (check-map-key '#,who k k-pred k-str))
                          ;; clear
                          (lambda (ht) (void))
                          ;; equal-key-proc
                          #f)))))

(define-annotation-constructor (Map/again Map.later_of)
  ()
  #'immutable-hash? map-static-infos
  2
  #f
  (make-map-later-chaperoner 'Map)
  (lambda (static-infoss)
    #`((#%index-result #,(cadr static-infoss))))
  "converter annotation not supported for elements;\n checking needs a predicate annotation for the map content"
  #'()
  #:parse-of parse-annotation-of/chaperone)

(define-annotation-syntax Map.by
  (annotation-prefix-operator
   #'Map.by
   '((default . stronger))
   'macro
   (lambda (stx)
     (parse-key-comp stx
                     (lambda (stx arg-stxes str mapper)
                       (syntax-parse stx
                         #:datum-literals (op |.| of)
                         [(form-id (~and dot (op |.|)) (~and of-id of) . tail)
                          (parse-annotation-of #'(of-id . tail)
                                               (key-comp-map?-id mapper) map-static-infos
                                               2 #f
                                               map-annotation-make-predicate
                                               map-annotation-make-static-info
                                               #'map-build-convert #`(#,(key-comp-empty-stx mapper)))]
                         [(_ . tail)
                          (values (annotation-predicate-form (key-comp-map?-id mapper)
                                                             map-static-infos)
                                  #'tail)]))))))

(define-syntax (map-build-convert arg-id build-convert-stxs kws data)
  (syntax-parse data
    [(empty-ht)
     #`(for/fold ([map empty-ht])
                 ([(k v) (in-immutable-hash #,arg-id)])
         #:break (not map)
         (#,(car build-convert-stxs)
          k
          (lambda (k)
            (#,(cadr build-convert-stxs)
             v
             (lambda (v)
               (hash-set map k v))
             (lambda () #f)))
          (lambda () #f)))]))

(define-static-info-syntax Map-pair-build
  (#%call-result #,map-static-infos)
  (#%function-arity -1)
  (#%indirect-static-info indirect-function-static-info))

(define-static-info-syntax ObjectMap-pair-build
  (#%indirect-static-info Map-pair-build))
(define-static-info-syntax NowMap-pair-build
  (#%indirect-static-info Map-pair-build))
(define-static-info-syntax NumberOrObjectMap-pair-build
  (#%indirect-static-info Map-pair-build))

(define-reducer-syntax Map
  (reducer-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ . tail)
        (values
         (reducer/no-break #'build-map-reduce
                           #'([ht #hashalw()])
                           #'build-map-add
                           map-static-infos
                           #'ht)
         #'tail)]))))

(define-reducer-syntax Map.by
  (reducer-transformer
   (lambda (stx)
     (parse-key-comp stx
                     (lambda (stx arg-stxes str mapper)
                       (syntax-parse stx
                         [(_ . tail)
                          (values
                           (reducer/no-break #'build-map-reduce
                                             #`([ht #,(key-comp-empty-stx mapper)])
                                             #'build-map-add
                                             map-static-infos
                                             #'ht)
                           #'tail)]))))))

(define-syntax (build-map-reduce stx)
  (syntax-parse stx
    [(_ ht-id e) #'e]))

(define-syntax (build-map-add stx)
  (syntax-parse stx
    [(_ ht-id e)
     #'(let-values ([(k v) e])
         (hash-set ht-id k v))]))

(define (ephemeron-mutable-hash? m)
  (and (hash? m) (hash-ephemeron? m)))

(define-annotation-syntax WeakMutableMap (identifier-annotation #'ephemeron-mutable-hash? weak-mutable-map-static-infos))
(define-annotation-syntax ReadableMap (identifier-annotation #'hash? readable-map-static-infos))

(define-annotation-constructor (MutableMap MutableMap.now_of)
  ()
  #'mutable-hash? mutable-map-static-infos
  2
  #f
  (lambda (arg-id predicate-stxs)
    #`(for/and ([(k v) (in-hash #,arg-id)])
        (and (#,(car predicate-stxs) k)
             (#,(cadr predicate-stxs) v))))
  (lambda (static-infoss)
    ;; no static info, since mutable and content is checked only initially
    #'())
  "converter annotation not supported for elements;\n immediate checking needs a predicate annotation for the mutable map content"
  #'())

(define-annotation-constructor (MutableMap/again MutableMap.later_of)
  ()
  #'mutable-hash? mutable-map-static-infos
  2
  #f
  (make-map-later-chaperoner 'MutableMap)
  (lambda (static-infoss)
    #`((#%index-result #,(cadr static-infoss))))
  #'mutable-map-build-convert #'()
  #:parse-of parse-annotation-of/chaperone)

(define (raise-item-check-error who what x annot-str)
  (raise-binding-failure who what x annot-str))

(define (check-map-item x who what pred annot-str)
  (unless (pred x) (raise-item-check-error who what x annot-str))
  x)

(define (check-map-key who k k-pred k-str)
  (check-map-item k who "key" k-pred k-str))
(define (check-map-val who v v-pred v-str)
  (check-map-item v who "value" v-pred v-str))

(define-syntax (mutable-map-build-convert arg-id build-convert-stxs kws data)
  (with-syntax ([[(k-annot-str v-annot-str . _) _] data])
    #`(let ([k-cvt #,(car build-convert-stxs)]
            [k-str k-annot-str]
            [v-cvt #,(cadr build-convert-stxs)]
            [v-str v-annot-str])
        (impersonate-hash ht
                          ;; ref
                          (lambda (ht k)
                            (values (convert-map-key k k-cvt k-str)
                                    (lambda (ht k v)
                                      (convert-map-val v v-cvt v-str))))
                          ;; set
                          (lambda (ht k v)
                            (values (convert-map-key k k-cvt k-str)
                                    (convert-map-val v v-cvt v-str)))
                          ;; remove
                          (lambda (ht k) (convert-map-key k k-cvt k-str))
                          ;; key
                          (lambda (ht k) (convert-map-key k k-cvt k-str))
                          ;; clear
                          (lambda (ht) (void))
                          ;; equal-key-proc
                          #f))))

(define (convert-map-item x what cvt annot-str)
  (cvt
   x
   (lambda (x) x)
   (lambda ()
     (raise-item-check-error 'MutableMap what x annot-str))))

(define (convert-map-key k k-cvt k-str)
  (convert-map-item k "key" k-cvt k-str))
(define (convert-map-val v v-cvt v-str)
  (convert-map-item v "value" v-cvt v-str))

(define-annotation-syntax MutableMap.by
  (annotation-prefix-operator
   #'MutableMap.by
   '((default . stronger))
   'macro
   (lambda (stx)
     (parse-key-comp stx
                     (lambda (stx arg-stxes str mapper)
                       (syntax-parse stx
                         [(_ . tail)
                          (values (annotation-predicate-form (key-comp-mutable-map?-id mapper)
                                                             mutable-map-static-infos)
                                  #'tail)]))))))

(define MutableMap-build
  (let ([MutableMap (lambda args
                      (hash-copy (build-map 'MutableMap #hashalw() args)))])
    MutableMap))
(define MutableObjectMap-build
  (let ([|MutableMap.by(===)| (lambda args
                                (hash-copy (build-map '|MutableMap.by(===)| #hasheq() args)))])
    |MutableMap.by(===)|))
(define MutableNowMap-build
  (let ([|MutableMap.by(is_now)| (lambda args
                                   (hash-copy (build-map '|MutableMap.by(is_now)| #hash() args)))])
    |MutableMap.by(is_now)|))
(define MutableNumberOrObjectMap-build
  (let ([|MutableMap.by(is_number_or_object)| (lambda args
                                                (hash-copy (build-map '|MutableMap.by(is_number_or_object)| #hasheqv() args)))])
    |MutableMap.by(is_number_or_object)|))

(define-annotation-syntax WeakMutableMap.by
  (annotation-prefix-operator
   #'WeakMutableMap.by
   '((default . stronger))
   'macro
   (lambda (stx)
     (parse-key-comp stx
                     (lambda (stx arg-stxes str mapper)
                       (syntax-parse stx
                         [(_ . tail)
                          (values (annotation-predicate-form (key-comp-weak-mutable-map?-id mapper)
                                                             weak-mutable-map-static-infos)
                                  #'tail)]))))))

(define WeakMutableMap-build
  (let ([WeakMutableMap (lambda args
                          (hash-copy/ephemeron (build-map 'WeakMutableMap #hashalw() args)))])
    WeakMutableMap))
(define WeakMutableObjectMap-build
  (let ([|WeakMutableMap.by(===)| (lambda args
                                    (hash-copy/ephemeron (build-map '|WeakMutableMap.by(===)| #hasheq() args)))])
    |WeakMutableMap.by(===)|))
(define WeakMutableNowMap-build
  (let ([|WeakMutableMap.by(is_now)| (lambda args
                                       (hash-copy/ephemeron (build-map '|WeakMutableMap.by(is_now)| #hash() args)))])
    |WeakMutableMap.by(is_now)|))
(define WeakMutableNumberOrObjectMap-build
  (let ([|WeakMutableMap.by(is_number_or_object)| (lambda args
                                                    (hash-copy/ephemeron (build-map '|WeakMutableMap.by(is_number_or_object)| #hasheqv() args)))])
    |WeakMutableMap.by(is_number_or_object)|))

(define-for-syntax (parse-mutable-map stx repetition? map-build-id mutable-map-build-id map-copy-id)
  (syntax-parse stx
    [(form-id (~and content (_::braces . _)) . tail)
     (define-values (shape argss)
       (parse-setmap-content #'content
                             #:shape 'map
                             #:who (syntax-e #'form-id)
                             #:repetition? repetition?
                             #:list->map #'list->map
                             #:no-splice "mutable maps"))
     (values (cond
               [repetition?
                (build-compound-repetition
                 stx
                 (car argss)
                 (lambda args
                   (values (quasisyntax/loc stx
                             (#,map-copy-id (#,map-build-id #,@args)))
                           mutable-map-static-infos)))]
               [else
                (wrap-static-info*
                 (quasisyntax/loc stx
                   (#,map-copy-id (#,map-build-id #,@(if (null? argss) null (car argss)))))
                 mutable-map-static-infos)])
             #'tail)]
    [(form-id . tail) (values (if repetition?
                                  (identifier-repetition-use mutable-map-build-id)
                                  (relocate+reraw #'form-id mutable-map-build-id))
                              #'tail)]))

(define-syntax MutableMap
  (expression-transformer
   (lambda (stx) (parse-mutable-map stx #f #'Map-build #'MutableMap-build #'Map.copy))))

(define-syntax MutableMap.by
  (expression-transformer
   (lambda (stx)
     (parse-key-comp stx
                     (lambda (stx arg-stxes str mapper)
                       (parse-mutable-map stx #f
                                          (key-comp-map-build-id mapper)
                                          (key-comp-mutable-map-build-id mapper)
                                          #'Map.copy))))))

(define-repetition-syntax MutableMap
  (repetition-transformer
   (lambda (stx) (parse-mutable-map stx #t #'Map-build #'MutableMap-build #'Map.copy))))

(define-repetition-syntax MutableMap.by
  (repetition-transformer
   (lambda (stx)
     (parse-key-comp stx
                     (lambda (stx arg-stxes str mapper)
                       (parse-mutable-map stx #t
                                          (key-comp-map-build-id mapper)
                                          (key-comp-mutable-map-build-id mapper)
                                          #'Map.copy))))))

(define-static-info-syntax MutableMap-build
  (#%call-result #,mutable-map-static-infos)
  (#%function-arity -1)
  (#%indirect-static-info indirect-function-static-info))

(define-static-info-syntax MutableObjectMap-build
  (#%indirect-static-info MutableMap-build))
(define-static-info-syntax MutableNowMap-build
  (#%indirect-static-info MutableMap-build))
(define-static-info-syntax MutableNumberOrObjectMap-build
  (#%indirect-static-info MutableMap-build))

(define-syntax WeakMutableMap
  (expression-transformer
   (lambda (stx) (parse-mutable-map stx #f #'Map-build #'WeakMutableMap-build #'hash-copy/ephemeron))))

(define-syntax WeakMutableMap.by
  (expression-transformer
   (lambda (stx)
     (parse-key-comp stx
                     (lambda (stx arg-stxes str mapper)
                       (parse-mutable-map stx #f
                                          (key-comp-map-build-id mapper)
                                          (key-comp-weak-mutable-map-build-id mapper)
                                          #'hash-copy/ephemeron))))))

(define-repetition-syntax WeakMutableMap
  (repetition-transformer
   (lambda (stx) (parse-mutable-map stx #t #'Map-build #'WeakMutableMap-build #'hash-copy/ephemeron))))

(define-repetition-syntax WeakMutableMap.by
  (repetition-transformer
   (lambda (stx)
     (parse-key-comp stx
                     (lambda (stx arg-stxes str mapper)
                       (parse-mutable-map stx #t
                                          (key-comp-map-build-id mapper)
                                          (key-comp-weak-mutable-map-build-id mapper)
                                          #'hash-copy/ephemeron))))))

(define-static-info-syntax WeakMutableMap-build
  (#%call-result #,weak-mutable-map-static-infos)
  (#%function-arity -1)
  (#%indirect-static-info indirect-function-static-info))

(define-static-info-syntax WeakMutableObjectMap-build
  (#%indirect-static-info WeakMutableMap-build))
(define-static-info-syntax WeakMutableNowMap-build
  (#%indirect-static-info WeakMutableMap-build))
(define-static-info-syntax WeakMutableNumberOrObjectMap-build
  (#%indirect-static-info WeakMutableMap-build))

(define-for-syntax (parse-map-binding who stx opener+closer [mode #'("Map" immutable-hash? values)])
  (syntax-parse stx
    #:datum-literals (group)
    [(form-id (_ (group key-e ... (_::block (group val ...))) ...
                 (group key-b ... (_::block (group val-b ...)))
                 (group _::...-bind))
              . tail)
     (generate-map-binding (syntax->list #`((#,group-tag key-e ...) ...)) #`((#,group-tag val ...) ...)
                           #`(group Pair (parens (#,group-tag key-b ...) (#,group-tag val-b ...)))
                           #'tail
                           mode
                           #:rest-repetition? #t)]
    [(form-id (_ (group key-e ... (_::block (group val ...))) ...
                 (group _::&-bind rst ...))
              . tail)
     (generate-map-binding (syntax->list #`((#,group-tag key-e ...) ...)) #`((#,group-tag val ...) ...)
                           #`(#,group-tag rest-bind #,map-static-infos
                              (#,group-tag rst ...))
                           #'tail
                           mode)]
    [(form-id (_ (group key-e ... (_::block (group val ...))) ...) . tail)
     (generate-map-binding (syntax->list #`((#,group-tag key-e ...) ...)) #`((#,group-tag val ...) ...)
                           #f
                           #'tail
                           mode)]
    [(form-id wrong . tail)
     (raise-syntax-error who
                         (format "bad key-value combination within ~a" opener+closer)
                         #'wrong)]))

(define-for-syntax (generate-map-binding keys vals maybe-rest tail mode
                                         #:rest-repetition? [rest-repetition? #f])
  (with-syntax ([(key ...) keys]
                [(val ...) vals]
                [tail tail])
    (define tmp-ids (generate-temporaries #'(key ...)))
    (define rest-tmp (and maybe-rest (generate-temporary 'rest-tmp)))
    (define mode-desc (syntax-parse mode [(desc . _) (syntax-e #'desc)]))
    (define-values (composite new-tail)
      ((make-composite-binding-transformer (cons mode-desc (map shrubbery-syntax->string keys))
                                           #'(lambda (v) #t) ; predicate built into map-matcher
                                           (for/list ([tmp-id (in-list tmp-ids)])
                                             #`(lambda (v) #,tmp-id))
                                           (for/list ([arg (in-list tmp-ids)])
                                             #'())
                                           #:static-infos map-static-infos
                                           #:index-result-info? #t
                                           #:sequence-element-info? #t
                                           #:rest-accessor
                                           (and maybe-rest
                                                (if rest-repetition?
                                                    #`(lambda (v) (hash-pairs #,rest-tmp))
                                                    #`(lambda (v) #,rest-tmp)))
                                           #:rest-repetition? (and rest-repetition?
                                                                   'pair))
       #`(form-id (parens val ...) . tail)
       maybe-rest))
    (values
     (syntax-parse composite
       [composite::binding-form
        (binding-form
         #'map-infoer
         #`(#,mode (key ...) #,tmp-ids #,rest-tmp composite.infoer-id composite.data))])
     new-tail)))

(define-syntax (map-infoer stx)
  (syntax-parse stx
    [(_ static-infos (mode keys tmp-ids rest-tmp composite-infoer-id composite-data))
     #:with composite-impl::binding-impl #'(composite-infoer-id static-infos composite-data)
     #:with composite-info::binding-info #'composite-impl.info
     (binding-info #'composite-info.annotation-str
                   #'composite-info.name-id
                   #'composite-info.static-infos
                   #'composite-info.bind-infos
                   #'map-matcher
                   #'map-committer
                   #'map-binder
                   #'(mode keys tmp-ids rest-tmp
                           composite-info.matcher-id composite-info.committer-id composite-info.binder-id
                           composite-info.data))]))

(define-syntax (map-matcher stx)
  (syntax-parse stx
    [(_ arg-id ([desc pred filter] keys tmp-ids rest-tmp composite-matcher-id composite-committer-id composite-binder-id composite-data)
        IF success failure)
     (define key-tmps (generate-temporaries #'keys))
     #`(IF (pred arg-id)
           (begin
             #,@(for/foldr ([forms (append (if (syntax-e #'rest-tmp)
                                               (list #`(define rest-tmp
                                                         (hash-remove*
                                                          (filter arg-id)
                                                          (list #,@key-tmps))))
                                               '())
                                           (list #'(composite-matcher-id 'map composite-data IF success failure)))])
                           ([key (in-list (syntax->list #'keys))]
                            [key-tmp-id (in-list key-tmps)]
                            [val-tmp-id (in-list (syntax->list #'tmp-ids))])
                  (list #`(define #,key-tmp-id (rhombus-expression #,key))
                        #`(define #,val-tmp-id (hash-ref arg-id #,key-tmp-id unsafe-undefined))
                        #`(IF (not (eq? #,val-tmp-id unsafe-undefined))
                              (begin #,@forms)
                              failure))))
           failure)]))

(define-syntax (map-committer stx)
  (syntax-parse stx
    [(_ arg-id (mode keys tmp-ids rest-tmp composite-matcher-id composite-committer-id composite-binder-id composite-data))
     #`(composite-committer-id 'map composite-data)]))

(define-syntax (map-binder stx)
  (syntax-parse stx
    [(_ arg-id (mode keys tmp-ids rest-tmp composite-matcher-id composite-committer-id composite-binder-id composite-data))
     #`(composite-binder-id 'map composite-data)]))


;; macro to optimize to an inline functional update
(define-syntax (Map.append/optimize stx)
  (syntax-parse stx
    [(_ map1 map2)
     (syntax-parse (unwrap-static-infos #'map2)
       [(id:identifier k v)
        #:when (free-identifier=? (expr-quote Map-build) #'id)
        #'(hash-set map1 k v)]
       [_
        #'(Map.append map1 map2)])]))

;; for `++`
(define-static-info-syntax Map.append/optimize
  (#%call-result #,map-static-infos))

(define hash-extend*
  (case-lambda
    [(ht key val) (hash-set ht key val)]
    [(ht . args) (hash-extend*/proc ht args)]))

(define (hash-extend*/proc ht args)
  (let loop ([ht ht] [args args])
    (cond
      [(null? args) ht]
      [(null? (cdr args)) (error 'hash-extend* "argument count went wrong")]
      [else (loop (hash-set ht (car args) (cadr args)) (cddr args))])))

(define (hash-assert v)
  (unless (immutable-hash? v)
    (raise-arguments-error* 'Map rhombus-realm
                            "not an immutable map for splicing"
                            "value" v))
  v)

(set-primitive-contract! 'hash? "ReadableMap")
(set-primitive-contract! '(and/c hash? immutable?) "Map")
(set-primitive-contract! '(and/c hash? (not/c immutable?)) "MutableMap")

(define (check-readable-map who ht)
  (unless (hash? ht)
    (raise-argument-error* who rhombus-realm "ReadableMap" ht)))

(define/method (Map.length ht)
  #:inline
  #:primitive (hash-count)
  #:static-infos ((#%call-result #,int-static-infos))
  (hash-count ht))

(define/method (Map.keys ht [try-sort? #f])
  #:static-infos ((#%call-result #,treelist-static-infos))
  (check-readable-map who ht)
  (list->treelist (hash-keys ht try-sort?)))

(define-sequence-syntax Map.to_sequence/optimize
  (lambda () #'Map.to_sequence)
  (lambda (stx)
    (syntax-parse stx
      [[(id-k id-v) (_ mp-expr)] #'[(id-k id-v) (in-hash mp-expr)]]
      [_ #f])))

(define/method (Map.to_sequence ht)
  #:inline
  #:primitive (in-hash)
  #:static-infos ((#%call-result ((#%sequence-constructor #t))))
  (in-hash ht))

(define/method (Map.values ht)
  #:static-infos ((#%call-result #,treelist-static-infos))
  (check-readable-map who ht)
  (list->treelist (hash-values ht)))

(define/method Map.get
  #:inline
  #:primitive (hash-ref)
  (case-lambda
    [(ht key) (hash-ref ht key)]
    [(ht key default) (hash-ref ht key default)]))

(define (check-map who ht)
  (unless (immutable-hash? ht)
    (raise-argument-error* who rhombus-realm "Map" ht)))

(define (hash-append a b)
  (let-values ([(a b)
                (if (and ((hash-count a) . < . (hash-count b))
                         (cond
                           [(custom-map-ref a #f)
                            => (lambda (a-cm)
                                 (eq? a-cm (custom-map-ref b #f)))]
                           [(hash-equal-always? a)
                            (hash-equal-always? b)]
                           [(hash-eq? a)
                            (hash-eq? b)]
                           [(hash-eqv? a)
                            (hash-eqv? b)]
                           [(hash-equal? a)
                            (hash-equal? b)]
                           [else
                            ;; shouldn't get here, but just in case
                            #f]))
                    (values b a)
                    (values a b))])
    (for/fold ([a a]) ([(k v) (in-immutable-hash b)])
      (hash-set a k v))))

(define/method Map.append
  #:static-infos ((#%call-result #,map-static-infos))
  (case-lambda
    [()
     #hashalw()]
    [(ht)
     (check-map who ht)
     ht]
    [(ht1 ht2)
     (check-map who ht1)
     (check-map who ht2)
     (hash-append ht1 ht2)]
    [(ht . hts)
     (check-map who ht)
     (for ([ht (in-list hts)])
       (check-map who ht))
     (for/fold ([new-ht ht])
               ([ht (in-list hts)])
       (hash-append new-ht ht))]))

(define/method (Map.has_key ht key)
  (check-readable-map who ht)
  (hash-has-key? ht key))

(define/method (Map.copy ht)
  #:inline
  #:primitive (hash-copy)
  #:static-infos ((#%call-result #,mutable-map-static-infos))
  (cond
    [(custom-map-ref ht #f)
     => (lambda (cm) ((custom-map-copy cm) ht))]
    [else (hash-copy ht)]))

(define (hash-copy/ephemeron ht)
  (cond
    [(custom-map-ref ht #f)
     => (lambda (cm) ((custom-map-weak-copy cm) ht))]
    [else
     (define mht (cond
                   [(hash-eq? ht) (make-ephemeron-hasheq)]
                   [(hash-eqv? ht) (make-ephemeron-hasheqv)]
                   [(hash-equal? ht) (make-ephemeron-hash)]
                   [else (make-ephemeron-hashalw)]))
     (for ([(k v) (in-hash ht)])
       (hash-set! mht k v))
     mht]))

(define/method (Map.snapshot ht)
  #:static-infos ((#%call-result #,map-static-infos))
  (check-readable-map who ht)
  (cond
    [(custom-map-ref ht #f)
     => (lambda (cm) ((custom-map-snapshot cm) ht))]
    [else (hash-snapshot ht)]))

(define/method (Map.remove ht key)
  #:inline
  #:primitive (hash-remove)
  #:static-infos ((#%call-result #,map-static-infos))
  (hash-remove ht key))

(define/method (MutableMap.set ht key val)
  #:inline
  #:primitive (hash-set!)
  (hash-set! ht key val))

(define/method (MutableMap.delete ht key)
  #:inline
  #:primitive (hash-remove!)
  (hash-remove! ht key))
