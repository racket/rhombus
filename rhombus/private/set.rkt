#lang racket/base
(require (for-syntax racket/base
                     racket/symbol
                     syntax/parse/pre
                     racket/syntax
                     "srcloc.rkt"
                     "tag.rkt"
                     shrubbery/print)
         "treelist.rkt"
         "provide.rkt"
         "expression.rkt"
         "binding.rkt"
         "repetition.rkt"
         "compound-repetition.rkt"
         (submod "annotation.rkt" for-class)
         (submod "list.rkt" for-compound-repetition)
         "static-info.rkt"
         "reducer.rkt"
         "index-key.rkt"
         "append-key.rkt"
         "call-result-key.rkt"
         "function-arity-key.rkt"
         "sequence-constructor-key.rkt"
         "sequence-element-key.rkt"
         "parse.rkt"
         "literal.rkt"
         "realm.rkt"
         "setmap-parse.rkt"
         "parens.rkt"
         "composite.rkt"
         "op-literal.rkt"
         "hash-snapshot.rkt"
         "mutability.rkt"
         "define-arity.rkt"
         (submod "define-arity.rkt" for-info)
         "indirect-static-info-key.rkt"
         "class-primitive.rkt"
         "rest-bind.rkt")

(provide (for-spaces (rhombus/namespace
                      #f
                      rhombus/bind
                      rhombus/repet
                      rhombus/reducer
                      rhombus/annot)
                     Set)
         (for-spaces (rhombus/namespace
                      #f
                      rhombus/repet
                      rhombus/annot)
                     MutableSet)
         (for-spaces (rhombus/namespace
                      rhombus/bind
                      rhombus/annot)
                     ReadableSet
                     ;; temporary:
                     (rename-out [ReadableSet SetView])))

(module+ for-binding
  (provide (for-syntax parse-set-binding)))

(module+ for-ref
  (provide set?
           set-ht
           set))

(module+ for-builtin
  (provide set?
           mutable-set?
           set-method-table
           mutable-set-method-table))

(module+ for-info
  (provide (for-syntax set-static-infos)
           Set-build))

(module+ for-build
  (provide set-append
           set-extend*
           set-assert
           list->set))

(module+ for-append
  (provide set-append
           immutable-set?))

(struct set (ht)
  #:property prop:equal+hash
  (list (lambda (self other eql? mode)
          (eql? (set-ht self) (set-ht other)))
        (lambda (self hash-code mode)
          (hash-code (set-ht self))))
  #:property prop:sequence
  (lambda (s)
    (in-set s)))

(define-for-syntax any-set-static-infos
  #'((#%index-get Set.get)
     (#%sequence-constructor in-set)))

(define-primitive-class ReadableSet readable-set
  #:lift-declaration
  #:no-constructor-static-info
  #:instance-static-info #,any-set-static-infos
  #:existing
  #:opaque
  #:fields ()
  #:namespace-fields
  ([empty empty-readable-set]
   #:no-methods)
  #:properties
  ()
  #:methods
  ([length Set.length]
   [get Set.get]
   [to_list Set.to_list]
   [copy Set.copy]
   [snapshot Set.snapshot]))

(define-primitive-class Set set
  #:lift-declaration
  #:no-constructor-static-info
  #:instance-static-info ((#%append Set.append/optimize)
                          . #,any-set-static-infos)
  #:existing
  #:opaque
  #:parent #f readable-set
  #:fields ()
  #:namespace-fields
  ([empty empty-set]
   [length Set.length]
   [get Set.get]
   [to_list Set.to_list]
   [copy Set.copy]
   [snapshot Set.snapshot]
   of)
  #:properties
  ()
  #:methods
  (append
   union
   intersect
   remove))

(define-primitive-class MutableSet mutable-set
  #:lift-declaration
  #:no-constructor-static-info
  #:instance-static-info ((#%index-set MutableSet.set)
                          . #,any-set-static-infos)
  #:existing
  #:opaque
  #:parent #f readable-set
  #:fields ()
  #:namespace-fields
  ()
  #:properties
  ()
  #:methods
  (set
   delete))

(define (immutable-set? v) (and (set? v) (immutable-hash? (set-ht v))))
(define (mutable-set? v) (and (set? v) (mutable-hash? (set-ht v))))

(define (check-readable-set who s)
  (unless (set? s)
    (raise-argument-error* who rhombus-realm "ReadableSet" s)))

(define/method (Set.length s)
  (check-readable-set who s)
  (hash-count (set-ht s)))

(define/method (Set.get s v)
  (check-readable-set who s)
  (hash-ref (set-ht s) v #f))

(define-syntax (Set-build stx)
  (syntax-parse stx
    [(_ elem ...)
     #`(set (hashalw (~@ elem #t) ...))]))

(define Set-build*
  (let ([Set (lambda vals
               (define base-ht (hashalw))
               (set (for/fold ([ht base-ht]) ([val (in-list vals)])
                      (hash-set ht val #t))))])
    Set))

(define (list->set l) (apply Set-build* l))

(define (set->list s) (hash-keys (set-ht s)))

(define empty-set (set #hashalw()))
(define-static-info-syntax empty-set
  #:defined set-static-infos)

(define empty-readable-set empty-set)
(define-static-info-syntax empty-readable-set
  #:defined readable-set-static-infos)

(define-for-syntax (make-empty-set-binding name+hash?-id+static-infos)
  (binding-transformer
   (lambda (stx)
     (syntax-parse stx
       [(form-id . tail)
        (values
         (binding-form #'empty-set-infoer name+hash?-id+static-infos)
         #'tail)]))))

(define-binding-syntax empty-set
  (make-empty-set-binding
   #`["Set.empty" immutable-hash? #,set-static-infos]))
(define-binding-syntax empty-readable-set
  (make-empty-set-binding
   #`["ReadableSet.empty" hash? #,readable-set-static-infos]))

(define-syntax (empty-set-infoer stx)
  (syntax-parse stx
    [(_ up-static-infos [name-str hash? static-infos])
     (binding-info #'name-str
                   #'empty
                   (static-infos-union #'static-infos #'up-static-infos)
                   #'()
                   #'empty-set-matcher
                   #'literal-commit-nothing
                   #'literal-bind-nothing
                   #'hash?)]))

(define-syntax (empty-set-matcher stx)
  (syntax-parse stx
    [(_ arg-id hash? IF success fail)
     #'(IF (and (set? arg-id) (hash? (set-ht arg-id)) (eqv? 0 (hash-count (set-ht arg-id))))
           success
           fail)]))

(define-reducer-syntax Set
  (reducer-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ . tail)
        (values
         (reducer/no-break #'build-set-reduce
                           #'([ht #hashalw()])
                           #'build-set-add
                           set-static-infos
                           #'ht)
         #'tail)]))))

(define-syntax (build-set-reduce stx)
  (syntax-parse stx
    [(_ ht-id e) #'(set e)]))

(define-syntax (build-set-add stx)
  (syntax-parse stx
    [(_ ht-id v) #'(hash-set ht-id v #t)]))

(define-for-syntax (parse-set stx repetition?)
  (syntax-parse stx
    [(form-id (~and content (_::braces . _)) . tail)
     (define-values (shape argss) (parse-setmap-content #'content
                                                        #:shape 'set
                                                        #:who (syntax-e #'form-id)
                                                        #:repetition? repetition?
                                                        #:list->set #'list->set))
     (values (relocate-wrapped
              (respan (datum->syntax #f (list #'form-id #'content)))
              (build-setmap stx argss
                            #'Set-build
                            #'set-extend*
                            #'set-append
                            #'set-assert
                            set-static-infos
                            #:repetition? repetition?
                            #:list->setmap #'list->set))
             #'tail)]
    [(_ . tail) (values (if repetition?
                            (identifier-repetition-use #'Set-build*)
                            #'Set-build*)
                        #'tail)]))

(define-syntax Set
  (expression-transformer
   (lambda (stx) (parse-set stx #f))))

(define-for-syntax (make-set-binding-transformer mode)
  (binding-transformer
   (lambda (stx)
     (syntax-parse stx
       [(form-id (~and content (_::braces . _)) . tail)
        (parse-set-binding (syntax-e #'form-id) stx "braces" mode)]
       [(form-id (_::parens arg ...) . tail)
        (parse-set-binding (syntax-e #'form-id) stx "parentheses" mode)]))))

(define-binding-syntax Set
  (make-set-binding-transformer 'Set))

(define-binding-syntax ReadableSet
  (make-set-binding-transformer 'ReadableSet))

(define-repetition-syntax Set
  (repetition-transformer
   (lambda (stx) (parse-set stx #t))))

(define-for-syntax (parse-set-binding who stx opener+closer [mode 'Set])
  (syntax-parse stx
    #:datum-literals (group)
    [(form-id (_ (group key-e ...) ...
                 (group elem-b ...)
                 (group _::...-bind))
              . tail)
     (generate-set-binding (syntax->list #`((#,group-tag key-e ...) ...))
                           #`(#,group-tag elem-b ...)
                           #'tail
                           mode
                           #:rest-repetition? #t)]
    [(form-id (_ (group elem-e ...) ...
                 (group _::&-bind rst ...))
              . tail)
     (generate-set-binding (syntax->list #`((#,group-tag elem-e ...) ...))
                           #`(#,group-tag rest-bind #,set-static-infos
                              (#,group-tag rst ...))
                           #'tail
                           mode)]
    [(form-id (_ (group elem-e ...) ...) . tail)
     (generate-set-binding (syntax->list #`((#,group-tag elem-e ...) ...))
                           #f
                           #'tail
                           mode)]))

(define-for-syntax (generate-set-binding keys maybe-rest tail mode
                                         #:rest-repetition? [rest-repetition? #f])
  (with-syntax ([(key ...) keys]
                [tail tail])
    (define rest-tmp (and maybe-rest (generate-temporary 'rest-tmp)))
    (define-values (composite new-tail)
      ((make-composite-binding-transformer (cons (symbol->immutable-string mode) (map shrubbery-syntax->string keys))
                                           #'(lambda (v) #t) ; predicate built into set-matcher
                                           '()
                                           '()
                                           #:static-infos set-static-infos
                                           #:sequence-element-info? #t
                                           #:rest-accessor
                                           (and maybe-rest
                                                (if rest-repetition?
                                                    #`(lambda (v) (set->list #,rest-tmp))
                                                    #`(lambda (v) #,rest-tmp)))
                                           #:rest-repetition? rest-repetition?)
       #`(form-id (parens) . tail)
       maybe-rest))
    (values
     (syntax-parse composite
       [composite::binding-form
        (binding-form
         #'set-infoer
         #`(#,mode (key ...) #,rest-tmp composite.infoer-id composite.data))])
     new-tail)))

(define-syntax (set-infoer stx)
  (syntax-parse stx
    [(_ static-infos (mode keys rest-tmp composite-infoer-id composite-data))
     #:with composite-impl::binding-impl #'(composite-infoer-id static-infos composite-data)
     #:with composite-info::binding-info #'composite-impl.info
     (binding-info #'composite-info.annotation-str
                   #'composite-info.name-id
                   #'composite-info.static-infos
                   #'composite-info.bind-infos
                   #'set-matcher
                   #'set-committer
                   #'set-binder
                   #'(mode keys rest-tmp composite-info.matcher-id composite-info.committer-id composite-info.binder-id composite-info.data))]))

(define-syntax (set-matcher stx)
  (syntax-parse stx
    [(_ arg-id (mode keys rest-tmp composite-matcher-id composite-binder-id composite-committer-id composite-data)
        IF success failure)
     (define key-tmps (generate-temporaries #'keys))
     #`(IF (#,(if (eq? (syntax-e #'mode) 'ReadableSet) #'set? #'immutable-set?) arg-id)
           #,(let loop ([keys (syntax->list #'keys)]
                        [key-tmp-ids key-tmps])
               (cond
                 [(and (null? keys) (syntax-e #'rest-tmp))
                  #`(begin
                      (define rest-tmp (set-remove*/snapshot arg-id (list #,@key-tmps)))
                      (composite-matcher-id 'set composite-data IF success failure))]
                 [(null? keys)
                  #`(composite-matcher-id 'set composite-data IF success failure)]
                 [else
                  #`(begin
                      (define #,(car key-tmp-ids) (rhombus-expression #,(car keys)))
                      (IF (set-ref arg-id #,(car key-tmp-ids))
                          #,(loop (cdr keys) (cdr key-tmp-ids))
                          failure))]))
           failure)]))

(define (set-remove*/snapshot s ks)
  (set (for/fold ([ht (hash-snapshot (set-ht s))]) ([k (in-list ks)])
         (hash-remove ht k))))

(define-syntax (set-committer stx)
  (syntax-parse stx
    [(_ arg-id (mode keys rest-tmp composite-matcher-id composite-committer-id composite-binder-id composite-data))
     #`(composite-committer-id 'set composite-data)]))

(define-syntax (set-binder stx)
  (syntax-parse stx
    [(_ arg-id (mode keys rest-tmp composite-matcher-id composite-committer-id composite-binder-id composite-data))
     #`(composite-binder-id 'set composite-data)]))

(define-sequence-syntax in-set
  (lambda () #'in-set*)
  (lambda (stx)
    (syntax-case stx ()
      [[(d) (_ s)]
       #'[(d)
          (:do-in
           ([(ht) (set-ht s)])
           (void)
           ([i (hash-iterate-first ht)])
           i
           ([(d) (hash-iterate-key ht i)])
           #t
           #t
           [(hash-iterate-next ht i)])]]
      [_ #f])))

(define (in-set* s) (in-hash-keys (set-ht s)))

(define-annotation-constructor (Set of)
  ()
  #'immutable-set? set-static-infos
  1
  #f
  (lambda (arg-id predicate-stxs)
    #`(for/and ([v (in-hash-keys (set-ht #,arg-id))])
        (#,(car predicate-stxs) v)))
  (lambda (static-infoss)
    #`((#%sequence-element #,(car static-infoss))))
  #'set-build-convert #'())

(define-syntax (set-build-convert arg-id build-convert-stxs kws data)
  #`(for/fold ([ht #hashalw()] #:result (and ht (set ht)))
              ([v (in-hash-keys (set-ht #,arg-id))])
      #:break (not ht)
      (#,(car build-convert-stxs)
       v
       (lambda (v) (hash-set ht v #t))
       (lambda () #f))))

(define-static-info-syntax Set-build*
  (#%call-result #,set-static-infos)
  (#%function-arity -1)
  (#%indirect-static-info indirect-function-static-info))

(define (MutableSet-build . vals)
  (define ht (make-hashalw))
  (for ([v (in-list vals)])
    (hash-set! ht v #t))
  (set ht))

(define-for-syntax (parse-mutable-set stx repetition?)
  (syntax-parse stx
    [(form-id (~and content (_::braces . _)) . tail)
     (define-values (shape argss)
       (parse-setmap-content #'content
                             #:shape 'set
                             #:who (syntax-e #'form-id)
                             #:repetition? repetition?
                             #:list->set #'list->set
                             #:no-splice "mutable sets"))
     (values (cond
               [repetition?
                (build-compound-repetition
                 stx
                 (car argss)
                 (lambda args
                   (values (quasisyntax/loc stx
                             (MutableSet-build #,@args))
                           mutable-set-static-infos)))]
               [else (wrap-static-info*
                      (quasisyntax/loc stx
                        (MutableSet-build #,@(if (null? argss) null (car argss))))
                      mutable-set-static-infos)])
             #'tail)]
    [(_ . tail) (values (if repetition?
                            (identifier-repetition-use #'MutableSet-build)
                            #'MutableSet-build)
                        #'tail)]))

(define-annotation-syntax MutableSet (identifier-annotation #'mutable-set? mutable-set-static-infos))
(define-annotation-syntax ReadableSet (identifier-annotation #'set? readable-set-static-infos))

(define-syntax MutableSet
  (expression-transformer
   (lambda (stx) (parse-mutable-set stx #f))))

(define-repetition-syntax MutableSet
  (repetition-transformer
   (lambda (stx) (parse-mutable-set stx #t))))

(define-static-info-syntax MutableSet-build
  (#%call-result #,mutable-set-static-infos)
  (#%function-arity -1)
  (#%indirect-static-info indirect-function-static-info))

(define (set-ref s v)
  (hash-ref (set-ht s) v #f))

;; macro to optimize to an inline functional update
(define-syntax (Set.append/optimize stx)
  (syntax-parse stx
    [(_ set1 set2)
     (syntax-parse (unwrap-static-infos #'set2)
       [(id:identifier v)
        #:when (free-identifier=? (expr-quote Set-build) #'id)
        #'(set (hash-set (set-ht set1) v #t))]
       [_
        #'(Set.append set1 set2)])]))

;; for `++`
(define-static-info-syntax Set.append/optimize
  (#%call-result #,set-static-infos))

(define set-extend*
  (case-lambda
    [(set1 val) (set (hash-set (set-ht set1) val #t))]
    [(set1 . vals) (set-extend*/proc set1 vals)]))

(define (set-extend*/proc set1 vals)
  (set (for/fold ([ht (set-ht set1)]) ([k (in-list vals)])
         (hash-set ht k #t))))

(define (set-assert v)
  (unless (immutable-set? v)
    (raise-arguments-error* 'Set rhombus-realm
                            "not a set for splicing"
                            "value" v))
  v)

(define/method (Set.copy s)
  #:static-infos ((#%call-result #,mutable-set-static-infos))
  (check-readable-set who s)
  (set (hash-copy (set-ht s))))

(define/method (Set.snapshot s)
  #:static-infos ((#%call-result #,set-static-infos))
  (check-readable-set who s)
  (define ht (set-ht s))
  (if (immutable-hash? ht)
      s
      (set (hash-snapshot ht))))

(define (check-set who s)
  (unless (immutable-set? s)
    (raise-argument-error* who rhombus-realm "Set" s)))

(define (set-append/hash a b)
  (let-values ([(a b)
                (if ((hash-count a) . < . (hash-count b))
                    (values b a)
                    (values a b))])
    (for/fold ([a a]) ([k (in-hash-keys b)])
      (hash-set a k #t))))

(define (set-append s1 s2)
  (set (set-append/hash (set-ht s1) (set-ht s2))))

(define (set-append-all s1 ss)
  (set (for/fold ([ht (set-ht s1)])
                 ([s (in-list ss)])
         (set-append/hash ht (set-ht s)))))

(define/method Set.append
  #:static-infos ((#%call-result #,set-static-infos))
  (case-lambda
    [() empty-set]
    [(s)
     (check-set who s)
     s]
    [(s1 s2)
     (check-set who s1)
     (check-set who s2)
     (set-append s1 s2)]
    [(s1 . ss)
     (check-set who s1)
     (for ([s (in-list ss)])
       (check-set who s))
     (set-append-all s1 ss)]))

(define/method Set.union
  #:static-infos ((#%call-result #,set-static-infos))
  (case-lambda
    [() empty-set]
    [(s)
     (check-set who s)
     s]
    [(s1 s2)
     (check-set who s1)
     (check-set who s2)
     (set-append s1 s2)]
    [(s1 . ss)
     (check-set who s1)
     (for ([s (in-list ss)])
       (check-set who s))
     (set-append-all s1 ss)]))

(define (set-intersect/hash a b)
  (let-values ([(a b)
                (if ((hash-count a) . < . (hash-count b))
                    (values b a)
                    (values a b))])
    (for/hashalw ([k (in-hash-keys b)]
                  #:when (hash-ref a k #f))
      (values k #t))))

(define/method Set.intersect
  #:static-infos ((#%call-result #,set-static-infos))
  (case-lambda
    [() empty-set]
    [(s)
     (check-set who s)
     s]
    [(s1 s2)
     (check-set who s1)
     (check-set who s2)
     (set (set-intersect/hash (set-ht s1) (set-ht s2)))]
    [(s1 . ss)
     (check-set who s1)
     (for ([s (in-list ss)])
       (check-set who s))
     (set (for/fold ([ht (set-ht s1)])
                    ([s (in-list ss)])
            (set-intersect/hash ht (set-ht s))))]))

(define/method (Set.remove s v)
  #:static-infos ((#%call-result #,set-static-infos))
  (check-set who s)
  (set (hash-remove (set-ht s) v)))

(define (check-mutable-set who s)
  (unless (mutable-set? s)
    (raise-argument-error* who rhombus-realm "MutableSet" s)))

(define/method (MutableSet.set s v in?)
  (check-mutable-set who s)
  (if in?
      (hash-set! (set-ht s) v #t)
      (hash-remove! (set-ht s) v)))

(define/method (MutableSet.delete s v)
  (check-mutable-set who s)
  (hash-remove! (set-ht s) v))

(define/method (Set.to_list s [try-sort? #f])
  #:static-infos ((#%call-result #,treelist-static-infos))
  (check-set who s)
  (list->treelist (hash-keys (set-ht s) try-sort?)))
