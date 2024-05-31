#lang racket/base
(require (for-syntax racket/base
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
         "class-primitive.rkt"
         "rest-bind.rkt"
         "hash-remove.rkt"
         "key-comp.rkt"
         "key-comp-property.rkt"
         "number.rkt"
         "same-hash.rkt")

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
                     MutableSet
                     WeakMutableSet)
         (for-spaces (rhombus/namespace
                      rhombus/bind
                      rhombus/annot)
                     ReadableSet
                     ;; temporary:
                     (rename-out [ReadableSet SetView])))

(module+ for-binding
  (provide (for-syntax parse-set-binding)))

(module+ for-index
  (provide set?
           mutable-set?
           set-ref
           set-set!))

(module+ for-builtin
  (provide set?
           mutable-set?
           set-method-table
           mutable-set-method-table))

(module+ for-info
  (provide (for-syntax get-set-static-infos)
           Set-build))

(module+ for-build
  (provide set-append
           set-extend*
           set-assert
           list->set))

(module+ for-append
  (provide set-append
           immutable-set?))

(module+ for-print
  (provide set?
           mutable-set?
           set->list
           set-ht))

(module+ for-key-comp
  (provide immutable-equal-always-set?
           Set-build Set-build* list->set
           mutable-equal-always-set?
           MutableSet-build
           weak-mutable-equal-always-set?
           WeakMutableSet-build
           (for-space rhombus/statinfo
                      Set-build*
                      MutableSet-build
                      WeakMutableSet-build)

           immutable-object-set?
           ObjectSet-build ObjectSet-build* list->object-set
           mutable-object-set?
           MutableObjectSet-build
           weak-mutable-object-set?
           WeakMutableObjectSet-build
           (for-space rhombus/statinfo
                      ObjectSet-build*
                      MutableObjectSet-build
                      WeakMutableObjectSet-build)

           immutable-now-set?
           NowSet-build NowSet-build* list->now-set
           mutable-now-set?
           MutableNowSet-build
           weak-mutable-now-set?
           WeakMutableNowSet-build
           (for-space rhombus/statinfo
                      NowSet-build*
                      MutableNowSet-build
                      WeakMutableNowSet-build)

           immutable-number-or-object-set?
           NumberOrObjectSet-build NumberOrObjectSet-build* list->number-or-object-set
           mutable-number-or-object-set?
           MutableNumberOrObjectSet-build
           weak-mutable-number-or-object-set?
           WeakMutableNumberOrObjectSet-build
           (for-space rhombus/statinfo
                      NumberOrObjectSet-build*
                      MutableNumberOrObjectSet-build
                      WeakMutableNumberOrObjectSet-build)))

(module+ for-key-comp-macro
  (provide set?
           set
           set-ht))

(struct set (ht)
  #:sealed
  #:property prop:equal+hash
  (list (lambda (self other eql? mode)
          (eql? (set-ht self) (set-ht other)))
        (lambda (self hash-code mode)
          (hash-code (set-ht self))))
  #:property prop:sequence
  (lambda (s)
    (Set.to_sequence s)))

(define-static-info-getter get-any-set-static-infos
  (#%index-get Set.get)
  (#%sequence-constructor Set.to_sequence/optimize))

(define-primitive-class ReadableSet readable-set
  #:lift-declaration
  #:no-constructor-static-info
  #:instance-static-info #,(get-any-set-static-infos)
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
   [snapshot Set.snapshot]
   [to_sequence Set.to_sequence]))

(define-primitive-class Set set
  #:lift-declaration
  #:no-constructor-static-info
  #:instance-static-info ((#%append Set.append/optimize)
                          . #,(get-any-set-static-infos))
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
   [to_sequence Set.to_sequence]
   of
   [later_of Set.later_of]
   [by Set.by])
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
                          . #,(get-any-set-static-infos))
  #:existing
  #:opaque
  #:parent #f readable-set
  #:fields ()
  #:namespace-fields
  ([now_of MutableSet.now_of]
   [later_of MutableSet.later_of]
   [by MutableSet.by])
  #:properties
  ()
  #:methods
  (set
   delete))

(define-primitive-class WeakMutableSet weak-mutable-set
  #:lift-declaration
  #:no-constructor-static-info
  #:instance-static-info #,(get-mutable-set-static-infos)
  #:existing
  #:opaque
  #:parent #f mutable-set
  #:fields ()
  #:namespace-fields
  ([by WeakMutableSet.by])
  #:properties
  ()
  #:methods
  ())

(define (immutable-set? v) (and (set? v) (immutable-hash? (set-ht v))))
(define (mutable-set? v) (and (set? v) (mutable-hash? (set-ht v))))
(define (weak-mutable-set? v) (and (set? v) (mutable-hash? (set-ht v)) (hash-weak? (set-ht v))))
(define (immutable-equal-always-set? v) (and (set? v) (immutable-hash? (set-ht v)) (hash-equal-always? (set-ht v))))
(define (immutable-object-set? v) (and (set? v) (immutable-hash? (set-ht v)) (hash-eq? (set-ht v))))
(define (immutable-now-set? v) (and (set? v) (immutable-hash? (set-ht v)) (not (custom-map-ref (set-ht v) #f)) (hash-equal? (set-ht v))))
(define (immutable-number-or-object-set? v) (and (set? v) (immutable-hash? (set-ht v)) (hash-eqv? (set-ht v))))
(define (mutable-equal-always-set? v) (and (set? v) (mutable-hash? (set-ht v)) (hash-equal-always? (set-ht v))))
(define (mutable-object-set? v) (and (set? v) (mutable-hash? (set-ht v)) (hash-eq? (set-ht v))))
(define (mutable-now-set? v) (and (set? v) (mutable-hash? (set-ht v)) (not (custom-map-ref (set-ht v) #f)) (hash-equal? (set-ht v))))
(define (mutable-number-or-object-set? v) (and (set? v) (mutable-hash? (set-ht v)) (hash-eqv? (set-ht v))))
(define (weak-mutable-equal-always-set? v) (and (set? v) (mutable-hash? (set-ht v)) (hash-weak? (set-ht v)) (hash-equal-always? (set-ht v))))
(define (weak-mutable-object-set? v) (and (set? v) (mutable-hash? (set-ht v)) (hash-weak? (set-ht v)) (hash-eq? (set-ht v))))
(define (weak-mutable-now-set? v) (and (set? v) (mutable-hash? (set-ht v)) (hash-weak? (set-ht v)) (not (custom-map-ref (set-ht v) #f)) (hash-equal? (set-ht v))))
(define (weak-mutable-number-or-object-set? v) (and (set? v) (mutable-hash? (set-ht v)) (hash-weak? (set-ht v)) (hash-eqv? (set-ht v))))

(define (check-readable-set who s)
  (unless (set? s)
    (raise-argument-error* who rhombus-realm "ReadableSet" s)))

(define/method (Set.length s)
  #:static-infos ((#%call-result #,(get-int-static-infos)))
  (check-readable-set who s)
  (hash-count (set-ht s)))

(define (set-ref s v)
  (hash-ref (set-ht s) v #f))

(define/method (Set.get s v)
  (check-readable-set who s)
  (set-ref s v))

(define-syntax (Set-build stx)
  (syntax-parse stx
    [(_ elem ...)
     #`(set (hashalw (~@ elem #t) ...))]))

(define-syntax (ObjectSet-build stx)
  (syntax-parse stx
    [(_ elem ...)
     #`(set (hasheq (~@ elem #t) ...))]))

(define-syntax (NowSet-build stx)
  (syntax-parse stx
    [(_ elem ...)
     #`(set (hash (~@ elem #t) ...))]))

(define-syntax (NumberOrObjectSet-build stx)
  (syntax-parse stx
    [(_ elem ...)
     #`(set (hasheqv (~@ elem #t) ...))]))

(define Set-build*
  (let ([Set (lambda vals
               (define base-ht (hashalw))
               (set (for/fold ([ht base-ht]) ([val (in-list vals)])
                      (hash-set ht val #t))))])
    Set))

(define ObjectSet-build*
  (let ([|Set.by(===)| (lambda vals
                         (define base-ht (hasheq))
                         (set (for/fold ([ht base-ht]) ([val (in-list vals)])
                                (hash-set ht val #t))))])
    |Set.by(===)|))

(define NowSet-build*
  (let ([|Set.by(is_now)| (lambda vals
                            (define base-ht (hash))
                            (set (for/fold ([ht base-ht]) ([val (in-list vals)])
                                   (hash-set ht val #t))))])
    |Set.by(is_now)|))

(define NumberOrObjectSet-build*
  (let ([|Set.by(is_number_or_object)| (lambda vals
                                         (define base-ht (hasheqv))
                                         (set (for/fold ([ht base-ht]) ([val (in-list vals)])
                                                (hash-set ht val #t))))])
    |Set.by(is_number_or_object)|))

(define (list->set l) (apply Set-build* l))
(define (list->object-set l) (apply ObjectSet-build* l))
(define (list->now-set l) (apply NowSet-build* l))
(define (list->number-or-object-set l) (apply NumberOrObjectSet-build* l))

(define (set->list s [try-sort? #f]) (hash-keys (set-ht s) try-sort?))

(define empty-set (set #hashalw()))
(define-static-info-syntax empty-set
  #:getter get-set-static-infos)

(define empty-readable-set empty-set)
(define-static-info-syntax empty-readable-set
  #:getter get-readable-set-static-infos)

(define-for-syntax (make-empty-set-binding get-name+hash?-id+static-infos)
  (binding-transformer
   (lambda (stx)
     (syntax-parse stx
       [(form-id . tail)
        (values
         (binding-form #'empty-set-infoer (get-name+hash?-id+static-infos))
         #'tail)]))))

(define-binding-syntax empty-set
  (make-empty-set-binding
   (lambda ()
     #`["Set.empty" immutable-hash? #,(get-set-static-infos)])))
(define-binding-syntax empty-readable-set
  (make-empty-set-binding
   (lambda ()
     #`["ReadableSet.empty" hash? #,(get-readable-set-static-infos)])))

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
     #'(IF (and (set? arg-id) (let ([ht (set-ht arg-id)])
                                (and (hash? ht)
                                     (eqv? 0 (hash-count ht)))))
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
                           (get-set-static-infos)
                           #'ht)
         #'tail)]))))

(define-reducer-syntax Set.by
  (reducer-transformer
   (lambda (stx)
     (parse-key-comp stx
                     (lambda (stx arg-stxes str mapper)
                       (syntax-parse stx
                         [(_ . tail)
                          (values
                           (reducer/no-break #'build-set-reduce
                                             #`([ht #,(key-comp-empty-stx mapper)])
                                             #'build-set-add
                                             (get-set-static-infos)
                                             #'ht)
                           #'tail)]))))))

(define-syntax (build-set-reduce stx)
  (syntax-parse stx
    [(_ ht-id e) #'(set e)]))

(define-syntax (build-set-add stx)
  (syntax-parse stx
    [(_ ht-id v) #'(hash-set ht-id v #t)]))

(define-for-syntax (parse-set stx arg-stxes repetition? set-build-id set-build*-id list->set-id)
  (syntax-parse stx
    [(form-id (~and content (_::braces . _)) . tail)
     (define-values (shape argss) (parse-setmap-content #'content
                                                        #:shape 'set
                                                        #:who (syntax-e #'form-id)
                                                        #:repetition? repetition?))
     (values (relocate-wrapped
              (respan (datum->syntax #f (append (list #'form-id) arg-stxes (list #'content))))
              (build-setmap stx argss
                            set-build-id
                            #'set-extend*
                            #'set-append
                            #'set-assert
                            (get-set-static-infos)
                            #:repetition? repetition?
                            #:list->setmap list->set-id))
             #'tail)]
    [(form-id . tail) (values (if repetition?
                                  (identifier-repetition-use set-build*-id)
                                  (relocate+reraw #'form-id set-build*-id))
                        #'tail)]))

(define-syntax Set
  (expression-transformer
   (lambda (stx) (parse-set stx '() #f #'Set-build #'Set-build* #'list->set))))

(define-syntax Set.by
  (expression-transformer
   (lambda (stx)
     (parse-key-comp stx
                     (lambda (stx arg-stxes str mapper)
                       (parse-set stx arg-stxes #f
                                  (key-comp-set-build-id mapper)
                                  (key-comp-set-build*-id mapper)
                                  (key-comp-list->set-id mapper)))))))

(define-for-syntax (make-set-binding-transformer mode)
  (lambda (stx)
    (syntax-parse stx
      [(form-id (~and content (_::braces . _)) . tail)
       (parse-set-binding (syntax-e #'form-id) stx "braces" mode)]
      [(form-id (_::parens arg ...) . tail)
       (parse-set-binding (syntax-e #'form-id) stx "parentheses" mode)])))

(define-binding-syntax Set
  (binding-transformer
   (make-set-binding-transformer #'("Set" immutable-set? values))))

(define-binding-syntax ReadableSet
  (binding-transformer
   (make-set-binding-transformer #'("ReadableSet" set? hash-snapshot))))

(define-binding-syntax Set.by
  (binding-transformer
   (lambda (stx)
     (parse-key-comp stx
                     (lambda (stx arg-stxes str mapper)
                       (define mode #`(#,str #,(key-comp-set?-id mapper) values))
                       ((make-set-binding-transformer mode) stx))))))

(define-repetition-syntax Set
  (repetition-transformer
   (lambda (stx) (parse-set stx '() #t #'Set-build #'Set-build* #'list->set))))

(define-repetition-syntax Set.by
  (repetition-transformer
   (lambda (stx)
     (parse-key-comp stx
                     (lambda (stx arg-stxes str mapper)
                       (parse-set stx arg-stxes #t
                                  (key-comp-set-build-id mapper)
                                  (key-comp-set-build*-id mapper)
                                  (key-comp-list->set-id mapper)))))))

(define-for-syntax (parse-set-binding who stx opener+closer [mode #'("Set" set? values)])
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
                           #`(#,group-tag rest-bind #,(get-set-static-infos)
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
    (define mode-desc (syntax-parse mode [(desc . _) (syntax-e #'desc)]))
    (define-values (composite new-tail)
      (composite-binding-transformer #`(form-id (parens) . tail)
                                     #:rest-arg maybe-rest
                                     (cons mode-desc (map shrubbery-syntax->string keys))
                                     #'(lambda (v) #t) ; predicate built into set-matcher
                                     '()
                                     '()
                                     #:static-infos (get-set-static-infos)
                                     #:sequence-element-info? #t
                                     #:rest-accessor
                                     (and maybe-rest
                                          (if rest-repetition?
                                              #`(lambda (v) (set->list #,rest-tmp))
                                              #`(lambda (v) #,rest-tmp)))
                                     #:rest-repetition? rest-repetition?))
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
    [(_ arg-id ([desc pred filter] keys rest-tmp composite-matcher-id composite-binder-id composite-committer-id composite-data)
        IF success failure)
     (define key-tmps (generate-temporaries #'keys))
     #`(IF (pred arg-id)
           (begin
             (define ht (set-ht arg-id))
             #,@(for/foldr ([forms (append (if (syntax-e #'rest-tmp)
                                               (list #`(define rest-tmp
                                                         (set (hash-remove*
                                                               (filter ht)
                                                               (list #,@key-tmps)))))
                                               '())
                                           (list #'(composite-matcher-id 'set composite-data IF success failure)))])
                           ([key (in-list (syntax->list #'keys))]
                            [key-tmp-id (in-list key-tmps)])
                  (list #`(define #,key-tmp-id (rhombus-expression #,key))
                        #`(IF (hash-ref ht #,key-tmp-id #f)
                              (begin #,@forms)
                              failure))))
           failure)]))

(define-syntax (set-committer stx)
  (syntax-parse stx
    [(_ arg-id (mode keys rest-tmp composite-matcher-id composite-committer-id composite-binder-id composite-data))
     #`(composite-committer-id 'set composite-data)]))

(define-syntax (set-binder stx)
  (syntax-parse stx
    [(_ arg-id (mode keys rest-tmp composite-matcher-id composite-committer-id composite-binder-id composite-data))
     #`(composite-binder-id 'set composite-data)]))

(define-for-syntax set-annotation-make-predicate
  (lambda (arg-id predicate-stxs)
    #`(for/and ([v (in-immutable-hash-keys (set-ht #,arg-id))])
        (#,(car predicate-stxs) v))))

(define-for-syntax set-annotation-make-static-info
  (lambda (static-infoss)
    #`((#%sequence-element #,(car static-infoss)))))

(define-annotation-constructor (Set of)
  ()
  #'immutable-set? #,(get-set-static-infos)
  1
  #f
  set-annotation-make-predicate
  set-annotation-make-static-info
  #'set-build-convert #'(#hashalw()))

(define-for-syntax (make-set-later-chaperoner who)
  (lambda (predicate-stxes annot-strs)
    #`(lambda (st)
        (let ([k-pred #,(car predicate-stxes)]
              [k-str #,(car annot-strs)])
          (chaperone-struct
           st
           set-ht
           (lambda (st ht)
             (chaperone-hash ht
                             ;; ref
                             (lambda (ht k)
                               (values (check-set-element '#,who k k-pred k-str)
                                       (lambda (ht k v) v)))
                             ;; set
                             (lambda (ht k v)
                               (values (check-set-element '#,who k k-pred k-str) v))
                             ;; remove
                             (lambda (ht k) (check-set-element '#,who k k-pred k-str))
                             ;; key
                             (lambda (ht k) (check-set-element '#,who k k-pred k-str))
                             ;; clear
                             (lambda (ht) (void))
                             ;; equal-key-proc
                             #f)))))))

(define-annotation-constructor (Set/again Set.later_of)
  ()
  #'immutable-set? #,(get-set-static-infos)
  1
  #f
  (make-set-later-chaperoner 'Set)
  (lambda (static-infoss) #'())
  #'mutable-set-build-convert #'()
  #:parse-of parse-annotation-of/chaperone)

(define-annotation-syntax Set.by
  (annotation-prefix-operator
   '((default . stronger))
   'macro
   (lambda (stx)
     (parse-key-comp stx
                     (lambda (stx arg-stxes str mapper)
                       (syntax-parse stx
                         #:datum-literals (op |.| of)
                         [(form-id (~and dot (op |.|)) (~and of-id of) . tail)
                          (parse-annotation-of #'(of-id . tail)
                                               (key-comp-set?-id mapper) (get-set-static-infos)
                                               1 #f
                                               set-annotation-make-predicate
                                               set-annotation-make-static-info
                                               #'set-build-convert #`(#,(key-comp-empty-stx mapper)))]
                         [(_ . tail)
                          (values (annotation-predicate-form (key-comp-set?-id mapper)
                                                             (get-set-static-infos))
                                  #'tail)]))))))

(define-syntax (set-build-convert arg-id build-convert-stxs kws data)
  (syntax-parse data
    [(empty-ht)
     #`(for/fold ([ht empty-ht] #:result (and ht (set ht)))
                 ([v (in-immutable-hash-keys (set-ht #,arg-id))])
         #:break (not ht)
         (#,(car build-convert-stxs)
          v
          (lambda (v) (hash-set ht v #t))
          (lambda () #f)))]))

(define-static-info-syntaxes (Set-build*
                              ObjectSet-build*
                              NowSet-build*
                              NumberOrObjectSet-build*)
  (#%call-result #,(get-set-static-infos))
  (#%function-arity -1)
  . #,(indirect-get-function-static-infos))

(define (mutable-set-build ht vals)
  (for ([v (in-list vals)])
    (hash-set! ht v #t))
  (set ht))

(define (MutableSet-build . vals)
  (mutable-set-build (make-hashalw) vals))
(define (MutableObjectSet-build . vals)
  (mutable-set-build (make-hasheq) vals))
(define (MutableNowSet-build . vals)
  (mutable-set-build (make-hash) vals))
(define (MutableNumberOrObjectSet-build . vals)
  (mutable-set-build (make-hasheqv) vals))

(define (WeakMutableSet-build . vals)
  (mutable-set-build (make-weak-hashalw) vals))
(define (WeakMutableObjectSet-build . vals)
  (mutable-set-build (make-weak-hasheq) vals))
(define (WeakMutableNowSet-build . vals)
  (mutable-set-build (make-weak-hash) vals))
(define (WeakMutableNumberOrObjectSet-build . vals)
  (mutable-set-build (make-weak-hasheqv) vals))

(define-for-syntax (parse-mutable-set stx repetition? mutable-set-build-id)
  (syntax-parse stx
    [(form-id (~and content (_::braces . _)) . tail)
     (define-values (shape argss)
       (parse-setmap-content #'content
                             #:shape 'set
                             #:who (syntax-e #'form-id)
                             #:repetition? repetition?
                             #:no-splice "mutable sets"))
     (values (cond
               [repetition?
                (build-compound-repetition
                 stx
                 (car argss)
                 (lambda args
                   (values (quasisyntax/loc stx
                             (#,mutable-set-build-id #,@args))
                           (get-mutable-set-static-infos))))]
               [else (wrap-static-info*
                      (quasisyntax/loc stx
                        (#,mutable-set-build-id #,@(if (null? argss) null (car argss))))
                      (get-mutable-set-static-infos))])
             #'tail)]
    [(form-id . tail) (values (if repetition?
                                  (identifier-repetition-use mutable-set-build-id)
                                  (relocate+reraw #'form-id mutable-set-build-id))
                              #'tail)]))

(define-annotation-syntax WeakMutableSet (identifier-annotation weak-mutable-set? #,(get-mutable-set-static-infos)))
(define-annotation-syntax ReadableSet (identifier-annotation set? #,(get-readable-set-static-infos)))

(define-annotation-constructor (MutableSet MutableSet.now_of)
  ()
  #'mutable-set? #,(get-mutable-set-static-infos)
  1
  #f
  (lambda (arg-id predicate-stxs)
    #`(for/and ([k (in-hash-keys (set-ht #,arg-id))])
        (#,(car predicate-stxs) k)))
  (lambda (static-infoss) #'())
  "converter annotation not supported for elements;\n immediate checking needs a predicate annotation for the mutable set content"
  #'())

(define-annotation-constructor (MutableSet/again MutableSet.later_of)
  ()
  #'mutable-set? #,(get-mutable-set-static-infos)
  1
  #f
  (make-set-later-chaperoner 'MutableSet)
  (lambda (static-infoss) #'())
  #'mutable-set-build-convert #'()
  #:parse-of parse-annotation-of/chaperone)

(define (raise-item-check-error who what x annot-str)
  (raise-binding-failure 'MutableMap what x annot-str))

(define (check-set-element who k k-pred k-str)
  (unless (k-pred k) (raise-item-check-error who "element" k k-str))
  k)

(define-syntax (mutable-set-build-convert arg-id build-convert-stxs kws data)
  (with-syntax ([[(k-annot-str v-annot-str . _) _] data])
    #`(let ([k-cvt #,(car build-convert-stxs)]
            [k-str k-annot-str]
            [v-cvt #,(cadr build-convert-stxs)]
            [v-str v-annot-str])
        (impersonate-struct
         st
         struct:set
         set-ht
         (lambda (ht)
           (impersonate-hash ht
                             ;; ref
                             (lambda (ht k)
                               (values (convert-set-element k k-cvt k-str)
                                       (lambda (ht k v) v)))
                             ;; set
                             (lambda (ht k v)
                               (values (convert-set-element k k-cvt k-str) v))
                             ;; remove
                             (lambda (ht k) (convert-set-element k k-cvt k-str))
                             ;; key
                             (lambda (ht k) (convert-set-element k k-cvt k-str))
                             ;; clear
                             (lambda (ht) (void))
                             ;; equal-key-proc
                             #f))))))

(define (convert-set-element x cvt annot-str)
  (cvt
   x
   (lambda (x) x)
   (lambda ()
     (raise-item-check-error 'MutableSet "element" x annot-str))))

(define-annotation-syntax MutableSet.by
  (annotation-prefix-operator
   '((default . stronger))
   'macro
   (lambda (stx)
     (parse-key-comp stx
                     (lambda (stx arg-stxes str mapper)
                       (syntax-parse stx
                         [(_ . tail)
                          (values (annotation-predicate-form (key-comp-mutable-set?-id mapper)
                                                             (get-mutable-set-static-infos))
                                  #'tail)]))))))

(define-annotation-syntax WeakMutableSet.by
  (annotation-prefix-operator
   '((default . stronger))
   'macro
   (lambda (stx)
     (parse-key-comp stx
                     (lambda (stx arg-stxes str mapper)
                       (syntax-parse stx
                         [(_ . tail)
                          (values (annotation-predicate-form (key-comp-weak-mutable-set?-id mapper)
                                                             (get-mutable-set-static-infos))
                                  #'tail)]))))))

(define-syntax MutableSet
  (expression-transformer
   (lambda (stx) (parse-mutable-set stx #f #'MutableSet-build))))

(define-repetition-syntax MutableSet
  (repetition-transformer
   (lambda (stx) (parse-mutable-set stx #t  #'MutableSet-build))))

(define-syntax WeakMutableSet
  (expression-transformer
   (lambda (stx) (parse-mutable-set stx #f #'WeakMutableSet-build))))

(define-repetition-syntax WeakMutableSet
  (repetition-transformer
   (lambda (stx) (parse-mutable-set stx #t  #'WeakMutableSet-build))))

(define-syntax MutableSet.by
  (expression-transformer
   (lambda (stx)
     (parse-key-comp stx
                     (lambda (stx arg-stxes str mapper)
                       (parse-mutable-set stx #f
                                          (key-comp-mutable-set-build-id mapper)))))))

(define-repetition-syntax MutableSet.by
  (repetition-transformer
   (lambda (stx)
     (parse-key-comp stx
                     (lambda (stx arg-stxes str mapper)
                       (parse-mutable-set stx #t
                                          (key-comp-mutable-set-build-id mapper)))))))

(define-static-info-syntaxes (MutableSet-build
                              MutableObjectSet-build
                              MutableNowSet-build
                              MutableNumberOrObjectSet-build)
  (#%call-result #,(get-mutable-set-static-infos))
  (#%function-arity -1)
  . #,(indirect-get-function-static-infos))

(define-syntax WeakMutableSet.by
  (expression-transformer
   (lambda (stx)
     (parse-key-comp stx
                     (lambda (stx arg-stxes str mapper)
                       (parse-mutable-set stx #f
                                          (key-comp-weak-mutable-set-build-id mapper)))))))

(define-repetition-syntax WeakMutableSet.by
  (repetition-transformer
   (lambda (stx)
     (parse-key-comp stx
                     (lambda (stx arg-stxes str mapper)
                       (parse-mutable-set stx #t
                                          (key-comp-weak-mutable-set-build-id mapper)))))))

(define-static-info-syntaxes (WeakMutableSet-build
                              WeakMutableObjectSet-build
                              WeakMutableNowSet-build
                              WeakMutableNumberOrObjectSet-build)
  (#%call-result #,(get-weak-mutable-set-static-infos))
  (#%function-arity -1)
  . #,(indirect-get-function-static-infos))

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
  (#%call-result #,(get-set-static-infos)))

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
  #:static-infos ((#%call-result #,(get-mutable-set-static-infos)))
  (check-readable-set who s)
  (set (hash-copy (set-ht s))))

(define/method (Set.snapshot s)
  #:static-infos ((#%call-result #,(get-set-static-infos)))
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
                (if (and ((hash-count a) . < . (hash-count b))
                         (same-hash? a b))
                    (values b a)
                    (values a b))])
    (for/fold ([a a]) ([k (in-immutable-hash-keys b)])
      (hash-set a k #t))))

(define (set-append s1 s2)
  (set (set-append/hash (set-ht s1) (set-ht s2))))

(define (set-append-all s1 ss)
  (set (for/fold ([ht (set-ht s1)])
                 ([s (in-list ss)])
         (set-append/hash ht (set-ht s)))))

(define/method Set.append
  #:static-infos ((#%call-result #,(get-set-static-infos)))
  (case-lambda
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
  #:static-infos ((#%call-result #,(get-set-static-infos)))
  (case-lambda
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
  (define new-ht (same-hash-empty a))
  (let-values ([(a b)
                (if ((hash-count a) . < . (hash-count b))
                    (values b a)
                    (values a b))])
    (for/fold ([new-ht new-ht])
              ([k (in-immutable-hash-keys b)]
               #:when (hash-ref a k #f))
      (hash-set new-ht k #t))))

(define/method Set.intersect
  #:static-infos ((#%call-result #,(get-set-static-infos)))
  (case-lambda
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
  #:static-infos ((#%call-result #,(get-set-static-infos)))
  (check-set who s)
  (set (hash-remove (set-ht s) v)))

(define (check-mutable-set who s)
  (unless (mutable-set? s)
    (raise-argument-error* who rhombus-realm "MutableSet" s)))

(define (set-set! s v in?)
  (if in?
      (hash-set! (set-ht s) v #t)
      (hash-remove! (set-ht s) v)))

(define/method (MutableSet.set s v in?)
  (check-mutable-set who s)
  (set-set! s v in?))

(define/method (MutableSet.delete s v)
  (check-mutable-set who s)
  (hash-remove! (set-ht s) v))

(define/method (Set.to_list s [try-sort? #f])
  #:static-infos ((#%call-result #,(get-treelist-static-infos)))
  (check-set who s)
  (list->treelist (set->list s try-sort?)))

(define-sequence-syntax Set.to_sequence/optimize
  (lambda () #'Set.to_sequence)
  (lambda (stx)
    (syntax-parse stx
      [[(id) (_ st-expr)]
       #'[(id) (in-hash-keys (let ([st st-expr])
                               (check-readable-set 'Set.to_sequence st)
                               (set-ht st)))]]
      [_ #f])))

(define/method (Set.to_sequence st)
  #:static-infos ((#%call-result ((#%sequence-constructor #t))))
  (check-readable-set who st)
  (in-hash-keys (set-ht st)))
