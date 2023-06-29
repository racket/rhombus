#lang racket/base
(require (for-syntax racket/base
                     racket/symbol
                     syntax/parse/pre
                     racket/syntax
                     "srcloc.rkt"
                     "tag.rkt"
                     "with-syntax.rkt"
                     shrubbery/print)
         "provide.rkt"
         "expression.rkt"
         "binding.rkt"
         "repetition.rkt"
         "compound-repetition.rkt"
         (submod "annotation.rkt" for-class)
         (submod "dot.rkt" for-dot-provider)
         (submod "list.rkt" for-compound-repetition)
         "name-root.rkt"
         "static-info.rkt"
         "reducer.rkt"
         "index-key.rkt"
         "append-key.rkt"
         "call-result-key.rkt"
         "function-arity-key.rkt"
         "sequence-constructor-key.rkt"
         "parse.rkt"
         "literal.rkt"
         "realm.rkt"
         "setmap-parse.rkt"
         "dot-parse.rkt"
         "parens.rkt"
         "composite.rkt"
         "define-arity.rkt"
         (only-in "lambda-kwrest.rkt" hash-remove*)
         "op-literal.rkt"
         "hash-snapshot.rkt"
         "mutability.rkt")

(provide (for-spaces (rhombus/namespace
                      #f
                      rhombus/bind
                      rhombus/repet
                      rhombus/reducer
                      rhombus/annot)
                     Set)
         (for-spaces (#f
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
           set-method-table))

(module+ for-info
  (provide (for-syntax set-static-info)
           Set-build))

(module+ for-build
  (provide set-append
           set-append/proc
           set-extend*
           set-assert
           list->set))

(struct set (ht)
  #:property prop:equal+hash
  (list (lambda (self other eql? mode)
          (eql? (set-ht self) (set-ht other)))
        (lambda (self hash-code mode)
          (hash-code (set-ht self))))
  #:property prop:sequence
  (lambda (s)
    (in-set s)))

(define (immutable-set? v) (and (set? v) (immutable-hash? (set-ht v))))
(define (mutable-set? v) (and (set? v) (mutable-hash? (set-ht v))))

(define (set-member? s v)
  (hash-ref (set-ht s) v #f))

(define (set-member! s v in?)
  (if in?
      (hash-set! (set-ht s) v #t)
      (hash-remove! (set-ht s) v)))

(define/arity #:name Set.length (set-count s)
  (hash-count (set-ht s)))

(define-syntax set-instance
  (dot-provider
   (dot-parse-dispatch
    (lambda (field-sym field ary 0ary nary fail-k)
      (case field-sym
        [(length) (0ary #'set-count)]
        [(copy) (0ary #'Set.copy mutable-set-static-info)]
        [(snapshot) (0ary #'Set.snapshot set-static-info)]
        [(remove) (nary #'Set.remove 2 #'Set.remove set-static-info)]
        [(append) (nary #'Set.append -1 #'Set.append set-static-info)]
        [(union) (nary #'Set.union -1 #'Set.union set-static-info)]
        [(intersect) (nary #'Set.intersect -1 #'Set.intersect set-static-info)]
        [(to_list) (nary #'Set.to_list 3 #'Set.to_list list-static-infos)]
        [else (fail-k)])))))

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

(define-syntaxes (empty-set empty-set-view)
  (let ([t (expression-transformer
            (lambda (stx)
              (syntax-parse stx
                [(form-id . tail)
                 (values #'(set #hashalw()) #'tail)])))])
    (values t t)))

(define-for-syntax (make-empty-set-binding hash?-id)
  (binding-transformer
   (lambda (stx)
     (syntax-parse stx
       [(form-id . tail)
        (values (binding-form #'empty-set-infoer hash?-id) #'tail)]))))

(define-binding-syntax empty-set (make-empty-set-binding #'immutable-hash?))
(define-binding-syntax empty-set-view (make-empty-set-binding #'hash?))

(define-syntax (empty-set-infoer stx)
  (syntax-parse stx
    [(_ static-infos hash?)
     (binding-info "Set.empty"
                   #'empty-set
                   #'static-infos
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
                           set-static-info
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
              (respan #'(form-id content))
              (build-setmap stx argss
                            #'Set-build
                            #'set-extend*
                            #'set-append
                            #'set-assert
                            set-static-info
                            #:repetition? repetition?
                            #:list->setmap #'list->set))
             #'tail)]
    [(_ . tail) (values (if repetition?
                            (identifier-repetition-use #'Set-build*)
                            #'Set-build*)
                        #'tail)]))

(define-name-root Set
  #:fields
  ([empty empty-set]
   [length set-count]
   [append Set.append]
   [union Set.union]
   [intersect Set.intersect]
   [remove Set.remove]
   [to_list Set.to_list]
   [copy Set.copy]
   [snapshot Set.snapshot]
   of))

(define-name-root ReadableSet
  #:fields
  ([empty empty-set-view]))

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
    #:datum-literals (parens block group op)
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
                           #`(#,group-tag rst ...)
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
                                           #:rest-accessor
                                           (and maybe-rest
                                                (if rest-repetition?
                                                    #`(lambda (v) (set->list #,rest-tmp))
                                                    #`(lambda (v) #,rest-tmp)))
                                           #:rest-repetition? (and rest-repetition?
                                                                   'pair))
       #`(form-id (parens) . tail)
       maybe-rest))
    (with-syntax-parse ([composite::binding-form composite])
      (values
       (binding-form #'set-infoer
                     #`(#,mode (key ...)
                        #,rest-tmp
                        composite.infoer-id
                        composite.data))
       new-tail))))

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
                      (define rest-tmp (set-remove*/copy arg-id (list #,@key-tmps)))
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

(define (set-remove*/copy s ks)
  (define h (set-ht s))
  (set (hash-remove* (if (immutable? h) h (hash-map/copy h values #:kind 'immutable)) ks)))

(define-syntax (set-committer stx)
  (syntax-parse stx
    [(_ arg-id (mode keys rest-tmp composite-matcher-id composite-committer-id composite-binder-id composite-data))
     #`(composite-committer-id 'set composite-data)]))

(define-syntax (set-binder stx)
  (syntax-parse stx
    [(_ arg-id (mode keys rest-tmp composite-matcher-id composite-committer-id composite-binder-id composite-data))
     #`(composite-binder-id 'set composite-data)]))

(define-sequence-syntax in-set
  (lambda (stx) #'in-set*)
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

(define-for-syntax set-static-info
  #'((#%index-get set-member?)
     (#%append set-append)
     (#%sequence-constructor in-set)
     (#%dot-provider set-instance)))

(define-for-syntax mutable-set-static-info
  #`((#%index-set set-member!)
     . #,set-static-info))

(define-annotation-constructor (Set of)
  ()
  #'immutable-set? set-static-info
  1
  #f
  (lambda (arg-id predicate-stxs)
    #`(for/and ([v (in-hash-keys (set-ht #,arg-id))])
        (#,(car predicate-stxs) v)))
  (lambda (static-infoss)
    #`())
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
  (#%call-result #,set-static-info)
  (#%function-arity -1))

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
                           mutable-set-static-info)))]
               [else (wrap-static-info*
                      (quasisyntax/loc stx
                        (MutableSet-build #,@(if (null? argss) null (car argss))))
                      mutable-set-static-info)])
             #'tail)]
    [(_ . tail) (values (if repetition?
                            (identifier-repetition-use #'MutableSet-build)
                            #'MutableSet-build)
                        #'tail)]))

(define-annotation-syntax MutableSet (identifier-annotation #'mutable-set? mutable-set-static-info))
(define-annotation-syntax ReadableSet (identifier-annotation #'set? set-static-info))

(define-syntax MutableSet
  (expression-transformer
   (lambda (stx) (parse-mutable-set stx #f))))

(define-repetition-syntax MutableSet
  (repetition-transformer
   (lambda (stx) (parse-mutable-set stx #t))))

(define-static-info-syntax MutableSet-build
  (#%call-result #,mutable-set-static-info))

(define (set-ref s v)
  (hash-ref (set-ht s) v #f))

;; macro to optimize to an inline functional update
(define-syntax (set-append stx)
  (syntax-parse stx
    [(_ set1 set2)
     (syntax-parse (unwrap-static-infos #'set2)
       [(id:identifier v)
        #:when (free-identifier=? (expr-quote Set-build) #'id)
        #'(set (hash-set (set-ht set1) v #t))]
       [_
        #'(set-append/proc set1 set2)])]))

(define (set-append/proc set1 set2)
  (define ht1 (set-ht set1))
  (define ht2 (set-ht set2))
  (let-values ([(ht1 ht2)
                (if ((hash-count ht2) . < . (hash-count ht1))
                    (values ht1 ht2)
                    (values ht2 ht1))])
    (set (for/fold ([ht ht1]) ([k (in-hash-keys ht2)])
           (hash-set ht k #t)))))

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

(define/arity (Set.copy s)
  #:static-infos ((#%call-result #,mutable-set-static-info))
  (unless (set? s) (raise-argument-error* 'Set.copy rhombus-realm "ReadableSet" s))
  (set (hash-copy (set-ht s))))

(define/arity (Set.snapshot s)
  #:static-infos ((#%call-result #,set-static-info))
  (unless (set? s) (raise-argument-error* 'Set.copy rhombus-realm "ReadableSet" s))
  (define ht (set-ht s))
  (if (immutable-hash? ht)
      s
      (set (hash-snapshot ht))))

(define (set-union who s1 ss)
  (unless (set? s1)
    (raise-argument-error* who "Set" s1))
  (let loop ([s s1] [ss ss])
       (if (null? ss)
           s
           (let ([s1 (car ss)])
             (unless (set? s1)
               (raise-argument-error* who "Set" s1))
             (loop (set-append s s1) (cdr ss))))))

(define/arity Set.append
  #:static-infos ((#%call-result #,mutable-set-static-info))
  (case-lambda
    [() (set #hashalw())]
    [(s)
     (unless (set? s)
       (raise-argument-error* 'Set.append "Set" s))
     (Set.snapshot s)]
    [(s1 . ss)
     (set-union 'Set.append s1 ss)]))

(define/arity Set.union
  #:static-infos ((#%call-result #,mutable-set-static-info))
  (case-lambda
    [() (set #hashalw())]
    [(s)
     (unless (set? s)
       (raise-argument-error* 'Set.union "Set" s))
     (Set.snapshot s)]
    [(s1 . ss)
     (set-union 'Set.union s1 ss)]))

(define/arity Set.intersect
  #:static-infos ((#%call-result #,mutable-set-static-info))
  (case-lambda
    [() (set #hashalw())]
    [(s)
     (define who 'Set.intersect)
     (unless (set? s)
       (raise-argument-error* who "Set" s))
     (Set.snapshot s)]
    [(s1 . ss)
     (define who 'Set.intersect)
     (unless (set? s1)
       (raise-argument-error* who "Set" s1))
     (define (int a b)
       (if ((hash-count a) . < . (hash-count b))
           (int b a)
           (for/hashalw ([k (in-hash-keys b)]
                         #:when (hash-ref a k #f))
             (values k #t))))
     (set
      (let loop ([ht (set-ht s1)] [ss ss])
        (if (null? ss)
            ht
            (let ([s1 (car ss)])
              (unless (set? s1)
                (raise-argument-error* who "Set" s1))
              (loop (int ht (set-ht s1))
                    (cdr ss))))))]))

(define/arity (Set.remove s v)
  #:static-infos ((#%call-result #,mutable-set-static-info))
  (unless (immutable-set? s)
    (raise-argument-error* 'Set.remove rhombus-realm "Set" s))
  (set (hash-remove (set-ht s) v)))

(define/arity (Set.to_list s [try-sort? #f])
  #:static-infos ((#%call-result #,list-static-infos))
  (unless (set? s) (raise-argument-error* 'Set.to_list rhombus-realm "ReadableSet" s))
  (hash-keys (set-ht s) try-sort?))

(define set-method-table
  (hash 'length (let ([length (lambda (s)
                                (unless (set? s)
                                  (raise-argument-error* 'Set.length rhombus-realm "ReadableSet" s))
                                (hash-count (set-ht s)))])
                  (method1 length))
        'copy (method1 Set.copy)
        'snapshot (method1 Set.snapshot)
        'remove (method2 Set.remove)
        'append (method* Set.append)
        'union (method* Set.union)
        'intersect (method* Set.intersect)
        'to_list (lambda (s)
                   (lambda ([try-sort? #f])
                     (Set.to_list s try-sort?)))))
