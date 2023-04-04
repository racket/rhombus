#lang racket/base
(require racket/unsafe/undefined
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse/pre
                     "srcloc.rkt"
                     "with-syntax.rkt"
                     "tag.rkt"
                     shrubbery/print)
         "provide.rkt"
         "expression.rkt"
         "binding.rkt"
         "repetition.rkt"
         "compound-repetition.rkt"
         (submod "annotation.rkt" for-class)
         (submod "dot.rkt" for-dot-provider)
         "literal.rkt"
         "static-info.rkt"
         "ref-result-key.rkt"
         "map-ref-set-key.rkt"
         "call-result-key.rkt"
         "function-arity-key.rkt"
         "composite.rkt"
         "parse.rkt"
         "realm.rkt"
         "reducer.rkt"
         "name-root.rkt"
         "setmap-parse.rkt"
         "dot-parse.rkt"
         "parens.rkt"
         (only-in "lambda-kwrest.rkt" hash-remove*)
         "op-literal.rkt"
         (only-in "pair.rkt"
                  Pair))

(provide (for-spaces (rhombus/namespace
                      #f
                      rhombus/bind
                      rhombus/repet
                      rhombus/annot
                      rhombus/reducer)
                     Map)
         (for-spaces (#f
                      rhombus/repet)
                     MutableMap))

(module+ for-binding
  (provide (for-syntax parse-map-binding)))

(module+ for-info
  (provide (for-syntax map-static-info)
           Map-build))

(module+ for-builtin
  (provide map-method-table))

(module+ for-build
  (provide hash-append
           hash-append/proc
           hash-extend*
           hash-assert
           list->map))

(define map-method-table
  (hash 'length (method1 hash-count)
        'values (method1 hash-values)
        'keys (method1 hash-keys)
        'has_key (lambda (ht) (lambda (key) (hash-has-key? ht key)))))

(define Map-build hashalw) ; inlined version of `Map.from_interleaved`

(define (Map.from_interleaved . args)
  (define ht (hashalw))
  (let loop ([ht ht] [args args])
    (cond
      [(null? args) ht]
      [(null? (cdr args))
       (raise-arguments-error* 'Map rhombus-realm
                               (string-append "key does not have a value"
                                              " (i.e., an odd number of arguments were provided)")
                               "key" (car args))]
      [else (loop (hash-set ht (car args) (cadr args)) (cddr args))])))

(define Map-pair-build
  (let ([Map (lambda args
               (build-map 'Map args))])
    Map))

(define (build-map who args)
  (define ht (hashalw))
  (let loop ([ht ht] [args args])
    (cond
      [(null? args) ht]
      [else
       (define arg (car args))
       (cond
         [(and (pair? arg) (pair? (cdr arg)) (null? (cddr arg)))
          (loop (hash-set ht (car arg) (cadr arg)) (cdr args))]
         [else
          (raise-argument-error* who rhombus-realm "[_, _]" arg)])])))

(define (list->map key+vals)
  (for/hashalw ([key+val (in-list key+vals)])
    (values (car key+val) (cdr key+val))))

(define (hash-pairs ht)
  (for/list ([p (in-hash-pairs ht)]) p))

(define-syntax empty-map
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       [(form-id . tail)
        (values #'#hashalw() #'tail)]))))

(define-binding-syntax empty-map
  (binding-transformer
   (lambda (stx)
     (syntax-parse stx
       [(form-id . tail)
        (values (binding-form #'empty-map-infoer #'#hashalw()) #'tail)]))))

(define-syntax (empty-map-infoer stx)
  (syntax-parse stx
    [(_ static-infos datum)
     (binding-info "Map.empty"
                   #'empty-map
                   #'static-infos
                   #'()
                   #'empty-map-matcher
                   #'literal-bind-nothing
                   #'literal-commit-nothing
                   #'datum)]))

(define-syntax (empty-map-matcher stx)
  (syntax-parse stx
    [(_ arg-id datum IF success fail)
     #'(IF (and (hash? arg-id) (eqv? 0 (hash-count arg-id)))
           success
           fail)]))

(define-for-syntax (parse-map stx repetition?)
  (syntax-parse stx
    #:datum-literals (braces)
    [(form-id (~and content (braces . _)) . tail)
     (define-values (shape argss) (parse-setmap-content #'content
                                                        #:shape 'map
                                                        #:who (syntax-e #'form-id)
                                                        #:repetition? repetition?
                                                        #:list->map #'list->map))
     (values (build-setmap stx argss
                           #'Map-build
                           #'hash-extend*
                           #'hash-append
                           #'hash-assert
                           map-static-info
                           #:repetition? repetition?
                           #:list->setmap #'list->map)
             #'tail)]
    [(_ . tail) (values (cond
                          [repetition? (identifier-repetition-use #'Map-pair-build)]
                          [else #'Map-pair-build])
                        #'tail)]))

(define-name-root Map
  #:fields
  ([empty empty-map]
   [length hash-count]
   [keys hash-keys]
   [values hash-values]
   [has_key hash-has-key?]
   of))

(define-syntax Map
  (expression-transformer
   (lambda (stx) (parse-map stx #f))))

(define-binding-syntax Map
  (binding-transformer
   (lambda (stx)
     (syntax-parse stx
       [(form-id (~and content (_::braces . _)) . tail)
        (parse-map-binding (syntax-e #'form-id) stx "braces")]
       [(form-id (_::parens arg ...) . tail)
        (let loop ([args (syntax->list #'(arg ...))] [keys '()] [vals '()])
          (cond
            [(null? args) (generate-map-binding (reverse keys) (reverse vals) #f #'tail)]
            [else
             (syntax-parse (car args)
               #:datum-literals (group brackets)
               [(group (brackets key val))
                (loop (cdr args) (cons #'key keys) (cons #'val vals))]
               [_ (raise-syntax-error #f
                                      "expected [<key-expr>, <value-binding>]"
                                      stx
                                      (car args))])]))]))))

(define-repetition-syntax Map
  (repetition-transformer
   (lambda (stx) (parse-map stx #t))))

(define-for-syntax map-static-info
  #'((#%map-ref hash-ref)
     (#%map-append hash-append)
     (#%sequence-constructor in-hash)
     (#%dot-provider hash-instance)))

(define-for-syntax mutable-map-static-info
  #`((#%map-set! hash-set!)
     . #,map-static-info))

(define-annotation-constructor (Map of)
  ()
  #'hash? map-static-info
  2
  #f
  (lambda (arg-id predicate-stxs)
    #`(for/and ([(k v) (in-hash #,arg-id)])
        (and (#,(car predicate-stxs) k)
             (#,(cadr predicate-stxs) v))))
  (lambda (static-infoss)
    #`((#%ref-result #,(cadr static-infoss)))))

(define-syntax hash-instance
  (dot-provider-more-static
   (dot-parse-dispatch
    (lambda (field-sym field ary 0ary nary fail-k)
      (case field-sym
        [(length) (0ary #'hash-count)]
        [(keys) (0ary #'hash-keys)]
        [(values) (0ary #'hash-values)]
        [(has_key) (nary #'hash-has-key? 2 #'hash-has-key?)]
        [else #f])))))

(define-static-info-syntax Map-pair-build
  (#%call-result #,map-static-info)
  (#%function-arity -1))

(define-reducer-syntax Map
  (reducer-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_)
        #`[(begin)
           ([ht #hashalw()])
           (add-to-map ht)
           #,map-static-info]]))))

(define-syntax-rule (add-to-map ht e)
  (let-values ([(k v) e])
    (hash-set ht k v)))

(define MutableMap-build
  (let ([MutableMap (lambda args
                      (hash-copy (build-map 'MutableMap args)))])
    MutableMap))

(define-for-syntax (parse-mutable-map stx repetition?)
  (syntax-parse stx
    #:datum-literals (braces)
    [(form-id (~and content (braces . _)) . tail)
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
                             (hash-copy (Map-build #,@args)))
                           mutable-map-static-info)))]
               [else
                (wrap-static-info*
                 (quasisyntax/loc stx
                   (hash-copy (Map-build #,@(car argss))))
                 mutable-map-static-info)])
             #'tail)]
    [(_ . tail) (values (if repetition?
                            (identifier-repetition-use #'MutableMap-build)
                            #'MutableMap-build)
                        #'tail)]))

(define-syntax MutableMap
  (expression-transformer
   (lambda (stx) (parse-mutable-map stx #f))))

(define-repetition-syntax MutableMap
  (repetition-transformer
   (lambda (stx) (parse-mutable-map stx #t))))

(define-static-info-syntax MutableMap-build
  (#%call-result #,mutable-map-static-info))

(define-for-syntax (parse-map-binding who stx opener+closer)
  (syntax-parse stx
    #:datum-literals (parens block group)
    [(form-id (_ (group key-e ... (block (group val ...))) ...
                 (group key-b ... (block (group val-b ...)))
                 (group _::...-bind))
              . tail)
     (generate-map-binding (syntax->list #`((#,group-tag key-e ...) ...)) #`((#,group-tag val ...) ...)
                           #`(group Pair (parens (#,group-tag key-b ...) (#,group-tag val-b ...)))
                           #'tail
                           #:rest-repetition? #t)]
    [(form-id (_ (group key-e ... (block (group val ...))) ...
                 (group _::&-bind rst ...))
              . tail)
     (generate-map-binding (syntax->list #`((#,group-tag key-e ...) ...)) #`((#,group-tag val ...) ...)
                           #'(group rst ...)
                           #'tail)]
    [(form-id (_ (group key-e ... (block (group val ...))) ...) . tail)
     (generate-map-binding (syntax->list #`((#,group-tag key-e ...) ...)) #`((#,group-tag val ...) ...) #f #'tail)]
    [(form-id wrong . tail)
     (raise-syntax-error who
                         (format "bad key-value combination within ~a" opener+closer)
                         #'wrong)]))

(define-for-syntax (generate-map-binding keys vals maybe-rest tail
                                         #:rest-repetition? [rest-repetition? #f])
  (with-syntax ([(key ...) keys]
                [(val ...) vals]
                [tail tail])
    (define tmp-ids (generate-temporaries #'(key ...)))
    (define rest-tmp (and maybe-rest (generate-temporary 'rest-tmp)))
    (define-values (composite new-tail)
      ((make-composite-binding-transformer (cons "Map" (map shrubbery-syntax->string keys))
                                           #'(lambda (v) #t) ; predicate built into map-matcher
                                           (for/list ([tmp-id (in-list tmp-ids)])
                                             #`(lambda (v) #,tmp-id))
                                           (for/list ([arg (in-list tmp-ids)])
                                             #'())
                                           #:ref-result-info? #t
                                           #:rest-accessor
                                           (and maybe-rest
                                                (if rest-repetition?
                                                    #`(lambda (v) (hash-pairs #,rest-tmp))
                                                    #`(lambda (v) #,rest-tmp)))
                                           #:rest-repetition? (and rest-repetition?
                                                                   'pair))
       #`(form-id (parens val ...) . tail)
       maybe-rest))
    (with-syntax-parse ([composite::binding-form composite])
      (values
       (binding-form #'map-infoer
                     #`((key ...)
                        #,tmp-ids
                        #,rest-tmp
                        composite.infoer-id
                        composite.data))
       new-tail))))

(define-syntax (map-infoer stx)
  (syntax-parse stx
    [(_ static-infos (keys tmp-ids rest-tmp composite-infoer-id composite-data))
     #:with composite-impl::binding-impl #'(composite-infoer-id static-infos composite-data)
     #:with composite-info::binding-info #'composite-impl.info
     (binding-info #'composite-info.annotation-str
                   #'composite-info.name-id
                   #'composite-info.static-infos
                   #'composite-info.bind-infos
                   #'map-matcher
                   #'map-committer
                   #'map-binder
                   #'(keys tmp-ids rest-tmp
                           composite-info.matcher-id composite-info.committer-id composite-info.binder-id
                           composite-info.data))]))

(define-syntax (map-matcher stx)
  (syntax-parse stx
    [(_ arg-id (keys tmp-ids rest-tmp composite-matcher-id composite-committer-id composite-binder-id composite-data)
        IF success failure)
     (define key-tmps (generate-temporaries #'keys))
     #`(IF (hash? arg-id)
           #,(let loop ([keys (syntax->list #'keys)]
                        [key-tmp-ids key-tmps]
                        [val-tmp-ids (syntax->list #'tmp-ids)])
               (cond
                 [(and (null? keys) (syntax-e #'rest-tmp))
                  #`(begin
                      (define rest-tmp (hash-remove*/copy arg-id (list #,@key-tmps)))
                      (composite-matcher-id 'map composite-data IF success failure))]
                 [(null? keys)
                  #`(composite-matcher-id 'map composite-data IF success failure)]
                 [else
                  #`(begin
                      (define #,(car key-tmp-ids) (rhombus-expression #,(car keys)))
                      (define #,(car val-tmp-ids) (hash-ref arg-id #,(car key-tmp-ids) unsafe-undefined))
                      (IF (not (eq? #,(car val-tmp-ids) unsafe-undefined))
                          #,(loop (cdr keys) (cdr key-tmp-ids) (cdr val-tmp-ids))
                          failure))]))
           failure)]))

;; hash-remove*/copy : (Hashof K V) (Listof K) -> (ImmutableHashof K V)
(define (hash-remove*/copy h ks)
  (hash-remove* (if (immutable? h) h (hash-map/copy h values #:kind 'immutable)) ks))

(define-syntax (map-committer stx)
  (syntax-parse stx
    [(_ arg-id (keys tmp-ids rest-tmp composite-matcher-id composite-committer-id composite-binder-id composite-data))
     #`(composite-committer-id 'map composite-data)]))

(define-syntax (map-binder stx)
  (syntax-parse stx
    [(_ arg-id (keys tmp-ids rest-tmp composite-matcher-id composite-committer-id composite-binder-id composite-data))
     #`(composite-binder-id 'map composite-data)]))


;; macro to optimize to an inline functional update
(define-syntax (hash-append stx)
  (syntax-parse stx
    [(_ map1 map2)
     (syntax-parse (unwrap-static-infos #'map2)
       [(id:identifier k:keyword v)
        #:when (free-identifier=? (expr-quote Map-build) #'id)
        #'(hash-set map1 'k v)]
       [(id:identifier k v)
        #:when (free-identifier=? (expr-quote Map-build) #'id)
        #'(hash-set map1 k v)]
       [_
        #'(hash-append/proc map1 map2)])]))

(define (hash-append/proc map1 map2)
  (for/fold ([ht map1]) ([(k v) (in-hash map2)])
    (hash-set ht k v)))

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
  (unless (hash? v)
    (raise-arguments-error* 'Map rhombus-realm
                            "not a map for splicing"
                            "value" v))
  v)

(define-static-info-syntaxes (hash-count hash-values)
  (#%function-arity 2))

(define-static-info-syntaxes (hash-keys)
  (#%function-arity 6))

(define-static-info-syntaxes (hash-has-key?)
  (#%function-arity 4))
