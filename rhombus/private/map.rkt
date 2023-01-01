#lang racket/base
(require racket/unsafe/undefined
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     "srcloc.rkt"
                     "with-syntax.rkt"
                     "tag.rkt"
                     shrubbery/print)
         "expression.rkt"
         "expression+binding.rkt"
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
         "composite.rkt"
         "parse.rkt"
         "realm.rkt"
         "reducer.rkt"
         "name-root.rkt"
         "setmap-parse.rkt"
         "dot-parse.rkt"
         "parens.rkt"
         (only-in "lambda-kwrest.rkt" hash-remove*)
         (only-in "rest-marker.rkt" &))

(provide (rename-out [Map-expr Map])
         (for-space rhombus/binding Map)
         (for-space rhombus/annotation Map)
         (for-space rhombus/reducer Map)
         (for-space rhombus/static-info Map)

         (rename-out [MutableMap-expr MutableMap])
         (for-space rhombus/static-info MutableMap))

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

(define (Map . args)
  (build-map 'Map args))

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

(define-syntax empty-map
  (make-expression+binding-prefix-operator
   #'empty-map
   '((default . stronger))
   'macro
   ;; expression
   (lambda (stx)
     (syntax-parse stx
       [(form-id . tail)
        (values #'#hashalw() #'tail)]))
   ;; binding
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
                   #'datum)]))

(define-syntax (empty-map-matcher stx)
  (syntax-parse stx
    [(_ arg-id datum IF success fail)
     #'(IF (and (hash? arg-id) (eqv? 0 (hash-count arg-id)))
           success
           fail)]))

(define-name-root Map-expr
  #:fields
  ([empty empty-map]
   [length hash-count]
   [keys hash-keys]
   [values hash-values]
   [has_key hash-has-key?])
  #:root
  (let ()
    (define (parse-map stx repetition?)
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
                              [repetition? (identifier-repetition-use #'Map)]
                              [else #'Map])
                            #'tail)]))
    (make-expression+repetition-transformer
     #'Map
     (lambda (stx) (parse-map stx #f))
     (lambda (stx) (parse-map stx #t)))))


(define-for-syntax map-static-info
  #'((#%map-ref hash-ref)
     (#%map-append hash-append)
     (#%sequence-constructor in-hash)
     (#%dot-provider hash-instance)))

(define-for-syntax mutable-map-static-info
  #`((#%map-set! hash-set!)
     . #,map-static-info))

(define-annotation-constructor Map
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
        [(has_key) (0ary #'hash-has-key?)]
        [else #f])))))

(define-static-info-syntax Map
  (#%call-result #,map-static-info))

(define-reducer-syntax Map
  (reducer-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_)
        #`[begin
           ([ht #hashalw()])
           (add-to-map ht)
           #,map-static-info]]))))

(define-syntax-rule (add-to-map ht e)
  (let-values ([(k v) e])
    (hash-set ht k v)))

(define MutableMap
  (lambda args
    (hash-copy (build-map 'MutableMap args))))

(define-syntax MutableMap-expr
  (let ()
    (define (parse-map stx repetition?)
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
                                (identifier-repetition-use #'MutableMap)
                                #'MutableMap)
                            #'tail)]))
    (make-expression+repetition-transformer
     #'MutableMap
     (lambda (stx) (parse-map stx #f))
     (lambda (stx) (parse-map stx #t)))))

(define-static-info-syntax MutableMap
  (#%call-result #,mutable-map-static-info))

(define-name-root Map
  #:space rhombus/binding
  #:fields ([empty empty-map])
  #:root
  (binding-prefix-operator
   #'Map
   '((default . stronger))
   'macro
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

(define-for-syntax (parse-map-binding who stx opener+closer)
  (syntax-parse stx
    #:datum-literals (parens block group op)
    #:literals (&)
    [(form-id (_ (group key-e ... (block (group val ...))) ...
                 (group (op &) rst ...))
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

(define-for-syntax (generate-map-binding keys vals maybe-rest tail)
  (with-syntax ([(key ...) keys]
                [(val ...) vals]
                [tail tail])
    (define tmp-ids (generate-temporaries #'(key ...)))
    (define rest-tmp (and maybe-rest (generate-temporary 'rest-tmp)))
    (define-values (composite new-tail)
      ((make-composite-binding-transformer (cons "Map" (map shrubbery-syntax->string keys))
                                           #'(lambda (v) #t)
                                           (for/list ([tmp-id (in-list tmp-ids)])
                                             #`(lambda (v) #,tmp-id))
                                           (for/list ([arg (in-list tmp-ids)])
                                             #'())
                                           #:ref-result-info? #t
                                           #:rest-accessor
                                           (and maybe-rest
                                                #`(lambda (v) #,rest-tmp))
                                           #:rest-repetition? #f)
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
                   #'map-binder
                   #'(keys tmp-ids rest-tmp composite-info.matcher-id composite-info.binder-id composite-info.data))]))

(define-syntax (map-matcher stx)
  (syntax-parse stx
    [(_ arg-id (keys tmp-ids rest-tmp composite-matcher-id composite-binder-id composite-data)
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

(define-syntax (map-binder stx)
  (syntax-parse stx
    [(_ arg-id (keys tmp-ids rest-tmp composite-matcher-id composite-binder-id composite-data))
     #`(composite-binder-id 'map composite-data)]))


;; macro to optimize to an inline functional update
(define-syntax (hash-append stx)
  (syntax-parse stx
    [(_ map1 map2)
     (syntax-parse (unwrap-static-infos #'map2)
       #:literals (Map-build)
       [(Map-build k:keyword v)
        #'(hash-set map1 'k v)]
       [(Map-build k v)
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
