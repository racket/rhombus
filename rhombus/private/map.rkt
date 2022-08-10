#lang racket/base
(require racket/unsafe/undefined
         (for-syntax racket/base
                     syntax/parse
                     "srcloc.rkt"
                     "with-syntax.rkt")
         "expression.rkt"
         "expression+binding.rkt"
         "binding.rkt"
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
         "setmap-parse.rkt")

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

(define map-method-table
  (hash 'count hash-count))

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
   [count hash-count])
  #:root
  (expression-transformer
   #'Map
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (braces)
       [(form-id (~and content (braces . _)) . tail)
        (define-values (shape args) (parse-setmap-content #'content
                                                          #:shape 'map
                                                          #:who (syntax-e #'form-id)))
        (values (wrap-static-info*
                 (quasisyntax/loc stx
                   (Map-build #,@args))
                 map-static-info)
                #'tail)]
       [(_ . tail) (values #'Map #'tail)]))))

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
  (lambda (arg-id predicate-stxs)
    #`(for/and ([(k v) (in-hash #,arg-id)])
        (and (#,(car predicate-stxs) k)
             (#,(cadr predicate-stxs) v))))
  (lambda (static-infoss)
    #`((#%ref-result #,(cadr static-infoss)))))

(define-syntax hash-instance
  (dot-provider
   (lambda (lhs dot-stx field-stx)
     (case (syntax-e field-stx)
       [(count) #`(hash-count #,lhs)]
       [else #f]))))

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
  (expression-transformer
   #'MutableMap
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (braces)
       [(form-id (~and content (braces . _)) . tail)
        (define-values (shape args) (parse-setmap-content #'content
                                                          #:shape 'map
                                                          #:who (syntax-e #'form-id)))
        (values (wrap-static-info*
                 (quasisyntax/loc stx
                   (hash-copy (Map-build #,@args)))
                 mutable-map-static-info)
                #'tail)]
       [(_ . tail) (values #'MutableMap #'tail)]))))

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
       #:datum-literals (braces parens)
       [(form-id (~and content (braces . _)) . tail)
        (parse-map-binding stx "braces")]
       [(form-id (parens arg ...) . tail)
        (let loop ([args (syntax->list #'(arg ...))] [keys '()] [vals '()])
          (cond
            [(null? args) (generate-map-binding (reverse keys) (reverse vals) #'tail)]
            [else
             (syntax-parse (car args)
               #:datum-literals (group brackets)
               [(group (brackets key val))
                (loop (cdr args) (cons #'key keys) (cons #'val vals))]
               [_ (raise-syntax-error #f
                                      "expected [<key-expr>, <value-binding>]"
                                      stx
                                      (car args))])]))]))))

(define-for-syntax (parse-map-binding stx opener+closer)
  (syntax-parse stx
    #:datum-literals (parens block group op)
    [(form-id (_ (group key-e ... (block (group val ...))) ...) . tail)
     (generate-map-binding #'((group key-e ...) ...) #'((group val ...) ...) #'tail)]
    [(form-id wrong . tail)
     (raise-syntax-error #f
                         (format "bad key-value combination within ~a" opener+closer)
                         (relocate (span-srcloc #'form-id #'wrong)
                                   #'(form-id wrong)))]))

(define-for-syntax (generate-map-binding keys vals tail)
  (with-syntax ([(key ...) keys]
                [(val ...) vals]
                [tail tail])
    (define tmp-ids (generate-temporaries #'(key ...)))
    (define-values (composite new-tail)
      ((make-composite-binding-transformer "Map"
                                           #'(lambda (v) #t)
                                           (for/list ([tmp-id (in-list tmp-ids)])
                                             #`(lambda (v) #,tmp-id))
                                           (for/list ([arg (in-list tmp-ids)])
                                             #'())
                                           #:ref-result-info? #t)
       #`(form-id (parens val ...) . tail)))
    (with-syntax-parse ([composite::binding-form composite])
      (values
       (binding-form #'map-infoer
                     #`((key ...)
                        #,tmp-ids
                        composite.infoer-id
                        composite.data))
       new-tail))))

(define-syntax (map-infoer stx)
  (syntax-parse stx
    [(_ static-infos (keys tmp-ids composite-infoer-id composite-data))
     #:with composite-impl::binding-impl #'(composite-infoer-id static-infos composite-data)
     #:with composite-info::binding-info #'composite-impl.info
     (binding-info #'composite-info.annotation-str
                   #'composite-info.name-id
                   #'composite-info.static-infos
                   #'composite-info.bind-infos
                   #'map-matcher
                   #'map-binder
                   #'(keys tmp-ids composite-info.matcher-id composite-info.binder-id composite-info.data))]))

(define-syntax (map-matcher stx)
  (syntax-parse stx
    [(_ arg-id (keys tmp-ids composite-matcher-id composite-binder-id composite-data)
        IF success failure)
     #`(IF (hash? arg-id)
           #,(let loop ([keys (syntax->list #'keys)]
                        [tmp-ids (syntax->list #'tmp-ids)])
               (cond
                 [(null? keys)
                  #`(composite-matcher-id 'map composite-data IF success failure)]
                 [else
                  #`(begin
                      (define #,(car tmp-ids) (hash-ref arg-id (rhombus-expression #,(car keys)) unsafe-undefined))
                      (IF (not (eq? #,(car tmp-ids) unsafe-undefined))
                          #,(loop (cdr keys) (cdr tmp-ids))
                          failure))]))
           failure)]))
  
(define-syntax (map-binder stx)
  (syntax-parse stx
    [(_ arg-id (keys tmp-ids composite-matcher-id composite-binder-id composite-data))
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
