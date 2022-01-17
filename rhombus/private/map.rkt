#lang racket/base
(require racket/unsafe/undefined
         (for-syntax racket/base
                     syntax/parse
                     "srcloc.rkt"
                     "with-syntax.rkt")
         "expression.rkt"
         "binding.rkt"
         (submod "annotation.rkt" for-class)
         "static-info.rkt"
         "ref-result-key.rkt"
         "map-ref-set-key.rkt"
         "call-result-key.rkt"
         "composite.rkt"
         "parse.rkt"
         "realm.rkt"
         "folder.rkt")

(provide Map
         (for-space rhombus/binding Map)
         (for-space rhombus/annotation Map)
         (for-space rhombus/static-info Map)
         (for-space rhombus/folder Map)

         make_map
         (for-space rhombus/static-info make_map))

(module+ for-binding
  (provide (for-syntax parse-map-binding)))

(module+ for-info
  (provide (for-syntax map-static-info)))

(define Map
  (lambda args
    (define ht (hash))
    (let loop ([ht ht] [args args])
      (cond
        [(null? args) ht]
        [(null? (cdr args))
         (raise-arguments-error* 'Map rhombus-realm
                                 (string-append "key does not have a value"
                                                " (i.e., an odd number of arguments were provided)")
                                 "key" (car args))]
        [else (loop (hash-set ht (car args) (cadr args)) (cddr args))]))))

(define-for-syntax map-static-info
  #'((#%map-ref hash-ref)
     (#%map-set! hash-set!)
     (#%map-append hash-append)
     (#%sequence-constructor in-hash)))

(define-annotation-syntax Map
  (annotation-constructor #'Map #'hash? map-static-info
                          2
                          (lambda (arg-id predicate-stxs)
                            #`(for/and ([(k v) (in-hash #,arg-id)])
                                (and (#,(car predicate-stxs) k)
                                     (#,(cadr predicate-stxs) v))))
                          (lambda (static-infoss)
                            #`((#%ref-result #,(cadr static-infoss))))))

(define-static-info-syntax Map
  (#%call-result ((#%map-ref hash-ref)
                  (#%sequence-constructor in-hash))))

(define-folder-syntax Map
  (folder-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_)
        #`[begin
           ([ht #hash()])
           (add-to-map ht)
           #,map-static-info]]))))

(define-syntax-rule (add-to-map ht e)
  (let-values ([(k v) e])
    (hash-set ht k v)))

(define make_map
  (lambda args
    (hash-copy (apply Map args))))

(define-static-info-syntax make_map
  (#%call-result ((#%map-ref hash-ref)
                  (#%map-set! hash-set!)
                  (#%sequence-constructor in-hash))))

(define-binding-syntax Map
  (binding-prefix-operator
   #'Map
   '((default . stronger))
   'macro
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (parens)
       [(form-id (parens arg ...) . tail)
        (let loop ([args (syntax->list #'(arg ...))] [keys '()] [vals '()])
          (cond
            [(null? args) (generate-map-binding (reverse keys) (reverse vals) #'tail)]
            [(null? (cdr args))
             (raise-syntax-error #f
                                 (string-append "key expression does not have a value expression"
                                                " (i.e., an odd number of forms were provided)")
                                 stx
                                 (car args))]
            [else (loop (cddr args) (cons (car args) keys) (cons (cadr args) vals))]))]))))

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
       #:literals (Map)
       [(Map k:keyword v)
        #'(hash-set map1 'k v)]
       [(Map k v)
        #'(hash-set map1 k v)]
       [_
        #'(hash-append/proc map1 map2)])]))

(define (hash-append/proc map1 map2)
  (for/fold ([ht map1]) ([(k v) (in-hash map2)])
    (hash-set ht k v)))
