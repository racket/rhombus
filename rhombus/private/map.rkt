#lang racket/base
(require racket/unsafe/undefined
         (for-syntax racket/base
                     syntax/parse
                     "srcloc.rkt"
                     "with-syntax.rkt")
         "expression.rkt"
         "binding.rkt"
         (submod "contract.rkt" for-struct)
         "static-info.rkt"
         "ref-result-key.rkt"
         "indexed-ref-set-key.rkt"
         "call-result-key.rkt"
         (only-in "assign.rkt"
                  [= rhombus=])
         "composite.rkt"
         "parse.rkt")

(provide Map
         (for-space rhombus/binding Map)
         (for-space rhombus/contract Map)
         (for-space rhombus/static-info Map)

         make_map
         (for-space rhombus/static-info make_map))

(define Map
  (make-keyword-procedure
   (lambda (kws vals . more)
     (define base-ht (if (and (null? more) (not (null? kws)))
                         (hasheq)
                         (hash)))
     (define ht (for/fold ([ht base-ht]) ([key (in-list kws)]
                                          [val (in-list vals)])
                  (hash-set ht key val)))
     (let loop ([ht ht] [more more])
       (cond
         [(null? more) ht]
         [(null? (cdr more))
          (raise-arguments-error 'Map
                                 (string-append "key does not have a value"
                                                " (i.e., an odd number of arguments were provided)")
                                 "key" (car more))]
         [else (loop (hash-set ht (car more) (cadr more)) (cddr more))])))))

(define-contract-syntax Map
  (contract-constructor #'Map #'hash? #'((#%indexed-ref hash-ref)
                                         (#%indexed-set! hash-set!))
                        2
                        (lambda (arg-id predicate-stxs)
                          #`(for/and ([(k v) (in-hash #,arg-id)])
                              (and (#,(car predicate-stxs) k)
                                   (#,(cadr predicate-stxs) v))))
                        (lambda (static-infoss)
                          #`((#%ref-result #,(cadr static-infoss))))))

(define-static-info-syntax Map
  (#%call-result ((#%indexed-ref hash-ref))))

(define (make_map . l)
  (hash-copy (apply hash l)))

(define-static-info-syntax make_map
  (#%call-result ((#%indexed-ref hash-ref)
                  (#%indexed-set! hash-set!))))

(define-binding-syntax Map
  (binding-prefix-operator
   #'Map
   '((default . stronger))
   'macro
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (parens block group op)
       #:literals (rhombus=)
       [(form-id (parens (group key:keyword (block (group val ...))) ...) . tail)
        ;; eager parsing of key forms, partly because we expect then
        ;; to be constants, but more generaly because we think of them
        ;; as earlier than the value patterns
        (define tmp-ids (generate-temporaries #'(key ...)))
        (define-values (composite new-tail)
          ((make-composite-binding-transformer #'(lambda (v) #t)
                                               (for/list ([tmp-id (in-list tmp-ids)])
                                                 #`(lambda (v) #,tmp-id))
                                               (for/list ([arg (in-list tmp-ids)])
                                                 #'()))
           #`(form-id (parens (group val ...) ...) . tail)))
        (with-syntax-parse ([composite::binding-form composite])
          (values
           (binding-form #'map-infoer
                         #`((key ...)
                            #,tmp-ids
                            composite.infoer-id
                            composite.data))
           new-tail))]
       [(form-id (~and wrong (brackets . _)) . tail)
        (raise-syntax-error #f
                            "bad group within brackets"
                            (relocate (span-srcloc #'form-id #'wrong)
                                      #'(form-id wrong)))]))))

(define-syntax (map-infoer stx)
  (syntax-parse stx
    [(_ static-infos (keys tmp-ids composite-infoer-id composite-data))
     #:with composite-impl::binding-impl #'(composite-infoer-id static-infos composite-data)
     #:with composite-info::binding-info #'composite-impl.info
     (binding-info #'composite-info.name-id
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
                      (define #,(car tmp-ids) (hash-ref arg-id (quote #,(car keys)) unsafe-undefined))
                      (IF (not (eq? #,(car tmp-ids) unsafe-undefined))
                          #,(loop (cdr keys) (cdr tmp-ids))
                          failure))]))
           failure)]))
  
(define-syntax (map-binder stx)
  (syntax-parse stx
    [(_ arg-id (keys tmp-ids composite-matcher-id composite-binder-id composite-data))
     #`(composite-binder-id 'map composite-data)]))
