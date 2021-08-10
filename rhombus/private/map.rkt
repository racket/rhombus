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

(define-syntax Map
  (expression-transformer
   #'Map
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (brackets block group op)
       [(_ (brackets (group key ... (op rhombus=) val ...) ...) . tail)
        (values #'(hash (~@ (rhombus-expression (group key ...))
                            (rhombus-expression (group val ...)))
                        ...)
                #'tail)]
       [(form-id (~and wrong (brackets . _)) . tail)
        (raise-syntax-error #f
                            "bad group within brackets"
                            (relocate (span-srcloc #'form-id #'wrong)
                                      #'(form-id wrong)))]
       [(_ . tail) (values #'Map-op
                           #'tail)]))))

(define Map-op
  (let ([Map (lambda args
               (apply hash args))])
    Map))
       
(define-contract-syntax Map
  (identifier-contract #'Map #'hash? #'((#%indexed-ref hash-ref)
                                        (#%indexed-set! hash-set!))))

(define-static-info-syntax Map
  (#%call-result ((#%indexed-ref hash-ref))))

(define-static-info-syntax Map-op
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
       #:datum-literals (brackets block group op)
       [(form-id (brackets (group key ... (op rhombus=) val ...) ...) . tail)
        ;; eager parsing of key forms, partly because we expect then
        ;; to be constants, but more generaly because we think of them
        ;; as earlier than the value patterns
        (define key-parseds (syntax-parse #'((group key ...) ...)
                              [(key::expression ...) #'(key.parsed ...)]))
        (define tmp-ids (generate-temporaries #'((key ...) ...)))
        (define-values (composite new-tail)
          ((make-composite-binding-transformer #'(lambda (v) #t)
                                               (for/list ([tmp-id (in-list tmp-ids)])
                                                 #`(lambda (v) #,tmp-id))
                                               (for/list ([arg (in-list tmp-ids)])
                                                 #'()))
           #`(form-id (parens (group val ...) ...) . tail)))
        (with-syntax-parse ([composite::binding-form composite])
          (values
           (binding-form #'map
                         #'composite.static-infos
                         #'composite.bind-ids
                         #'map-matcher
                         #'map-binder
                         #`(#,key-parseds
                            #,tmp-ids
                            composite.matcher-id
                            composite.binder-id
                            composite.data))
           new-tail))]
       [(form-id (~and wrong (brackets . _)) . tail)
        (raise-syntax-error #f
                            "bad group within brackets"
                            (relocate (span-srcloc #'form-id #'wrong)
                                      #'(form-id wrong)))]))))

(define-syntax (map-matcher stx)
  (syntax-parse stx
    [(_ arg-id (keys tmp-ids composite-matcher-id composite-binder-id  composite-data)
        IF success failure)
     #`(IF (hash? arg-id)
           #,(let loop ([keys (syntax->list #'keys)]
                        [tmp-ids (syntax->list #'tmp-ids)])
               (cond
                 [(null? keys)
                  #`(composite-matcher-id 'map composite-data IF success failure)]
                 [else
                  #`(begin
                      (define #,(car tmp-ids) (hash-ref arg-id #,(car keys) unsafe-undefined))
                      (IF (not (eq? #,(car tmp-ids) unsafe-undefined))
                          #,(loop (cdr keys) (cdr tmp-ids))
                          failure))]))
           failure)]))
  
(define-syntax (map-binder stx)
  (syntax-parse stx
    [(_ arg-id (keys tmp-ids composite-matcher-id composite-binder-id  composite-data))
     #`(composite-binder-id 'map composite-data)]))
