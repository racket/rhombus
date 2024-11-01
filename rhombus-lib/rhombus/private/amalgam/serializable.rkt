#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "entry-point-adjustment.rkt")
         racket/private/serialize-structs
         "entry-point.rkt"
         (only-in "syntax-parameter.rkt"
                  with-syntax-parameters
                  syntax-parameters-key)
         "declaration.rkt"
         "parens.rkt"
         "extract-rhs.rkt")

(provide (for-syntax check-serializable
                     build-deserialize-submodule)
         default-serializer
         make-class-serialize-info)

(module+ deserializer
  (provide deserializer))

(define-for-syntax (check-serializable serializable
                                       orig-stx
                                       prefab? has-private-fields?)
  (with-syntax ([(form-id ver serialize-rhs deserialize-rhs . _) serializable])
    (define (fail msg)
      (raise-syntax-error #f msg orig-stx #'form-id))
    (when prefab?
      (fail "custom serialization not support for prefab classes"))
    (unless (memq (syntax-local-context) '(module top-level))
      (fail "serialization not supported for nested class declaration"))
    (when has-private-fields?
      (when (or (not (syntax-e #'serialize-rhs))
                (not (syntax-e #'deserialize-rhs)))
        (fail (format "custom ~aserialization required for a class with private fields"
                      (if (syntax-e #'serialize-rhs) "de" "")))))))

(define-syntax (default-serializer stx)
  (syntax-parse stx
    [(_ [super-name-field ... public-name-field ...])
     #`(lambda (obj) (vector (super-name-field obj) ... (public-name-field obj) ...))]))

(define (make-class-serialize-info proc deser-mod-name varref can-cycle?)
  (make-serialize-info
   proc
   (if varref
       (cons 'deserialize
             (module-path-index-join `(submod "." ,deser-mod-name)
                                     (variable-reference->module-path-index varref)))
       deser-mod-name)
   can-cycle?
   (or (current-load-relative-directory) (current-directory))))

(define-for-syntax (build-deserialize-submodule serializer-stx-params
                                                keywords
                                                stx
                                                #:top? [top? #f])
  (with-syntax ([(deserialize-submodule-name serializable
                                             deserializer-name
                                             constructor-name)
                 stx])
    (cond
      [(not (syntax-e #'deserialize-submodule-name))
       null]
      [else
       (with-syntax ([(deserializer-name deserialize-rhs deserializer-cycle-name deserialize-cycle-rhs)
                      (with-continuation-mark
                          syntax-parameters-key serializer-stx-params
                          (syntax-parse #'serializable
                            [(_ _ _ #f . _)
                             (if (ormap syntax-e keywords)
                                 (list #'kw-deserializer
                                       (let ([args (generate-temporaries keywords)])
                                         (with-syntax ([(arg ...) args]
                                                       [((c-arg ...) ...)
                                                        (for/list ([arg (in-list args)]
                                                                   [kw (in-list keywords)])
                                                          (if (syntax-e kw)
                                                              (list kw arg)
                                                              (list arg)))])
                                           #`(lambda (arg ...)
                                               (constructor-name c-arg ... ...))))
                                       #f
                                       #f)
                                 (list #'constructor-name #f #f #f))]
                            [(_ _ _ d-rhs ds-shell ds-fill)
                             (syntax-parse #'d-rhs
                               [(~var deserialize-rhs (:entry-point no-adjustments))
                                (cond
                                  [(not (syntax-e #'ds-shell))
                                   (list #'deserializer-name #'deserialize-rhs.parsed #f #f)]
                                  [else
                                   (syntax-parse #'ds-shell
                                     [(~var shell-rhs (:entry-point no-adjustments))
                                      #:with (~var fill-rhs (:entry-point no-adjustments)) #'ds-fill
                                      (list #'deserializer-name #'deserialize-rhs.parsed
                                            #'deserialize-cycle
                                            #'(lambda ()
                                                (define v (shell-rhs.parsed))
                                                (values v
                                                        (lambda (copy-v)
                                                          (fill-rhs.parsed v copy-v)))))])])])]))])
         (append
          (if (syntax-e #'deserialize-rhs)
              (list
               #`(define deserializer-name deserialize-rhs))
              null)
          (if (syntax-e #'deserialize-cycle-rhs)
              (list
               #`(define deserializer-cycle-name deserialize-cycle-rhs))
              null)
          (list
           (if (or top? (eq? 'top-level (syntax-local-context)))
               #`(define deserialize-submodule-name
                   (make-deserialize-info deserializer-name
                                          deserializer-cycle-name))
               #`(module* deserialize-submodule-name #f
                   (require racket/private/serialize-structs)
                   (provide deserialize)
                   (define deserialize
                     (make-deserialize-info deserializer-name
                                            deserializer-cycle-name))
                   (module declare-preserve-for-embedding racket/kernel))))))])))

(define-syntax deserializer
  (declaration-transformer
    (lambda (stx)
      (unless (eq? (syntax-local-context) 'module)
        (raise-syntax-error #f "allowed only in a module body" stx))
      (syntax-parse stx
        #:datum-literals (group)
        [(form-id (_::block (~alt
                             (~optional (group #:deserialize (~and (_::block . _) d-rhs))
                                        #:defaults ([d-rhs #'#f]))
                             (~optional (group (~and ds #:deserialize_shell) (~and (_::block . _) ds-rhs))
                                        #:defaults ([ds-rhs #'#f]
                                                    [ds #'#f]))
                             (~optional (group (~and df #:deserialize_fill) (~and (_::block . _) df-rhs))
                                        #:defaults ([df-rhs #'#f]
                                                    [df #'#f])))
                            ...))
         (when (not (syntax-e #'d-rhs))
           (raise-syntax-error #f "missing a deserialize entry point" stx))
         (when (and (syntax-e #'ds) (not (syntax-e #'df)))
           (raise-syntax-error #f "need deserialize fill to go with deserialize shell" stx #'ds))
         (when (and (syntax-e #'df) (not (syntax-e #'ds)))
           (raise-syntax-error #f "need deserialize shell to go with deserialize fill" stx #'df))
         (cons
          #'(provide deserialize)
          (build-deserialize-submodule (hasheq)
                                       null
                                       #`(deserialize (form-id #f #f
                                                               #,(extract-rhs #'d-rhs)
                                                               #,(and (syntax-e #'ds-rhs)
                                                                      (extract-rhs #'ds-rhs))
                                                               #,(and (syntax-e #'df-rhs)
                                                                      (extract-rhs #'df-rhs)))
                                                      deserializer
                                                      #f)
                                       #:top? #t))]))))
