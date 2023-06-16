#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/syntax-local
                     "class-parse.rkt")
         (submod "dot.rkt" for-dot-provider)
         "static-info.rkt"
         (submod "equal.rkt" for-parse)
         "dot-provider-key.rkt"
         (submod "with.rkt" for-update)
         "parse.rkt"
         "reconstructor.rkt"
         "realm.rkt")

(provide (for-syntax build-class-reconstructor))

(define-for-syntax (build-class-reconstructor super final?
                                              reconstructor-rhs method-private
                                              names)
  (with-syntax ([(name name? constructor-name name-instance
                       indirect-static-infos reconstructor-name
                       [(con-field-name con-field-arg con-acc) ...]
                       [private-field-name ...]
                       [private-field-desc ...]
                       [super-name ...])
                 names])
    (define direct? (and final? (eq? reconstructor-rhs 'default)))
    (if (syntax-e #'reconstructor-name)
        (append
         (if (eq? reconstructor-rhs 'default)
             (list
              #`(define reconstructor-name
                  #,(make-reconstructor super
                                        #'constructor-name
                                        (syntax->list #'(con-field-name ...))
                                        (syntax->list #'(con-field-arg ...))
                                        (syntax->list #'(con-acc ...)))))
             ;; non-default reconstructor has handled in `build-methods`
             null)
         (list
          #`(define-update-syntax name-instance
              (make-update-syntax (quote-syntax (name #,(and direct? #'constructor-name)
                                                      name-instance indirect-static-infos))))))
        null)))

(define-for-syntax (make-reconstructor super constructor-name names args accs)
  #`(lambda (obj #,@(for/list ([name (in-list names)]
                               [arg (in-list args)]
                               [acc (in-list accs)]
                               #:unless (identifier? arg))
                      (if super
                          #`[#,name (#,acc obj)]
                          name)))
      (#,constructor-name #,@(apply
                              append
                              (for/list ([name (in-list names)]
                                         [b-arg (in-list args)]
                                         #:unless (identifier? b-arg))
                                (define arg (if (box? (syntax-e b-arg)) (unbox (syntax-e b-arg)) b-arg))
                                (cond
                                  [(keyword? (syntax-e arg)) (list arg name)]
                                  [else (list name)]))))))

(define-for-syntax (make-update-syntax constructor-info)
  (update-transformer
   (lambda (form1 with-id names rhss static?)
     (unless constructor-info
       (raise-syntax-error #f "no default `with` for class with a custom constructor" with-id))
     (define-values (desc constructor-id static-infos direct?)
       (syntax-parse constructor-info
         [(name constructor-id name-instance indirect-static-infos)
          (define desc (syntax-local-value* (in-class-desc-space #'name) class-desc-ref))
          (unless desc (error "cannot find annotation binding for update provider"))
          (values desc
                  #'constructor-id
                  #'((#%dot-provider name-instance) . indirect-static-infos)
                  (syntax-e #'constructor-id))]))
     (define ctr-fields
       (for/fold ([updates #hasheq()]) ([field (in-list (class-desc-fields desc))])
         (hash-set updates (car field) field)))
     (define arg-ids (generate-temporaries rhss))
     (define updates
       (for/fold ([updates #hasheq()]) ([name (in-list names)]
                                        [arg-id (in-list arg-ids)])
         (define f (hash-ref ctr-fields (syntax-e name) #f))
         (cond
           [(not f)
            (when static?
              (raise-syntax-error #f "no such public field in class" with-id name))
            #f]
           [(identifier? (field-desc-constructor-arg f))
            (when static?
              (raise-syntax-error #f "field is not a constructor argument" with-id name))
            #f]
           [else (and updates
                      (hash-set updates (syntax-e name) arg-id))])))
     (cond
       [(not updates) #f]
       [else
        (define expr
          (with-syntax ([(arg-id ...) arg-ids]
                        [(rhs-expr ...) rhss])
            #`(let ([obj #,form1]
                    [arg-id rhs-expr]
                    ...)
                (#,(if direct? constructor-id #`(extract-reconstructor '#,with-id obj))
                 #,@(if direct? null (list #'obj))
                 #,@(apply
                     append
                     (for/list ([field (in-list (class-desc-fields desc))]
                                #:do [(define b-arg (field-desc-constructor-arg field))]
                                #:unless (identifier? b-arg))
                       (define arg (if (box? (syntax-e b-arg)) (unbox (syntax-e b-arg)) b-arg))
                       (define rhs (or (hash-ref updates (field-desc-name field) #f)
                                       #`(#,(field-desc-accessor-id field) obj)))
                       (cond
                         [(and direct? (keyword? (syntax-e arg))) (list arg rhs)]
                         [else (list rhs)])))))))
        (wrap-static-info* expr static-infos)]))))

(define (extract-reconstructor who obj)
  (define r (reconstructor-ref obj #f))
  (unless r
    (raise-arguments-error* who rhombus-realm
                            "value does not support functional update"
                            "value" obj))
  (cdr r))

