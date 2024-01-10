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

(provide (for-syntax extract-reconstructor-fields
                     build-class-reconstructor))

(define-for-syntax (extract-reconstructor-fields stxes options super reconstructor-rhs
                                                 public-field-names public-field-arguments public-name-fields)
  (define-values (new-names new-args new-accs new-rhss)
    (cond
      [(hash-ref options 'reconstructor-fields #f)
       => (lambda (stx)
            (with-syntax ([(orig-id (id ...) rhss) stx])
              (when (not (syntax? reconstructor-rhs))
                (raise-syntax-error #f "reconstructor fields supplied without a reconstructor" stxes #'orig-id))
              (define ids (syntax->list #'(id ...)))
              (values (syntax->list #'(id ...))
                      (map (lambda (id) #f) ids)
                      (generate-temporaries ids)
                      (syntax->list #'rhss))))]
      [else
       (for/lists (names args accs rhss) ([name (in-list public-field-names)]
                                          [arg (in-list public-field-arguments)]
                                          [acc (in-list public-name-fields)]
                                          #:unless (identifier? arg))
         (values name
                 (if (box? (syntax-e arg)) (unbox (syntax-e arg)) arg)
                 #`(lambda (obj) (#,acc obj))
                 #f))]))
  (define-values (super-names super-args super-accs)
    (cond
      [(not super) (values null null null)]
      [(class-desc-reconstructor-fields super)
       => (lambda (l)
            (when (not (syntax? reconstructor-rhs))
              (raise-syntax-error #f "superclass requires custom reconstructor" stxes))
            (values (map car l)
                    (map (lambda (x) #f) l)
                    (map cdr l)))]
      [else
       (for/lists (names args accs) ([field (in-list (class-desc-fields super))]
                                     #:unless (identifier? (field-desc-constructor-arg field)))
         (values (field-desc-name field)
                 (field-desc-constructor-arg field)
                 (field-desc-accessor-id field)))]))
  (values (append super-names new-names)
          (append super-args new-args)
          (append super-accs new-accs)
          (append (map (lambda (x) #f) super-names) new-rhss)))

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
             ;; non-default reconstructor is handled in `build-methods`
             null)
         (list
          #`(define-update-syntax name-instance
              (make-update-syntax (quote-syntax (name #,(and direct? #'constructor-name)
                                                      name-instance indirect-static-infos))))))
        null)))

(define-for-syntax (make-reconstructor super constructor-name names args accs)
  #`(lambda (obj #,@(for/list ([name (in-list names)]
                               [arg (in-list args)]
                               [acc (in-list accs)])
                      (if super
                          #`[#,name (#,acc obj)]
                          name)))
      (#,constructor-name #,@(apply
                              append
                              (for/list ([name (in-list names)]
                                         [arg (in-list args)])
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
     (define (recon-field-name f) (car f))
     (define (recon-field-arg f) (cadr f))
     (define (recon-field-acc f) (caddr f))
     (define recon-fields
       (cond
         [(class-desc-reconstructor-fields desc)
          => (lambda (l)
               (for/list ([field (in-list l)])
                 (list (car field) #'#f (cdr field))))]
         [else
          (for/list ([field (in-list (class-desc-fields desc))]
                     #:do [(define arg (field-desc-constructor-arg field))]
                     #:unless (identifier? arg))
            (list (field-desc-name field)
                  (if (box? (syntax-e arg))
                      (unbox (syntax-e arg))
                      arg)
                  (field-desc-accessor-id field)))]))
     (define recon-field-ht
       (for/fold ([updates #hasheq()]) ([field (in-list recon-fields)])
         (hash-set updates (car field) field)))
     (define arg-ids (generate-temporaries rhss))
     (define updates
       (for/fold ([updates #hasheq()]) ([name (in-list names)]
                                        [arg-id (in-list arg-ids)])
         (define f (hash-ref recon-field-ht (syntax-e name) #f))
         (cond
           [(not f)
            (when static?
              (raise-syntax-error #f "no such reconstructor argument in class" with-id name))
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
                     (for/list ([field (in-list recon-fields)]
                                #:do [(define arg (recon-field-arg field))])
                       (define rhs (or (hash-ref updates (recon-field-name field) #f)
                                       #`(#,(recon-field-acc field) obj)))
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
