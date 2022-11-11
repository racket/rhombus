#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "class-parse.rkt")
         "entry-point.rkt"
         "error.rkt"
         "realm.rkt"
         racket/unsafe/undefined)

(provide (for-syntax build-class-constructor
                     need-class-constructor-wrapper?
                     encode-protocol))

(define-for-syntax (build-class-constructor super constructor-id options
                                            constructor-fields super-constructor-fields added-fields
                                            keywords super-keywords
                                            defaults super-defaults
                                            need-constructor-wrapper?
                                            abstract-name
                                            has-defaults? super-has-defaults?
                                            final?
                                            exposed-internal-id
                                            names)
  (with-syntax ([(name make-name make-all-name constructor-name constructor-maker-name
                       name?
                       name-defaults)
                 names])
    (append
     (if (syntax-e #'name-defaults)
         (list
          ;; default-value expressions should see only earlier fields
          ;; from `constructor-fields`, so use some temporary names
          ;; to make sure they can't be referenced
          (let ([super-tmps (generate-temporaries super-constructor-fields)]
                [tmps (generate-temporaries constructor-fields)])
            #`(define (name-defaults #,@(append super-tmps tmps))
                (let-values #,(cond
                                [super-has-defaults?
                                 #`([#,super-tmps
                                     (#,(class-desc-defaults-id super) . #,super-tmps)])]
                                [else '()])
                  (let* #,(for/list ([f (in-list constructor-fields)]
                                     [tmp (in-list tmps)]
                                     [df (in-list defaults)])
                            (cond
                              [(syntax-e df)
                               #`[#,f (if (eq? #,tmp unsafe-undefined)
                                          (let ([#,f . #,df]) #,f)
                                          #,tmp)]]
                              [else
                               #`[#,f #,tmp]]))
                    (values #,@super-tmps #,@constructor-fields))))))
         null)
     (if need-constructor-wrapper?
         (list
          #`(define make-name
              (let ([name
                     (lambda #,(apply append (for/list ([f (in-list (append super-constructor-fields constructor-fields))]
                                                        [kw (in-list (append super-keywords keywords))]
                                                        [df (in-list (append super-defaults defaults))])
                                               (let ([arg (if (syntax-e df)
                                                              (if final?
                                                                  #`[#,f . #,df]
                                                                  #`[#,f unsafe-undefined])
                                                              f)])
                                                 (if (keyword? (syntax-e kw))
                                                     (list kw arg)
                                                     (list arg)))))
                       (let-values #,(cond
                                       [(and super-has-defaults? (or final? (not has-defaults?)))
                                        #`([#,super-constructor-fields
                                            (#,(class-desc-defaults-id super) . #,super-constructor-fields)])]
                                       [(and has-defaults? (not final?))
                                        (define fields (append super-constructor-fields constructor-fields))
                                        #`([#,fields (name-defaults . #,fields)])]
                                       [else '()])
                         #,(if abstract-name
                               #`(raise-abstract-methods 'name)
                               #`(make-all-name #,@(or (and super (class-desc-all-fields super))
                                                       (for/list ([f (in-list (if super (class-desc-fields super) null))])
                                                         (if (identifier? (field-desc-constructor-arg f))
                                                             (field-desc-constructor-arg f)
                                                             (field-desc-name f))))
                                                #,@constructor-fields
                                                #,@(map added-field-arg-id added-fields)))))])
                name)))
         null)
     (if exposed-internal-id
         (list
          #`(define-syntax #,exposed-internal-id (make-rename-transformer (quote-syntax make-name))))
         null)
     (if constructor-id
         (cond
           [(and final?
                 (not super))
            (list
             #`(define constructor-name
                 (let-syntax ([#,constructor-id (make-rename-transformer (quote-syntax make-name))])
                   (let ([name (wrap-constructor name name?
                                                 #,(hash-ref options 'constructor-rhs))])
                     name))))]
           [else
            (list
             #`(define constructor-maker-name
                 (lambda (#,constructor-id)
                   (let ([name (wrap-constructor name name?
                                                 #,(hash-ref options 'constructor-rhs))])
                     name)))
             #`(define constructor-name
                 #,(cond
                     [super
                      (compose-constructor
                       #'make-name
                       #`([#,(encode-protocol keywords defaults) constructor-maker-name]
                          #,@(or (class-desc-constructor-makers super)
                                 (list (list (encode-protocol super-keywords super-defaults) #f)))))]
                     [else #'(constructor-maker-name make-name)])))])
         null))))

(define-for-syntax (need-class-constructor-wrapper? extra-fields keywords defaults constructor-id
                                                    super-has-keywords? super-has-defaults?
                                                    abstract-name
                                                    super)
  (or (pair? extra-fields)
      (any-stx? keywords)
      (any-stx? defaults)
      (and (or super-has-keywords?
               super-has-defaults?)
           (or (not (class-desc-constructor-makers super))
               constructor-id))
      abstract-name
      (and super
           (or (class-desc-all-fields super)
               (for/or ([f (in-list (class-desc-fields super))])
                 (identifier? (field-desc-constructor-arg f)))))))

(define-syntax (wrap-constructor stx)
  (syntax-parse stx
    [(_ name predicate-id g)
     #:do [(define adjustments (entry-point-adjustments
                                '()
                                (lambda (body)
                                  #`(let ([r #,body])
                                      (if (predicate-id r)
                                          r
                                          #,(quasisyntax/loc #'g
                                              (raise-constructor-result-error 'name r)))))
                                #f))]
     #:with (~var lam (:entry-point adjustments)) #'g
     #'lam.parsed]))

(define (raise-constructor-result-error who val)
  (raise-contract-error who
                        (string-append "constructor result does not match annotation\n"
                                       "  result: ~v")
                        val))

;; Beware that this function generates code that is quadratic in the
;; length of `makers` (i.e., in the depth of subclassing with custom
;; constructors)
(define-for-syntax (compose-constructor real-make makers)
  (define argss
    (let loop ([makers makers])
      (syntax-parse makers
        [([proto _])
         (list (generate-protocol-formal-arguments #'proto))]
        [([proto _] . rest)
         (define super-args (loop #'rest))
         (cons (append (car super-args) (generate-protocol-formal-arguments #'proto))
               super-args)])))
  (let loop ([makers makers]
             [argss argss]
             [final-make real-make])
    (syntax-parse makers
      [([proto id:identifier])
       ;; root ancestor has a custom protocol
       #`(id #,final-make)]
      [([_ #f])
       ;; represents some number of ancestors that use the default protocol;
       ;; we don't expect to get here, though, since the last case below
       ;; looks ahead to cover this case
       (error "should not get here")]
      [([proto id] . rest)
       ;; look ahead by one in `rest` so we can get the arity right
       (define formal-args (generate-protocol-formal-arguments #'proto))
       (define ancestor-formal-args (cadr argss))
       (define args (protocol-formal->actual formal-args))
       (define ancestor-args (protocol-formal->actual ancestor-formal-args))
       (syntax-parse #'rest
         [([ancestor-proto #f])
          #`(id
             (lambda #,ancestor-formal-args
               (lambda #,args
                 (#,final-make #,@ancestor-args #,@args))))]
         [([ancestor-proto ancestor-id] . _)
          #`(id
             ;; The position after `constructor` is syntactically constrainted, but
             ;; not so much that we can statically extract its signature. So, we
             ;; dynamically adjust a generic wrapper to match the constructor's
             ;; signature
             (make-keyword-procedure-like
              ancestor-id
              (lambda (kws kw-args . next-args)
                (lambda #,formal-args
                  (keyword-apply #,(loop #'rest
                                         (cdr argss)
                                         #`(lambda #,ancestor-formal-args
                                             (#,final-make #,@ancestor-args #,@args)))
                                 kws kw-args
                                 next-args)))))])])))

(define-for-syntax (encode-protocol keywords defaults)
  (if (for/and ([kw (in-list keywords)]
                [df (in-list defaults)])
        (and (not (syntax-e kw))
             (not (syntax-e df))))
      (length keywords)
      (for/list ([kw (in-list keywords)]
                 [df (in-list defaults)])
        (if (syntax-e df)
            (box kw)
            kw))))

(define-for-syntax (generate-protocol-formal-arguments proto)
  (syntax-parse proto
    [count:exact-integer
     (generate-temporaries (for/list ([i (syntax-e #'count)]) i))]
    [(arg-desc ...)
     (apply append
            (for/list ([desc (syntax->list #'(arg-desc ...))])
              (define id (car (generate-temporaries '(arg))))
              (define arg (if (box? (syntax-e desc))
                              #`[#,id unsafe-undefined]
                              id))
              (if (or (keyword? (syntax-e desc))
                      (and (box? (syntax-e desc))
                           (keyword? (syntax-e (unbox (syntax-e desc))))))
                  (list desc arg)
                  (list arg))))]))

(define-for-syntax (protocol-formal->actual args)
  (for/list ([arg (in-list args)])
    (syntax-parse arg
      [(id . _) #'id]
      [_ arg])))

(define (make-keyword-procedure-like make-proc gen-proc)
  (define proc (make-proc void)) ; we know that `make-proc` immediately returns a procedure
  (define-values (allow-kws req-kws) (procedure-keywords proc))
  (procedure-reduce-keyword-arity-mask (make-keyword-procedure gen-proc)
                                       (procedure-arity-mask proc)
                                       allow-kws
                                       req-kws))

(define (raise-abstract-methods name)
  (raise-arguments-error* name rhombus-realm "cannot instantiate class with abstract methods"))
