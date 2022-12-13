#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "class-parse.rkt"
                     "with-syntax.rkt")
         racket/unsafe/undefined
         racket/stxparam
         "call-result-key.rkt"
         "dot-provider-key.rkt"
         "static-info.rkt"
         "class-this.rkt"
         "entry-point.rkt"
         "error.rkt"
         "realm.rkt"
         "parse.rkt")

(provide (for-syntax build-class-constructor
                     need-class-constructor-wrapper?
                     encode-protocol))

(define-for-syntax (build-class-constructor super constructor-rhs
                                            added-fields constructor-private?s
                                            constructor-fields super-constructor-fields super-constructor+-fields
                                            keywords super-keywords super-constructor+-keywords
                                            defaults super-defaults super-constructor+-defaults
                                            method-private
                                            need-constructor-wrapper?
                                            abstract-name
                                            has-defaults? super-has-defaults?
                                            final?
                                            names)
  (with-syntax ([(name make-name make-all-name constructor-name constructor-maker-name
                       name?
                       name-defaults
                       make-internal-name
                       name-instance
                       [private-field-name ...]
                       [private-field-desc ...])
                 names])
    (define (build-field-init id) #`(#%plain-app #,id))
    (append
     (if (syntax-e #'name-defaults)
         (list
          ;; default-value expressions should see only earlier fields
          ;; from `constructor-fields`, so use some temporary names
          ;; to make sure they can't be referenced
          (let ([super-tmps (generate-temporaries super-constructor+-fields)]
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
                     (lambda #,(apply append (for/list ([f (in-list (append super-constructor+-fields constructor-fields))]
                                                        [kw (in-list (append super-constructor+-keywords keywords))]
                                                        [df (in-list (append (cond
                                                                               [(and super
                                                                                     (class-desc-constructor-makers super))
                                                                                ;; no need for optional arguments, because protocol
                                                                                ;; composition will supply `unsafe-undefined` for
                                                                                ;; missing arguments
                                                                                (map (lambda (v) #'#f) super-constructor+-defaults)]
                                                                               [else
                                                                                super-constructor+-defaults])
                                                                             defaults))])
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
                                        #`([#,super-constructor+-fields
                                            (#,(class-desc-defaults-id super) . #,super-constructor+-fields)])]
                                       [(and has-defaults? (not final?))
                                        (define fields (append super-constructor+-fields constructor-fields))
                                        #`([#,fields (name-defaults . #,fields)])]
                                       [else '()])
                         #,(if abstract-name
                               #`(raise-abstract-methods 'name)
                               #`(make-all-name #,@(or (and super
                                                            (class-desc-all-fields super)
                                                            (let loop ([fields (class-desc-all-fields super)]
                                                                       [c+-fs super-constructor+-fields])
                                                              (cond
                                                                [(null? c+-fs) fields]
                                                                [(identifier? (car fields))
                                                                 (cons (build-field-init (car fields)) (loop (cdr fields) c+-fs))]
                                                                [else (cons (car c+-fs) (loop (cdr fields) (cdr c+-fs)))])))
                                                       (for/list ([f (in-list (if super (class-desc-fields super) null))])
                                                         (if (identifier? (field-desc-constructor-arg f))
                                                             (build-field-init (field-desc-constructor-arg f))
                                                             (field-desc-name f))))
                                                #,@constructor-fields
                                                #,@(map (lambda (f) (build-field-init (added-field-arg-id f))) added-fields)))))])
                name)))
         null)
     (if constructor-rhs
         (let ([constructor-rhs (if (eq? constructor-rhs 'synthesize)
                                    (make-default-constructor super
                                                              super-constructor-fields constructor-fields
                                                              super-keywords keywords
                                                              super-defaults defaults
                                                              constructor-private?s
                                                              #'make-name)
                                    constructor-rhs)])
           (with-syntax-parse ([((private-method-name private-method-id/property) ...)
                                (for/list ([m-name (in-list (sort (hash-keys method-private)
                                                                  symbol<?))])
                                  (define id/property (hash-ref method-private m-name))
                                  (list (datum->syntax #'name m-name)
                                        id/property))]
                               [private-tables-spec
                                #'(cons (cons (quote-syntax name)
                                              (hasheq (~@ 'private-method-name
                                                          (quote-syntax private-method-id/property))
                                                      ...
                                                      (~@ 'private-field-name
                                                          private-field-desc)
                                                      ...))
                                        (get-private-tables))]
                               [super-this #`(#:c #,(wrap-static-info #'make-name #'#%call-result
                                                                      (let ([si #'((#%dot-provider name-instance))])
                                                                        (if super
                                                                            #`((#%call-result #,si))
                                                                            si))))])
             (cond
               [(and final?
                     (not super))
                (list
                 #`(define constructor-name
                     (syntax-parameterize ([this-id (quote-syntax super-this)]
                                           [private-tables private-tables-spec])
                       (let ([name (wrap-constructor name name? #,constructor-rhs)])
                         name))))]
               [else
                (list
                 #`(define constructor-maker-name
                     (lambda (make-name)
                       (syntax-parameterize ([this-id (quote-syntax super-this)]
                                             [private-tables private-tables-spec])
                         (let ([name (wrap-constructor name name? #,constructor-rhs)])
                           name))))
                 #`(define constructor-name
                     #,(cond
                         [super
                          (compose-constructor
                           #'make-name
                           #`([#,(encode-protocol keywords defaults keywords defaults) constructor-maker-name]
                              #,@(or (class-desc-constructor-makers super)
                                     (list (list (encode-protocol super-keywords super-defaults
                                                                  super-constructor+-keywords super-constructor+-defaults)
                                                 #f)))))]
                         [else #'(constructor-maker-name make-name)])))])))
         null)
     (if (syntax-e #'make-internal-name)
         (cond
           [(not super)
            ;; no superclass => can use `make-name` directly
            (list
             #`(define-syntax make-internal-name (make-rename-transformer (quote-syntax make-name))))]
           [(not (class-desc-constructor-makers super))
            ;; we can directly build the curried version
            (list
             #`(define make-internal-name
                 #,(make-curried-default-constructor super-constructor-fields constructor-fields
                                                     super-keywords keywords
                                                     super-defaults defaults
                                                     #'make-name)))]
           [else
            ;; need to use `compose-constructor`, but the `super` function passed
            ;; in is the one we want as the
            (list
             #`(define make-internal-name
                 (let ([constructor-maker-name (lambda (make-name) make-name)])
                   #,(compose-constructor
                      #'make-name
                      #`([#,(encode-protocol keywords defaults keywords defaults) constructor-maker-name]
                         #,@(class-desc-constructor-makers super))))))])
         null))))

(define-for-syntax (need-class-constructor-wrapper? extra-fields keywords defaults constructor-rhs
                                                    has-private-constructor-fields?
                                                    super-has-keywords? super-has-defaults?
                                                    abstract-name
                                                    super)
  (or (pair? extra-fields)
      (any-stx? keywords)
      (any-stx? defaults)
      (and (or super-has-keywords?
               super-has-defaults?)
           (or (not (class-desc-constructor-makers super))
               constructor-rhs
               has-private-constructor-fields?))
      abstract-name
      (and super
           (or (class-desc-all-fields super)
               (for/or ([f (in-list (class-desc-fields super))])
                 (identifier? (field-desc-constructor-arg f)))))))

(define-syntax (wrap-constructor stx)
  (syntax-parse stx
    #:datum-literals (parsed block)
    [(_ name predicate-id (parsed e)) #'e]
    [(_ name predicate-id (block g))
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
         (cons (append-protocols (car super-args) (generate-protocol-formal-arguments #'proto))
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
       (define formal-args (internal-protocol (generate-protocol-formal-arguments #'proto)))
       (define ancestor-formal-args (external-protocol (cadr argss)))
       (define ancestor-all-formal-args (internal-protocol (cadr argss)))
       (define args (protocol-formal->actual formal-args))
       (define ancestor-args (protocol-formal->actual ancestor-formal-args))
       (define ancestor-all-args (protocol-formal->actual ancestor-all-formal-args))
       (syntax-parse #'rest
         [([ancestor-proto #f])
          #`(id
             (lambda #,ancestor-formal-args
               (lambda #,args
                 (#,final-make #,@ancestor-args #,@args))))]
         [([ancestor-proto ancestor-id] . _)
          #`(id
             ;; The position after `constructor` is syntactically constrained, but
             ;; not so much that we can statically extract its signature. So, we
             ;; dynamically adjust a generic wrapper to match the constructor's
             ;; signature
             (make-keyword-procedure-like
              ancestor-id
              (lambda (kws kw-args . next-args)
                (lambda #,formal-args
                  (keyword-apply #,(loop #'rest
                                         (cdr argss)
                                         #`(lambda #,ancestor-all-formal-args
                                             (#,final-make #,@ancestor-all-args #,@args)))
                                 kws kw-args
                                 next-args)))))])])))

(define-for-syntax (encode-protocol keywords defaults
                                    +keywords +defaults)
  (define (encode keywords defaults)
    (for/list ([kw (in-list keywords)]
               [df (in-list defaults)])
      (if (syntax-e df)
          (box kw)
          kw)))
  (cond
    [(not (= (length keywords) (length +keywords)))
     (vector (encode keywords defaults)
             (encode +keywords +defaults))]
    [(for/and ([kw (in-list keywords)]
               [df (in-list defaults)])
       (and (not (syntax-e kw))
            (not (syntax-e df))))
     (length keywords)]
    [else
     (encode keywords defaults)]))

(define-for-syntax (generate-protocol-formal-arguments proto)
  (let loop ([proto proto] [optionals? #t])
    (syntax-parse proto
      [count:exact-integer
       (generate-temporaries (for/list ([i (syntax-e #'count)]) i))]
      [#(proto internal-proto)
       (vector (loop #'proto #t)
               (loop #'internal-proto #f))]
      [(arg-desc ...)
       (apply append
              (for/list ([desc (syntax->list #'(arg-desc ...))])
                (define id (car (generate-temporaries '(arg))))
                (define arg (if (and optionals?
                                     (box? (syntax-e desc)))
                                #`[#,id unsafe-undefined]
                                id))
                (if (or (keyword? (syntax-e desc))
                        (and (box? (syntax-e desc))
                             (keyword? (syntax-e (unbox (syntax-e desc))))))
                    (list desc arg)
                    (list arg))))])))

(define-for-syntax (external-protocol v) (if (vector? v) (vector-ref v 0) v))
(define-for-syntax (internal-protocol v) (if (vector? v) (vector-ref v 1) v))
(define-for-syntax (append-protocols a b)
  (cond
    [(vector? a) (if (vector? b)
                     (vector (append (vector-ref a 0) (vector-ref b 0))
                             (append (vector-ref a 1) (vector-ref b 1)))
                     (vector (append (vector-ref a 0) b)
                             (append (vector-ref a 1) b)))]
    [(vector? b) (vector (append a (vector-ref b 0))
                         (append a (vector-ref b 1)))]
    [else (append a b)]))

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

(define-for-syntax (make-default-constructor super
                                             super-constructor-fields constructor-fields
                                             super-keywords constructor-keywords
                                             super-defaults constructor-defaults
                                             constructor-private?s
                                             make-name)
  (define proto (encode-protocol constructor-keywords constructor-defaults
                                 constructor-keywords constructor-defaults))
  (define all-formal-args (generate-protocol-formal-arguments proto))
  (define (filter-map-arg proc)
    (let loop ([args all-formal-args] [private?s constructor-private?s])
      (cond
        [(null? args) '()]
        [(keyword? (syntax-e (car args))) (cons (car args) (loop (cdr args) private?s))]
        [else
         (define a (proc (car args) (car private?s)))
         (if a
             (cons a (loop (cdr args) (cdr private?s)))
             (loop (cdr args) (cdr private?s)))])))
  (define formal-args (filter-map-arg (lambda (arg private?)
                                        (and (not private?) arg))))
  (define all-args (filter-map-arg (lambda (arg private?)
                                     (if private?
                                         #'unsafe-undefined
                                         (syntax-parse arg
                                           [(id . _) #'id]
                                           [_ arg])))))
  (cond
    [(not super)
     #`(parsed
        (lambda #,formal-args
          (#,make-name #,@all-args)))]
    [else
     (define super-proto (encode-protocol super-keywords super-defaults
                                          super-keywords super-defaults))
     (define super-formal-args (external-protocol (generate-protocol-formal-arguments super-proto)))
     (define super-all-args (protocol-formal->actual super-formal-args))
     #`(parsed
        (lambda (#,@super-formal-args #,@formal-args)
          ((#,make-name #,@super-all-args) #,@all-args)))]))

(define-for-syntax (make-curried-default-constructor super-constructor-fields constructor-fields
                                                     super-keywords constructor-keywords
                                                     super-defaults constructor-defaults
                                                     make-name)
  (define super-proto (encode-protocol super-keywords super-defaults
                                       super-keywords super-defaults))
  (define proto (encode-protocol constructor-keywords constructor-defaults
                                 constructor-keywords constructor-defaults))
  (define all-formal-args (generate-protocol-formal-arguments proto))
  (define all-args (protocol-formal->actual all-formal-args))
  (define super-formal-args (external-protocol (generate-protocol-formal-arguments super-proto)))
  (define super-all-args (protocol-formal->actual super-formal-args))
  #`(lambda (#,@super-formal-args)
      (lambda (#,@all-formal-args)
        (#,make-name #,@super-all-args #,@all-args))))
