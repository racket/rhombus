#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "annotation-string.rkt"
                     "tag.rkt"
                     "keyword-sort.rkt")
         "parse.rkt"
         "binding.rkt"
         "repetition.rkt"
         "static-info.rkt"
         "index-result-key.rkt"
         "sequence-element-key.rkt"
         "values-key.rkt"
         "repetition.rkt"
         "if-blocked.rkt"
         "parens.rkt")

;; `make-composite-binding-transformer` is mostly generic with respect
;; to a composite datatype, but the `rest` support is currently
;; hardwired to lists.

(provide (for-syntax make-composite-binding-transformer))

(define-for-syntax (make-composite-binding-transformer constructor-str ; string name for constructor or map list, used for contract
                                                       predicate     ; predicate for the composite value
                                                       accessors     ; one accessor per component
                                                       static-infoss ; one set of static info per component
                                                       #:keywords [keywords #f] ; #f or a list of keywords and #f
                                                       #:steppers [steppers #f] ; a sequence of `cdr`s for lists
                                                       #:static-infos [composite-static-infos #'()] ; for the composite value
                                                       #:accessor->info? [accessor->info? #f] ; extend composite info?
                                                       #:index-result-info? [index-result-info? #f]
                                                       #:sequence-element-info? [sequence-element-info? #f]
                                                       #:rest-accessor [rest-accessor #f] ; for a list-like "rest"
                                                       #:rest-to-repetition [rest-to-repetition #'values] ; to convert "rest" to a list
                                                       #:rest-repetition? [rest-repetition? #t]) ; #t, #f, or 'pair
  (lambda (tail [rest-arg #f] [stx-in #f])
    (syntax-parse tail
      [(form-id (tag::parens a_g ...) . new-tail)
       #:do [(define stx (or stx-in
                             (quasisyntax/loc #'form-id
                               (#,group-tag form-id (tag a_g ...)))))]
       #:with (a::binding ...) (sort-with-respect-to-keywords keywords (syntax->list #'(a_g ...)) stx)
       #:with (a-parsed::binding-form ...) #'(a.parsed ...)
       ;; `rest-a` will have either 0 items or 1 item
       #:with (rest-a::binding ...) (if rest-arg (list rest-arg) null)
       #:with (rest-a-parsed::binding-form ...) #'(rest-a.parsed ...)
       (define as (syntax->list #'(a ...)))
       (unless (= (length as) (length accessors))
         (raise-syntax-error #f
                             (format (string-append "pattern arguments not the expected number\n"
                                                    "  expected: ~a\n"
                                                    "  given: ~a")
                                     (length accessors)
                                     (length as))
                             stx))
       (values
        (binding-form
         #'composite-infoer
         #`(#,constructor-str
            #,predicate
            #,composite-static-infos
            #,steppers
            #,accessors
            #,static-infoss
            (a-parsed.infoer-id ... )
            (a-parsed.data ...)
            #,accessor->info? #,index-result-info? #,sequence-element-info?
            #,(and rest-arg
                   #`(#,rest-accessor
                      #,rest-to-repetition
                      #,rest-repetition?
                      rest-a-parsed.infoer-id ...
                      rest-a-parsed.data ...))))
        #'new-tail)]
      [_ (raise-syntax-error #f
                             "bad syntax"
                             (or stx-in tail))])))

(define-syntax (composite-infoer stx)
  (syntax-parse stx
    [(_ static-infos (constructor-str predicate (composite-static-info ...)
                                      steppers accessors ((static-info ...) ...)
                                      (infoer-id ...) (data ...)
                                      accessor->info? index-result-info? sequence-element-info?
                                      rest-data))
     #:with (arg-static-infos ...) (cond
                                     [(syntax-e #'accessor->info?)
                                      (for/list ([accessor (in-list (syntax->list #'accessors))])
                                        (or (static-info-lookup #'static-infos accessor)
                                            #'()))]
                                     [else
                                      (define infos
                                        (or (and (syntax-e #'index-result-info?)
                                                 (static-info-lookup #'static-infos #'#%index-result))
                                            (and (syntax-e #'sequence-element-info?)
                                                 (static-info-lookup #'static-infos #'#%sequence-element))
                                            #'()))
                                      (for/list ([accessor (in-list (syntax->list #'accessors))])
                                        infos)])
     #:with (a-impl::binding-impl ...) #'((infoer-id (static-info ... . arg-static-infos) data) ...)
     #:with (a-info::binding-info ...) #'(a-impl.info ...)

     (define-values (new-rest-data rest-static-infos rest-name-id rest-annotation-str rest-bind-ids+static-infos
                                   rest-repetition?)
       (syntax-parse #'rest-data
         [#f (values #'#f #'() #'rest #f #'() #f)]
         [(rest-accessor rest-to-repetition rest-repetition? rest-infoer-id rest-a-data)
          #:with rest-static-infos
          (case (syntax-e #'rest-repetition?)
            [(pair)
             (define (static-info-lookup/pair infos key)
               (define maybe-infos (static-info-lookup infos key))
               (and maybe-infos
                    (syntax-parse (normalize-static-infos/values 2 maybe-infos)
                      [(() ()) #f]
                      [(car-infos cdr-infos) #'((car car-infos) (cdr cdr-infos))])))
             (or (and (syntax-e #'index-result-info?)
                      (static-info-lookup/pair #'static-infos #'#%index-result))
                 (and (syntax-e #'sequence-element-info?)
                      (static-info-lookup/pair #'static-infos #'#%sequence-element))
                 #'())]
            [(#t)
             (or (and (syntax-e #'index-result-info?)
                      (static-info-lookup #'static-infos #'#%index-result))
                 (and (syntax-e #'sequence-element-info?)
                      (static-info-lookup #'static-infos #'#%sequence-element))
                 #'())]
            [(#f)
             (define (static-info-lookup/wrap infos key)
               (define maybe-infos (static-info-lookup infos key))
               (and maybe-infos (list #`(#,key #,maybe-infos))))
             (append
              (or (and (syntax-e #'index-result-info?)
                       (static-info-lookup/wrap #'static-infos #'#%index-result))
                  '())
              (or (and (syntax-e #'sequence-element-info?)
                       (static-info-lookup/wrap #'static-infos #'#%sequence-element))
                  '()))])
          #:with rest-impl::binding-impl #'(rest-infoer-id rest-static-infos rest-a-data)
          #:with rest-info::binding-info #'rest-impl.info
          #:with (rest-tmp-id) (generate-temporaries #'(rest-info.name-id))
          #:with rest-seq-tmp-ids (and (syntax-e #'rest-repetition?)
                                       (generate-temporaries #'(rest-info.bind-id ...)))
          #:with no-rest-map? (free-identifier=? #'always-succeed #'rest-info.matcher-id)
          (values #'(rest-tmp-id rest-accessor
                                 rest-to-repetition no-rest-map?
                                 rest-repetition? rest-info rest-seq-tmp-ids)
                  #'rest-info.static-infos
                  #'rest-info.name-id
                  #'rest-info.annotation-str
                  (with-syntax ([(bind-uses ...)
                                 (if (syntax-e #'rest-repetition?)
                                     (for/list ([uses (in-list (syntax->list #'(rest-info.bind-uses ...)))]
                                                [id (in-list (syntax->list #'(rest-info.bind-id ...)))])
                                       (define depth (uses->depth uses))
                                       (unless depth
                                         (raise-syntax-error #f "cannot bind within a repetition" id))
                                       (list (add1 depth)))
                                     #'(rest-info.bind-uses ...))])
                    #'((rest-info.bind-id bind-uses rest-info.bind-static-info ...) ...))
                  (syntax-e #'rest-repetition?))]))

     (define all-composite-static-infos
       (let* ([composite-static-infos #'(composite-static-info ... . static-infos)]
              [composite-static-infos (if (or (null? (syntax-e rest-static-infos))
                                              (not (null? (syntax-e #'accessors))))
                                          composite-static-infos
                                          #`(#,@(case rest-repetition?
                                                  [(pair)
                                                   (define car-infos
                                                     (or (static-info-lookup rest-static-infos #'car)
                                                         #'()))
                                                   (define cdr-infos
                                                     (or (static-info-lookup rest-static-infos #'cdr)
                                                         #'()))
                                                   (append
                                                    (if (syntax-e #'index-result-info?)
                                                        (list #`(#%index-result #,cdr-infos))
                                                        '())
                                                    (if (syntax-e #'sequence-element-info?)
                                                        (list #`(#%sequence-element
                                                                 ((#%values (#,car-infos #,cdr-infos)))))
                                                        '()))]
                                                  [(#t)
                                                   (append
                                                    (if (syntax-e #'index-result-info?)
                                                        (list #`(#%index-result #,rest-static-infos))
                                                        '())
                                                    (if (syntax-e #'sequence-element-info?)
                                                        (list #`(#%sequence-element #,rest-static-infos))
                                                        '()))]
                                                  [(#f) rest-static-infos])
                                             . #,composite-static-infos))]
              [composite-static-infos (cond
                                        [(syntax-e #'accessor->info?)
                                         #`(#,@(for/list ([accessor (in-list (syntax->list #'accessors))]
                                                          [static-infos (in-list (syntax->list #'(a-info.static-infos ...)))])
                                                 #`(#,accessor #,static-infos))
                                            . #,composite-static-infos)]
                                        [else composite-static-infos])])
         composite-static-infos))
     (binding-info (build-annotation-str #'constructor-str (syntax->list #'(a-info.annotation-str ...)) rest-annotation-str
                                         #:rest-repetition? rest-repetition?)
                   #'composite
                   all-composite-static-infos
                   #`((a-info.bind-id a-info.bind-uses a-info.bind-static-info ...) ... ... . #,rest-bind-ids+static-infos)
                   #'composite-matcher
                   #'composite-committer
                   #'composite-binder
                   #`(predicate steppers accessors #,(generate-temporaries #'(a-info.name-id ...))
                                (a-info.name-id ...) (a-info.matcher-id ...) (a-info.committer-id ...) (a-info.binder-id ...) (a-info.data ...)
                                #,new-rest-data))]))

(define-syntax (composite-matcher stx)
  (syntax-parse stx
    [(_ c-arg-id
        (predicate steppers accessors tmp-ids
                   name-ids matcher-ids committer-ids binder-ids datas
                   rest-data)
        IF success-expr fail-expr)
     #`(IF (predicate c-arg-id)
           #,(let loop ([c-arg-id #'c-arg-id]
                        [steppers (syntax->list #'steppers)]
                        [accessors (syntax->list #'accessors)]
                        [tmp-ids (syntax->list #'tmp-ids)]
                        [name-ids (syntax->list #'name-ids)]
                        [matcher-ids (syntax->list #'matcher-ids)]
                        [datas (syntax->list #'datas)])
               (cond
                 [(null? name-ids)
                  (syntax-parse #'rest-data
                    [#f #`(IF #t success-expr fail-expr)]
                    [(rest-tmp-id rest-accessor
                                  rest-to-repetition no-rest-map?
                                  rest-repetition? rest-info rest-seq-tmp-ids)
                     (cond
                       [(syntax-e #'rest-repetition?)
                        #`(begin
                            (define rest-tmp-id
                              #,(make-rest-match c-arg-id #'rest-accessor #'rest-info #'(lambda (arg) #f)
                                                 #'rest-to-repetition (syntax-e #'no-rest-map?)))
                            (IF rest-tmp-id
                                success-expr
                                fail-expr))]
                       [else
                        (make-arg-getter c-arg-id #'rest-accessor #'rest-info
                                         #'rest-tmp-id
                                         #'IF
                                         #'success-expr
                                         #'fail-expr)])])]
                 [else
                  (define new-c-arg-id (if steppers
                                           (car (generate-temporaries '(c-arg-id)))
                                           c-arg-id))
                  #`(begin
                      #,@(if steppers
                             #`((define #,new-c-arg-id (#,(car steppers) #,c-arg-id)))
                             #'())
                      (define #,(car tmp-ids) (let ([#,(car name-ids) (#,(car accessors) #,new-c-arg-id)])
                                                #,(car name-ids)))
                      (#,(car matcher-ids) #,(car tmp-ids) #,(car datas)
                       IF
                       #,(loop new-c-arg-id
                               (and steppers (cdr steppers)) (cdr accessors)
                               (cdr tmp-ids) (cdr name-ids) (cdr matcher-ids)
                               (cdr datas))
                       fail-expr))]))
           fail-expr)]))

(define-syntax (composite-committer stx)
  (syntax-parse stx
    [(_ c-arg-id (predicate steppers accessors (tmp-id ...)
                            name-ids matcher-ids (committer-id ...) binder-ids (data ...)
                            rest-data))
     #`(begin
         (committer-id tmp-id data)
         ...
         #,@(syntax-parse #'rest-data
              [#f #'()]
              [(rest-tmp-id rest-accessor
                            rest-to-repetition no-rest-map?
                            rest-repetition? rest-info rest-seq-tmp-ids)
               #:with rest::binding-info #'rest-info
               (if (syntax-e #'rest-repetition?)
                   #'((define-values rest-seq-tmp-ids (rest-tmp-id)))
                   #'((rest.committer-id rest-tmp-id rest.data)))]))]))

(define-syntax (composite-binder stx)
  (syntax-parse stx
    [(_ c-arg-id (predicate steppers accessors (tmp-id ...)
                            name-ids matcher-ids committer-ids (binder-id ...) (data ...)
                            rest-data))
     #`(begin
         (binder-id tmp-id data)
         ...
         #,@(syntax-parse #'rest-data
              [#f #'()]
              [(rest-tmp-id rest-accessor
                            rest-to-repetition no-rest-map?
                            rest-repetition? rest-info rest-seq-tmp-ids)
               #:with rest::binding-info #'rest-info
               (if (syntax-e #'rest-repetition?)
                   (with-syntax ([(depth ...) (for/list ([uses (in-list (syntax->list #'(rest.bind-uses ...)))])
                                                (add1 (uses->depth uses)))]
                                 [(rest-seq-tmp-id ...) #'rest-seq-tmp-ids]
                                 [(rep-bind-id ...) (in-repetition-space #'(rest.bind-id ...))])
                     (with-syntax ([(rest-seq-tmp-id-as-rep ...)
                                    (if (syntax-e #'no-rest-map?)
                                        #'((rest-to-repetition rest-seq-tmp-id)
                                           ...)
                                        #'rest-seq-tmp-ids)])
                       #'((define-syntaxes (rest.bind-id rep-bind-id)
                            (make-expression+repetition (quote-syntax rest.bind-id)
                                                        (quote-syntax rest-seq-tmp-id-as-rep)
                                                        (quote-syntax (rest.bind-static-info ...))
                                                        #:depth depth))
                          ...)))
                   #'((rest.binder-id rest-tmp-id rest.data)))]))]))

;; ------------------------------------------------------------

;; make a "getter" expression for an argument, used for "rest-getter"
;; without assuming ellipses
(define-for-syntax (make-arg-getter c-arg-id accessor arg-info arg-id IF success fail)
  (syntax-parse arg-info
    [arg::binding-info
     #`(begin
         (define #,arg-id (let ([arg.name-id (#,accessor #,c-arg-id)])
                            arg.name-id))
         (arg.matcher-id #,arg-id arg.data
                         #,IF
                         #,success
                         #,fail))]))

;; a match result for a "rest" match is a function that gets
;; lists of results; the binder step for each element is delayed
;; until the whole list is found to match
(define-for-syntax (make-rest-match c-arg-id accessor rest-info fail
                                    rest-to-repetition no-rest-map?)
  (syntax-parse rest-info
    [rest::binding-info
     (define get-rest #`(let ([rest.name-id (#,accessor #,c-arg-id)])
                          rest.name-id))
     (if no-rest-map?
         (if (null? (syntax-e #'(rest.bind-id ...)))
             #`(lambda () (values))
             #`(lambda () #,get-rest))
         #`(get-rest-getters
            '(rest.bind-id ...)
            (#,rest-to-repetition #,get-rest)
            (lambda (arg-id)
              (rest.matcher-id arg-id rest.data
                               if/blocked
                               (lambda ()
                                 (rest.committer-id arg-id rest.data)
                                 (rest.binder-id arg-id rest.data)
                                 (values (maybe-repetition-as-list rest.bind-id rest.bind-uses)
                                         ...))
                               (#,fail arg-id)))))]))

(define-syntax (maybe-repetition-as-list stx)
  (syntax-parse stx
    [(_ id (_ ... 0 _ ...)) #'(rhombus-expression (group id))]
    [(_ id (_ ... depth:exact-integer _ ...))
     ;; unchecked, because this may be an internal identifier where expansion
     ;; hasn't bothered to bind the identifier as a repetition
     (repetition-as-list/unchecked #'(group id) (syntax-e #'depth))]))

;; run-time support for "rest" matching
(define (get-rest-getters rest-syms rest-val get-one-getter)
  (and (list? rest-val)
       (let loop ([rest-val rest-val] [accum null])
         (cond
           [(null? rest-val) (build-overall-getter rest-syms accum)]
           [else
            (define getter (get-one-getter (car rest-val)))
            (and getter
                 (loop (cdr rest-val) (cons getter accum)))]))))

;; more run-time support for "rest" matching, creates the function
;; that is called after a successful match to get all the results
(define (build-overall-getter rest-syms accum)
  (cond
    [(and (pair? rest-syms) (null? (cdr rest-syms)))
     ;; simple case: single-values
     (lambda ()
       (for/list ([getter (in-list (reverse accum))])
         (getter)))]
    [else
     ;; each getter returns multiple values
     (lambda ()
       (apply
        values
        (let loop ([getters (reverse accum)])
          (cond
            [(null? getters)
             (for/list ([rest-sym (in-list rest-syms)]) null)]
            [else
             (define vals (call-with-values (car getters) list))
             (define rest-valss (loop (cdr getters)))
             (for/list ([val (in-list vals)]
                        [rest-vals (in-list rest-valss)])
               (cons val rest-vals))]))))]))

(define-for-syntax (build-annotation-str constructor-str arg-annotation-strs rest-annotation-str
                                         #:rest-repetition? rest-repetition?)
  (define c-str (syntax-e constructor-str))
  (annotation-string-from-pattern
   (string-append
    (if (pair? c-str) (syntax-e (car c-str)) c-str)
    (if (pair? c-str) "{" "(")
    (if (and (pair? c-str)
             (null? arg-annotation-strs))
        ;; Set mode
        (apply string-append
               (for/list ([key-str (in-list (cdr c-str))]
                          [i (in-naturals)])
                 (string-append
                  (if (zero? i) "" ", ")
                  (syntax-e key-str))))
        ;; Map mode
        (apply string-append
               (for/list ([a-str (in-list arg-annotation-strs)]
                          [key-str (in-list (if (pair? c-str)
                                                (cdr c-str)
                                                arg-annotation-strs))]
                          [i (in-naturals)])
                 (string-append
                  (if (zero? i) "" ", ")
                  (if (pair? c-str)
                      (string-append (syntax-e key-str) ": ")
                      "")
                  (annotation-string-to-pattern (syntax-e a-str))))))
    (if rest-annotation-str
        (string-append (if (and (null? arg-annotation-strs)
                                (or (not (pair? c-str))
                                    (null? (cdr c-str))))
                           "" ", ")
                       (annotation-string-to-pattern
                        (if (eq? rest-repetition? 'pair)
                            (annotation-string-convert-pair (syntax-e rest-annotation-str))
                            (syntax-e rest-annotation-str)))
                       (if rest-repetition? ", ..." ""))
        "")
    (if (pair? c-str) "}" ")"))))

(define-for-syntax (uses->depth uses)
  (for/or ([use (in-list (syntax->list uses))])
    (define u (syntax-e use))
    (and (exact-integer? u) u)))
