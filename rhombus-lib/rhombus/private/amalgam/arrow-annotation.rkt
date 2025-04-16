#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     shrubbery/print
                     enforest/name-parse
                     shrubbery/property
                     "srcloc.rkt"
                     "syntax-map.rkt"
                     "annot-context.rkt")
         racket/unsafe/undefined
         shrubbery/print
         "treelist.rkt"
         (only-in "annotation.rkt" ::)
         (submod "annotation.rkt" for-class)
         (submod "annotation.rkt" for-arrow)
         "binding.rkt"
         (submod "equal.rkt" for-parse)
         "op-literal.rkt"
         "call-result-key.rkt"
         "function-arity-key.rkt"
         "function-arity.rkt"
         (submod "define-arity.rkt" for-info)
         (only-in "values.rkt"
                  [values rhombus:values])
         "static-info.rkt"
         "if-blocked.rkt"
         "parens.rkt"
         "realm.rkt"
         "sorted-list-subset.rkt"
         "parse.rkt")

(provide (for-space rhombus/annot
                    ->
                    #%parens))

(module+ for-arrow-annot
  (provide (for-syntax parse-arrow-all-of)))

(define-annotation-syntax #%parens
  (annotation-prefix-operator
   #f
   '((default . stronger))
   'macro
   (lambda (stxes ctx)
     (syntax-parse stxes
       [(_ (~and head (_::parens . args)) . tail)
        (let ([args (syntax->list #'args)])
          (syntax-parse #'tail
            [(arrow::name . _)
             #:when (free-identifier=? (in-annotation-space #'arrow.name) (annot-quote ->))
             (parens-arrow-annotation #'arrow.name args #'head #'tail)]
            [_
             (cond
               [(null? args)
                (raise-syntax-error #f "empty annotation" #'head)]
               [(pair? (cdr args))
                (raise-syntax-error #f "too many annotations" #'head)]
               [else
                (syntax-parse (car args)
                  [(~var c (:annotation ctx))
                   (values (relocate+reraw #'head #'c.parsed) #'tail)])])]))]))))

(define-annotation-syntax ->
  (annotation-infix-operator
   #f
   (lambda () `((default . stronger)))
   'macro
   (lambda (lhs stx ctx)
     (arrow-annotation (list (list #f #f #f lhs)) #f #f #f #f lhs stx))
   'right))

(begin-for-syntax
  (define-syntax-class ::-bind
    #:attributes ()
    #:description "`::` operator"
    #:opaque
    (pattern ::name
             #:when (free-identifier=? (in-binding-space #'name)
                                       (bind-quote ::)))))

(define-for-syntax (parens-arrow-annotation arrow-name args head tail)
  (define-values (multi-kw+name+opt+lhs rest-name+ann rest-ann-whole? kw-rest-name+ann kw-rest-first?)
    (syntax-parse args
      #:datum-literals (group)
      [((group #:any))
       (values null '#:any #f '#:any #f)]
      [_
       (parse-annotation-sequence arrow-name args #f)]))
  (arrow-annotation multi-kw+name+opt+lhs
                    rest-name+ann rest-ann-whole?
                    kw-rest-name+ann kw-rest-first?
                    head tail))

(define-for-syntax (parse-annotation-sequence arrow-name args as-result? #:ctx [ctx empty-annot-context])
  (define-values (non-rest-args rest-name+ann rest-ann-whole? kw-rest-name+ann kw-rest-first?)
    (extract-rest-args arrow-name args as-result?))
  (define (check-keyword kw)
    (when as-result?
      (raise-syntax-error #f "keywords are not allowed on result annotations"
                          arrow-name
                          kw)))
  (define (check-optional eql)
    (when as-result?
      (raise-syntax-error #f "optional results are not allowed"
                          arrow-name
                          eql)))
  (define multi-kw+name+opt+lhs
    (let loop ([args non-rest-args] [has-opt? #f])
      (cond
        [(null? args) null]
        [else
         (define arg (car args))
         (define (non-opt)
           (when has-opt?
             (raise-syntax-error #f "non-optional argument follows an optional by-position argument"
                                 arrow-name
                                 arg)))
         (define-values (a opt?)
           (syntax-parse arg
             #:datum-literals (group op)
             [(group (~and kw #:any))
              (raise-syntax-error #f (string-append "`~any` allowed only by itself as "
                                                    (if as-result?
                                                        "a result"
                                                        "an argument"))
                                  arrow-name
                                  #'kw)]
             [(group kw:keyword (_::block (group name:identifier _:::-bind c ... _::equal _::_-bind)))
              (check-keyword #'kw)
              (values (list #'kw #'name #t (syntax-parse #'(group c ...)
                                             [c::annotation #'c.parsed]))
                      #f)]
             [(group kw:keyword (_::block (group c ... _::equal _::_-bind)))
              (check-keyword #'kw)
              (values (list #'kw #f #t (syntax-parse #'(group c ...)
                                         [c::annotation #'c.parsed]))
                      #f)]
             [(group kw:keyword (_::block (group name:identifier _:::-bind c ...)))
              (check-keyword #'kw)
              (values (list #'kw #'name #f (syntax-parse #'(group c ...)
                                             [c::annotation #'c.parsed]))
                      #f)]
             [(group kw:keyword (_::block c::annotation))
              (check-keyword #'kw)
              (values (list #'kw #f #f #'c.parsed)
                      #f)]
             [(group name:identifier _:::-bind c ... eql::equal _::_-bind)
              (check-optional #'eql)
              (values (list #f #'name #t (syntax-parse #'(group c ...)
                                           [c::annotation #'c.parsed]))
                      #t)]
             [(group c ... eql::equal _::_-bind)
              (check-optional #'eql)
              (values (list #f #f #t (syntax-parse #'(group c ...)
                                       [c::annotation #'c.parsed]))
                      #t)]
             [(group name:identifier _:::-bind c ...)
              (non-opt)
              (values (list #f #'name #f (syntax-parse #'(group c ...)
                                           [(~var c (:annotation ctx)) #'c.parsed]))
                      #f)]
             [(~var c (:annotation ctx))
              (non-opt)
              (values (list #f #f #f #'c.parsed)
                      #f)]))
         (cons a (loop (cdr args) (or has-opt? opt?)))])))
  (if as-result?
      (values (map (lambda (l) (list (cadr l) (cadddr l))) multi-kw+name+opt+lhs) rest-name+ann rest-ann-whole?)
      (values multi-kw+name+opt+lhs rest-name+ann rest-ann-whole? kw-rest-name+ann kw-rest-first?)))

(define-for-syntax (extract-rest-args arrow-name args as-result?)
  (define (no-second)
    (if as-result?
        "second rest result not allowed"
        "second rest argument not allowed"))
  (let loop ([args args] [non-rest-accum '()] [rest-name+ann #f] [rest-ann-whole? #f]
                         [kw-rest-name+ann #f] [kw-rest-first? #f])
    (define (parse-ann c)
      (syntax-parse #`(group . #,c)
        #:datum-literals (group)
        [(group name:identifier _:::-bind . c)
         (syntax-parse #'(group . c)
           [c::annotation (list #'name #'c.parsed)])]
        [c::annotation (list #f #'c.parsed)]))
    (syntax-parse args
      #:datum-literals (group)
      [() (values (reverse non-rest-accum) rest-name+ann rest-ann-whole? kw-rest-name+ann kw-rest-first?)]
      [((group dots::...-bind) . _)
       (raise-syntax-error #f "misplaced ellipsis" arrow-name #'dots)]
      [(g (group dots::...-bind) . args)
       (when rest-name+ann
         (raise-syntax-error #f (no-second) arrow-name #'dots))
       (syntax-parse #'g
         #:datum-literals (group)
         [(group name:identifier _:::-bind . c)
          (raise-syntax-error #f "name not allowed for repeated rest" arrow-name #'name)]
         [c::annotation
          (loop #'args non-rest-accum (list #f #'c.parsed) #f kw-rest-name+ann kw-rest-first?)])]
      [((group amp::&-bind . c) . args)
       (when rest-name+ann
         (raise-syntax-error #f (no-second) arrow-name #'amp))
       (define name+ann (parse-ann #'c))
       (loop #'args non-rest-accum name+ann #t kw-rest-name+ann kw-rest-first?)]
      [((group amp::~&-bind . c) . args)
       (when as-result?
         (raise-syntax-error #f "keyword-rest result not allowed" arrow-name #'amp))
       (when kw-rest-name+ann
         (raise-syntax-error #f "second keyword-rest argument not allowed" arrow-name #'amp))
       (define name+ann (parse-ann #'c))
       (loop #'args non-rest-accum rest-name+ann rest-ann-whole? name+ann (not rest-name+ann))]
      [(g . args)
       (when (or rest-name+ann kw-rest-name+ann)
         (raise-syntax-error #f "non-rest argument not allowed after rest argument" arrow-name #'g))
       (loop #'args (cons #'g non-rest-accum) rest-name+ann rest-ann-whole? kw-rest-name+ann kw-rest-first?)])))

(define-for-syntax (arrow-annotation multi-kw+name+opt+lhs
                                     rest-name+ann rest-ann-whole?
                                     kw-rest-name+ann kw-rest-first?
                                     head stx)
  (define arrow (syntax-parse stx [(a . _) #'a]))
  (define ctx (make-arg-context multi-kw+name+opt+lhs))
  (define-values (multi-name+rhs res-rest-name+ann res-rest-ann-whole? loc tail)
    (let ()
      (define (multi args p-res tail)
        (define-values (multi-name+rhs res-rest-name+ann res-rest-ann-whole?)
          (parse-annotation-sequence head args #t #:ctx ctx))
        (values multi-name+rhs
                res-rest-name+ann res-rest-ann-whole?
                (datum->syntax #f (list head arrow p-res))
                tail))
      (define (any-result any-res tail)
        (values null
                '#:any #t
                (datum->syntax #f (list head arrow any-res))
                tail))
      (syntax-parse stx
        #:datum-literals (group)
        [(_ (~and any-res #:any) . tail)
         (any-result #'any-res #'tail)]
        [(_ (~and any-res (_::parens (group #:any))) . tail)
         (any-result #'any-res #'tail)]
        [(_ vals (~and p-res (_::parens res ...)) . tail)
         #:when (free-identifier=? (in-annotation-space #'vals) (annot-quote rhombus:values))
         (multi (syntax->list #'(res ...)) #'p-res #'tail)]
        [(_ (~and p-res (_::parens res ...)) . tail)
         (multi (syntax->list #'(res ...)) #'p-res #'tail)]
        [(_ . tail)
         #:with (~var res (:annotation-infix-op+form+tail arrow ctx)) #`(group . tail)
         (values (list (list #f #'res.parsed))
                 #f #f
                 (datum->syntax #f (list head arrow #'res.parsed))
                 #'res.tail)])))
  (define multi-kws (map car multi-kw+name+opt+lhs))
  (define multi-names (map cadr multi-kw+name+opt+lhs))
  (define multi-opts (map caddr multi-kw+name+opt+lhs))
  (define multi-lhs (map cadddr multi-kw+name+opt+lhs))
  (define multi-rhs (map cadr multi-name+rhs))
  (define arity (if (eq? rest-name+ann '#:any)
                    #false
                    (summarize-arity (datum->syntax #f multi-kws) (datum->syntax #f multi-opts)
                                     (and rest-name+ann #t) (and kw-rest-name+ann #t))))
  (syntax-parse (list multi-lhs multi-name+rhs)
    [((l::annotation-binding-form ...) ((rhs-name r::annotation-binding-form) ...))
     #:with (lhs-i::binding-form ...) #'(l.binding ...)
     #:with (lhs-impl::binding-impl ...) #'((lhs-i.infoer-id () lhs-i.data) ...)
     #:with (lhs::binding-info ...) #'(lhs-impl.info ...)
     #:with (arg-id ...) (for/list ([name-id (in-list (syntax->list #'(lhs.name-id ...)))])
                           ((make-syntax-introducer) (datum->syntax #f (syntax-e name-id))))
     #:with (lhs-str ...) (map shrubbery-syntax->string multi-lhs)
     #:with (rhs-str ...) (map shrubbery-syntax->string multi-rhs)
     #:with (lhs-kw ...) multi-kws
     #:with (lhs-name ...) multi-names
     #:with (lhs-opt ...) multi-opts
     #:with who 'function
     (define static-infos
       #`(#,@(if (eq? rest-name+ann '#:any)
                 null
                 #`((#%function-arity #,arity)))
          #,@(if res-rest-name+ann
                 null
                 #`((#%call-result #,(let ([sis #'(r.static-infos ...)])
                                       (define sis-l (syntax->list sis))
                                       (if (= 1 (length sis-l))
                                           (car sis-l)
                                           #`((#%values #,sis)))))))
          #,@(indirect-get-function-static-infos)))
     (values
      (relocate+reraw
       loc
       (annotation-binding-form
        (binding-form
         #'arrow-infoer
         #`[who #,arity
                'who
                ([lhs l.body l.static-infos lhs-str lhs-kw lhs-name lhs-opt] ...)
                #,(and rest-name+ann
                       (syntax-parse rest-name+ann
                         [#:any rest-name+ann]
                         [(name a::annotation-binding-form)
                          #`(name a.binding a.body a.static-infos #,rest-ann-whole? #,(shrubbery-syntax->string #'a))]
                         [_ #f]))
                #,(and kw-rest-name+ann
                       (syntax-parse kw-rest-name+ann
                         [#:any kw-rest-name+ann]
                         [(name a::annotation-binding-form)
                          #`(name a.binding a.body a.static-infos #,(shrubbery-syntax->string #'a))]
                         [_ #f]))
                #,kw-rest-first?
                ([r.binding r.body r.static-infos rhs-name rhs-str] ...)
                #,(and res-rest-name+ann
                       (syntax-parse res-rest-name+ann
                         [#:any res-rest-name+ann]
                         [(name a::annotation-binding-form)
                          #`(name a.binding a.body a.static-infos #,res-rest-ann-whole? #,(shrubbery-syntax->string #'a))]))
                #,static-infos])
        (if (and (andmap not multi-kws)
                 (andmap not multi-opts)
                 (not rest-name+ann)
                 (not kw-rest-name+ann))
            ;; simple case, indirection through `lambda` to get name from context
            #'(lambda (arg-id ...) (who arg-id ...))
            ;; complex case:
            #'who)
        static-infos))
      tail)]))

(define-for-syntax (parse-arrow-all-of stx ctx)
  (syntax-parse stx
    [(form-id (~and args (p-tag::parens in-g ...)) . tail)
     #:with (who-expr (g ...)) (extract-name stx #'(in-g ...))
     #:with ((~var a (:annotation ctx)) ...) #'(g ...)
     (define loc (datum->syntax #f (list #'form-id
                                         ;; drop `~name` from reported annotation form
                                         (cons #'p-tag (syntax->list #'(g ...))))))
     (for ([a (in-list (syntax->list #'(a.parsed ...)))]
           [g (in-list (syntax->list #'(g ...)))])
       (unless (syntax-parse a
                 [a::annotation-binding-form
                  #:with b::binding-form #'a.binding
                  (free-identifier=? #'b.infoer-id #'arrow-infoer)]
                 [_ #f])
         (raise-syntax-error #f "not a function annotation" loc g)))
     (cond
       [(= 1 (length (syntax->list #'(a.parsed ...))))
        (define a1 (car (syntax-e #'(a.parsed ...))))
        (values
         (cond
           [(syntax-e #'who-expr)
            (syntax-parse a1
              [a::annotation-binding-form
               #:with ab::binding-form #'a.binding
               (syntax-parse #'ab.data
                 [[id arity orig-who-expr . rest]
                  (relocate+reraw
                   a1
                   (annotation-binding-form
                    (binding-form #'ab.infoer-id
                                  #`[id arity who-expr . rest])
                    #'a.body
                    #'a.static-infos))])])]
           [else a1])
         #'tail)]
       [else
        (syntax-parse #'(a.parsed ...)
          [(a::annotation-binding-form ...)
           #:with (ab::binding-form ...) #'(a.binding ...)
           #:with ([_ arity . _] ...) #'(ab.data ...)
           #:with who 'function
           (define all-arity (and-arity-summaries (syntax->datum #'(arity ...))))
           (define static-infos
             #`(#,@(if all-arity
                       #`((#%function-arity #,all-arity))
                       null)
                (#%call-result (#:at_arities
                                #,(for/list ([sis (syntax->list #'(a.static-infos ...))]
                                             [arity (syntax->list #'(arity ...))])
                                    #:break (not (syntax-e arity))
                                    #`[#,arity
                                       #,(static-info-lookup sis #'#%call-result)])))
                #,@(indirect-get-function-static-infos)))
           (values
            (relocate+reraw
             loc
             (annotation-binding-form
              (binding-form #'all-of-infoer
                            #`(who #,all-arity who-expr ([ab.infoer-id ab.data] ...) #,static-infos))
              #'who
              static-infos))
            #'tail)])])]))

(define-syntax (arrow-infoer stx)
  (syntax-parse stx
    [(_ in-static-infos (result-id arity who-expr lhss rest kw-rest kw-rest-first? rhs res-rest static-infos))
     (syntax-parse #'rhs
       [([rhs-i::binding-form . rhs-tail] ...)
        #:with (rhs-impl::binding-impl ...) #`((rhs-i.infoer-id () rhs-i.data) ...)
        (with-syntax ([rest (syntax-parse #'rest
                              [(name a-i::binding-form . a-rest)
                               #:with a-impl::binding-impl #`(a-i.infoer-id () a-i.data)
                               #'(name a-impl.info . a-rest)]
                              [_ #'rest])]
                      [kw-rest (syntax-parse #'kw-rest
                                 [(name a-i::binding-form . a-rest)
                                  #:with a-impl::binding-impl #`(a-i.infoer-id () a-i.data)
                                  #'(name a-impl.info . a-rest)]
                                 [_ #'kw-rest])]
                      [res-rest (syntax-parse #'res-rest
                                  [(name a-i::binding-form . a-rest)
                                   #:with a-impl::binding-impl #`(a-i.infoer-id () a-i.data)
                                   #'(name a-impl.info . a-rest)]
                                  [_ #'res-rest])])
          (binding-info "function"
                        #'function
                        #'static-infos
                        #'((result (0) . static-infos))
                        #'arrow-oncer
                        #'arrow-matcher
                        #'()
                        #'arrow-committer
                        #'arrow-binder
                        #'(result-id arity who-expr lhss rest kw-rest kw-rest-first? ([rhs-impl.info . rhs-tail] ...) res-rest)))])]))

(define-syntax (arrow-oncer stx)
  (syntax-parse stx
    [(_ (result-id arity who-expr
                   ([lhs::binding-info . _] ...)
                   rest
                   kw-rest
                   kw-rest-first?
                   ([rhs::binding-info . _] ...)
                   res-rest))
     (with-syntax ([(rest-once ...)
                    (syntax-parse #'rest
                      [(name a::binding-info . a-rest)
                       #'((a.oncer-id a.data))]
                      [_ #'()])]
                   [(kw-rest-once ...)
                    (syntax-parse #'kw-rest
                      [(name a::binding-info . a-rest)
                       #'((a.oncer-id a.data))]
                      [_ #'()])]
                   [(res-rest-once ...)
                    (syntax-parse #'res-rest
                      [(name a::binding-info . a-rest)
                       #'((a.oncer-id a.data))]
                      [_ #'()])])
       #'(begin
           (lhs.oncer-id lhs.data)
           ...
           (rhs.oncer-id rhs.data)
           ...
           rest-once ...
           kw-rest-once ...
           res-rest-once ...))]))

(define-syntax (arrow-matcher stx)
  (syntax-parse stx
    [(_ arg-id (result-id arity _ lhss _ _ _ _ _) IF success fail)
     #`(IF (and (procedure? arg-id)
                #,(let ([a (syntax-e #'arity)])
                    (cond
                      [(not a) #'#t]
                      [(and (integer? a)
                            (positive? a)
                            (zero? (bitwise-and a (sub1 a))))
                       #`(procedure-arity-includes? arg-id #,(sub1 (integer-length a)))]
                      [else
                       #`(function-arity-match? arg-id 'arity)])))
           success
           fail)]))

(define-syntax (arrow-committer stx)
  (syntax-parse stx
    [(_ arg-id () data)
     #'(begin)]))

(define-syntax (arrow-binder stx)
  (syntax-parse stx
    [(_ arg-id () data)
     (do-arrow-binder #'arg-id #'data #'#%app #f)]))

(define-for-syntax (do-arrow-binder arg-id data fail-k who-stx)
  (syntax-parse data
    [(result-id arity who-expr
                ([lhs::binding-info lhs-body lhs-static-infos lhs-str lhs-kw lhs-name lhs-opt] ...)
                rest
                kw-rest
                kw-rest-first?
                ([rhs::binding-info rhs-body rhs-static-infos rhs-name rhs-str] ...)
                res-rest)
     #:with (((lhs-bind-id lhs-bind-use . lhs-bind-static-infos) ...) ...) #'(lhs.bind-infos ...)
     #:with (((rhs-bind-id rhs-bind-use . rhs-bind-static-infos) ...) ...) #'(rhs.bind-infos ...)
     #:with (lhs-arg-id ...) (for/list ([name-id (in-list (syntax->list #'(lhs.name-id ...)))])
                               ((make-syntax-introducer) (datum->syntax #f (syntax-e name-id))))
     #:with (left-id ...) (for/list ([name (in-list (syntax->list #'(lhs-name ...)))]
                                     [default-id (in-list (generate-temporaries #'(lhs-arg-id ...)))])
                            (if (syntax-e name)
                                name
                                default-id))
     #:with (res-in-id ...)  (for/list ([name-id (in-list (syntax->list #'(rhs.name-id ...)))])
                               ((make-syntax-introducer) (datum->syntax #f (syntax-e name-id))))
     #:with (res-id ...)  (for/list ([name (in-list (syntax->list #'(rhs-name ...)))]
                                     [default-id (in-list (generate-temporaries #'(res-in-id ...)))])
                            (if (syntax-e name)
                                name
                                default-id))
     #:with (check-not-undefined ...) (for/list ([opt (in-list (syntax->list #'(lhs-opt ...)))])
                                        (if (syntax-e #'p)
                                            #'(lambda (v) (not (eq? v unsafe-undefined)))
                                            #'(lambda (v) #t)))
     #:with fail-k fail-k
     (with-syntax ([((lhs-arg ...) ...)
                    (for/list ([arg-id (in-list (syntax->list #'(lhs-arg-id ...)))]
                               [kw (in-list (syntax->list #'(lhs-kw ...)))]
                               [opt (in-list (syntax->list #'(lhs-opt ...)))]
                               ;; If there's a keyword-rest arg, we'll have to
                               ;; extract arguments manually
                               #:unless (and (syntax-e #'kw-rest) (syntax-e kw)))
                      (define var (if (not (syntax-e opt))
                                      arg-id
                                      (list arg-id #'unsafe-undefined)))
                      (if (syntax-e kw)
                          (list kw var)
                          (list var)))]
                   [((left-kw+id ...) ...)
                    (for/list ([left-id (in-list (syntax->list #'(left-id ...)))]
                               [kw (in-list (syntax->list #'(lhs-kw ...)))])
                      (if (syntax-e kw)
                          (list kw left-id)
                          (list left-id)))])
       (define (generate-rest rest kw-rest success-k fail-k raise-rest-argument-annotation-failure check-always?)
         (define (add-normal-cwv l)
           (if check-always? (cons #'call-with-values l) l))
         (with-syntax ([success-k success-k]
                       [fail-k fail-k]
                       [raise-rest-argument-annotation-failure raise-rest-argument-annotation-failure])
           (define (any-result) (list #'call-with-unchanged-values #'#%app '() '() '()))
           (syntax-parse rest
             [#f (add-normal-cwv (list #'#%app '() (if (syntax-e kw-rest) '(null) '()) '()))]
             [#:any (if check-always?
                        (any-result)
                        (list #'apply #'rest-arg-id #'(rest-id) #'([(rest-id) () (success-k rest-arg-id)])))]
             [(name a::binding-info a-body a-static-infos whole? a-str)
              #:with ((a-bind-id a-bind-use . a-bind-static-infos) ...) #'a.bind-infos
              #:with rest-list-id (or (and (syntax-e #'name) #'name) #'rest-list)
              (cond
                [(and check-always?
                      (free-identifier=? #'a.matcher-id #'always-succeed)
                      (null? (syntax-e #'(res-id ...))))
                 ;; no argument checking or constraint on number of result => tail-call original
                 (any-result)]
                [else
                 (define a-block
                   #'(let ()
                       (a.matcher-id rest-arg-id a.data
                                     if/blocked
                                     (let ()
                                       (a.committer-id rest-arg-id a.evidence-ids a.data)
                                       (a.binder-id rest-arg-id a.evidence-ids a.data)
                                       (define-static-info-syntax/maybe a-bind-id . a-bind-static-infos)
                                       ...
                                       (success-k
                                        a-body))
                                     (fail-k
                                      (lambda ()
                                        (raise-rest-argument-annotation-failure (who) rest-arg-id 'a-str whole?))))))
                 (add-normal-cwv
                  (list #'apply #'rest-arg-id #'(rest-id)
                        (if (syntax-e #'whole?)
                            #`([(rest-list-id)
                                a-static-infos
                                (let ([rest-arg-id (list->treelist rest-arg-id)])
                                  #,a-block)]
                               [(rest-id) () (success-k (rest-treelist->list rest-list-id))])
                            #`([(rest-id)
                                ()
                                (let loop ([args rest-arg-id] [accum '()])
                                  (if (null? args)
                                      (success-k (reverse accum))
                                      (let ([success-k
                                             (lambda (v)
                                               (loop (cdr args) (cons v accum)))]
                                            [rest-arg-id (car args)])
                                        #,a-block)))]))))])])))
       (with-syntax ([(f-apply rest-arg-id rest-id (rest-bind ...))
                      (generate-rest #'rest #'kw-rest #'success-k #'fail-k #'raise-rest-argument-annotation-failure #f)])
         (with-syntax ([((maybe-make-keyword-procedure ...) (kw-arg-id ...) (kw-id ...) f/kw-apply (kw-preamble ...) (kw-rest-bind ...))
                        (syntax-parse #'kw-rest
                          [#f (list #'(begin) '() '() #'f-apply '() '())]
                          [#:any (list #'(make-keyword-procedure/reduce-arity-like f)
                                       #'(kws-arg-id kw-vals-arg-id) #'(kws-id kw-vals-id) #'keyword-apply
                                       #'()
                                       #'([(kws-id kw-vals-id) () (success-k kws-arg-id kw-vals-arg-id)]))]
                          [(name a::binding-info a-body a-static-infos a-str)
                           #:with ((a-bind-id a-bind-use . a-bind-static-infos) ...) #'a.bind-infos
                           #:with kw-rest-map-id (or (and (syntax-e #'name) #'name) #'kw-rest-map)
                           (define kws (for/list ([kw (in-list (syntax->list #'(lhs-kw ...)))]
                                                  #:when (syntax-e kw))
                                         kw))
                           (define req-kws (for/list ([kw (in-list (syntax->list #'(lhs-kw ...)))]
                                                      [opt (in-list (syntax->list #'(lhs-opt ...)))]
                                                      #:when (and (syntax-e kw)
                                                                  (not (syntax-e opt))))
                                             kw))
                           (define arity-mask (car (syntax-e #'arity)))
                           (list #`(make-keyword-procedure/reduce-arity #,req-kws #,arity-mask)
                                 #'(kws-arg-id kw-vals-arg-id) #'(kws-id kw-vals-id) #'keyword-apply
                                 (if (null? kws)
                                     (list #'(define kw-map (keywords->map kws-arg-id kw-vals-arg-id)))
                                     (append
                                      (list #'(define kw-map/all (keywords->map kws-arg-id kw-vals-arg-id)))
                                      (for/list ([arg-id (in-list (syntax->list #'(lhs-arg-id ...)))]
                                                 [kw (in-list (syntax->list #'(lhs-kw ...)))]
                                                 [opt (in-list (syntax->list #'(lhs-opt ...)))]
                                                 ;; If there's a keyword-rest arg, we'll have to
                                                 ;; extract arguments manually
                                                 #:when (syntax-e kw))
                                        #`(define #,arg-id (hash-ref kw-map/all '#,kw unsafe-undefined)))
                                      (list #`(define kw-map (drop-keywords kw-map/all '#,kws)))))
                                 #'([(kw-rest-map-id)
                                     a-static-infos
                                     (let ()
                                       (a.matcher-id kw-map a.data
                                                     if/blocked
                                                     (let ()
                                                       (a.committer-id kw-map a.evidence-ids a.data)
                                                       (a.binder-id kw-map a.evidence-ids a.data)
                                                       (define-static-info-syntax/maybe a-bind-id . a-bind-static-infos)
                                                       ...
                                                       (success-k
                                                        a-body))
                                                     (fail-k
                                                      (lambda ()
                                                        (raise-keyword-rest-argument-annotation-failure (who) kw-map 'a-str)))))]
                                    [(kws-id kw-vals-id)
                                     ()
                                     (call-with-values (lambda () (rest-map->keywords kw-rest-map-id))
                                                       (lambda (kws vals) (success-k kws vals)))]))])])
           (with-syntax ([(rest-bind ...) (if (syntax-e #'kw-rest-first?)
                                              #'(kw-rest-bind ... rest-bind ...)
                                              #'(rest-bind ... kw-rest-bind ...))]
                         [(call-with-values/rest r-apply res-rest-arg-id res-rest-id (res-rest-bind ...))
                          (generate-rest #'res-rest #'#f #'values #'#%app #'raise-rest-result-annotation-failure #t)])
             (define inner-proc
               (no-srcloc
                #`(lambda (kw-arg-id ... lhs-arg ... ... . rest-arg-id)
                    (let ([who (lambda () #,(or who-stx
                                                #'who-expr))])
                      kw-preamble ...
                      (let*-values-with-static-infos/k
                       success-k
                       ([(left-id)
                         lhs-static-infos
                         (cond
                           [(check-not-undefined lhs-arg-id)
                            (lhs.matcher-id lhs-arg-id lhs.data
                                            if/blocked
                                            (let ()
                                              (lhs.committer-id lhs-arg-id lhs.evidence-ids lhs.data)
                                              (lhs.binder-id lhs-arg-id lhs.evidence-ids lhs.data)
                                              (define-static-info-syntax/maybe lhs-bind-id . lhs-bind-static-infos)
                                              ...
                                              (let ([left-id lhs-body])
                                                (success-k left-id)))
                                            (fail-k
                                             (lambda ()
                                               (raise-argument-annotation-failure (who) lhs-arg-id 'lhs-str))))]
                           [else (success-k lhs-arg-id)])]
                        ...
                        rest-bind ...)
                       (call-with-values/rest
                        (lambda ()
                          ;; At first by-position argument that's `undefined`, stop passing by-position arguments
                          (cond
                            #,@(let loop ([args (syntax->list #'((left-kw+id ...) ...))]
                                          [kws (syntax->list #'(lhs-kw ...))]
                                          [opts (syntax->list #'(lhs-opt ...))]
                                          [accum '()])
                                 (cond
                                   [(null? args) '()]
                                   [(or (not (syntax-e (car opts)))
                                        (syntax-e (car kws)))
                                    (loop (cdr args) (cdr kws) (cdr opts)
                                          (cons (car args) accum))]
                                   [else
                                    (cons
                                     #`[(eq? #,(car (syntax-e (car args))) unsafe-undefined)
                                        #,(with-syntax ([((left-kw+id ...) ...)
                                                         (append (reverse accum)
                                                                 (for/list ([arg (in-list (cdr args))]
                                                                            [kw (in-list (cdr kws))]
                                                                            #:when (syntax-e kw))
                                                                   arg))])
                                            #`(f/kw-apply f kw-id ... left-kw+id ... ... . rest-id))]
                                     (loop (cdr args) (cdr kws) (cdr opts) (cons (car args) accum)))]))
                            [else
                             (f/kw-apply f kw-id ... left-kw+id ... ... . rest-id)]))
                        (case-lambda
                          [(res-in-id ... . res-rest-arg-id)
                           (let*-values-with-static-infos
                            ([(res-id)
                              rhs-static-infos
                              (let ()
                                (rhs.matcher-id res-in-id rhs.data
                                                if/flattened
                                                (void)
                                                (raise-result-annotation-failure (who) res-in-id 'rhs-str))
                                (rhs.committer-id res-in-id rhs.evidence-ids rhs.data)
                                (rhs.binder-id res-in-id rhs.evidence-ids rhs.data)
                                (define-static-info-syntax/maybe rhs-bind-id . rhs-bind-static-infos)
                                ...
                                rhs-body)]
                             ...
                             res-rest-bind ...)
                            (r-apply values res-id ... . res-rest-id))]
                          [args
                           (raise-result-arity-error (who) '#,(length (syntax->list #'(res-in-id ...))) args)])))))))
             (if (not arg-id)
                 inner-proc
                 #`(define result-id
                     (let ([f #,arg-id])
                       (maybe-make-keyword-procedure
                        ...
                        #,inner-proc))))))))]))

(define-syntax (all-of-infoer stx)
  (syntax-parse stx
    [(_ in-static-infos (result-id all-arity who-expr cases static-infos))
     (binding-info "function"
                   #'function
                   #'static-infos
                   #'((result (0) . static-infos))
                   #'empty-oncer
                   #'all-of-matcher
                   #'()
                   #'all-of-committer
                   #'all-of-binder
                   #'(result-id all-arity who-expr cases))]))

(define-syntax (all-of-matcher stx)
  (syntax-parse stx
    [(_ arg-id (result-id all-arity who-expr ([a-infoer (~and a-data (_ arity . _))] ...)) IF success fail)
     #`(IF (and (procedure? arg-id)
                #,@(for/list ([arity (in-list (syntax->list #'(arity ...)))]
                              [a-infoer (in-list (syntax->list #'(a-infoer ...)))]
                              [a-data (in-list (syntax->list #'(a-data ...)))])
                     (let ([a (syntax-e arity)])
                       (cond
                         [(not a) #'#t]
                         [(and (integer? a)
                               (positive? a)
                               (zero? (bitwise-and a (sub1 a))))
                          #`(procedure-arity-includes? arg-id #,(sub1 (integer-length a)))]
                         [else
                          #`(function-arity-match? arg-id '#,arity)]))))
           success
           fail)]))

(define-syntax (all-of-committer stx)
  (syntax-parse stx
    [(_ arg-id () data)
     #'(begin)]))

(define-syntax (all-of-binder stx)
  (syntax-parse stx
    [(_ arg-id () (result-id all-arity who-expr ([a-infoer (~and a-data (_ arity . _))] ...)))
     (define arities (syntax->list #'(arity ...)))
     (define no-keywords? (andmap (lambda (a) (integer? (syntax-e a))) arities))
     (cond
       [(and no-keywords?
             (andmap (lambda (a)
                       (let* ([a (abs (syntax-e a))])
                         (zero? (bitwise-and a (sub1 a)))))
                     arities)
             (for/fold ([covered 0]) ([a (in-list arities)])
               (and covered
                    (syntax-e a)
                    (zero? (bitwise-and covered (syntax-e a)))
                    (bitwise-ior covered (syntax-e a)))))
        ;; No keywords, no optional arguments other than rests, and
        ;; independent arities; generate a `case-lambda` for the dispatch
        #`(define result-id
            (let ([f arg-id]
                  [who (lambda () #,(if (syntax-e #'who-expr)
                                        #'who-expr
                                        #'(quote result-id)))])
              (case-lambda
                #,@(for/list ([arity (in-list arities)]
                              [infoer (in-list (syntax->list #'(a-infoer ...)))]
                              [data (in-list (syntax->list #'(a-data ...)))])
                     (syntax-parse #`(#,infoer () #,data)
                       [a-impl::binding-impl
                        #:with a::binding-info #'a-impl.info
                        #:with ((a-bind-id a-bind-use . a-bind-static-infos) ...) #'a.bind-infos
                        ;; Since we started with `->` annotations, we know that we can
                        ;; skip the matcher and committer
                        (define mask (syntax-e arity))
                        (define args (let loop ([a mask] [n 0])
                                       (cond
                                         [(= a 1) '()]
                                         [(= a -1) 'rest-args]
                                         [else (cons (string->symbol (format "arg~a" n))
                                                     (loop (arithmetic-shift a -1)
                                                           (add1 n)))])))
                        (if (mask . < . 0)
                            #`[#,args (apply #,(do-arrow-binder #f #'a.data #'#%app #'(who))
                                             #,@(let loop ([args args])
                                                  (if (pair? args)
                                                      (cons (car args) (loop (cdr args)))
                                                      (list args))))]
                            #`[#,args (#,(do-arrow-binder #f #'a.data #'#%app #'(who))
                                       #,@args)])])))))]
       [else
        ;; Some keywords, optional arguments, or overlapping arities that
        ;; might be decided by argument annotations
        (define body
          #`(let ([who (lambda () #,(if (syntax-e #'who-expr)
                                        #'who-expr
                                        #'(quote result-id)))])
              (let ([len (length args)])
                #,(let loop ([arities arities]
                             [infoers (syntax->list #'(a-infoer ...))]
                             [datas (syntax->list #'(a-data ...))])
                  (cond
                    [(null? arities)
                     #'(error (who) "no matching case for arguments")]
                    [else
                     (define arity (car arities))
                     (define infoer (car infoers))
                     (define data (car datas))
                     (syntax-parse #`(#,infoer () #,data)
                       [a-impl::binding-impl
                        #:with a::binding-info #'a-impl.info
                        #:with ((a-bind-id a-bind-use . a-bind-static-infos) ...) #'a.bind-infos
                        ;; Since we started with `->` annotations, we know that we can
                        ;; skip the matcher and committer
                        (define-values (mask required-kws allowed-kws)
                          (syntax-parse arity
                            [(mask required allowed) (values (syntax-e #'mask)
                                                             (syntax->datum #'required)
                                                             (syntax->datum #'allowed))]
                            [_ (values (syntax-e arity) null null)]))
                        (define has-kw-rest? (or (not mask)
                                                 (not allowed-kws)))
                        #`(let ([next (lambda ()
                                        #,(loop (cdr arities)
                                                (cdr infoers)
                                                (cdr datas)))])
                            (if (and #,(if mask
                                           #`(bitwise-bit-set? #,mask len)
                                           #'#t)
                                     #,(if (null? required-kws)
                                           #'#t
                                           #`(sorted-list-subset? '#,required-kws kws))
                                     #,(if (or no-keywords?
                                               (eq? allowed-kws #f))
                                           #'#t
                                           #`(sorted-list-subset? kws '#,allowed-kws)))
                                (let ([esc-next (lambda (err) (next))])
                                  #,(let ([proc (do-arrow-binder #f #'a.data #'esc-next #'(who))])
                                      (cond
                                        [no-keywords?
                                         #`(apply #,proc args)]
                                        [has-kw-rest?
                                         ;; `do-arrow-binder` skips the `make-keyword-proc` wrapper
                                         #`(apply #,proc kws kw-args args)]
                                        [else
                                         #`(keyword-apply #,proc kws kw-args args)])))
                                (next)))])])))))
        (if no-keywords?
            #`(define result-id
                (let ([f arg-id])
                  (procedure-reduce-arity-mask
                   (lambda args #,body)
                   'all-arity)))
            (syntax-parse #'all-arity
              [#false
               #`(define result-id
                   (let ([f arg-id])
                     (procedure-reduce-arity-like
                      f
                      (make-keyword-procedure (lambda (kws kw-args . args) #,body)))))]
              [(mask req allow)
               #`(define result-id
                   (let ([f arg-id])
                     (procedure-reduce-keyword-arity-mask
                      (make-keyword-procedure (lambda (kws kw-args . args) #,body))
                      'mask
                      'req
                      'allow)))]))])]))

(define-syntax (let*-values-with-static-infos stx)
  (syntax-parse stx
    [(_ () body)
     #'body]
    [(_ ([ids () rhs] . binds) body)
     #'(let-values ([ids rhs])
         (let*-values-with-static-infos
          binds
          body))]
    [(_ ([(id) static-infos rhs] . binds) body)
     #'(let-values ([(tmp) (let ([id  rhs]) id)])
         (define id tmp)
         (define-static-info-syntax/maybe id . static-infos)
         (let*-values-with-static-infos
          binds
          body))]))

(define-syntax (let*-values-with-static-infos/k stx)
  (syntax-parse stx
    [(_ success-k () body)
     #'(let ([success-k values])
         body)]
    [(_ success-k ([ids () rhs] . binds) body)
     #'(let ([success-k
              (lambda ids
                (let*-values-with-static-infos/k
                 success-k
                 binds
                 body))])
         rhs)]
    [(_ success-k ([(id) static-infos rhs] . binds) body)
     #'(let ([success-k
              (lambda (tmp)
                (define id tmp)
                (define-static-info-syntax/maybe id . static-infos)
                (let*-values-with-static-infos/k
                 success-k
                 binds
                 body))])
         rhs)]))

(define (raise-argument-annotation-failure who val ctc)
  (raise-binding-failure who "argument" val ctc))

(define (raise-result-annotation-failure who val ctc)
  (raise-binding-failure who "result" val ctc))

(define (raise-rest-argument-annotation-failure who val ctc whole?)
  (if whole?
      (raise-binding-failure who "rest-argument list" val ctc)
      (raise-binding-failure who "argument" val ctc)))

(define (raise-rest-result-annotation-failure who val ctc whole?)
  (if whole?
      (raise-binding-failure who "rest-result list" val ctc)
      (raise-binding-failure who "result" val ctc)))

(define (raise-keyword-rest-argument-annotation-failure who val ctc)
  (raise-binding-failure who "keyword rest-argument map" val ctc))

(define (raise-result-arity-error who n args)
  (apply raise-result-arity-error*
         who rhombus-realm
         n
         #f
         args))

(define (function-arity-match? f arity)
  (define-values (f-req-kws f-allow-kws) (procedure-keywords f))
  (cond
    [(number? arity)
     (define mask arity)
     (and (null? f-req-kws)
          (= mask (bitwise-and mask (procedure-arity-mask f))))]
    [else
     (define mask (car arity))
     (define req-kws (cadr arity))
     (define allow-kws (caddr arity))
     (and (= mask (bitwise-and mask (procedure-arity-mask f)))
          (if (not allow-kws)
              (not f-allow-kws)
              (or (not f-allow-kws)
                  (for/and ([allow-kw (in-list allow-kws)])
                    (memq allow-kw f-allow-kws))))
          (for/and ([f-req-kw (in-list f-req-kws)])
            (memq f-req-kw req-kws)))]))

(define (rest-treelist->list l)
  (cond
    [(treelist? l)
     (treelist->list l)]
    [else (error "rest annotation converted to a non-List value")]))

(define (keywords->map kws kw-vals)
  (for/hashalw ([kw (in-list kws)]
                [kw-val (in-list kw-vals)])
    (values kw kw-val)))

(define (rest-map->keywords h)
  (cond
    [(hash? h)
     (define kws (hash-keys h #t))
     (values kws
             (for/list ([kw (in-list kws)])
               (hash-ref h kw)))]
    [else (error "keyword-rest annotation converted to a non-Map value")]))

(define (drop-keywords h kws)
  (for/fold ([h h]) ([kw (in-list kws)])
    (hash-remove h kw)))

(define-syntax (make-keyword-procedure/reduce-arity stx)
  (syntax-parse stx
    [(_ () _ e) #'(make-keyword-procedure e)]
    [(_ (kw ...) arity-mask e) #'(procedure-reduce-keyword-arity-mask
                                  (make-keyword-procedure e)
                                  arity-mask
                                  '(kw ...)
                                  #f)]))

(define (make-keyword-procedure/reduce-arity-like like-proc proc)
  (procedure-reduce-arity-like like-proc (make-keyword-procedure proc)))

(define (procedure-reduce-arity-like like-proc proc)
  (define-values (req-kws allowed-kws) (procedure-keywords like-proc))
  (procedure-reduce-keyword-arity-mask
   proc
   (procedure-arity-mask like-proc)
   req-kws
   allowed-kws))

(define-syntax (call-with-unchanged-values stx)
  (syntax-parse stx
    [(_ generator receiver) #'(generator)]
    [_ (error "should not get here")]))

(define-for-syntax (extract-name stx gs)
  (define (shift prop from to)
    (datum->syntax
     #f
     (cons (prop (car (syntax-e to)) (prop (car (syntax-e from))))
           (cdr (syntax-e to)))))
  (let loop ([gs (syntax->list gs)] [name #f] [accum null])
    (cond
      [(null? gs) (list name (reverse accum))]
      [else
       (syntax-parse (car gs)
         #:datum-literals (group)
         [(group #:name (b-tag::block . b))
          (when name
            (raise-syntax-error #f "second name expression not allowed"
                                stx
                                (car gs)))
          (define new-name #'(who-to-symbol (rhombus-body-at b-tag . b)))
          (cond
            [(and (null? accum) (pair? (cdr gs)))
             ;; shift prefix
             (loop (cons (shift syntax-raw-prefix-property (car gs) (cadr gs))
                         (cddr gs))
                   new-name
                   accum)]
            [(pair? accum)
             ;; shift suffix
             (loop (cdr gs) new-name (cons (shift syntax-raw-suffix-property (car gs) (car accum))
                                           (cdr accum)))]
            [else
             (loop (cdr gs) new-name accum)])]
         [_
          (loop (cdr gs) name (cons (car gs) accum))])])))

(define (who-to-symbol s)
  (cond
    [(symbol? s) s]
    [(string? s) (string->symbol s)]
    [(syntax? s) (string->symbol (format "~a" (shrubbery-syntax->string s)))]
    [else
     (raise-annotation-failure '|-> ~name result| s "error.Who")]))

(define-for-syntax (make-arg-context multi-kw+name+opt+lhs)
  (define args
    (for/fold ([args empty-equal_name_and_scopes-map]
               [i 0]
               #:result args)
              ([kw+name+opt+lhs (in-list multi-kw+name+opt+lhs)])
      (define kw (car kw+name+opt+lhs))
      (define name (cadr kw+name+opt+lhs))
      (values
       (if name
           (hash-set args (syntax-local-introduce name) (or kw i))
           args)
       (if kw i (add1 i)))))
  (annotation-context args #f))
