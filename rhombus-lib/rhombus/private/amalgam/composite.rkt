#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     shrubbery/property
                     "annotation-string.rkt"
                     "tag.rkt"
                     "keyword-sort.rkt"
                     "maybe-as-original.rkt"
                     "origin.rkt"
                     "srcloc.rkt")
         racket/treelist
         "parse.rkt"
         "binding.rkt"
         "repetition.rkt"
         "static-info.rkt"
         "index-result-key.rkt"
         "sequence-element-key.rkt"
         "values-key.rkt"
         "repetition.rkt"
         "if-blocked.rkt"
         "parens.rkt"
         (only-in "underscore.rkt"
                  [_ rhombus-_]))

(provide (for-syntax composite-binding-transformer))

(module+ for-rest
  (provide maybe-repetition-as-list
           build-overall-rest-getter
           (for-syntax deepen-repetition
                       make-rest-match
                       make-repetition-bind)))

(define-for-syntax (composite-binding-transformer tail
                                                  #:rest-arg [rest-arg #f]
                                                  #:post-args [post-args null]
                                                  #:stx-info [stx-in #f]
                                                  constructor-str ; string name for constructor or map list, used for contract
                                                  predicate     ; predicate for the composite value
                                                  accessors     ; one accessor per component
                                                  static-infoss ; one set of static info per component
                                                  #:keywords [keywords #f] ; #f or a list of keywords and #f
                                                  #:steppers [steppers #f] ; a sequence of `cdr`s for lists
                                                  #:static-infos [composite-static-infos #'()] ; for the composite value
                                                  #:bounds-key [bounds-key #f]
                                                  #:accessor->info? [accessor->info? #f] ; extend composite info?
                                                  #:index-result-info? [index-result-info? #f]
                                                  #:sequence-element-info? [sequence-element-info? #f]
                                                  #:list-index-static-infos? [list-index-static-infos? #f]
                                                  #:stream-element-info? [stream-element-info? #f]
                                                  #:rest-accessor [rest-accessor #f] ; for a list-like "rest"
                                                  #:rest-to-repetition [rest-to-repetition #'in-list] ; to convert "rest" to a sequence
                                                  #:rest-repetition? [rest-repetition? #t] ; #t, #f, or 'pair
                                                  #:rest-repetition-min [rest-repetition-min 0]
                                                  #:rest-repetition-max [rest-repetition-max #f])
  (syntax-parse tail
    [(form-id (tag::parens a-g ...) . new-tail)
     #:do [(define stx (or stx-in
                           (quasisyntax/loc #'form-id
                             (#,group-tag form-id (tag a-g ...)))))]
     #:with (a::binding ...) (sort-with-respect-to-keywords keywords (syntax->list #'(a-g ...)) stx
                                                            #:make-missing (lambda (kw) #'(group rhombus-_)))
     #:with (a-parsed::binding-form ...) #'(a.parsed ...)
     ;; `rest-a` will have either 0 items or 1 item
     #:with (rest-a::binding ...) (if rest-arg (list rest-arg) null)
     #:with (rest-a-parsed::binding-form ...) #'(rest-a.parsed ...)
     #:with (post-a::binding ...) post-args
     #:with (post-a-parsed::binding-form ...) #'(post-a.parsed ...)
     (define as (syntax->list #'(a ... post-a ...)))
     (unless (= (length as) (length accessors))
       (raise-syntax-error #f
                           (format (string-append "pattern arguments not the expected number\n"
                                                  "  expected: ~a\n"
                                                  "  given: ~a")
                                   (length accessors)
                                   (length as))
                           stx))
     (values
      (transfer-origins
       (syntax->list #'(a.parsed ... rest-a.parsed ... post-a.parsed ...))
       (binding-form
        #'composite-infoer
        #`(#,constructor-str
           #,predicate #,composite-static-infos #,bounds-key
           #,steppers #,accessors #,static-infoss
           (a-parsed.infoer-id ... post-a-parsed.infoer-id ...) (a-parsed.data ... post-a-parsed.data ...)
           #,(length post-args)
           #,accessor->info? #,index-result-info? #,sequence-element-info? #,list-index-static-infos? #,stream-element-info?
           #,(and rest-arg
                  #`(#,rest-accessor
                     #,rest-to-repetition
                     #,rest-repetition?
                     #,rest-repetition-min
                     #,rest-repetition-max
                     rest-a-parsed.infoer-id ...
                     rest-a-parsed.data ...)))))
      #'new-tail)]
    [_ (raise-syntax-error #f
                           "bad syntax"
                           (or stx-in tail))]))

(define-syntax (composite-infoer stx)
  (syntax-parse stx
    [(_ static-infos (constructor-str
                      predicate init-composite-static-infos bounds-key
                      steppers accessors ((static-info ...) ...)
                      (infoer-id ...) (data ...)
                      num-post
                      accessor->info? index-result-info? sequence-element-info? list-index-static-infos? stream-element-info?
                      rest-data))
     #:with (arg-static-infos ...) (cond
                                     [(syntax-e #'accessor->info?)
                                      (for/list ([accessor (in-list (syntax->list #'accessors))])
                                        (or (static-info-lookup #'static-infos accessor)
                                            #'()))]
                                     [(and (syntax-e #'list-index-static-infos?)
                                           (syntax-e #'index-result-info?)
                                           (static-info-lookup #'static-infos #'#%index-result))
                                      => (lambda (si)
                                           (for/list ([accessor (in-list (syntax->list #'accessors))]
                                                      [i (in-naturals)])
                                             (extract-index-result si i)))]
                                     [else
                                      (define infos
                                        (or (and (or (syntax-e #'index-result-info?)
                                                     (syntax-e #'stream-element-info?))
                                                 (extract-index-uniform-result
                                                  (static-info-lookup #'static-infos #'#%index-result)))
                                            (and (or (syntax-e #'sequence-element-info?)
                                                     (syntax-e #'stream-element-info?))
                                                 (static-info-lookup #'static-infos #'#%sequence-element))
                                            #'()))
                                      (for/list ([accessor (in-list (syntax->list #'accessors))]
                                                 [i (in-naturals)])
                                        (if (and (i . > . 0)
                                                 (syntax-e #'stream-element-info?)
                                                 (not (static-infos-empty? infos)))
                                            #`((#%sequence-element #,infos))
                                            infos))])
     #:with (a-impl::binding-impl ...) #'((infoer-id (static-info ... . arg-static-infos) data) ...)
     #:with (a-info::binding-info ...) #'(a-impl.info ...)

     (define (add-index-static-infos si)
       (for/fold ([si si]) ([a-si (in-list (syntax->list #'(a-info.static-infos ...)))]
                            [i (in-naturals)])
         (add-index-result si i a-si)))

     (define-values (new-rest-data rest-static-infos rest-name-id rest-annotation-str rest-bind-ids+static-infos
                                   rest-repetition? rest-repetition-min rest-repetition-max
                                   rest-evidence-ids)
       (syntax-parse #'rest-data
         [#f (values #'#f #'() #'rest #f #'() #f 0 #f #'(() ()))]
         [(rest-accessor rest-to-repetition rest-repetition? rest-repetition-min rest-repetition-max rest-infoer-id rest-a-data)
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
                      (extract-index-uniform-result (static-info-lookup/pair #'static-infos #'#%index-result)))
                 (and (syntax-e #'sequence-element-info?)
                      (static-info-lookup/pair #'static-infos #'#%sequence-element))
                 #'())]
            [(#t)
             (or (and (syntax-e #'index-result-info?)
                      (extract-index-uniform-result (static-info-lookup #'static-infos #'#%index-result)))
                 (and (syntax-e #'sequence-element-info?)
                      (static-info-lookup #'static-infos #'#%sequence-element))
                 #'())]
            [(#f)
             (define (static-info-lookup/wrap infos key)
               (define maybe-infos (static-info-lookup infos key))
               (and maybe-infos (list #`(#,key #,maybe-infos))))
             (append
              (or (and (syntax-e #'index-result-info?)
                       (extract-index-uniform-result (static-info-lookup/wrap #'static-infos #'#%index-result)))
                  '())
              (or (and (syntax-e #'sequence-element-info?)
                       (static-info-lookup/wrap #'static-infos #'#%sequence-element))
                  '()))])
          #:with rest-impl::binding-impl #'(rest-infoer-id rest-static-infos rest-a-data)
          #:with rest-info::binding-info #'rest-impl.info
          #:with (rest-tmp-id) (generate-temporaries #'(rest-info.name-id))
          #:with rest-seq-tmp-ids (generate-temporaries #'(rest-info.bind-id ...))
          #:with no-rest-map? (free-identifier=? #'always-succeed #'rest-info.matcher-id)
          (values #'(rest-tmp-id rest-accessor
                                 rest-to-repetition no-rest-map?
                                 rest-repetition? rest-info rest-seq-tmp-ids)
                  #'rest-info.static-infos
                  #'rest-info.name-id
                  #'rest-info.annotation-str
                  (if (syntax-e #'rest-repetition?)
                      (deepen-repetition #'rest-info.bind-infos #'rest-to-repetition (syntax-e #'no-rest-map?))
                      #'rest-info.bind-infos)
                  (syntax-e #'rest-repetition?)
                  (or (syntax-e #'rest-repetition-min) 0)
                  (syntax-e #'rest-repetition-max)
                  #'(rest-tmp-id rest-info.evidence-ids))]))

     (define-values (min-len max-len)
       (if (syntax-e #'bounds-key)
           (let ([a-min-len (length (syntax->list #'(a-info.name-id ...)))])
             (syntax-parse (and (not rest-repetition?)
                                (static-info-lookup rest-static-infos #'bounds-key))
               #:datum-literals (group)
               [(group min max)
                (values (+ a-min-len (syntax-e #'min))
                        (and (syntax-e #'max) (+ a-min-len (syntax-e #'max))))]
               [_ (if (syntax-e #'rest-data)
                      (values (+ a-min-len rest-repetition-min)
                              (and rest-repetition-max (+ a-min-len rest-repetition-max)))
                      (values a-min-len a-min-len))]))
           (values #f #f)))

     (define all-composite-static-infos
       (let* ([composite-static-infos #'init-composite-static-infos]
              [composite-static-infos (if (syntax-e #'bounds-key)
                                          #`((bounds-key (group #,min-len #,max-len)) . #,composite-static-infos)
                                          composite-static-infos)]
              [composite-static-infos (static-infos-and composite-static-infos #'static-infos)]
              [composite-static-infos (if (or (static-infos-empty? rest-static-infos)
                                              (not (null? (syntax-e #'accessors))))
                                          (cond
                                            [(and (syntax-e #'list-index-static-infos?)
                                                  (pair? (syntax-e #'accessors)))
                                             #`((#%index-result #,(add-index-static-infos #f))
                                                . #,composite-static-infos)]
                                            [else
                                             composite-static-infos])
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
                                                        (list #`(#%index-result #,(if (syntax-e #'list-index-static-infos?)
                                                                                      (add-index-static-infos rest-static-infos)
                                                                                      rest-static-infos)))
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
     (define tmp-ids (generate-temporaries #'(a-info.name-id ...)))
     (with-syntax ([predicate (if (syntax-e #'bounds-key)
                                  #`(predicate #,min-len #,max-len)
                                  #'predicate)])
       (define all-annotation-strs (syntax->list #'(a-info.annotation-str ...)))
       (define num-post-strs (syntax-e #'num-post))
       (binding-info (build-annotation-str #'constructor-str
                                           (if (zero? num-post-strs)
                                               all-annotation-strs
                                               (reverse (list-tail (reverse all-annotation-strs) num-post-strs)))
                                           rest-annotation-str
                                           (list-tail all-annotation-strs (- (length all-annotation-strs) num-post-strs))
                                           #:rest-repetition? rest-repetition?
                                           #:rest-repetition-min rest-repetition-min
                                           #:rest-repetition-max rest-repetition-max)
                     #'composite
                     all-composite-static-infos
                     #`((a-info.bind-id a-info.bind-uses a-info.bind-static-info ...) ... ... . #,rest-bind-ids+static-infos)
                     #'composite-oncer
                     #'composite-matcher
                     #`(#,tmp-ids a-info.evidence-ids ... #,rest-evidence-ids)
                     #'composite-committer
                     #'composite-binder
                     #`(predicate steppers accessors #,tmp-ids
                                  (a-info.name-id ...) (a-info.oncer-id ...) (a-info.matcher-id ...)
                                  (a-info.committer-id ...) (a-info.binder-id ...) (a-info.data ...)
                                  #,new-rest-data)))]))

(define-syntax (composite-oncer stx)
  (syntax-parse stx
    [(_ (predicate steppers accessors tmp-ids
                   name-ids (oncer-id ...) matcher-ids committer-ids binder-ids (data ...)
                   rest-data))
     (with-syntax ([(rest-once ...)
                    (syntax-parse #'rest-data
                      [#f #'()]
                      [(rest-tmp-id rest-accessor
                                    rest-to-repetition no-rest-map?
                                    rest-repetition? rest-info::binding-info rest-seq-tmp-ids)
                       #'((rest-info.oncer-id rest-info.data))])])
       #`(begin
           (oncer-id data)
           ...
           rest-once
           ...))]))

(define-syntax (composite-matcher stx)
  (syntax-parse stx
    [(_ c-arg-id
        (predicate steppers accessors tmp-ids
                   name-ids oncer-ids matcher-ids committer-ids binder-ids datas
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
    [(_ c-arg-id all-evidence-ids (predicate steppers accessors (tmp-id ...)
                                             name-ids oncer-ids matcher-ids (committer-id ...) binder-ids (data ...)
                                             rest-data))
     #:with ((tmp-id/evidence ...) evidence-ids ... (rest-tmp-id/evidence rest-evidence-ids)) #'all-evidence-ids
     #`(begin
         (committer-id tmp-id/evidence evidence-ids data)
         ...
         #,@(syntax-parse #'rest-data
              [#f #'()]
              [(rest-tmp-id rest-accessor
                            rest-to-repetition no-rest-map?
                            rest-repetition? rest-info rest-seq-tmp-ids)
               #:with rest::binding-info #'rest-info
               (if (syntax-e #'rest-repetition?)
                   #'((define-values rest-seq-tmp-ids (rest-tmp-id/evidence)))
                   #'((rest.committer-id rest-tmp-id/evidence rest-evidence-ids rest.data)))]))]))

(define-syntax (composite-binder stx)
  (syntax-parse stx
    [(_ c-arg-id all-evidence-ids (predicate steppers accessors (tmp-id ...)
                                             name-ids oncer-ids matcher-ids committer-ids (binder-id ...) (data ...)
                                             rest-data))
     #:with ((tmp-id/evidence ...) evidence-ids ... (rest-tmp-id/evidence rest-evidence-ids)) #'all-evidence-ids
     #`(begin
         #,@(map
             (lambda (b)
               (relocate+reraw stx b))
             (syntax->list
              #`((binder-id tmp-id/evidence evidence-ids data)
                 ...
                 #,@(syntax-parse #'rest-data
                      [#f #'()]
                      [(rest-tmp-id rest-accessor
                                    rest-to-repetition no-rest-map?
                                    rest-repetition? rest-info rest-seq-tmp-ids)
                       #:with rest::binding-info #'rest-info
                       (if (syntax-e #'rest-repetition?)
                           (make-repetition-bind #'(rest.bind-uses ...)
                                                 #'(rest.bind-id ...)
                                                 #'((rest.bind-static-info ...) ...)
                                                 #'rest-seq-tmp-ids
                                                 (syntax-e #'no-rest-map?)
                                                 #'rest-to-repetition)
                           #'((rest.binder-id rest-tmp-id/evidence rest-evidence-ids rest.data)))])))))]))

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
            #,(cond
                [(free-identifier=? rest-to-repetition #'in-list)
                 get-rest]
                [(free-identifier=? rest-to-repetition #'in-treelist)
                 #`(treelist->list #,get-rest)]
                [else
                 #`(for/list ([x (#,rest-to-repetition #,get-rest)]) x)])
            (lambda (arg-id)
              (rest.matcher-id arg-id rest.data
                               if/blocked
                               (lambda ()
                                 (rest.committer-id arg-id rest.evidence-ids rest.data)
                                 (rest.binder-id arg-id rest.evidence-ids rest.data)
                                 (values (maybe-repetition-as-list rest.bind-id rest.bind-uses)
                                         ...))
                               (#,fail arg-id)))))]))

(define-syntax (maybe-repetition-as-list stx)
  (syntax-parse stx
    [(_ id (_ ... (#:repet ()) _ ...)) #'(rhombus-expression (group id))]
    [(_ id (_ ... (#:repet (sequencer ...)) _ ...))
     ;; unchecked, because this may be an internal identifier where expansion
     ;; hasn't bothered to bind the identifier as a repetition
     (syntax-parse #'(group id)
       [rep::repetition
        (let loop ([sequencers (syntax->list #'(sequencer ...))]
                   [rep #'rep.parsed])
          (if (null? (cdr sequencers))
              (repetition-as-list/unchecked rep 1 #:origin? #f)
              (loop (cdr sequencers) (consume-repetition rep #'for/list #'()))))])]))

;; run-time support for "rest" matching
(define (get-rest-getters rest-syms rest-val get-one-getter)
  (let loop ([rest-val rest-val] [accum null])
    (cond
      [(null? rest-val) (build-overall-rest-getter rest-syms accum)]
      [else
       (define getter (get-one-getter (car rest-val)))
       (and getter
            (loop (cdr rest-val) (cons getter accum)))])))

;; more run-time support for "rest" matching, creates the function
;; that is called after a successful match to get all the results
(define (build-overall-rest-getter rest-syms accum)
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

(define-for-syntax (make-repetition-bind rest-bind-usess
                                         rest-bind-ids
                                         rest-bind-static-infoss
                                         rest-seq-tmp-ids
                                         no-rest-map?
                                         rest-to-repetition)
  (with-syntax ([(rest-bind-uses ...) rest-bind-usess]
                [(rest-bind-id ...) rest-bind-ids]
                [((rest-bind-static-info ...) ...) rest-bind-static-infoss])
    (with-syntax ([((sequencer ...) ...) (for/list ([uses (in-list (syntax->list #'(rest-bind-uses ...)))])
                                           (define sequencers (uses->sequencers uses))
                                           (if no-rest-map?
                                               sequencers
                                               (for/list ([sequencer (in-list (syntax->list sequencers))])
                                                 #'in-list)))]
                  [(rest-seq-tmp-id ...) rest-seq-tmp-ids]
                  [(rep-bind-id ...) (in-repetition-space #'(rest-bind-id ...))]
                  [(elem-tmp-id ...) (generate-temporaries #'(rest-bind-id ...))])
      (with-syntax ([((elem-tmp-id/copy ...) ...)
                     (for/list ([elem-tmp-id (in-list (syntax->list #'(elem-tmp-id ...)))]
                                [sequencers (in-list (syntax->list #'((sequencer ...) ...)))])
                       (for/list ([sequencer (in-list (syntax->list sequencers))])
                         elem-tmp-id))])
        (with-syntax ([(rest-seq-tmp-id-as-rep ...)
                       (with-syntax ([in-seq (if no-rest-map?
                                                 rest-to-repetition
                                                 #'in-list)])
                         #'((([(elem-tmp-id) (in-seq rest-seq-tmp-id)]
                              ...)
                             ([(elem-tmp-id/copy) (sequencer elem-tmp-id/copy)])
                             ...)
                            ...))]
                      [define-syntaxes (syntax-raw-property #'define-syntaxes "def")])
          #'((define-syntaxes (rest-bind-id rep-bind-id)
               (make-expression+repetition (quote-syntax rest-seq-tmp-id-as-rep)
                                           (quote-syntax elem-tmp-id)
                                           (lambda ()
                                             (quote-syntax (rest-bind-static-info ...)))))
             ...))))))

(define-for-syntax (build-annotation-str constructor-str
                                         arg-annotation-strs
                                         rest-annotation-str
                                         post-arg-annotation-strs
                                         #:rest-repetition? rest-repetition?
                                         #:rest-repetition-min rest-repetition-min
                                         #:rest-repetition-max rest-repetition-max)
  (define c-str (syntax-e constructor-str))
  (define kind (and (list? c-str) (syntax-e (car c-str))))
  (define-values (mode-desc key-strs default?s)
    (case kind
      [(#:set) (values (syntax-e (cadr c-str))
                       (syntax-e (caddr c-str))
                       #f)]
      [(#:map) (values (syntax-e (cadr c-str))
                       (syntax-e (caddr c-str))
                       (syntax-e (cadddr c-str)))]
      [else (values #f #f #f)]))
  (define (args-string arg-annotation-strs first?)
    (apply string-append
           (case kind
             [(#:set)
              (for/list ([key-str (in-list key-strs)]
                         [i (in-naturals (if first? 0 1))])
                (string-append
                 (if (zero? i) "" ", ")
                 (syntax-e key-str)))]
             [(#:map)
              (for/list ([key-str (in-list key-strs)]
                         [a-str (in-list arg-annotation-strs)]
                         [default? (in-list default?s)]
                         [i (in-naturals (if first? 0 1))])
                (string-append
                 (if (zero? i) "" ", ")
                 (syntax-e key-str) ": "
                 (annotation-string-to-pattern (syntax-e a-str))
                 (if (syntax-e default?) " = ...." "")))]
             [else
              (for/list ([a-str (in-list arg-annotation-strs)]
                         [i (in-naturals (if first? 0 1))])
                (string-append
                 (if (zero? i) "" ", ")
                 (annotation-string-to-pattern (syntax-e a-str))))])))
  (annotation-string-from-pattern
   (string-append
    (if kind mode-desc c-str)
    (if kind "{" "(")
    (args-string arg-annotation-strs #t)
    (if rest-annotation-str
        (string-append
         (if (and (null? arg-annotation-strs)
                  (or (not kind)
                      (null? key-strs)))
             ""
             ", ")
         (annotation-string-to-pattern
          (case rest-repetition?
            [(pair) (annotation-string-convert-pair (syntax-e rest-annotation-str))]
            [else (syntax-e rest-annotation-str)]))
         (if rest-repetition? ", ..." "")
         (cond
           [(= rest-repetition-min 1) " ~nonempty"]
           [(eqv? rest-repetition-max 1) " ~once"]
           [else ""]))
        "")
    (args-string post-arg-annotation-strs (and (null? arg-annotation-strs)
                                               (not rest-annotation-str)))
    (if kind "}" ")"))))

(define-for-syntax (deepen-repetition bind-infos rest-to-repetition no-rest-map?)
  (with-syntax ([((rest-info-bind-id rest-info-bind-uses rest-info-bind-static-info ...) ...)
                 bind-infos])
    (with-syntax ([(bind-uses ...)
                   (let ([in-seq (if no-rest-map? rest-to-repetition #'in-list)])
                     (for/list ([uses (in-list (syntax->list #'(rest-info-bind-uses ...)))]
                                [id (in-list (syntax->list #'(rest-info-bind-id ...)))])
                       (define sequencers (uses->sequencers uses))
                       (unless sequencers
                         (raise-syntax-error #f "cannot bind within a repetition" id))
                       #`((#:repet (#,in-seq #,@sequencers)))))])
      #'((rest-info-bind-id bind-uses rest-info-bind-static-info ...) ...))))

(define-for-syntax (uses->sequencers uses)
  (for/or ([use (in-list (syntax->list uses))])
    (syntax-parse use
      [(#:repet (sequencer ...)) #'(sequencer ...)]
      [_ #f])))
