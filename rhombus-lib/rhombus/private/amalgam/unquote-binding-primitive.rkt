#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/name-parse
                     enforest/hier-name-parse
                     enforest/syntax-local
                     shrubbery/print
                     "name-path-op.rkt"
                     "attribute-name.rkt"
                     "origin.rkt")
         racket/treelist
         syntax/parse/pre
         "pack.rkt"
         (only-in "expression.rkt"
                  in-expression-space)
         (only-in "definition.rkt"
                  in-defn-space)
         (submod "syntax-class-primitive.rkt" for-quasiquote)
         "pattern-variable.rkt"
         "unquote-binding.rkt"
         "unquote-binding-identifier.rkt"
         "name-root-space.rkt"
         "name-root-ref.rkt"
         "order.rkt"
         "order-primitive.rkt"
         "parens.rkt"
         (submod "function-parse.rkt" for-call)
         (only-in "import.rkt" as open)
         (submod  "import.rkt" for-meta)
         (submod "syntax-class.rkt" for-anonymous-syntax-class)
         "sequence-pattern.rkt"
         (only-in "static-info.rkt" unwrap-static-infos))

(provide (for-space rhombus/unquote_bind
                    #%parens
                    ::
                    pattern
                    &&
                    \|\|
                    !
                    #%literal
                    #%block
                    group_option_sequence
                    term_option_sequence))

(module+ for-match-ns
  (provide (for-space rhombus/unquote_bind
                      delimit
                      commit)))

(module+ for-parse-pattern
  (provide (for-syntax parse-pattern)))

;; `#%quotes` is implemented in "quasiquote.rkt" because it recurs as
;; nested quasiquote matching, `_` is in "quasiquote.rkt" so it can be
;; matched literally, `cut` "quasiquote.rkt" so it can be handled
;; specially where allowed, and plain identifiers are implemented in
;; "unquote-binding-identifier.rkt"

(define-unquote-binding-syntax #%parens
  (unquote-binding-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ (_::parens g::unquote-binding) . tail)
        (values #'g.parsed
                #'tail)]
       [(_ (_::parens) . tail)
        ;; empty parentheses match an empty group, which
        ;; is only useful for matching an empty group tail
        (case (current-unquote-binding-kind)
          [(grouplet)
           (values #`((group) () () ())
                   #'tail)]
          [(term1)
           (raise-syntax-error #f "incompatible with this context" stx)]
          [else (values #'#f #'())])]))))

(begin-for-syntax
  (define-splicing-syntax-class :syntax-class-args
    (pattern (~seq (~and args (_::parens . _))))
    (pattern (~seq)
             #:with args #'#f))
  (define (parse-syntax-class-args stx-class rator-in arity-mask class-args auto-args)
    (cond
      [(or (not arity-mask)
           (and (bitwise-bit-set? arity-mask 0)
                (not (syntax-e class-args))))
       (when (syntax-e class-args)
         (raise-syntax-error #f
                             "syntax class does not expect arguments"
                             stx-class))
       (if auto-args
           #`(#,rator-in #,@auto-args)
           rator-in)]
      [(not (syntax-e class-args))
       (raise-syntax-error #f
                           "syntax class expects arguments"
                           stx-class)]
      [else
       (define-values (call empty-tail to-anon-function?)
         (parse-function-call rator-in '() #`(#,stx-class #,class-args)
                              #:static? #t
                              #:rator-stx stx-class
                              #:rator-kind '|syntax class|
                              #:rator-arity arity-mask))
       (unwrap-static-infos call)])))

(begin-for-syntax
  (define-syntax-rule (define-expose-specifier-class class spec desc)
    (define-syntax-class class
      #:attributes (name)
      #:description desc
      #:opaque
      (pattern ::name
        #:when (free-identifier=? (in-import-space #'name)
                                  (impo-quote spec)))))
  (define-expose-specifier-class :as-id as "the literal `as`")
  (define-expose-specifier-class :open-id open "the literal `open`")

  (define (context-kind->class-kind ctx-kind)
    (case ctx-kind
      [(term1) 'term]
      [(grouplet group1) 'group]
      [(multi1) 'multi]
      [(block1) 'block])))

(define-unquote-binding-syntax ::
  (unquote-binding-infix-operator
   #f
   null
   'macro
   (lambda (form1 stx)
     (unless (or (identifier? form1)
                 (syntax-parse form1
                   [(underscore () () ())
                    (free-identifier=? #'underscore #'_)]
                   [_ #f]))
       (raise-syntax-error #f
                           "preceding term must be an identifier or `_`"
                           (syntax-parse stx
                             [(colons . _) #'colons])))
     (define (lookup-syntax-class stx-class)
       (define rsc (syntax-local-value* (in-syntax-class-space stx-class) syntax-class-ref))
       (or rsc
           (raise-syntax-error #f
                               "not bound as a syntax class"
                               stx-class)))
     (define (parse-open-block stx tail)
       (syntax-parse tail
         #:datum-literals (group)
         [((_::block g ...))
          (values
           (for/fold ([spec #hasheq()]) ([g (in-list (syntax->list #'(g ...)))])
             (syntax-parse g
               #:datum-literals (group)
               [(group open::open-id)
                (when (syntax? spec)
                  (raise-syntax-error #f "duplicate opening clause" stx #'open.name))
                (when (and (hash? spec) ((hash-count spec) . > . 0))
                  (raise-syntax-error #f "opening clause not allowed after specific fields" stx #'open.name))
                #'open.name]
               [(group field:identifier as::as-id bind:identifier)
                (when (syntax? spec)
                  (raise-syntax-error #f "specific field not allowed after opening clause" stx #'field))
                (define key (syntax-e #'field))
                (hash-set spec key (cons (cons #'field #'bind) (hash-ref spec key null)))]
               [(group field:identifier ...)
                (when (syntax? spec)
                  (raise-syntax-error #f "specific field not allowed after opening clause" stx
                                      (car (syntax-e #'(field ...)))))
                (for/fold ([spec spec]) ([field (in-list (syntax->list #'(field ...)))])
                  (define key (syntax-e field))
                  (hash-set spec key (cons (cons field field) (hash-ref spec key null))))]
               [_
                (raise-syntax-error #f "bad exposure clause" stx g)]))
           #'())]
         [_ (values #f tail)]))
     (define match-id (car (generate-temporaries (list form1))))
     (define (track stx-class-id stx)
       (define new-stx-class-id
         (add-origin (in-syntax-class-space (syntax-local-introduce stx-class-id))
                     stx-class-id))
       (transfer-origins (list form1 new-stx-class-id) stx))
     (syntax-parse stx
       #:datum-literals (group)
       [(_ (~and sc (_::parens (group . rest))) . tail)
        #:with (~var sc-hier (:hier-name-seq in-name-root-space in-expression-space name-path-op name-root-ref)) #'rest
        #:do [(define parser (syntax-local-value* (in-defn-space #'sc-hier.name) syntax-class-parser-ref))]
        #:when parser
        (define-values (open-attributes end-tail) (parse-open-block stx #'tail))
        (define rsc ((syntax-class-parser-proc parser) (string->symbol
                                                        (shrubbery-syntax->string #'sc-hier.name))
                                                       #'sc
                                                       (context-kind->class-kind
                                                        (current-unquote-binding-kind))
                                                       match-id
                                                       #'sc-hier.tail))
        (if rsc
            (values (track #'sc-hier.name
                           (build-syntax-class-pattern #'sc rsc #'#f open-attributes form1 match-id #f))
                    end-tail)
            ;; shortcut for kind mismatch
            (values #'#f #'()))]
       [(_ . rest)
        #:with (~var stx-class-hier (:hier-name-seq in-name-root-space in-syntax-class-space name-path-op name-root-ref)) #'rest
        (syntax-parse #'stx-class-hier.tail
          [(args::syntax-class-args . args-tail)
           (define-values (open-attributes tail) (parse-open-block stx #'args-tail))
           (values (track
                    #'stx-class-hier.name
                    (build-syntax-class-pattern #'stx-class-hier.name
                                                (lookup-syntax-class #'stx-class-hier.name)
                                                #'args.args
                                                open-attributes
                                                form1
                                                match-id
                                                #f))
                   tail)])]))
   'none))

(define-for-syntax (parse-pattern stx)
  (syntax-parse stx
    [(form-id
      (~optional (~seq form1:identifier
                       (~optional (~and #:open (~bind [open? #t])))))
      . tail)
     (parameterize ([inline-attr-depth (cond
                                         [(inline-attr-depth) => add1]
                                         [else 0])]
                    [inline-form-id #'form-id])
       (define rsc
         (parse-anonymous-syntax-class (syntax-e #'form-id)
                                       stx
                                       (context-kind->class-kind
                                        (current-unquote-binding-kind))
                                       #f
                                       #'tail))
       (values (if rsc
                   (build-syntax-class-pattern stx
                                               rsc
                                               #'#f
                                               (and (or (not (attribute form1))
                                                        (attribute open?))
                                                    #'form-id)
                                               (attribute form1)
                                               #f
                                               #t)
                   #'#f)
               #'()))]))

(define-unquote-binding-syntax pattern
  (unquote-binding-transformer parse-pattern))

(begin-for-syntax
  (struct open-attrib (sym bind-id var)))

;; used for `::` and for `pattern`, returns a parsed binding form that takes advantage
;; of a syntax class --- possibly an inlined syntax class and/or one with exposed fields
(define-for-syntax (build-syntax-class-pattern stx-class rsc class-args open-attributes-spec
                                               form1 match-id bind-dot?)
  (with-syntax ([id (if (identifier? form1) form1 #'wildcard)])
    (define (compat pack* unpack* #:splice? [splice? #f])
      (define sc (rhombus-syntax-class-class rsc))
      (define sc-call (parse-syntax-class-args stx-class
                                               sc
                                               (rhombus-syntax-class-arity-mask rsc)
                                               class-args
                                               (rhombus-syntax-class-auto-args rsc)))
      (define temp-id (car (generate-temporaries (list #'id))))
      (define vars (for/list ([l (in-list (syntax->list (rhombus-syntax-class-attributes rsc)))])
                     (syntax-list->pattern-variable l)))
      (define swap-to-root (and (rhombus-syntax-class-root-swap rsc)
                                (car (rhombus-syntax-class-root-swap rsc))))
      (define swap-root-to-id (and (rhombus-syntax-class-root-swap rsc)
                                   (cdr (rhombus-syntax-class-root-swap rsc))))
      (define swap-root-to (if (syntax? swap-root-to-id)
                               (syntax-e swap-root-to-id)
                               swap-root-to-id))
      (define-values (attribute-bindings attribute-vars)
        (for/lists (bindings descs) ([var (in-list vars)]
                                     [temp-attr (in-list (generate-temporaries (map pattern-variable-sym vars)))])
          (define name (pattern-variable-sym var))
          (define id (pattern-variable-id var))
          (define depth (pattern-variable-depth var))
          (define unpack*-form (pattern-variable-unpack* var))
          (define statinfos (normalize-pvar-statinfos (pattern-variable-statinfos var)))
          (define id-with-attr (compose-attr-name match-id name id))
          (values #`[#,temp-attr #,(cond
                                     [(eq? depth 'tail)
                                      ;; bridge from a primitive syntax class, where we don't want to convert to
                                      ;; a list and then convert back when the tail is used as a new tail in a
                                      ;; template
                                      #`(pack-tail* (syntax #,id-with-attr) 0)]
                                     [(not (or (and (identifier? unpack*-form)
                                                    (or (free-identifier=? unpack*-form #'unpack-tail-list*)
                                                        (free-identifier=? unpack*-form #'unpack-multi-tail-list*)))
                                               (let ([l (syntax->list unpack*-form)])
                                                 (and l
                                                      (free-identifier=? (car l) #'unpack-parsed*)))))
                                      ;; assume depth-compatible value checked on binding side, and
                                      ;; let `attribute` unpack syntax repetitions
                                      #`(pack-nothing* (attribute #,id-with-attr) #,depth)]
                                     [else
                                      #`(#,(cond
                                             [(let ([l (syntax->list unpack*-form)])
                                                (and l
                                                     (free-identifier=? (car l) #'unpack-parsed*)
                                                     (cadr l)))
                                              => (lambda (kw)
                                                   #`(pack-parsed* #,kw))]
                                             [(free-identifier=? unpack*-form #'unpack-tail-list*)
                                              #'pack-tail-list*]
                                             [(free-identifier=? unpack*-form #'unpack-multi-tail-list*)
                                              #'pack-multi-tail-list*]
                                             [else #'pack-term*])
                                         (syntax #,(let loop ([t id-with-attr] [depth depth])
                                                     (if (zero? depth)
                                                         t
                                                         (loop #`(#,t #,(quote-syntax ...)) (sub1 depth)))))
                                         #,depth)])]
                  (pattern-variable name id temp-attr (if (eq? depth 'tail) 1 depth) unpack*-form statinfos))))

      ;; #f or (list (list bind-id var))
      (define open-attributes
        (cond
          [(not open-attributes-spec) #f]
          [(hash? open-attributes-spec)
           (define found-attributes
             (for/hasheq ([var (in-list attribute-vars)])
               (values (pattern-variable-sym var) var)))
           (for/list ([name (in-hash-keys open-attributes-spec #t)]
                      #:do [(define field+binds (hash-ref open-attributes-spec name))
                            (define var-or-root
                              (cond
                                [(eq? name swap-to-root) #f]
                                [(eq? name swap-root-to) 'root]
                                [else
                                 (hash-ref found-attributes name #f)]))
                            (unless var-or-root
                              (raise-syntax-error #f
                                                  "not an attribute of the syntax class"
                                                  ;; pick abritaty open for the same field name
                                                  (caar field+binds)))]
                      [field+bind (in-list field+binds)])
             (open-attrib (syntax-e (car field+bind)) (cdr field+bind) var-or-root))]
          [else
           (append
            (if swap-root-to
                (let ([id (if (identifier? swap-root-to-id)
                              swap-root-to-id
                              (datum->syntax open-attributes-spec swap-root-to open-attributes-spec))])
                  (list (open-attrib (syntax-e id) id 'root)))
                null)
            (for/list ([var (in-list attribute-vars)]
                       #:unless (eq? (pattern-variable-sym var) swap-to-root))
              (define id (or (pattern-variable-id var)
                             (datum->syntax open-attributes-spec (pattern-variable-sym var) open-attributes-spec)))
              (open-attrib (syntax-e id) id var)))]))
      (define pack-depth 0)
      (define dotted-bind? (and sc (not (identifier? sc)) (rhombus-syntax-class-splicing? rsc)))
      (define instance-id (or match-id (car (generate-temporaries (list (inline-form-id))))))
      (define swap-to-root-var
        (for/first ([var (in-list attribute-vars)]
                    #:when (eq? (pattern-variable-sym var) swap-to-root))
          var))
      (define all-attribs
        (append
         (if swap-root-to
             (list
              (pattern-variable->list
               (pattern-variable swap-root-to #f temp-id pack-depth unpack* 'stx)))
             null)
         (for/list ([var (in-list attribute-vars)]
                    #:unless (eq? swap-to-root (pattern-variable-sym var)))
           (pattern-variable->list var #:keep-id? #f))))
      (define root-statinfos
        (if swap-root-to
            'stx
            (get-syntax-class-static-infos 'stx (datum->syntax #f all-attribs))))
      (define swap-to-root-statinfos
        (and swap-root-to
             (get-syntax-class-static-infos
              (normalize-pvar-statinfos (pattern-variable-statinfos swap-to-root-var))
              (datum->syntax #f all-attribs))))
      #`(#,((if splice?
                (lambda (p)
                  (with-syntax ([(tmp) (generate-temporaries '(tail))])
                    #`(~and (~seq tmp (... ...))
                            (~parse #,p (cons 'multi #'(tmp (... ...)))))))
                values)
            (if sc
                (if (identifier? sc)
                    (let ([p #`(~var #,instance-id #,sc-call)])
                      (if (rhombus-syntax-class-splicing? rsc)
                          #`(~seq #,p) ;; communicates to `&&`
                          p))
                    #`(~and #,(if dotted-bind?
                                  #`(~seq #,instance-id (... ...))
                                  instance-id)
                            #,sc)) ; inline syntax class
                (if (eq? 'group (rhombus-syntax-class-kind rsc))
                    #`(~and (_ _ . _) #,instance-id)
                    instance-id)))
         #,(let ([bindings (cons #`[#,temp-id (#,pack* (syntax #,(if dotted-bind?
                                                                     #`(#,instance-id (... ...))
                                                                     instance-id))
                                               #,pack-depth)]
                                 attribute-bindings)])
             ;; Find root representative and wrap it to hold the other fields
             (define root-index (if swap-root-to
                                    (for/or ([var (in-list attribute-vars)]
                                             [i (in-naturals 1)])
                                      (and (eq? swap-to-root (pattern-variable-sym var)) i))
                                    0))
             (append
              (let loop ([i 0] [bindings bindings])
                (cond
                  [(= i root-index) (cdr bindings)]
                  [else (cons (car bindings) (loop (add1 i) (cdr bindings)))]))
              (list
               (syntax-parse (list-ref bindings root-index)
                 [(id rhs) #`(id #,(build-wrap-syntax-for-attributes
                                    #'rhs
                                    (rhombus-syntax-class-key rsc)
                                    (datum->syntax #f all-attribs)))]))))
         #,(append
            (if (identifier? form1)
                (list (make-pattern-variable-bind #'id
                                                  (if swap-to-root-var
                                                      (pattern-variable-val-id swap-to-root-var)
                                                      temp-id)
                                                  (if swap-to-root-var
                                                      (pattern-variable-unpack* swap-to-root-var)
                                                      unpack*)
                                                  (if swap-to-root-var
                                                      (pattern-variable-depth swap-to-root-var)
                                                      pack-depth)
                                                  #:statinfos (if swap-to-root-var
                                                                  swap-to-root-statinfos
                                                                  (normalize-pvar-statinfos root-statinfos))
                                                  #:attribs all-attribs
                                                  #:key (rhombus-syntax-class-key rsc)))
                 null)
            (if (not open-attributes)
                null
                (for/list ([oa (in-list open-attributes)]
                           #:unless (eq? 'root (open-attrib-var oa)))
                  (define bind-id (open-attrib-bind-id oa))
                  (define var (open-attrib-var oa))
                  (make-pattern-variable-bind bind-id (pattern-variable-val-id var) (pattern-variable-unpack* var)
                                              (pattern-variable-depth var)
                                              #:statinfos (normalize-pvar-statinfos (pattern-variable-statinfos var))))))
         #,(append
            (if (identifier? form1)
                (list
                 (if swap-to-root
                     (pattern-variable->list (struct-copy pattern-variable swap-to-root-var
                                                          [sym (syntax-e #'id)]
                                                          [id #'id]
                                                          [statinfos swap-to-root-statinfos]))
                     (pattern-variable->list
                      (pattern-variable #'id #'id temp-id pack-depth unpack* root-statinfos))))
                null)
            (if (not open-attributes)
                null
                (for/list ([oa (in-list open-attributes)])
                  (define var (open-attrib-var oa))
                  (if (eq? var 'root)
                      (pattern-variable->list
                       (pattern-variable (open-attrib-sym oa) #f temp-id pack-depth unpack* root-statinfos))
                      (pattern-variable->list (struct-copy pattern-variable var
                                                           [sym (open-attrib-sym oa)]))))))))
    (define (incompat)
      (raise-syntax-error #f
                          "syntax class incompatible with this context"
                          stx-class))
    (define (retry) #'#f)
    (define ctx-kind (current-unquote-binding-kind))
    (cond
      [(eq? (rhombus-syntax-class-kind rsc) 'term)
       (cond
         [(not (eq? ctx-kind 'term1)) (retry)]
         [(rhombus-syntax-class-splicing? rsc)
          (compat #'pack-tail* #'unpack-element*)] ;; `unpack-element*` keeps `group` or `multi` wrapper
         [else (compat #'pack-term* #'unpack-term*)])]
      [(eq? (rhombus-syntax-class-kind rsc) 'group)
       (cond
         [(eq? ctx-kind 'term1) (incompat)]
         [(not (or (eq? ctx-kind 'grouplet) (eq? ctx-kind 'group1))) (retry)]
         [else (compat #'pack-group* #'unpack-group*)])]
      [(eq? (rhombus-syntax-class-kind rsc) 'multi)
       (cond
         [(or (eq? ctx-kind 'multi1) (eq? ctx-kind 'block1))
          (compat #'pack-tagged-multi* #'unpack-multi-as-term*)]
         [(eq? ctx-kind 'group1)
          (compat #'pack-tagged-multi* #'unpack-multi-as-term* #:splice? #t)]
         [else (incompat)])]
      [(eq? (rhombus-syntax-class-kind rsc) 'block)
       (cond
         [(eq? ctx-kind 'block1)
          (compat #'pack-block* #'unpack-multi-as-term*)]
         [else (incompat)])]
      [else
       (error "unrecognized kind"  (rhombus-syntax-class-kind rsc))])))

(define-for-syntax (normalize-id form)
  (if (identifier? form)
      (if (eq? (current-unquote-binding-kind) 'grouplet)
          #f
          (identifier-as-unquote-binding form (current-unquote-binding-kind)))
      form))

(define-for-syntax (norm-seq pat like-pat)
  (cond
    [(is-sequence-pattern? pat)
     pat]
    [(is-sequence-pattern? like-pat)
     #`(~seq #,pat)]
    [else pat]))

(define-for-syntax (norm-seq2 pat like-pat1 like-pat2)
  (norm-seq (norm-seq pat like-pat1) like-pat2))

(define-unquote-binding-syntax &&
  (unquote-binding-infix-operator
   (lambda () (order-quote logical_conjuction))
   null
   'automatic
   (lambda (form1 form2 stx)
     (syntax-parse (normalize-id form1)
       [#f #'#f]
       [(pat1 (idr1 ...) (sidr1 ...) (var1 ...))
        (syntax-parse (normalize-id form2)
          [#f #'#f]
          [(pat2 idrs2 sidrs2 vars2)
           #`(#,(norm-seq2 #`(~and #,(norm-seq #'pat1 #'pat2)
                                   #,(norm-seq #'pat2 #'pat1))
                           #'pat1
                           #'pat2)
              (idr1 ... . idrs2)
              (sidr1 ... . sidrs2)
              (var1 ... . vars2))])]))
   'left))

(define-unquote-binding-syntax \|\|
  (unquote-binding-infix-operator
   (lambda () (order-quote logical_disjuction))
   null
   'automatic
   (lambda (form1 form2 stx)
     (syntax-parse (normalize-id form1)
       [#f #'#f]
       [(pat1 idrs1 sidrs1 vars1)
        (syntax-parse (normalize-id form2)
          [#f #'#f]
          [(pat2 idrs2 sidrs2 vars2)
           #`(#,(norm-seq2 #`(~or* #,(norm-seq #'pat1 #'pat2)
                                   #,(norm-seq #'pat2 #'pat1))
                           #'pat1
                           #'pat2)
              ()
              ()
              ())])]))
   'left))

(define-unquote-binding-syntax !
  (unquote-binding-prefix-operator
   (lambda () (order-quote logical_negation))
   `()
   'automatic
   (lambda (form stx)
     (syntax-parse (and (eq? (current-unquote-binding-kind) 'term1)
                        (normalize-id form))
       [#f #'#f]
       [(pat _ _ _)
        (when (is-sequence-pattern? #'pat)
          (raise-syntax-error #f "only allowed before a term pattern" stx))
        #'((~not (~delimit-cut pat)) () () ())]))))

(define-for-syntax (make-match-operator stxparse-op)
  (unquote-binding-prefix-operator
   (lambda () (order-quote logical_negation))
   `()
   'automatic
   (lambda (form stx)
     (syntax-parse (and (eq? (current-unquote-binding-kind) 'term1)
                        (normalize-id form))
       [#f #'#f]
       [(pat idrs sidrs vars)
        (with-syntax ([stxparse-op stxparse-op])
          #`(#,(if (is-sequence-pattern? #'pat)
                   #'(~seq (stxparse-op pat))
                   #'(stxparse-op pat))
             idrs sidrs vars))]))))

(define-unquote-binding-syntax delimit
  (make-match-operator #'~delimit-cut))

(define-unquote-binding-syntax commit
  (make-match-operator #'~commit))

(define-unquote-binding-syntax #%literal
  (unquote-binding-transformer
   (lambda (stxes)
     (syntax-parse stxes
       [(_ x . _)
        (raise-syntax-error #f
                            (format "misplaced ~a within a syntax binding"
                                    (if (keyword? (syntax-e #'x))
                                        "keyword"
                                        "literal"))
                            #'x)]))))

(define-unquote-binding-syntax #%block
  (unquote-binding-transformer
   (lambda (stxes)
     (syntax-parse stxes
       [(_ b)
        (raise-syntax-error #f
                            "not allowed as a syntax binding by itself"
                            #'b)]))))

(define-for-syntax (make-option-sequence for-kind as-kind fail-contexts)
  (unquote-binding-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (group)
       [(form-id (~and all-alts (_::alts blk ...)))
        (cond
          [(eq? (current-unquote-binding-kind) for-kind)
           (define option-tags (generate-temporaries (attribute blk)))
           (define options-id (car (generate-temporaries '(options))))
           (define-values (rsc descs defaultss)
             (parse-anonymous-syntax-class (syntax-e #'form-id)
                                           stx
                                           (if (eq? as-kind #'group)
                                               'group
                                               'sequence)
                                           #:kind-kw as-kind
                                           #:for-option? #t
                                           #:commonize-fields? #t
                                           #:option-tags option-tags
                                           options-id
                                           #'(all-alts)))
           (define duplicate-messages
             (for/list ([desc (in-list descs)])
               (if desc
                   (format "multiple ~a not allowed" desc)
                   "mulitple uses of option not allowed")))
           (syntax-parse (build-syntax-class-pattern stx
                                                     rsc
                                                     #'#f
                                                     #'form-id
                                                     #f
                                                     options-id
                                                     #t)
             [(pat idrs sidrs vars)
              (define default-name-map ; sym -> expression
                (for*/hash  ([defaults (in-list defaultss)]
                             [default (in-list defaults)])
                  (values (syntax-e (car default)) (caddr default))))
              (define default-map
                (for/list ([pv (in-list (syntax->list #'vars))]
                           #:do [(define var (syntax-list->pattern-variable pv))
                                 (define e (hash-ref default-name-map (pattern-variable-sym var) #f))]
                           #:when e)
                  (list (pattern-variable-val-id var)
                        (pattern-variable-sym var)
                        e)))
              (with-syntax ([idrs (for/list ([idr (in-list (syntax->list #'idrs))]
                                             ;; commonized fields will always be `pack-nothing*`
                                             ;; and use `attribute` instead of `syntax`, so prune
                                             ;; any useless binding like `wildcard`
                                             #:unless (syntax-parse idr
                                                        #:literals (syntax)
                                                        #:datum-literals (maybe-syntax-wrap)
                                                        [[lhs (pack* (syntax _) _)] #t]
                                                        [[lhs (maybe-syntax-wrap (pack* (syntax _) _) . _)] #t]
                                                        [_ #f]))
                                    (define (get-default lhs depth)
                                      (for/or ([p (in-list default-map)])
                                        (and (free-identifier=? (car p) lhs)
                                             #`(lambda () (check-depth-of-default 'form-id '#,(cadr p) #,(caddr p) #,depth)))))
                                    (syntax-parse idr
                                      #:datum-literals (maybe-syntax-wrap pack-nothing*)
                                      [[lhs (pack-nothing* attr depth)]
                                       #`[lhs ((pack-success* #,(get-default #'lhs #'depth) depth) attr depth)]]
                                      [[lhs ((~and msw maybe-syntax-wrap) (pack-nothing* attr depth) . tail)]
                                       #`[lhs (msw ((pack-success* #,(get-default #'lhs #'depth) depth) attr depth) . tail)]]))])
                (with-syntax ([pat (with-syntax ([(option-tag ...) option-tags]
                                                 [(duplicate-message ...) duplicate-messages])
                                     #'(~delimit-cut
                                        (~and (~seq pat (... ...))
                                              ~!
                                              (~fail #:when (check-duplicate-matches (attribute option-tag))
                                                     duplicate-message)
                                              ...)))])
                  (values #'(pat idrs sidrs vars) #'())))])]
          [else
           (when (memq (current-unquote-binding-kind) fail-contexts)
             (raise-syntax-error #f
                                 "option sequence incompatible with this context"
                                 stx))
           (values #'#f #'())])]))))

(define-unquote-binding-syntax group_option_sequence
  (make-option-sequence 'group1 '#:group '(term1 grouplet)))

(define-unquote-binding-syntax term_option_sequence
  (make-option-sequence 'term1 '#:sequence '()))

(define (check-duplicate-matches matches)
  (let loop ([matches matches] [found #f])
    (cond
      [(null? matches) #f]
      [(car matches) (or found
                         (loop (cdr matches) (car matches)))]
      [else (loop (cdr matches) found)])))

(define (check-depth-of-default who var-name top-val top-depth)
  (let loop ([val top-val] [depth top-depth])
    (cond
      [(zero? depth)
       val]
      [else
       (unless (treelist? val)
         (raise-arguments-error who
                                "default value does not match expected depth"
                                "field" (unquoted-printing-string (symbol->string var-name))
                                "expected depth" top-depth
                                "value" top-val))
       (for/list ([v (in-treelist val)])
         (loop v (sub1 depth)))])))
