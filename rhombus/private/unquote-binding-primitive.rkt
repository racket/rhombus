#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/hier-name-parse
                     enforest/name-parse
                     enforest/syntax-local
                     "name-path-op.rkt"
                     "attribute-name.rkt")
         syntax/parse/pre
         "pack.rkt"
         "syntax-class-primitive.rkt"
         (only-in "expression.rkt"
                  in-expression-space)
         (submod "syntax-class-primitive.rkt" for-quasiquote)
         (only-in "annotation.rkt"
                  ::)
         (only-in "repetition.rkt"
                  in-repetition-space)
         "pattern-variable.rkt"
         "unquote-binding.rkt"
         "unquote-binding-identifier.rkt"
         "name-root-space.rkt"
         "name-root-ref.rkt"
         "space.rkt"
         "parens.rkt"
         (submod "function-parse.rkt" for-call)
         (only-in "import.rkt" as open)
         (submod  "import.rkt" for-meta)
         (submod "syntax-class.rkt" for-pattern-clause))

(provide (for-space rhombus/unquote_bind
                    #%parens
                    ::
                    pattern
                    &&
                    \|\|
                    #%literal
                    #%block))

;; `#%quotes` is implemented in "quasiquote.rkt" because it recurs as
;; nested quasiquote matching, `_` is in "quasiquote.rkt" so it can be
;; matched literally, and plain identifiers are implemented in
;; "unquote-binding-identifier.rkt"

(define-unquote-binding-syntax #%parens
  (unquote-binding-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ (parens g::unquote-binding) . tail)
        (values #'g.parsed
                #'tail)]
       [(_ (parens) . tail)
        ;; empty parentheses match an empty group, which
        ;; is only useful for matching an empty group tail
        (case (current-unquote-binding-kind)
          [(group)
           (values #`((group) () () ())
                   #'tail)]
          [(term)
           (raise-syntax-error #f "incompatible with this context" #'self)]
          [else (values #'#f #'())])]))))

(begin-for-syntax
  (define-splicing-syntax-class :syntax-class-args
    (pattern (~seq (~and args (_::parens . _))))
    (pattern (~seq)
             #:attr args #'#f))
  (define (parse-syntax-class-args stx-class rator-in arity class-args)
    (cond
      [(not arity)
       (when (syntax-e class-args)
         (raise-syntax-error #f
                             "syntax class does not expect arguments"
                             stx-class))
       rator-in]
      [(not (syntax-e class-args))
       (raise-syntax-error #f
                           "syntax class expects arguments"
                           stx-class)]
      [else
       (define-values (call empty-tail)
         (parse-function-call rator-in '() #`(#,stx-class #,class-args)
                              #:static? #t
                              #:rator-stx stx-class
                              #:rator-kind '|syntax class|
                              #:rator-arity arity))
       call])))

(define-unquote-binding-syntax ::
  (unquote-binding-infix-operator
   (in-unquote-binding-space #'::)
   null
   'macro
   (lambda (form1 stx)
     (unless (or (identifier? form1)
                 (syntax-parse form1
                   [(underscore () () ())
                    (free-identifier=? #'underscore #'_)]))
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
           (for/fold ([open #hasheq()]) ([g (in-list (syntax->list #'(g ...)))])
             (syntax-parse g
               #:datum-literals (group)
               [(group id:identifier)
                (free-identifier=? (in-import-space #'id) (in-import-space #'open))
                (when (syntax? open)
                  (raise-syntax-error #f "redundant opening clause" stx #'id))
                (when (and (hash? open) ((hash-count open) . > . 0))
                  (raise-syntax-error #f "opening clause not allowed after specific fields" stx #'id))
                #'id]
               [(group field:identifier as:identifier bind:identifier)
                (free-identifier=? (in-import-space #'id) (in-import-space #'as))
                (when (syntax? open)
                  (raise-syntax-error #f "specific field not allowed after opening clause" stx #'field))
                (define key (syntax-e #'field))
                (hash-set open key (cons (cons #'field #'bind) (hash-ref open key null)))]
               [(group field:identifier ...)
                (when (syntax? open)
                  (raise-syntax-error #f "specific field not allowed after opening clause" stx
                                      (car (syntax-e #'(field ...)))))
                (for/fold ([open open]) ([field (in-list (syntax->list #'(field ...)))])
                  (define key (syntax-e field))
                  (hash-set open key (cons (cons field field) (hash-ref open key null))))]
               [_
                (raise-syntax-error #f "bad exposure clause" stx g)]))
           #'())]
         [_ (values #f tail)]))
     (define match-id (car (generate-temporaries (list form1))))
     (syntax-parse stx
       #:datum-literals (group)
       [(_ (~and sc (_::parens (group . rest))) . tail)
        #:with (~var sc-hier (:hier-name-seq in-name-root-space in-expression-space name-path-op name-root-ref)) #'rest
        #:do [(define parser (syntax-local-value* #'sc-hier.name syntax-class-parser-ref))]
        #:when parser
        (define-values (open-attributes end-tail) (parse-open-block stx #'tail))
        (define rsc ((syntax-class-parser-proc parser) (or (syntax-property #'sc-hier.name 'rhombus-dotted-name)
                                                           (syntax-e #'sc-hier.name))
                                                       #'sc
                                                       (current-unquote-binding-kind)
                                                       match-id
                                                       #'sc-hier.tail))
        (if rsc
            (values (build-syntax-class-pattern #'sc rsc #'#f open-attributes form1 match-id)
                    end-tail)
            ;; shortcut for kind mismatch
            (values #'#f #'()))]
       [(_ . rest)
        #:with (~var stx-class-hier (:hier-name-seq in-name-root-space in-syntax-class-space name-path-op name-root-ref)) #'rest
        (syntax-parse #'stx-class-hier.tail
          #:datum-literals ()
          [(args::syntax-class-args . args-tail)
           (define-values (open-attributes tail) (parse-open-block stx #'args-tail))
           (values (build-syntax-class-pattern #'stx-class-hier.name
                                               (lookup-syntax-class #'stx-class-hier.name)
                                               #'args.args
                                               open-attributes
                                               form1
                                               match-id)
                   tail)])]))
   'none))

(define-unquote-binding-syntax pattern
  (unquote-binding-transformer
   (lambda (stx)
     (define inline-id #f)
     (define rsc (parse-pattern-clause stx (current-unquote-binding-kind)))
     (values (if rsc
                 (build-syntax-class-pattern stx
                                             rsc
                                             #'#f
                                             (syntax-parse stx [(form-id . _) #'form-id])
                                             #f
                                             inline-id)
                 #'#f)
             #'()))))

(begin-for-syntax
  (struct open-attrib (sym bind-id var)))

;; used for `::` and for `pattern`, returns a parsed binding form that takes advantage
;; of a syntax class --- possibly an inlined syntax class and/or one with exposed fields
(define-for-syntax (build-syntax-class-pattern stx-class rsc class-args open-attributes-spec
                                               form1 match-id)
  (with-syntax ([id (if (identifier? form1) form1 #'wildcard)])
    (define (compat pack* unpack*)
      (define sc (rhombus-syntax-class-class rsc))
      (define sc-call (parse-syntax-class-args stx-class
                                               sc
                                               (rhombus-syntax-class-arity rsc)
                                               class-args))
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
                                     [temp-attr (in-list (generate-temporaries (map pattern-variable-sym vars)))]
                                     [bind-counter (in-naturals)])
          (define name (pattern-variable-sym var))
          (define id (pattern-variable-id var))
          (define depth (pattern-variable-depth var))
          (define unpack*-id (pattern-variable-unpack*-id var))
          (define id-with-attr (compose-attr-name match-id name id bind-counter))
          (values #`[#,temp-attr #,(cond
                                     [(eq? depth 'tail)
                                      ;; bridge from a primitive syntax class, where we don't want to convert to
                                      ;; a list and then convert back when the tail is used as a new tail in a
                                      ;; template
                                      #`(pack-tail* (syntax #,id-with-attr) 0)]
                                     [(not (or (free-identifier=? unpack*-id #'unpack-tail-list*)
                                               (free-identifier=? unpack*-id #'unpack-multi-tail-list*)
                                               (free-identifier=? unpack*-id #'unpack-parsed*)))
                                      ;; assume depth-compatible value checked on binding side, and
                                      ;; let `attribute` unpack syntax repetitions
                                      #`(pack-nothing* (attribute #,id-with-attr) #,depth)]
                                     [else
                                      #`(#,(cond
                                             [(free-identifier=? unpack*-id #'unpack-tail-list*)
                                              #'pack-tail-list*]
                                             [(free-identifier=? unpack*-id #'unpack-multi-tail-list*)
                                              #'pack-multi-tail-list*]
                                             [(free-identifier=? unpack*-id #'unpack-parsed*)
                                              #'pack-parsed*]
                                             [else #'pack-term*])
                                         (syntax #,(let loop ([t id-with-attr] [depth depth])
                                                     (if (zero? depth)
                                                         t
                                                         (loop #`(#,t #,(quote-syntax ...)) (sub1 depth)))))
                                         #,depth)])]
                  (pattern-variable name id temp-attr (if (eq? depth 'tail) 1 depth) unpack*-id))))

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
      (define instance-id (or match-id (car (generate-temporaries '(inline)))))
      (define swap-to-root-var
        (for/first ([var (in-list attribute-vars)]
                    #:when (eq? (pattern-variable-sym var) swap-to-root))
          var))
      #`(#,(if sc
               (if (identifier? sc)
                   #`(~var #,instance-id #,sc-call)
                   #`(~and #,(if dotted-bind?
                                 #`(~seq #,instance-id (... ...))
                                 instance-id)
                           #,sc)) ; inline syntax class
                instance-id)
         #,(cons #`[#,temp-id (#,pack* (syntax #,(if dotted-bind?
                                                     #`(#,instance-id (... ...))
                                                     instance-id))
                               #,pack-depth)]
                 attribute-bindings)
         #,(append
            (if (identifier? form1)
                (list (make-pattern-variable-bind #'id
                                                  (if swap-to-root-var
                                                      (pattern-variable-val-id swap-to-root-var)
                                                      temp-id)
                                                  (if swap-to-root-var
                                                      (pattern-variable-unpack*-id swap-to-root-var)
                                                      unpack*)
                                                  (if swap-to-root-var
                                                      (pattern-variable-depth swap-to-root-var)
                                                      pack-depth)
                                                  (append
                                                   (if swap-root-to
                                                       (list
                                                        (pattern-variable->list
                                                         (pattern-variable swap-root-to #f temp-id pack-depth unpack*)))
                                                       null)
                                                   (for/list ([var (in-list attribute-vars)]
                                                              #:unless (eq? swap-to-root (pattern-variable-sym var)))
                                                     (pattern-variable->list var #:keep-id? #f)))))
                null)
            (if (not open-attributes)
                null
                (for/list ([oa (in-list open-attributes)]
                           #:unless (eq? 'root (open-attrib-var oa)))
                  (define bind-id (open-attrib-bind-id oa))
                  (define var (open-attrib-var oa))
                  (make-pattern-variable-bind bind-id (pattern-variable-val-id var) (pattern-variable-unpack*-id var)
                                              (pattern-variable-depth var) null))))
         #,(append
            (if (identifier? form1)
                (list
                 (if swap-to-root
                     (pattern-variable->list (struct-copy pattern-variable swap-to-root-var
                                                          [sym swap-to-root]
                                                          [id #f]))
                     (list #'id #'id temp-id pack-depth unpack*)))
                null)
            (if (not open-attributes)
                null
                (for/list ([oa (in-list open-attributes)])
                  (define var (open-attrib-var oa))
                  (if (eq? var 'root)
                      (pattern-variable->list
                       (pattern-variable (open-attrib-sym oa) #f temp-id pack-depth unpack*))
                      (pattern-variable->list (struct-copy pattern-variable var
                                                           [sym (open-attrib-sym oa)]))))))))
    (define (incompat)
      (raise-syntax-error #f
                          "syntax class incompatible with this context"
                          stx-class))
    (define (retry) #'#f)
    (define kind (current-unquote-binding-kind))
    (cond
      [(eq? (rhombus-syntax-class-kind rsc) 'term)
       (cond
         [(not (eq? kind 'term)) (retry)]
         [(rhombus-syntax-class-splicing? rsc)
          (compat #'pack-tail* #'unpack-group*)]
         [else (compat #'pack-term* #'unpack-term*)])]
      [(eq? (rhombus-syntax-class-kind rsc) 'group)
       (cond
         [(eq? kind 'term) (incompat)]
         [(not (eq? kind 'group)) (retry)]
         [else (compat #'pack-group* #'unpack-group*)])]
      [(eq? (rhombus-syntax-class-kind rsc) 'multi)
       (cond
         [(or (eq? kind 'multi) (eq? kind 'block))
          (compat #'pack-tagged-multi* #'unpack-multi-as-term*)]
         [else (incompat)])]
      [(eq? (rhombus-syntax-class-kind rsc) 'block)
       (cond
         [(eq? kind 'block)
          (compat #'pack-block* #'unpack-multi-as-term*)]
         [else (incompat)])]
      [else
       (error "unrecognized kind" kind)])))

(define-for-syntax (normalize-id form)
  (if (identifier? form)
      (identifier-as-unquote-binding form (current-unquote-binding-kind))
      form))

(define-for-syntax (norm-seq pat like-pat)
  (syntax-parse pat
    [((~datum ~seq) . _) pat]
    [_ (syntax-parse like-pat
         [((~datum ~seq) . _) #`(~seq #,pat)]
         [_ pat])]))

(define-unquote-binding-syntax &&
  (unquote-binding-infix-operator
   (in-unquote-binding-space #'&&)
   null
   'automatic
   (lambda (form1 form2 stx)
     (syntax-parse (normalize-id form1)
       [#f #'#f]
       [(pat1 (idr1 ...) (sidr1 ...) (var1 ...))
        (syntax-parse (normalize-id form2)
          [#f #'#f]
          [(pat2 idrs2 sidrs2 vars2)
           #`((~and #,(norm-seq #'pat1 #'pat2) #,(norm-seq #'pat2 #'pat1))
              (idr1 ... . idrs2)
              (sidr1 ... . sidrs2)
              (var1 ... . vars2))])]))
   'left))

(define-unquote-binding-syntax \|\|
  (unquote-binding-infix-operator
   (in-unquote-binding-space #'\|\|)
   null
   'automatic
   (lambda (form1 form2 stx)
     (syntax-parse (normalize-id form1)
       [#f #'#f]
       [(pat1 idrs1 sidrs1 vars1)
        (syntax-parse (normalize-id form2)
          [#f #'#f]
          [(pat2 idrs2 sidrs2 vars2)
           #`((~or #,(norm-seq #'pat1 #'pat2)
                   #,(norm-seq #'pat2 #'pat1))
              ()
              ()
              ())])]))
   'left))

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

