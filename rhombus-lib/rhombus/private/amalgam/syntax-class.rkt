#lang racket/base
(require (for-syntax racket/base
                     racket/keyword
                     syntax/parse/pre
                     enforest/name-parse
                     "attribute-name.rkt")
         syntax/parse/pre
         "provide.rkt"
         (only-in "binding.rkt" in-binding-space)
         (submod "quasiquote.rkt" convert)
         (submod "quasiquote.rkt" shape-dispatch)
         (submod "syntax-class-primitive.rkt" for-quasiquote)
         (submod "syntax-class-primitive.rkt" for-syntax-class-syntax)
         "syntax-class-clause.rkt"
         (submod "syntax-class-clause-primitive.rkt" for-class)
         "syntax-class-arg.rkt"
         "pattern-clause.rkt"
         "pattern-variable.rkt"
         "definition.rkt"
         "name-root.rkt"
         "dotted-sequence-parse.rkt"
         "parse.rkt"
         "pack.rkt"
         "parens.rkt"
         "forwarding-sequence.rkt"
         "sequence-pattern.rkt"
         (only-in "static-info.rkt" static-infos-or)
         (rename-in "ellipsis.rkt"
                    [... rhombus...])
         "syntax-wrap.rkt")

(provide (for-spaces (rhombus/defn
                      rhombus/namespace)
                     syntax_class))

(module+ for-anonymous-syntax-class
  (provide (for-syntax parse-anonymous-syntax-class)))

(define-name-root syntax_class
  #:fields
  (together))

(define-for-syntax (parse-syntax-class stx #:for-together? [for-together? #f])
  (syntax-parse stx
    [(form-id name-seq::dotted-identifier-sequence args::class-args
              (_::alts alt ...))
     #:with full-name::dotted-identifier #'name-seq
     #:with class-name #'full-name.name
     #:with name-extends #'full-name.extends
     (build-syntax-class stx (syntax->list #'(alt ...)) null
                         #:define-class? #t
                         #:class/inline-name #'class-name
                         #:name-extends #'name-extends
                         #:class-formals #'args.formals
                         #:class-arity #'args.arity
                         #:for-together? for-together?)]
    [(form-id name-seq::dotted-identifier-sequence args::class-args
              ;; syntax-class clauses are implemented in "syntax-class-clause-primitive.rkt"
              (_::block clause::syntax-class-clause ...)
              ;; patterns can be spliced by one of the macros
              (~optional (_::alts alt ...)))
     #:with full-name::dotted-identifier #'name-seq
     #:with class-name #'full-name.name
     #:with name-extends #'full-name.extends
     (define parsed-clauses (syntax->list #'(clause.parsed ...)))
     (define-values (pattern-alts kind-kw class-desc fields-ht swap-root opaque?)
       (extract-clauses stx
                        parsed-clauses
                        (and (attribute alt)
                             (syntax->list #'(alt ...)))))
     (build-syntax-class stx pattern-alts parsed-clauses
                         #:define-class? #t
                         #:class/inline-name #'class-name
                         #:name-extends #'name-extends
                         #:class-formals #'args.formals
                         #:class-arity #'args.arity
                         #:kind-kw kind-kw
                         #:description-expr class-desc
                         #:fields fields-ht
                         #:swap-root swap-root
                         #:opaque? opaque?
                         #:for-together? for-together?)]))

(begin-for-syntax
  (struct definition+syntax-class-parser (def pars)
    #:property prop:definition-transformer (lambda (self) (definition+syntax-class-parser-def self))
    #:property prop:syntax-class-parser (lambda (self) (definition+syntax-class-parser-pars self))))

(define-defn-syntax syntax_class
  (definition+syntax-class-parser
    (definition-transformer
      (lambda (stx name-prefix)
        (parse-syntax-class stx)))
    (syntax-class-parser
     (lambda (who stx expected-kind name tail)
       (parse-anonymous-syntax-class who stx expected-kind name tail)))))

(begin-for-syntax
  (define-syntax-class :syntax-class-id
    #:attributes (name)
    #:description "the literal `syntax_class`"
    #:opaque
    (pattern ::name
             #:when (free-identifier=? (in-defn-space #'name)
                                       (defn-quote syntax_class)))))

(define-defn-syntax together
  (definition-transformer
    (lambda (stx name-prefix)
      (syntax-parse stx
        #:datum-literals (group)
        [(_ (_::block (group op::syntax-class-id . rest) ...))
         (define decls
           (for/list ([g (in-list (syntax->list #'((op . rest) ...)))])
             (parse-syntax-class g #:for-together? #t)))
         (append
          (map car decls)
          (map (lambda (stx) #`(finish-together-syntax-class #,stx)) (map cadr decls)))]))))

;; returns a `rhombus-syntax-class`
(define-for-syntax (parse-anonymous-syntax-class who orig-stx expected-kind name tail
                                                 #:kind-kw [kind-kw #f]
                                                 #:commonize-fields? [commonize-fields? #f]
                                                 #:for-option? [for-option? #f]
                                                 #:option-tags [option-tags #f])
  (syntax-parse tail
    [((_::alts alt ...))
     (build-syntax-class orig-stx (syntax->list #'(alt ...)) null
                         #:kind-kw kind-kw
                         #:commonize-fields? commonize-fields?
                         #:for-option? for-option?
                         #:option-tags option-tags
                         #:class/inline-name name
                         #:expected-kind expected-kind)]
    [((_::block clause::syntax-class-clause ...)
      ;; patterns can be spliced by one of the macros
      (~optional (_::alts alt ...)))
     (define parsed-clauses (syntax->list #'(clause.parsed ...)))
     (define-values (pattern-alts kind-kw class-desc fields-ht swap-root opaque?)
       (extract-clauses orig-stx
                        parsed-clauses
                        (and (attribute alt)
                             (syntax->list #'(alt ...)))))
     (build-syntax-class orig-stx pattern-alts parsed-clauses
                         #:class/inline-name name
                         #:kind-kw kind-kw
                         #:description-expr class-desc
                         #:fields fields-ht
                         #:swap-root swap-root
                         #:opaque? opaque?
                         #:expected-kind expected-kind)]
    [_ (raise-syntax-error who "bad syntax" orig-stx)]))

;; returns a `rhombus-syntax-class` if `define-syntaxed-id` is #f, otherwise
;; returns a list of definitions
(define-for-syntax (build-syntax-class stx
                                       alts
                                       track-stxes
                                       #:define-class? [define-class? #f]
                                       #:class/inline-name [class/inline-name #f]
                                       #:name-extends [name-extends #f]
                                       #:class-formals [class-formals #'#f]
                                       #:class-arity [class-arity #'#f]
                                       #:kind-kw [kind-kw #f]
                                       #:description-expr [description-expr #f]
                                       #:fields [fields-ht #f]
                                       #:swap-root [swap-root #f]
                                       #:opaque? [opaque? #f]
                                       #:commonize-fields? [commonize-fields? #f]
                                       #:option-tags [option-tags #f]
                                       #:for-option? [for-option? #f]
                                       #:expected-kind [expected-kind #f]
                                       #:for-together? [for-together? #f])
  (define-values (kind splicing?)
    (let ([kind (cond
                  [kind-kw (string->symbol (keyword->immutable-string kind-kw))]
                  [else (infer-pattern-kind alts)])])
      (cond
        [(eq? kind 'sequence) (values 'term #t)]
        [else (values kind #f)])))
  (cond
    [(and (or (eq? expected-kind 'block)
              (eq? expected-kind 'multi)
              (eq? expected-kind 'group))
          (eq? kind 'term))
     ;; shortcut to avoid redundant parsing when it's not going to work out
     #f]
    [else
     (define-values (patterns attributess descs defaultss)
       (cond
         [for-together?
          ;; delay parsing attributes until we've declared Rhombus syntax classes; this
          ;; requires fully declared fields
          (define attrs
            (cond
              [(not fields-ht) null]
              [else (for/list ([(k df-stx) (in-hash fields-ht)])
                      (define df (syntax-e df-stx))
                      (pattern-variable k
                                        (declared-field-id df)
                                        (car (generate-temporaries (list (declared-field-id df))))
                                        (or (syntax-e (declared-field-depth df)) 0)
                                        (if (syntax-e (declared-field-unpack*-id df))
                                            (declared-field-unpack*-id df)
                                            (quote-syntax unpack-term*))
                                        #'()))]))
          (values #`((#:delayed-patterns #,stx #,alts #,fields-ht
                      #,(map (lambda (pv)
                               (list (pattern-variable-sym pv)
                                     (pattern-variable-val-id pv)))
                             attrs)
                      #,kind
                      #,splicing?))
                  (list attrs)
                  (list #f)
                  (list null))]
         [else
          (for/lists (patterns attributess descs defaultss) ([alt-stx (in-list alts)])
            (parse-pattern-cases stx alt-stx kind splicing?
                                 #:keep-attr-id? (not define-class?)
                                 #:for-option? for-option?))]))
     (check-defaults-names stx attributess defaultss)
     (define attributes (intersect-attributes stx attributess fields-ht swap-root commonize-fields?))
     (cond
       [(not define-class?)
        ;; return a `rhombus-syntax-class` directly
        (define rsc
          (rhombus-syntax-class kind
                                (syntax-class-body->inline patterns class/inline-name option-tags splicing?)
                                (datum->syntax #f (map pattern-variable->list attributes))
                                splicing?
                                (syntax->datum class-arity)
                                (and swap-root
                                     (cons (syntax-e (car swap-root)) (cdr swap-root)))
                                #f
                                (if (null? attributes)
                                    #f
                                    (gensym (if class/inline-name (syntax-e class/inline-name) 'sc-dot)))))
        (if for-option?
            (values rsc descs defaultss)
            rsc)]
       [else
        (define class-name class/inline-name)
        (define internal-class-name ((make-syntax-introducer) class-name))
        (define define-class (if splicing?
                                 #'define-splicing-syntax-class
                                 #'define-syntax-class))
        (define (track-all stx) (for/fold ([stx stx]) ([track-stx (in-list track-stxes)])
                                  (syntax-track-origin stx track-stx #'none)))
        ;; in `for-together?` mode, expects a list of 2 definitions:
        (list
         ;; return a list of definitions
         (track-all
          (build-syntax-definition/maybe-extension
           'rhombus/stxclass class-name name-extends
           #`(rhombus-syntax-class '#,kind
                                   (quote-syntax #,internal-class-name)
                                   (quote-syntax #,(for/list ([var (in-list attributes)])
                                                     (pattern-variable->list var #:keep-id? #f)))
                                   #,splicing?
                                   '#,class-arity
                                   '#,swap-root
                                   #f
                                   #,(if (null? attributes)
                                         #f
                                         #`(gensym '#,class-name)))))
         #`(#,define-class #,(if (syntax-e class-formals)
                                 #`(#,internal-class-name . #,class-formals)
                                 internal-class-name)
            #:description #,(or description-expr #f)
            #:disable-colon-notation
            #:attributes #,(for/list ([var (in-list attributes)])
                             #`[#,(pattern-variable-sym var) #,(pattern-variable-depth var)])
            #,@(if opaque? '(#:opaque) '())
            #,@patterns))])]))

(define-syntax (finish-together-syntax-class stx)
  (syntax-parse stx
    #:datum-literals (delayed-patterns)
    [(_ (form content ... (#:delayed-patterns orig-stx alts fields-ht key+tmp-ids kind splicing?)))
     (define-values (patterns attributess descs defaults)
       (for/lists (patterns attributess descs defaults) ([alt-stx (in-list (syntax->list #'alts))])
         (parse-pattern-cases stx alt-stx (syntax-e #'kind) (syntax-e #'splicing?)
                              #:tmp-ids (for/hasheq ([key+tmp-id-stx (in-list (syntax->list #'key+tmp-ids))])
                                          (define key+tmp-id (syntax->list key+tmp-id-stx))
                                          (values (syntax-e (car key+tmp-id)) (cadr key+tmp-id))))))
     ;; check against `fields-ht`:
     (intersect-attributes #'orig-stx attributess (syntax-e #'fields-ht) #f #f)
     ;; return `form` with parsed patterns in place:
     #`(form content ... #,@patterns)]))

;; ----------------------------------------

(define-for-syntax (infer-pattern-kind alts)
  (for/fold ([kind 'term]) ([alt (in-list alts)])
    (cond
      [(eq? kind 'multi) 'multi]
      [else
       (syntax-parse alt
         #:datum-literals (group)
         [(_::block (group pat . _) . _)
          (define-values (new-kind tail)
            (quoted-shape-dispatch #'(pattern pat)
                                   in-binding-space
                                   (lambda (e) 'term)
                                   (lambda (e)
                                     ;; It's possible that `e` is a multi pattern,
                                     ;; but we infer 'sequence for simplicity
                                     'sequence)
                                   (lambda (e)
                                     (syntax-parse e
                                       #:datum-literals (multi)
                                       [(multi)
                                        ;; Special case: empty as 'sequence, unless
                                        ;; forced to 'multi by other patterns
                                        'sequence]
                                       [_
                                        'multi]))
                                   (lambda (e) 'term)))
          (cond
            [(and (eq? kind 'sequence) (eq? new-kind 'term)) kind]
            [else new-kind])]
         [_ kind])])))

(begin-for-syntax
  (define-syntax-class :attribute-lhs
    #:datum-literals (group op)
    #:literals (rhombus...)
    (pattern id:identifier
             #:with depth #'0)
    (pattern (_::brackets (group a::attribute-lhs) (group (op rhombus...)))
             #:with id #'a.id
             #:with depth #`#,(+ 1 (syntax-e #'a.depth)))))

;; Returns two values:
;;   * a `pattern` form suitable to use in `define-syntax-class`
;;   * a list of `pattern-variable`s that represent fields (i.e., attributes)
;;     of the syntax class
;; The first result has a restricted form that is recognized by `syntax-class-body->inline`
(define-for-syntax (parse-pattern-cases orig-stx stx kind splicing?
                                        #:keep-attr-id? [keep-attr-id? #f]
                                        #:for-option? [for-option? #f]
                                        #:tmp-ids [tmp-id-ht #f])
  (define-values (pat body)
    (syntax-parse stx
      #:datum-literals (group)
      [(_::block (group (~and pat (_::quotes . _))))
       (values #'pat #'())]
      [(_::block (group (~and pat (_::quotes . _))
                        (_::block body ...)))
       (values #'pat #'(body ...))]))

  (define in-quotes
    (cond
      [(eq? kind 'multi)
       (syntax-parse pat
         [(_ g ...) #'(multi g ...)])]
      [(eq? kind 'block)
       (syntax-parse pat
         #:datum-literals (group)
         [(_ (~and g (group (_::block . _)))) #'g]
         [_ (raise-syntax-error #f
                                "not a block pattern"
                                orig-stx
                                pat)])]
      [else
       (syntax-parse pat
         [(_ g) #'g]
         [(_)
          (if (and (eq? kind 'term) splicing?)
              #'(group)
              (raise-syntax-error #f
                                  "no groups in pattern"
                                  orig-stx
                                  pat))]
         [_ (raise-syntax-error #f
                                "multiple groups in pattern"
                                orig-stx
                                pat)])]))

  (define-values (p idrs sidrs vars can-be-empty?)
    (convert-pattern #:splice? (not (eq? kind 'group))
                     #:splice-pattern (and (not (eq? kind 'group))
                                           (not splicing?)
                                           (lambda (ps)
                                             (cond
                                               [(eq? kind 'multi)
                                                #`(_ #,@ps)]
                                               [(and (pair? ps) (null? (cdr ps))
                                                     (not (is-sequence-pattern? (car ps))))
                                                (car ps)]
                                               [else
                                                (raise-syntax-error #f
                                                                    "not a single-term pattern"
                                                                    orig-stx
                                                                    pat)])))
                     in-quotes))

  (define (bindings->defns idrs sidrs)
    (with-syntax ([([val-id val-rhs . _] ...) idrs]
                  [([(stx-id ...) stx-rhs . _] ...) sidrs])
      #'[(define val-id val-rhs)
         ...
         (define-syntaxes (stx-id ...) stx-rhs)
         ...]))

  (define-values (pattern-body all-attrs desc defaults)
    (let loop ([body (syntax->list body)]
               [rev-do null]
               [rev-body (list (bindings->defns idrs sidrs) '#:do)]
               [rev-attrs (reverse vars)]
               [desc #f]
               [defaults '()])
      (define (accum-do [end? #f])
        (if (null? rev-do)
            rev-body
            (list* #`[(rhombus-blocklet-forwarding-sequence
                       #,end? ; whether `let` is allowed
                       (rhombus-body-sequence
                        #,@(reverse rev-do)))]
                   '#:do
                   rev-body)))
      (cond
        [(null? body) (values (reverse (accum-do #t))
                              (reverse rev-attrs)
                              desc
                              (reverse defaults))]
        [else
         (define g (car body))
         (cond
           [(pattern-clause? g)
            (syntax-parse g
              [c::pattern-clause
               (syntax-parse #'c.parsed
                 [(#:splice g ...)
                  (loop (append (syntax->list #'(g ...))
                                (cdr body))
                        rev-do
                        rev-body
                        rev-attrs
                        desc
                        defaults)]
                 [(#:field id depth rhs)
                  #:with (tmp-id) (or (and tmp-id-ht
                                           (let ([id (hash-ref tmp-id-ht (syntax-e #'id) #f)])
                                             (and id (list id))))
                                      (generate-temporaries #'(id)))
                  #:with (pat-ids pat-rhs . pat-statinfos) (make-pattern-variable-bind #'id #'tmp-id (quote-syntax unpack-element*)
                                                                                       (syntax-e #'depth))
                  (loop (cdr body)
                        null
                        (list* #'[(define tmp-id rhs)
                                  (define-syntaxes pat-ids pat-rhs)] '#:do
                               (accum-do))
                        (cons (pattern-variable (syntax-e #'id) #'id #'tmp-id (syntax-e #'depth) (quote-syntax unpack-element*) #'pat-statinfos)
                              rev-attrs)
                        desc
                        defaults)]
                 [(#:also (_ pat-g ...) rhs)
                  (define-values (p idrs sidrs vars can-be-empty?)
                    (convert-pattern #'(multi pat-g ...)))
                  (loop (cdr body)
                        null
                        (list* (bindings->defns idrs sidrs) '#:do
                               #' (repack-as-multi rhs) p '#:with
                               (accum-do))
                        (append (reverse vars)
                                rev-attrs)
                        desc
                        defaults)]
                 [(#:when rhs)
                  (loop (cdr body)
                        null
                        (list* #'rhs '#:when
                               (accum-do))
                        rev-attrs
                        desc
                        defaults)]
                 [(#:default id depth rhs clause-stx)
                  (unless for-option?
                    (raise-syntax-error #f
                                        "default clause can be used only for option sequences"
                                        orig-stx
                                        #'clause-stx))
                  (loop (cdr body)
                        null
                        (list* #'rhs '#:when
                               (accum-do))
                        rev-attrs
                        desc
                        (cons (list #'id (syntax-e #'depth) #'rhs)
                              defaults))]
                 [(#:description str clause-stx)
                  (unless for-option?
                    (raise-syntax-error #f
                                        "description clause can be used only for option sequences"
                                        orig-stx
                                        #'clause-stx))
                  (when desc
                    (raise-syntax-error #f
                                        "multiple description clauses not allowed"
                                        orig-stx
                                        #'clause-stx))
                  (loop (cdr body)
                        rev-do
                        rev-body
                        rev-attrs
                        (syntax-e #'str)
                        defaults)])])]
           [else
            (loop (cdr body) (cons g rev-do) rev-body rev-attrs desc defaults)])])))

  (with-syntax ([((attr ...) ...)
                 (map (lambda (var)
                        #`(#:attr
                           [#,(if keep-attr-id?
                                  (or (pattern-variable-id var)
                                      ;; fallback for `open`ed syntax classes
                                      (datum->syntax (inline-form-id)
                                                     (pattern-variable-sym var)))
                                  (pattern-variable-sym var))
                            #,(pattern-variable-depth var)]
                           (#,(keep-syntax-wrap (pattern-variable-unpack* var))
                            (quote-syntax dots)
                            #,(pattern-variable-val-id var)
                            #,(pattern-variable-depth var))))
                      all-attrs)]
                [(body-form ...) pattern-body])
    (values #`(pattern #,p
                       #,@pattern-body
                       attr ... ...)
            all-attrs
            desc
            defaults)))

;; converts a `pattern` clause for `syntax-case` into a pattern suitable
;; for directly inlinding into a larger pattern
(define-for-syntax (syntax-class-body->inline patterns inline-name option-tags splicing?)
  (define (convert-body body)
    (if (null? body)
        '()
        (case (syntax-e (car body))
          [(#:do)
           (cons #`(~do . #,(cadr body))
                 (convert-body (cddr body)))]
          [(#:with)
           (cons #`(~post (~parse #,(cadr body) #,(caddr body)))
                 (convert-body (cdddr body)))]
          [(#:when)
           (cons #`(~post (~fail #:unless #,(cadr body)))
                 (convert-body (cddr body)))]
          [(#:attr)
           (with-syntax ([[name depth] (cadr body)])
             (define id.name (compose-attr-name inline-name (syntax-e #'name) #'name))
             (cons #`(~bind ([#,id.name depth] #,(caddr body)))
                   (convert-body (cdddr body))))]
          ;; should not reach
          [else (raise-syntax-error #f "unhandled pattern body" patterns body)])))
  #`(~or* #,@(for/list ([pattern (in-list patterns)]
                        [option-tag (in-list (or option-tags
                                                 (map (lambda (p) #f) patterns)))])
               (syntax-parse pattern
                 [(_ pat body ...)
                  #`(~and pat
                          #,@(if option-tag
                                 (list (if splicing?
                                           #`(~seq #,option-tag (... ...))
                                           option-tag))
                                 null)
                          #,@(convert-body (syntax->list #'(body ...))))]))))

;; ----------------------------------------

(define-for-syntax (check-defaults-names stx attributess defaultss)
  (for ([defaults (in-list defaultss)]
        [attributes (in-list attributess)])
    (for/fold ([seen #hasheq()]) ([default (in-list defaults)])
      (define sym (syntax-e (car default)))
      (when (hash-ref seen sym #f)
        (raise-syntax-error #f
                            "second default for pattern variable within an option"
                            stx
                            (car default)))
      (define var (for/or ([var (in-list attributes)])
                    (and (eq? (pattern-variable-sym var) sym)
                         var)))
      (unless var
        (raise-syntax-error #f
                            "not a pattern variable in the option"
                            stx
                            (car default)))
      (unless (= (pattern-variable-depth var) (cadr default))
        (raise-syntax-error #f
                            "depth for default does not match pattern variable depth"
                            stx
                            (car default)))
      (hash-set seen sym #t))))

(define-for-syntax (intersect-attributes stx attributess fields-ht swap-root commonize-fields?)
  (cond
    [(and (null? attributess) (not fields-ht) (not swap-root)) '()]
    [(and (null? (cdr attributess)) (not fields-ht) (not swap-root) (not commonize-fields?)) (car attributess)]
    [else
     ;; start with initial set
     (define ht0
       (for/hasheq ([var (in-list (car attributess))])
         (values (pattern-variable-sym var) var)))
     ;; intersect by pruning set
     (define ht1
       (for/fold ([ht0 ht0]) ([attributes (in-list (cdr attributess))])
         (for/fold ([ht #hasheq()]) ([var (in-list attributes)])
           (define prev-var (hash-ref ht0 (pattern-variable-sym var) #f))
           (if prev-var
               (hash-set ht
                         (pattern-variable-sym var)
                         (intersect-var stx var prev-var))
               ht))))
     ;; if commonizing, then add all fields back, intersecting each as generic
     (define ht
       (if commonize-fields?
           (for/fold ([ht1 ht1]) ([attributes (in-list attributess)])
             (for/fold ([ht1 ht1]) ([var (in-list attributes)])
               (define prev-var (hash-ref ht1 (pattern-variable-sym var) #f))
               (if (and prev-var (not (= 1 (length attributess))))
                   (raise-syntax-error #f
                                       "field appears in multiple option cases"
                                       stx
                                       (pattern-variable-id var))
                   (hash-set ht1 (pattern-variable-sym var)
                             (struct-copy pattern-variable var
                                          [unpack* #'unpack-element*])))))
           ht1))
     ;; filter by declared fields
     (define filtered-ht
       (if fields-ht
           (for/hasheq ([(k df-stx) (in-hash fields-ht)])
             (define df (syntax-e df-stx))
             (define var (hash-ref ht k #f))
             (unless var
               (raise-syntax-error #f
                                   "field not available from all pattern cases"
                                   stx
                                   (declared-field-id (syntax-e (hash-ref fields-ht k)))))
             (unless (or (not (syntax-e (declared-field-depth df)))
                         (= (syntax-e (declared-field-depth df))
                            (pattern-variable-depth var)))
               (raise-syntax-error #f
                                   "field implementation does not match declared depth"
                                   stx
                                   (declared-field-id df)))
             (unless (or (not (syntax-e (declared-field-unpack*-id df)))
                         (free-identifier=? (declared-field-unpack*-id df)
                                            (pattern-variable-unpack* var)))
               (raise-syntax-error #f
                                   "field implementation does not match declared kind"
                                   stx
                                   (declared-field-id df)))
             (values k var))
           ht))
     ;; check swap root
     (when swap-root
       (define var (hash-ref filtered-ht (syntax-e (car swap-root)) #f))
       (unless var
         (raise-syntax-error #f
                             "field to swap as root not found"
                             stx
                             (car swap-root)))
       (when (hash-ref filtered-ht (syntax-e (cdr swap-root)) #f)
         (raise-syntax-error #f
                             "field for root already exists"
                             stx
                             (cdr swap-root)))
       (unless (= 0 (pattern-variable-depth var))
         (raise-syntax-error #f
                             "field for root cannot be a repetition"
                             stx
                             (car swap-root))))
     ;; convert back to list
     (hash-values filtered-ht #t)]))

(define-for-syntax (intersect-var stx a b)
  (unless (eqv? (pattern-variable-depth a) (pattern-variable-depth b))
    (raise-syntax-error #f
                        "field with different repetition depths in different cases"
                        stx
                        (pattern-variable-sym a)))
  (struct-copy pattern-variable a
               [unpack*
                ;; keeping the same unpack, if possible, enables optimizations for
                ;; tail repetitions; otherwise, the term is sufficiently normalized
                ;; by matching that we can just use `unpack-element*`
                (let ()
                  (define (same-unpack? a b)
                    (cond
                      [(and (identifier? a) (identifier? b)) (free-identifier=? a b)]
                      [(or (identifier? a) (identifier? b)) #f]
                      [else
                       (define as (syntax->list a))
                       (define bs (syntax->list a))
                       (cond
                         [(and as bs (= (length as) (length bs))) (andmap same-unpack? as bs)]
                         [else (eq? (syntax-e a) (syntax-e b))])]))
                  (if (same-unpack? (pattern-variable-unpack* a) (pattern-variable-unpack* b))
                      (pattern-variable-unpack* a)
                      #'unpack-element*))]
               [statinfos (static-infos-or (normalize-pvar-statinfos (pattern-variable-statinfos a))
                                              (normalize-pvar-statinfos (pattern-variable-statinfos b)))]))
