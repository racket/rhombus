#lang racket/base
(require (for-syntax racket/base
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
         "parse.rkt"
         "pack.rkt"
         "parens.rkt"
         (rename-in "ellipsis.rkt"
                    [... rhombus...]))

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
    [(form-id class-name args::class-args
              (_::alts alt ...))
     (build-syntax-class stx (syntax->list #'(alt ...)) null
                         #:define-class-id #'define-syntax
                         #:class/inline-name #'class-name
                         #:class-formals #'args.formals
                         #:class-arity #'args.arity
                         #:for-together? for-together?)]
    [(form-id class-name args::class-args
              ;; syntax-class clauses are implemented in "syntax-class-clause-primitive.rkt"
              (_::block clause::syntax-class-clause ...)
              ;; patterns can be spliced by one of the macros
              (~optional (_::alts alt ...)))
     (define parsed-clauses (syntax->list #'(clause.parsed ...)))
     (define-values (pattern-alts kind-kw class-desc fields-ht swap-root opaque?)
       (extract-clauses stx
                        parsed-clauses
                        (and (attribute alt)
                             (syntax->list #'(alt ...)))))
     (build-syntax-class stx pattern-alts parsed-clauses
                         #:define-class-id #'define-syntax
                         #:class/inline-name #'class-name
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
      (lambda (stx)
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

(define-syntax together
  (definition-transformer
    (lambda (stx)
      (syntax-parse stx
        #:datum-literals (group)
        [(_ (_::block (group op::syntax-class-id . rest)) ...)
         (define decls
           (for/list ([g (in-list (syntax->list #'((op . rest) ...)))])
             (parse-syntax-class g #:for-together? #t)))
         (append
          (map car decls)
          (map (lambda (stx) #`(finish-together-syntax-class #,stx)) (map cadr decls)))]))))

;; returns a `rhombus-syntax-class`
(define-for-syntax (parse-anonymous-syntax-class who orig-stx expected-kind name tail)
  (syntax-parse tail
    [((_::alts alt ...))
     (build-syntax-class orig-stx (syntax->list #'(alt ...)) null
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

;; returns a `rhombus-syntax-class` if `define-syntax-id` is #f, otherwise
;; returns a list of definitions
(define-for-syntax (build-syntax-class stx
                                       alts
                                       track-stxes
                                       #:define-class-id [define-syntax-id #f]
                                       #:class/inline-name [class/inline-name #f]
                                       #:class-formals [class-formals #'#f]
                                       #:class-arity [class-arity #'#f]
                                       #:kind-kw [kind-kw #f]
                                       #:description-expr [description-expr #f]
                                       #:fields [fields-ht #f]
                                       #:swap-root [swap-root #f]
                                       #:opaque? [opaque? #f]
                                       #:expected-kind [expected-kind #f]
                                       #:for-together? [for-together? #f])
  (define-values (kind splicing?)
    (let ([kind (cond
                  [kind-kw (string->symbol (keyword->string kind-kw))]
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
     (define-values (patterns attributess)
       (cond
         [for-together?
          ;; delay parsing attributes until we've declared Rhombus syntax classes; this
          ;; require fully declared fields
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
                                            (quote-syntax unpack-term*))))]))
          (values #`((#:delayed-patterns #,stx #,alts #,fields-ht
                      #,(map (lambda (pv)
                               (list (pattern-variable-sym pv)
                                     (pattern-variable-val-id pv)))
                             attrs)
                      #,kind
                      #,splicing?))
                  (list attrs))]
         [else
          (for/lists (patterns attributess) ([alt-stx (in-list alts)])
            (parse-pattern-cases stx alt-stx kind splicing? #:keep-attr-id? (not define-syntax-id)))]))
     (define attributes (intersect-attributes stx attributess fields-ht swap-root))
     (cond
       [(not define-syntax-id)
        ;; return a `rhombus-syntax-class` directly
        (rhombus-syntax-class kind
                              (syntax-class-body->inline patterns class/inline-name)
                              (datum->syntax #f (map pattern-variable->list attributes))
                              splicing?
                              (syntax->datum class-arity)
                              (and swap-root
                                   (cons (syntax-e (car swap-root)) (cdr swap-root))))]
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
          #`(#,define-syntax-id #,(in-syntax-class-space class-name)
             (rhombus-syntax-class '#,kind
                                   (quote-syntax #,internal-class-name)
                                   (quote-syntax #,(for/list ([var (in-list attributes)])
                                                     (pattern-variable->list var #:keep-id? #f)))
                                   #,splicing?
                                   '#,class-arity
                                   '#,swap-root)))
         #`(#,define-class #,(if (syntax-e class-formals)
                                 #`(#,internal-class-name . #,class-formals)
                                 class-name)
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
     (define-values (patterns attributess)
       (for/lists (patterns attributess) ([alt-stx (in-list (syntax->list #'alts))])
         (parse-pattern-cases stx alt-stx (syntax-e #'kind) (syntax-e #'splicing?)
                              #:tmp-ids (for/hasheq ([key+tmp-id-stx (in-list (syntax->list #'key+tmp-ids))])
                                          (define key+tmp-id (syntax->list key+tmp-id-stx))
                                          (values (syntax-e (car key+tmp-id)) (cadr key+tmp-id))))))
     ;; check against `fields-ht`:
     (intersect-attributes #'orig-stx attributess (syntax-e #'fields-ht) #f)
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
                                   (lambda (e) 'sequence)
                                   (lambda (e)
                                     (syntax-parse e
                                       #:datum-literals (multi)
                                       [(multi)
                                        ;; special case: empty sequence can be spliced
                                        'sequence]
                                       [_ 'multi]))
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
             #:attr depth #'0)
    (pattern (_::brackets (group a::attribute-lhs) (group (op rhombus...)))
             #:attr id #'a.id
             #:attr depth #`#,(+ 1 (syntax-e #'a.depth)))))

;; Returns two values:
;;   * a `pattern` form suitable to use in `define-syntax-class`
;;   * a list of `pattern-variable`s that represent fields (i.e., attributes)
;;     of the syntax class
;; The first result has a restricted form that is recognized by `syntax-class-body->inline`
(define-for-syntax (parse-pattern-cases orig-stx stx kind splicing?
                                        #:keep-attr-id? [keep-attr-id? #f]
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
                                               [(= 1 (length ps))
                                                (car ps)]
                                               [else
                                                (raise-syntax-error #f
                                                                    "not a single-term pattern"
                                                                    orig-stx
                                                                    pat)])))
                     in-quotes))

  (define (bindings->defns idrs sidrs)
    (with-syntax ([([val-id val-rhs] ...) idrs]
                  [([(stx-id ...) stx-rhs] ...) sidrs])
      #'[(define val-id val-rhs)
         ...
         (define-syntaxes (stx-id ...) stx-rhs)
         ...]))

  (define-values (pattern-body all-attrs)
    (let loop ([body (syntax->list body)]
               [rev-do null]
               [rev-body (list (bindings->defns idrs sidrs) '#:do)]
               [rev-attrs (reverse vars)])
      (define (accum-do) (if (null? rev-do)
                             rev-body
                             (list* #`[(rhombus-body-sequence #,@(reverse rev-do))] '#:do
                                    rev-body)))
      (cond
        [(null? body) (values (reverse (accum-do))
                              (reverse rev-attrs))]
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
                        rev-attrs)]
                 [(#:field id depth rhs)
                  #:with (tmp-id) (or (and tmp-id-ht
                                           (let ([id (hash-ref tmp-id-ht (syntax-e #'id) #f)])
                                             (and id (list id))))
                                      (generate-temporaries #'(id)))
                  #:with (pat-ids pat-rhs) (make-pattern-variable-bind #'id #'tmp-id (quote-syntax unpack-element*)
                                                                       (syntax-e #'depth) null)
                  (loop (cdr body)
                        null
                        (list* #'[(define tmp-id rhs)
                                  (define-syntaxes pat-ids pat-rhs)] '#:do
                               (accum-do))
                        (cons (pattern-variable (syntax-e #'id) #'id #'tmp-id (syntax-e #'depth) (quote-syntax unpack-element*))
                              rev-attrs))]
                 [(#:also (_ pat-g ...) rhs)
                  (define-values (p idrs sidrs vars can-be-empty?)
                    (convert-pattern #'(multi pat-g ...)))
                  (loop (cdr body)
                        null
                        (list* (bindings->defns idrs sidrs) '#:do
                               #' (repack-as-multi rhs) p '#:with
                               (accum-do))
                        (append (reverse vars)
                                rev-attrs))]
                 [(#:when rhs)
                  (loop (cdr body)
                        null
                        (list* #'rhs '#:when
                               (accum-do))
                        rev-attrs)])])]
           [else
            (loop (cdr body) (cons g rev-do) rev-body rev-attrs)])])))

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
                           (#,(pattern-variable-unpack* var)
                            (quote-syntax dots)
                            #,(pattern-variable-val-id var)
                            #,(pattern-variable-depth var))))
                      all-attrs)]
                [(body-form ...) pattern-body])
    (values #`(pattern #,p
                       #,@pattern-body
                       attr ... ...)
            all-attrs)))

;; converts a `pattern` clause for `syntax-case` into a pattern suitable
;; for directly inlinding into a larger pattern
(define-for-syntax (syntax-class-body->inline patterns inline-name)
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
  #`(~or* #,@(for/list ([pattern (in-list patterns)])
               (syntax-parse pattern
                 [(_ pat body ...)
                  #`(~and pat #,@(convert-body (syntax->list #'(body ...))))]))))

;; ----------------------------------------

(define-for-syntax (intersect-attributes stx attributess fields-ht swap-root)
  (cond
    [(and (null? attributess) (not fields-ht) (not swap-root)) '()]
    [(and (null? (cdr attributess)) (not fields-ht) (not swap-root)) (car attributess)]
    [else
     ;; start with initial set
     (define ht0
       (for/hasheq ([var (in-list (car attributess))])
         (values (pattern-variable-sym var) var)))
     ;; intersect by pruning set
     (define ht
       (for/fold ([ht0 ht0]) ([attributes (in-list (cdr attributess))])
         (for/fold ([ht #hasheq()]) ([var (in-list attributes)])
           (define prev-var (hash-ref ht0 (pattern-variable-sym var) #f))
           (if prev-var
               (hash-set ht
                         (pattern-variable-sym var)
                         (intersect-var stx var prev-var))
               ht))))
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
       (unless (hash-ref filtered-ht (syntax-e (car swap-root)) #f)
         (raise-syntax-error #f
                             "field to swap as root not found"
                             stx
                             (car swap-root)))
       (when (hash-ref filtered-ht (syntax-e (cdr swap-root)) #f)
         (raise-syntax-error #f
                             "field for root already exists"
                             stx
                             (cdr swap-root))))
     ;; convert back to list
     (hash-values filtered-ht #t)]))

(define-for-syntax (intersect-var stx a b)
  (unless (eqv? (pattern-variable-depth a) (pattern-variable-depth b))
    (raise-syntax-error #f
                        "field with different depths in different cases"
                        stx
                        (pattern-variable-sym a)))
  ;; keeping the same unpack, if possible, enables optimizations for
  ;; tail repetitions; otherwise, the term is sufficiently normalized
  ;; by matching that we can just use `unpack-element*`
  (if (free-identifier=? (pattern-variable-unpack* a) (pattern-variable-unpack* b))
      a
      (struct-copy pattern-variable a
                   [unpack* #'unpack-element*])))
