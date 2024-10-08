#lang racket/base
(require (for-syntax racket/base
                     racket/keyword
                     racket/symbol
                     syntax/parse/pre
                     rhombus/private/enforest
                     enforest/name-parse
                     shrubbery/property
                     rhombus/private/name-path-op
                     rhombus/private/doc-spec
                     rhombus/private/treelist
                     rhombus/private/pack
                     syntax/strip-context
                     (lib "shrubbery/render/private/add-space.rkt")
                     "typeset-key-help.rkt")
         (for-label (only-in rhombus
                             meta))
         racket/symbol
         rhombus/private/version-case
         (prefix-in typeset-meta: (lib "shrubbery/render/private/typeset_meta.rhm"))
         "metavar.rkt"
         "nonterminal.rkt"
         "doc.rkt"
         (submod "doc.rkt" for-class)
         (lib "shrubbery/render/private/typeset-help.rkt")
         "typeset-key-help.rkt"
         "defining-element.rkt"
         shrubbery/print
         rhombus/private/module-path
         (only-in rhombus/private/name-root
                  name-root-ref
                  in-name-root-space
                  name-root-quote)
         (only-in "rhombus.rhm"
                  rhombusblock_etc
                  [rhombus rhombus:rhombus])
         (only-in rhombus/parse
                  rhombus-expression)
         (only-in scribble/manual
                  hspace
                  racketvarfont
                  racketidfont
                  tt
                  smaller)
         (submod scribble/racket id-element)
         (only-in scribble/core
                  table
                  paragraph
                  element
                  index-element
                  toc-target2-element
                  target-element
                  plain
                  style
                  table-cells)
         (only-in scribble/private/manual-vars
                  boxed-style)
         (only-in scribble/private/manual-bind
                  annote-exporting-library
                  id-to-target-maker
                  with-exporting-libraries)
         (only-in scribble/private/manual-vars
                  add-background-label)
         "mod-path.rkt")

(meta-if-version-at-least
 "8.14.0.5" ; assuming implies "scribble-lib" version 1.54
 (require (only-in scribble/manual-struct
                   exported-index-desc*))
 (begin
   (require (only-in scribble/manual-struct
                     exported-index-desc))
   (define (exported-index-desc* name libs extras)
     (exported-index-desc name libs))))

(provide typeset-doc
         define-nonterminal
         typeset-nontermref

         define-doc-syntax
         (for-syntax
          make-doc-transformer
          doc-typeset-rhombusblock
          add-metavariable
          extract-term-metavariables
          extract-group-metavariables
          extract-binding-metavariables
          extract-pattern-metavariables))

(define-syntax (typeset-doc stx)
  (syntax-parse stx
    #:datum-literals (parens group brackets block)
    [(_ context
        (parens (~alt (~optional (group #:literal (block (group literal-id ...) ...)))
                      (~optional (group #:nonterminal_key (block nt-key-g)))
                      (~optional (group #:nonterminal (block
                                                       (group nt-id (block nt-id-key-g))
                                                       ...)))
                      (~optional (group (~and meta-tag (~or #:meta #:also_meta)))))
                ...
                (~seq ((~and group-tag group) form ...) ...)
                (group
                 (brackets content-group ...))))
     #:with (_ . err-stx) stx
     (define pre-forms (map (lambda (stx) (datum->syntax #f (syntax-e stx)))
                            (syntax->list #'((group-tag form ...) ...))))
     ;; expand `~include`s:
     (define-values (forms include-bodys)
       (let loop ([pre-forms pre-forms] [accum '()] [accum-bodys '()])
         (cond
           [(null? pre-forms) (values (reverse accum) (reverse accum-bodys))]
           [else
            (syntax-parse (car pre-forms)
              #:datum-literals (group block)
              [(group #:include mod ... (block (group id:identifier ...) ...))
               (syntax-parse #'(group mod ...)
                 [mp::module-path
                  (define mod-path (module-path-convert-parsed #'mp.parsed))
                  (let mod-loop ([ids (syntax->list #'(id ... ...))]
                                 [accum accum]
                                 [accum-bodys accum-bodys])
                    (cond
                      [(null? ids)
                       (loop (cdr pre-forms) accum accum-bodys)]
                      [else
                       (define (bad-export)
                         (raise-syntax-error #f
                                             "not exported as a `DocSpec`"
                                             #'err-stx
                                             (car ids)))
                       (define v (dynamic-require `(submod ,(syntax->datum mod-path) doc) (syntax-e (car ids)) bad-export))
                       (unless (is_doc_spec v) (bad-export))
                       (define-values (headers content) (doc_spec_split v))
                       (define (reset-context stx) (unpack-group (replace-context (car ids) stx) 'doc #f))
                       (mod-loop (cdr ids)
                                 (append (reverse (map reset-context (treelist->list headers)))
                                         accum)
                                 (append (reverse (map reset-context (treelist->list content)))
                                         accum-bodys))]))]
                 [_
                  (raise-syntax-error #f
                                      "invalid module path"
                                      #'err-stx
                                      #'(mod ...))])]
              [(group #:include . _)
               (raise-syntax-error #f
                                   "bad syntax for `~include`"
                                   #'err-stx
                                   (car pre-forms))]
              [else
               (loop (cdr pre-forms) (cons (car pre-forms) accum) accum-bodys)])])))
     (define transformers (for/list ([form (in-list forms)])
                            (syntax-parse form
                              #:datum-literals (group)
                              [(group . (~var name (:hier-name-seq in-name-root-space in-doc-space name-path-op name-root-ref)))
                               (define t (syntax-local-value* (in-doc-space #'name.name) doc-transformer-ref))
                               (unless t
                                 (raise-syntax-error #f
                                                     "unknown doc form"
                                                     #'err-stx
                                                     #'name.name))
                               t]
                              [_ (raise-syntax-error #f
                                                     "unrecognized doc form"
                                                     #'err-stx
                                                     form)])))
     (define kind-strss (for/list ([form (in-list forms)]
                                   [t (in-list transformers)])
                          (define kind-str/s ((doc-transformer-extract-desc t) form))
                          (if (or (not kind-str/s)
                                  (string? kind-str/s))
                              (list kind-str/s)
                              kind-str/s)))
     (define all-space-namesss (for/list ([form (in-list forms)]
                                          [t (in-list transformers)])
                                 (define names/s ((doc-transformer-extract-space-sym t) form))
                                 (cond
                                   [(null? names/s) (error "spaces names should not be empty")]
                                   [(not (list? names/s)) (list (list names/s))]
                                   [(not (list? (car names/s))) (list names/s)]
                                   [else names/s])))
     (define sort-orderss (for/list ([form (in-list forms)]
                                     [t (in-list transformers)]
                                     [space-namess (in-list all-space-namesss)])
                            (define index/s ((doc-transformer-extract-sort-order t) form space-namess))
                            index/s))
     (define space-namess (for/list ([all-namess (in-list all-space-namesss)])
                            (for/list ([all-names (in-list all-namess)])
                              (car all-names))))
     (define extra-space-namesss (for/list ([all-namess (in-list all-space-namesss)])
                                   (for/list ([all-names (in-list all-namess)])
                                     (cdr all-names))))
     (define introducerss (for/list ([space-names (in-list space-namess)])
                            (for/list ([space-name (in-list space-names)])
                              (if (and space-name
                                       (not (eq? space-name 'grammar)))
                                  (let ([intro (make-interned-syntax-introducer space-name)])
                                    (lambda (x)
                                      (intro x 'add)))
                                  values))))
     (define def-hts+single?s (for/list ([form (in-list forms)]
                                         [t (in-list transformers)]
                                         [space-names (in-list space-namess)]
                                         [introducers (in-list introducerss)])
                                (define def-name/s ((doc-transformer-extract-defined t) form (car space-names)))
                                (define single? (not (list? def-name/s)))
                                (define def-names (if single?
                                                      (list def-name/s)
                                                      def-name/s))
                                (cons (for/list  ([def-name (in-list def-names)]
                                                  [space-name (in-list space-names)]
                                                  [introducer (in-list introducers)])
                                        (cond
                                          [def-name
                                            (unless (or (identifier? def-name)
                                                        (and (hash? (syntax-e def-name))
                                                             (identifier? (hash-ref (syntax-e def-name) 'target #f))))
                                              (raise-syntax-error 'doc
                                                                  "identifier or syntax hash table with 'target key"
                                                                  def-name))
                                            (define def-ht (if (identifier? def-name)
                                                               (hash 'target def-name)
                                                               (syntax-e def-name)))
                                            (unless (eq? space-name 'grammar)
                                              (define def-id (let ([root (hash-ref def-ht 'root #f)])
                                                               (if root
                                                                   (in-name-root-space root)
                                                                   (introducer (hash-ref def-ht 'target)))))
                                              (unless (identifier-binding def-id #f)
                                                (raise-syntax-error 'doc
                                                                    (format
                                                                     "identifier to document has no for-label binding in space ~s"
                                                                     space-name)
                                                                    def-id)))
                                            def-ht]
                                          [else #f]))
                                      single?)))
     (define def-htss (map car def-hts+single?s))
     (define single?s (map cdr def-hts+single?s))
     (define-values (nt-def-ht nt-space-name nt-introducer)
       (if (attribute nt-key-g)
           (nt-key-expand #'nt-key-g)
           (values (caar def-htss) (caar space-namess) (caar introducerss))))
     (define def-id-as-defss
       (for/fold ([rev-mk-as-defss '()] [rev-keyss '()] [seen #hash()]
                                        #:result (let ([key-rev-strsss (for/list ([rev-keys (in-list (reverse rev-keyss))])
                                                                        (for/list ([key (in-list (reverse rev-keys))])
                                                                          (hash-ref seen key '())))])
                                                   (for/list ([rev-mk-as-defs (in-list (reverse rev-mk-as-defss))]
                                                              [key-rev-strss (in-list key-rev-strsss)])
                                                     (for/list ([mk-as-def (in-list (reverse rev-mk-as-defs))]
                                                                [key-rev-strs (in-list key-rev-strss)])
                                                       (mk-as-def key-rev-strs)))))
                 ([def-hts (in-list def-htss)]
                  [sort-orders (in-list sort-orderss)]
                  [introducers (in-list introducerss)]
                  [space-names (in-list space-namess)]
                  [extra-space-namess (in-list extra-space-namesss)]
                  [kind-strs (in-list kind-strss)])
         (for/fold ([rev-mk-as-defs '()] [rev-keys '()] [seen seen]
                                         #:result (values (cons rev-mk-as-defs rev-mk-as-defss)
                                                          (cons rev-keys rev-keyss)
                                                          seen))
                   ([immed-def-ht (in-list def-hts)]
                    [immed-introducer (in-list introducers)]
                    [immed-space-name (in-list space-names)]
                    [extra-space-names (in-list extra-space-namess)]
                    [kind-str (in-list kind-strs)]
                    [sort-order (in-list sort-orders)])
           (cond
             [(not immed-def-ht)
              (values (cons (lambda (l) #f) rev-mk-as-defs)
                      (cons #f rev-keys)
                      seen)]
             [else
              (define def-ht (if (eq? immed-space-name 'grammar)
                                   ;; use key of a grammar non-terminal
                                   nt-def-ht
                                   immed-def-ht))
              (define introducer (if (eq? immed-space-name 'grammar)
                                     nt-introducer
                                     immed-introducer))
              (define extra-introducers (for/list ([extra-space-name (in-list extra-space-names)])
                                          (let ([intro (make-interned-syntax-introducer extra-space-name)])
                                            (lambda (x)
                                              (intro x 'add)))))
              (define space-name (if (eq? immed-space-name 'grammar)
                                     nt-space-name
                                     immed-space-name))
              (define root-id (hash-ref def-ht 'root #f))
              (define target-id (hash-ref def-ht 'target))
              (define def-id (if root-id
                                 (in-name-root-space root-id)
                                 (introducer target-id)))
              (define extra-def-ids (for/list ([extra-introducer (in-list extra-introducers)])
                                      (if root-id
                                          (in-name-root-space root-id)
                                          (extra-introducer target-id))))
              (define str-id (and root-id target-id))
              (define index-str (hash-ref def-ht 'raw #f))
              (define sym-path (cons
                                (if str-id
                                    (syntax-e str-id)
                                    null)
                                (if (eq? immed-space-name 'grammar)
                                    (hash-ref immed-def-ht 'target)
                                    null)))
              (define seen-key (cons (cons (syntax-e def-id) sym-path)
                                     space-name))
              (define raw-prefix-str
                (and (not (eq? immed-space-name 'grammar))
                     (hash-ref def-ht 'raw_prefix #f)))
              (define make-typeset-id
                (lambda (kind-rev-strs)
                  #`(make-def-id
                     #,(if (hash-ref seen seen-key #f)
                           #t
                           #'redef?)
                     meta?
                     (quote-syntax #,def-id)
                     (quote-syntax #,extra-def-ids)
                     (quote #,raw-prefix-str)
                     (quote-syntax #,str-id)
                     (quote #,index-str)
                     (quote #,(combine-kind-strs (reverse kind-rev-strs)))
                     (quote #,space-name)
                     (quote #,extra-space-names)
                     (quote #,(and (eq? immed-space-name 'grammar)
                                   (hash-ref immed-def-ht 'target)))
                     (quote #,immed-space-name)
                     (quote #,sort-order))))
              (values
               (cons (if (eq? immed-space-name 'grammar)
                         make-typeset-id
                         (lambda (kind-rev-strs)
                           #`(defining-element
                               #f
                               #,(make-typeset-id kind-rev-strs)
                               #,(if raw-prefix-str
                                     (string-length (syntax-e raw-prefix-str))
                                     0))))
                     rev-mk-as-defs)
               (cons seen-key rev-keys)
               (hash-set seen seen-key (cons kind-str (hash-ref seen seen-key '()))))]))))
     (define nonterm-vars (for/fold ([vars #hasheq()]) ([def-hts (in-list def-htss)]
                                                        [introducers (in-list introducerss)]
                                                        [space-names (in-list space-namess)])
                            (for/fold ([vars vars]) ([immed-def-ht (in-list def-hts)]
                                                     [immed-introducer (in-list introducers)]
                                                     [immed-space-name (in-list space-names)])
                              (cond
                                [(eq? immed-space-name 'grammar)
                                 (define target (hash-ref immed-def-ht 'target))
                                 (hash-set vars (syntax-e target) target)]
                                [else vars]))))
     (define all-vars (for/fold ([vars #hasheq()]) ([form (in-list forms)]
                                                    [t (in-list transformers)]
                                                    [space-names (in-list space-namess)])
                        ((doc-transformer-extract-metavariables t) form (car space-names) vars)))
     (define vars (let ([vars (for/fold ([vars all-vars]) ([id (in-list (syntax->list #'(~? (literal-id ... ...) ())))])
                                (hash-remove vars (syntax-e id)))])
                    (let ([vars (for/fold ([vars vars]) ([sym (in-hash-keys nonterm-vars)])
                                  (hash-remove vars sym))])
                      (for/fold ([vars vars]) ([nt-id (in-list (syntax->list #'(~? (nt-id ...) ())))])
                        (hash-remove vars (syntax-e nt-id))))))
     (define typesets (for/list ([form (in-list forms)]
                                 [t (in-list transformers)]
                                 [single? (in-list single?s)]
                                 [def-id-as-defs (in-list def-id-as-defss)]
                                 [space-names (in-list space-namess)])
                        ;; uses `def-id-as-def` in a context that binds `redef?` and `meta?`:
                        (extract-typeset t form single? def-id-as-defs space-names)))
     (with-syntax ([(typeset ...) typesets]
                   [(kind-str ...) (map car kind-strss)]
                   [(include-body ...) include-bodys]
                   [(meta-sep ...) (if (attribute meta-tag)
                                       (let ([nl (datum->syntax stx "\n")])
                                         (list nl nl))
                                       '())]
                   [(sep ...) (if (null? include-bodys)
                                  '()
                                  (let ([nl (datum->syntax stx "\n")])
                                    (list nl nl)))])
       #`(let-syntax (#,@(for/list ([mv (in-hash-values vars)])
                           (define id (metavar-id mv))
                           (define nt (and (metavar-nonterm? mv)
                                           (syntax-local-value* (in-nonterminal-space id) nonterminal-ref)))
                           #`[#,(typeset-meta:in_space id)
                              #,(if nt
                                    (nonterminal-transformer-id nt)
                                    #`(make-meta-id-transformer (quote-syntax #,id)))])
                      #,@(for/list ([id (in-hash-values nonterm-vars)])
                           #`[#,(typeset-meta:in_space id)
                              #,(nonterm-id-transformer id id nt-def-ht nt-space-name)])
                      #,@(for/list ([nt-id (in-list (syntax->list #'(~? (nt-id ...) ())))]
                                    [nt-id-key-g (in-list (syntax->list #'(~? (nt-id-key-g ...) ())))])
                           (define-values (nt-sym nt-def-ht nt-space-name nt-introducer) (nt-key-ref-expand nt-id-key-g))
                           #`[#,(typeset-meta:in_space nt-id)
                              #,(nonterm-id-transformer nt-id nt-sym nt-def-ht nt-space-name)])
                      [#,(typeset-meta:in_space (datum->syntax #'context '...)) (make-ellipsis-transformer)])
           (list
            (table
             boxed-style
             (insert-labels
              (list
               typeset
               ...)
              '(kind-str ...)))
            #,@(if (attribute meta-tag)
                   #`((smaller "Provided "
                               #,(if (eq? (syntax-e #'meta-tag) '#:also_meta)
                                     "both normally and  "
                                     "")
                               "as "
                               (rhombus-expression
                                (group rhombus:rhombus (parens (group meta) (group #:expo))))
                               ".")
                      meta-sep ...)
                   null)
            (rhombus-expression include-body)
            ...
            sep ...
            (rhombus-expression content-group)
            ...)))]))

(define-syntax (define-nonterminal stx)
  (syntax-parse stx
    #:datum-literals (parens group brackets block)
    [(_ context
        (block (group nt-id (block nt-id-key-g))
               ...))
     #`(begin
         #,@(apply
             append
             (for/list ([nt-id (in-list (syntax->list #'(nt-id ...)))]
                        [nt-id-key-g (in-list (syntax->list #'(nt-id-key-g ...)))])
               (define-values (nt-sym nt-def-ht nt-space-name nt-introducer) (nt-key-ref-expand nt-id-key-g))
               (with-syntax ([(tmp-id) (generate-temporaries (list nt-id))])
                 (list
                  #`(define-for-syntax tmp-id
                      #,(nonterm-id-transformer nt-id nt-sym nt-def-ht nt-space-name))
                  #`(define-syntax #,(in-nonterminal-space nt-id)
                      (nonterminal (quote-syntax tmp-id))))))))]))

(define-syntax (typeset-nontermref stx)
  (syntax-parse stx
    [(_ context id)
     (define nt (syntax-local-value* (in-nonterminal-space #'id) nonterminal-ref))
     (cond
       [nt
        #`(let-syntax ([gen (lambda (stx)
                              (call-nonterminal-transformer
                               #,(nonterminal-transformer-id nt)
                               (quote-syntax id)))])
            gen)]
       [else
        (raise-syntax-error 'nontermref "not bound as a nonterminal" #'id)])]))

(define-for-syntax (call-nonterminal-transformer nt id)
  (syntax-parse ((typeset-meta:Transformer_proc nt) id)
    #:datum-literals (parsed)
    [(parsed _ e) #'e]))

(define-for-syntax (make-ellipsis-transformer)
  (typeset-meta:make_Transformer
   (lambda (use-stx)
     #`(parsed #:rhombus/expr (tt "...")))))

(define (make-def-id redef? meta? id extra-ids prefix-str str-id index-str-in kind-str space extra-spaces
                     nonterm-sym immed-space sort-order)
  (define str-id-e (syntax-e str-id))
  (cond
    [redef?
     ((if meta? racketvarfont racketidfont)
      (make-id-element id (shrubbery-syntax->string (if str-id-e str-id id)) #t
                       #:space space
                       #:suffix (if str-id-e
                                    (list (if index-str-in
                                              (string->symbol index-str-in)
                                              (target-id-key-symbol str-id))
                                          space)
                                    space)))]
    [else
     (define str (if (eq? immed-space 'grammar)
                     (symbol->immutable-string nonterm-sym)
                     (shrubbery-syntax->string (if str-id-e
                                                   str-id
                                                   id))))
     (define index-str (or index-str-in str))
     (define nonterm-suffix (if (eq? immed-space 'grammar)
                                (list nonterm-sym)
                                null))
     (define (get-str+space space)
       (cond
         [str-id-e
          (append (list (if index-str-in
                            (string->symbol index-str-in)
                            (target-id-key-symbol str-id))
                        space)
                  nonterm-suffix)]
         [(null? nonterm-suffix) space]
         [else (cons space nonterm-suffix)]))
     (define str+space (get-str+space space))
     (define (get-id-space space)
       (if str-id-e
           ;; referring to a field of a namespace, so
           ;; `id` is bound in the namespace space, not
           ;; in `space`
           'rhombus/namespace
           space))
     (define id-space (get-id-space space))
     (define (make-content defn? [str str] #:meta? [meta? meta?] #:can-prefix? [can-prefix? #t])
       (define c ((if (or meta? (eq? immed-space 'grammar)) racketvarfont racketidfont)
                  (make-id-element id str defn? #:space id-space #:suffix str+space)))
       (if (and can-prefix? prefix-str)
           (list (racketidfont prefix-str) c)
           c))
     (define content (annote-exporting-library (make-content #t)))
     (for/fold ([content content]) ([id (in-list (cons id (syntax->list extra-ids)))]
                                    [space (in-list (cons space extra-spaces))]
                                    [idx (in-naturals)])
       (define id-space (get-id-space space))
       (define str+space (get-str+space space))
       (define target-maker (id-to-target-maker id #t #:space id-space #:suffix str+space))
       (cond
         [target-maker
          (define name (string->symbol str))
          (define content-as-defn? #t)
          (define ref-content (make-content content-as-defn? index-str #:meta? #f))
          (define ref-content/no-prefix (make-content content-as-defn? index-str #:meta? #f #:can-prefix? #f))
          (target-maker content
                        (lambda (tag)
                          (if (or nonterm-sym
                                  (idx . > . 0))
                              (begin
                                (target-element
                                 #f
                                 content
                                 tag))
                              (toc-target2-element
                               #f
                               (index-element
                                #f
                                content
                                tag
                                (list (datum-intern-literal index-str))
                                (list (list ref-content " " kind-str))
                                (with-exporting-libraries
                                  (lambda (libs)
                                    (exported-index-desc*
                                     name libs
                                     (hash 'kind kind-str
                                           'sort-order sort-order
                                           'display-from-libs (map module-path->rhombus-module-path libs))))))
                               tag
                               ref-content/no-prefix))))]
         [else content]))]))

(define-for-syntax (nonterm-id-transformer id sym nt-def-ht nt-space-name)
  (define root (hash-ref nt-def-ht 'root #f)) 
  #`(make-nonterm-id-transformer
     (quote #,id)
     (quote #,sym)
     (quote-syntax #,(if root
                         root
                         (hash-ref nt-def-ht 'target)))
     (quote #,(if root
                  (target-id-key-symbol (hash-ref nt-def-ht 'target))
                  #f))
     (quote #,nt-space-name)))

(define-for-syntax (make-nonterm-id-transformer id-sym sym def-id def-sub def-space-sym)
  (typeset-meta:make_Transformer
   (lambda (stx)
     (define space-sym (if def-sub
                           'rhombus/namespace
                           def-space-sym))
     (define sp-def-id (if space-sym
                           ((make-interned-syntax-introducer space-sym) def-id)
                           def-id))
     #`(parsed
        #:rhombus/expr
        (racketvarfont
         (make-id-element (quote-syntax #,sp-def-id) '#,(symbol->immutable-string id-sym) #f
                          #:space '#,space-sym
                          #:suffix #,(if def-sub
                                         #`(list '#,def-sub '#,def-space-sym '#,(or sym id-sym))
                                         #`(list '#,def-space-sym '#,(or sym id-sym)))))))))

(define-for-syntax (make-meta-id-transformer id)
  (typeset-meta:make_Transformer
   (lambda (use-stx)
     #`(parsed #:rhombus/expr (racketvarfont '#,(symbol->immutable-string (syntax-e id)))))))

(define-for-syntax (nt-key-expand nt-key-g)
  (define-values (root fields space-names)
    (syntax-parse nt-key-g
      #:datum-literals (op |.| parens group)
      [(_ root:identifier (~seq (op |.|) field:identifier) ... space:keyword)
       (values #'root
               (syntax->list #'(field ...))
               (full-space-names (string->symbol (keyword->immutable-string (syntax-e #'space)))))]
      [(_ root:identifier (~seq (op |.|) field:identifier) ... #:at space::name ...)
       (values #'root
               (syntax->list #'(field ...))
               (list (string->symbol (apply string-append (map symbol->string (map syntax-e (syntax->list #'(space.name ...))))))))]
      [(_ root:identifier (~seq (op |.|) field:identifier) ...  (op |.|) (parens (group (op name:identifier))) space:keyword)
       (values #'root
               (syntax->list #'(field ... name))
               (full-space-names (string->symbol (keyword->immutable-string (syntax-e #'space)))))]
      [(_ root:identifier (~seq (op |.|) field:identifier) ...)
       (values #'root
               (syntax->list #'(field ...))
               '(#f))]
      [(_ root:identifier (~seq (op |.|) field:identifier) ... (op |.|) (parens (group (op name:identifier))))
       (values #'root
               (syntax->list #'(field ... name))
               '(#f))]
      [(_ (op name:identifier) space:keyword)
       (values #'name
               '()
               (full-space-names (string->symbol (keyword->immutable-string (syntax-e #'space)))))]
      [(_ (op name:identifier) #:at space::name ...)
       (values #'name
               '()
               (list (string->symbol (apply string-append (map symbol->string (map syntax-e (syntax->list #'(space.name ...))))))))]
      [(_ (op name:identifier))
       (values #'name
               '()
               '(#f))]))
  (define (make-intro space-name)
    (if space-name
        (make-interned-syntax-introducer space-name)
        (lambda (x) x)))
  (cond
    [(null? fields)
     (define space-name (car space-names))
     (values (hash 'target root)
             space-name
             (make-intro space-name))]
    [else
     (define resolved
       (resolve-name-ref space-names
                         root
                         fields))
     (cond
       [(not resolved)
        (raise-syntax-error #f
                            "no label binding for nonterminal"
                            nt-key-g)]
       [else
        (define space-name (hash-ref resolved 'space))
        (values (hash 'root (hash-ref resolved 'root #f)
                      'target (hash-ref resolved 'target))
                space-name
                (make-intro space-name))])]))

(define-for-syntax (nt-key-ref-expand nt-key-g)
  (define-values (sym g)
    (syntax-parse nt-key-g
      #:datum-literals (op |.|)
      [(_ root (~seq (~and dot (op |.|)) field) ... name:identifier space:keyword)
       (values #'name #'(group root (~@ dot field) ... space))]
      [(_ root (~seq (~and dot (op |.|)) field) ...  name:identifier)
       (values #'name #'(group root (~@ dot field) ...))]
      [_ (values #f nt-key-g)]))
  (define-values (def-ht space-name introducer) (nt-key-expand g))
  (define root-id (hash-ref def-ht 'root #f))
  (let ([def-id (or root-id
                    (hash-ref def-ht 'target))]
        [introducer (if root-id in-name-root-space introducer)])
    (unless (identifier-label-binding (introducer def-id))
      (raise-syntax-error #f "no for-label binding the expected space" def-id)))
  (values sym def-ht space-name introducer))

(define-for-syntax (extract-typeset t stx single? def-id-as-defs space-names)
  (define (relocate to from-in from-property to-property)
    (define from (syntax-parse from-in
                   #:datum-literals (op)
                   [(op from) #'from]
                   [_ from-in]))
    (to-property (syntax-raw-property (datum->syntax to
                                                     (syntax-e to)
                                                     from)
                                      '())
                 (from-property from)))
  (define substs
    (for/list ([def-id-as-def (in-list def-id-as-defs)])
      (define (subst name #:as_wrap [wrap? #t] #:as_redef [as-redef? #f] #:as_meta [meta? #f])
        (cond
          [wrap?
           (define id (if (identifier? name) name (hash-ref (syntax-e name) 'target)))
           (datum->syntax
            #f
            (list
             (list 'op (relocate #'|#,| id syntax-raw-prefix-property syntax-raw-prefix-property))
             (list
              (relocate #'parens id syntax-raw-suffix-property syntax-raw-suffix-property)
              ;; span is taken from `parens` above, so more nested srclocs don't matter
              #`(group (parsed #:rhombus/expr
                               (let ([redef? #,as-redef?]
                                     [meta? #,meta?])
                                 #,def-id-as-def))))))]
          [else #`(let ([redef? #,as-redef?]
                        [meta? #,meta?])
                    #,def-id-as-def)]))
      subst))
  ((doc-transformer-extract-typeset t) stx
                                       (if single? (car space-names) space-names)
                                       (if single? (car substs) substs)))

(define (insert-labels l lbls)
  (cond
    [(null? l) null]
    [else
     (map
      list
      (append
       ((if (car lbls) (add-background-label (car lbls)) values)
        (list (car l)))
       (let loop ([l (cdr l)] [lbls (cdr lbls)])
         (cond
           [(null? l) null]
           [else
            (cons
             (paragraph plain (hspace 1))
             (append ((if (car lbls) (add-background-label (car lbls)) values)
                      (list (car l)))
                     (loop (cdr l) (cdr lbls))))]))))]))

(define-for-syntax (combine-kind-strs strs)
  (let ([strs (reverse (for/fold ([new-strs '()]) ([str (in-list strs)])
                         (if (member str new-strs)
                             new-strs
                             (cons str new-strs))))])
    (cond
      [(null? strs) "???"]
      [(null? (cdr strs)) (car strs)]
      [(null? (cddr strs)) (string-append (car strs) " and " (cadr strs))]
      [else (let loop ([strs strs])
              (cond
                [(null? (cdr strs)) (string-append "and " (car strs))]
                [else
                 (string-append (car strs) ", " (loop (cdr strs)))]))])))

;; ----------------------------------------

(define-for-syntax (add-metavariable vars id nonterm?)
  (hash-set vars (syntax-e id) (let ([mv (hash-ref vars (syntax-e id) #f)])
                                 (if mv
                                     (struct-copy metavar mv
                                                  [nonterm? (or nonterm?
                                                                (metavar-nonterm? mv))])
                                     (metavar id nonterm?)))))

(define-for-syntax (extract-binding-metavariables stx vars)
  (define (extract-binding-group stx vars)
    (syntax-parse stx
      #:datum-literals (group op :: :~)
      [(~or* (group t)
             (group t (op (~or* :: :~)) . _))
       (extract-binding-term #'t vars)]
      [_ vars]))
  (define (extract-binding-term stx vars)
    (define-splicing-syntax-class rst/dot
      #:datum-literals (group op ...)
      (pattern (~seq _ (group (op ...)))))
    (define-splicing-syntax-class rst/and
      #:datum-literals (group op &)
      (pattern (~seq (group (op &) . _))))
    (define (extract-rest stx vars rst/dot-k)
      (if stx
          (syntax-parse stx
            [(g _) (rst/dot-k #'g)]
            [((tag _ . more)) (extract-binding-group #'(tag . more) vars)])
          vars))
    (define (extract-list-rest stx vars)
      (extract-rest
       stx vars
       (lambda (stx) (extract-binding-group stx vars))))
    (define (maybe-extract-group stx vars)
      (syntax-parse stx
        [(g) (extract-binding-group #'g vars)]
        [_ vars]))
    (define (extract-map-rest stx vars)
      (extract-rest
       stx vars
       (lambda (stx)
         (syntax-parse stx
           #:datum-literals (block)
           [(tag t ... (block . gs))
            (maybe-extract-group
             #'gs
             (extract-binding-group #'(tag t ...) vars))]))))
    (syntax-parse stx
      #:datum-literals (brackets braces parens [hole _])
      [(brackets . (~or* (g ... rst:rst/dot)
                         (g ... rst:rst/and)
                         (g ...)))
       (extract-list-rest
        (attribute rst)
        (for/fold ([vars vars]) ([g (in-list (syntax->list #'(g ...)))])
          (extract-binding-group g vars)))]
      [(braces . more)
       (syntax-parse #'more
         #:datum-literals (block)
         [(~or* ((_ ... (block . gs)) ... rst:rst/dot)
                ((_ ... (block . gs)) ... rst:rst/and)
                ((_ ... (block . gs)) ...))
          (extract-map-rest
           (attribute rst)
           (for/fold ([vars vars]) ([gs (in-list (syntax->list #'(gs ...)))])
             (maybe-extract-group gs vars)))]
         [(~or* (_ ... rst:rst/dot)
                (_ ... rst:rst/and)
                _)
          (extract-list-rest (attribute rst) vars)])]
      [(parens g) (extract-binding-group #'g vars)]
      [hole vars]
      [id:identifier (add-metavariable vars #'id #f)]
      [_ vars]))
  (extract-binding-group stx vars))

(define-for-syntax (extract-group-metavariables g vars nonterm?)
  (syntax-parse g
    #:datum-literals (group)
    [(group t ...)
     (for/fold ([vars vars]) ([t (in-list (syntax->list #'(t ...)))])
       (extract-term-metavariables t vars nonterm?))]))

(define-for-syntax (extract-term-metavariables t vars nonterm?)
  (syntax-parse t
    #:datum-literals (parens brackets braces block quotes alts)
    [((~or* parens brackets braces block quotes) g ...)
     (for/fold ([vars vars]) ([g (in-list (syntax->list #'(g ...)))])
       (extract-group-metavariables g vars nonterm?))]
    [((~datum alts) b ...)
     (for/fold ([vars vars]) ([b (in-list (syntax->list #'(b ...)))])
       (extract-term-metavariables b vars nonterm?))]
    [id:identifier
     (if (and (not nonterm?) (identifier-binding (typeset-meta:in_space #'id)))
         vars
         (add-metavariable vars #'id nonterm?))]
    [_ vars]))

(define-for-syntax (extract-pattern-metavariables g vars)
  (syntax-parse g
    #:datum-literals (group block)
    [(group t ...)
     (for/fold ([vars vars] [after-$? #f] #:result vars) ([t (in-list (syntax->list #'(t ...)))])
       (syntax-parse t
         #:datum-literals (op parens brackets braces block quotes alts $)
         [(op $) (values vars #t)]
         [_:identifier (if after-$?
                           (values (extract-term-metavariables t vars #t) #f)
                           (values vars #f))]
         [((~or* parens brackets braces quotes block) g ...)
          (values (for/fold ([vars vars]) ([g (in-list (syntax->list #'(g ...)))])
                    (extract-pattern-metavariables g vars))
                  #f)]
         [(alts b ...)
          (values (for/fold ([vars vars]) ([b (in-list (syntax->list #'(b ...)))])
                    (extract-pattern-metavariables #`(group #,b) vars))
                  #f)]
         [_ (values vars #f)]))]))

;; ----------------------------------------

(define-for-syntax (doc-typeset-rhombusblock
                    form
                    #:at [at-form form]
                    #:pattern? [pattern? #f]
                    #:options [options #'((parens (group #:inset (block (group (parsed #:rhombus/expr #f))))))])
  (with-syntax ([t-form (if pattern?
                            (drop-pattern-escapes form)
                            form)]
                [t-block (syntax-raw-property
                          (datum->syntax #f 'block
                                         (syntax-parse at-form
                                           #:datum-literals (op parens)
                                           [(_ (op a) . _) #'a]
                                           [(_ (seq . _) . _) #'seq]
                                           [(_ a . _) #'a]))
                          "")]
                [(option ...) options])
    #'(rhombus-expression (group rhombusblock_etc option ... (t-block t-form)))))

(define-for-syntax (drop-pattern-escapes g)
  (syntax-parse g
    #:datum-literals (group)
    [((~and g group) t ...)
     (define new-ts
       (let loop ([ts (syntax->list #'(t ...))])
         (cond
           [(null? ts) null]
           [else
            (syntax-parse (car ts)
              #:datum-literals (op parens brackets braces quotes block alts $)
              [(op (~and esc $))
               #:when (pair? (cdr ts))
               (define pre #'esc)
               (define t (cadr ts))
               (cons (append-consecutive-syntax-objects (syntax-e t) pre t)
                     (loop (cddr ts)))]
              [((~and tag (~or* parens brackets braces quotes block)) g ...)
               (cons #`(tag
                        #,@(for/list ([g (in-list (syntax->list #'(g ...)))])
                             (drop-pattern-escapes g)))
                     (loop (cdr ts)))]
              [((~and tag alts) b ...)
               (cons #`(tag #,@(for/list ([b (in-list (syntax->list #'(b ...)))])
                                 (car (loop (list b)))))
                     (loop (cdr ts)))]
              [_ (cons (car ts) (loop (cdr ts)))])])))
     #`(g #,@new-ts)]))
