#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/syntax-local
                     enforest/hier-name-parse
                     shrubbery/property
                     rhombus/private/name-path-op
                     "add-space.rkt"
                     "typeset-key-help.rkt")
         (prefix-in typeset-meta: "typeset_meta.rhm")
         "metavar.rkt"
         "nonterminal.rkt"
         "doc.rkt"
         (submod "doc.rkt" for-class)
         "typeset-help.rkt"
         "typeset-key-help.rkt"
         "defining-element.rkt"
         shrubbery/print
         (only-in rhombus/private/name-root-ref
                  name-root-ref)
         (only-in rhombus/private/name-root-space
                  in-name-root-space
                  name-root-quote)
         (only-in "rhombus.rhm"
                  rhombusblock_etc)
         (only-in rhombus/parse
                  rhombus-expression)
         (only-in scribble/manual
                  hspace
                  racketvarfont
                  racketidfont
                  tt)
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
         (only-in scribble/manual-struct
                  thing-index-desc)
         (only-in scribble/private/manual-vars
                  add-background-label))

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
                                                       ...))))
                ...
                ((~and group-tag group) form ...) ...
                (group
                 (brackets content-group ...))))
     #:with (_ . err-stx) stx
     (define forms (map (lambda (stx) (datum->syntax #f (syntax-e stx)))
                        (syntax->list #'((group-tag form ...) ...))))
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
     (define kind-strs (for/list ([form (in-list forms)]
                                  [t (in-list transformers)])
                         ((doc-transformer-extract-desc t) form)))
     (define all-space-namess (for/list ([form (in-list forms)]
                                        [t (in-list transformers)])
                                ((doc-transformer-extract-space-sym t) form)))
     (define space-names (for/list ([all-names (in-list all-space-namess)])
                           (if (list? all-names)
                               (car all-names)
                               all-names)))
     (define extra-space-namess (for/list ([all-names (in-list all-space-namess)])
                                  (if (list? all-names)
                                      (cdr all-names)
                                      '())))
     (define introducers (for/list ([space-name (in-list space-names)])
                           (if (and space-name
                                    (not (eq? space-name 'grammar)))
                               (let ([intro (make-interned-syntax-introducer space-name)])
                                 (lambda (x)
                                   (intro x 'add)))
                               values)))
     (define def-names (for/list ([form (in-list forms)]
                                  [t (in-list transformers)]
                                  [space-name (in-list space-names)]
                                  [introducer (in-list introducers)])
                         (define def-name ((doc-transformer-extract-defined t) form space-name))
                         (when def-name
                           (unless (eq? space-name 'grammar)
                             (define def-id (if (identifier? def-name)
                                                (introducer def-name)
                                                (in-name-root-space (car (syntax-e def-name)))))
                             (unless (identifier-binding def-id #f)
                               (raise-syntax-error 'doc
                                                   (format
                                                    "identifier to document has no for-label binding in space ~s"
                                                    space-name)
                                                   def-id))))
                         def-name))
     (define-values (nt-def-name nt-space-name nt-introducer)
       (if (attribute nt-key-g)
           (nt-key-expand #'nt-key-g)
           (values (car def-names) (car space-names) (car introducers))))
     (define def-id-as-defs
       (for/fold ([rev-mk-as-defs '()] [rev-keys '()] [seen #hash()]
                                       #:result (let ([key-rev-strss (for/list ([key (in-list (reverse rev-keys))])
                                                                       (hash-ref seen key '()))])
                                                  (for/list ([mk-as-def (reverse rev-mk-as-defs)]
                                                             [key-rev-strs (in-list key-rev-strss)])
                                                    (mk-as-def key-rev-strs))))
                 ([immed-def-name (in-list def-names)]
                  [immed-introducer (in-list introducers)]
                  [immed-space-name (in-list space-names)]
                  [extra-space-names (in-list extra-space-namess)]
                  [kind-str (in-list kind-strs)])
         (cond
           [(not immed-def-name)
            (values (cons (lambda (l) #f) rev-mk-as-defs)
                    (cons #f rev-keys)
                    seen)]
           [else
            (define def-name (if (eq? immed-space-name 'grammar)
                                 ;; use key of a grammar non-terminal
                                 nt-def-name
                                 immed-def-name))
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
            (define def-id (if (identifier? def-name)
                               (introducer def-name)
                               (in-name-root-space (car (syntax-e def-name)))))
            (define extra-def-ids (for/list ([extra-introducer (in-list extra-introducers)])
                                    (if (identifier? def-name)
                                        (extra-introducer def-name)
                                        (in-name-root-space (car (syntax-e def-name))))))
            (define str-id (if (identifier? def-name)
                               #f
                               (cadr (syntax->list def-name))))
            (define index-str (let ([l (syntax->list def-name)])
                                (and (and (list? l) ((length l) . > . 2))
                                     (caddr l))))
            (define sym-path (cons
                              (if str-id
                                  (syntax-e str-id)
                                  null)
                              (if (eq? immed-space-name 'grammar)
                                  (syntax-e immed-def-name)
                                  null)))
            (define seen-key (cons (cons (syntax-e def-id) sym-path)
                                   space-name))
            (define make-typeset-id
              (lambda (kind-rev-strs)
                #`(make-def-id
                   #,(if (hash-ref seen seen-key #f)
                         #t
                         #'redef?)
                   (quote-syntax #,def-id)
                   (quote-syntax #,extra-def-ids)
                   (quote-syntax #,str-id)
                   (quote #,index-str)
                   (quote #,(combine-kind-strs (reverse kind-rev-strs)))
                   (quote #,space-name)
                   (quote #,extra-space-names)
                   (quote #,(and (eq? immed-space-name 'grammar)
                                 immed-def-name))
                   (quote #,immed-space-name))))
            (values
             (cons (if (eq? immed-space-name 'grammar)
                       make-typeset-id
                       (lambda (kind-rev-strs)
                         #`(defining-element
                             #f
                             #,(make-typeset-id kind-rev-strs))))
                   rev-mk-as-defs)
             (cons seen-key rev-keys)
             (hash-set seen seen-key (cons kind-str (hash-ref seen seen-key '()))))])))
     (define nonterm-vars (for/fold ([vars #hasheq()]) ([immed-def-name (in-list def-names)]
                                                        [immed-introducer (in-list introducers)]
                                                        [immed-space-name (in-list space-names)])
                            (if (eq? immed-space-name 'grammar)
                                (hash-set vars (syntax-e immed-def-name) immed-def-name)
                                vars)))
     (define all-vars (for/fold ([vars #hasheq()]) ([form (in-list forms)]
                                                  [t (in-list transformers)]
                                                  [space-name (in-list space-names)])
                        ((doc-transformer-extract-metavariables t) form space-name vars)))
     (define vars (let ([vars (for/fold ([vars all-vars]) ([id (in-list (syntax->list #'(~? (literal-id ... ...) ())))])
                                (hash-remove vars (syntax-e id)))])
                    (let ([vars (for/fold ([vars vars]) ([sym (in-hash-keys nonterm-vars)])
                                  (hash-remove vars sym))])
                      (for/fold ([vars vars]) ([nt-id (in-list (syntax->list #'(~? (nt-id ...) ())))])
                        (hash-remove vars (syntax-e nt-id))))))
     (define typesets (for/list ([form (in-list forms)]
                                 [t (in-list transformers)]
                                 [def-id-as-def (in-list def-id-as-defs)]
                                 [space-name (in-list space-names)])
                        ;; uses `def-id-as-def` in a context that binds `redef?`:
                        (extract-typeset t form def-id-as-def space-name)))
     (with-syntax ([(typeset ...) typesets]
                   [(kind-str ...) kind-strs])
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
                              #,(nonterm-id-transformer id id nt-def-name nt-space-name)])
                      #,@(for/list ([nt-id (in-list (syntax->list #'(~? (nt-id ...) ())))]
                                    [nt-id-key-g (in-list (syntax->list #'(~? (nt-id-key-g ...) ())))])
                           (define-values (nt-sym nt-def-name nt-space-name nt-introducer) (nt-key-ref-expand nt-id-key-g))
                           #`[#,(typeset-meta:in_space nt-id)
                              #,(nonterm-id-transformer nt-id nt-sym nt-def-name nt-space-name)])
                      [#,(typeset-meta:in_space (datum->syntax #'context '...)) (make-ellipsis-transformer)])
           (list
            (table
             boxed-style
             (insert-labels
              (list
               typeset
               ...)
              '(kind-str ...)))
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
               (define-values (nt-sym nt-def-name nt-space-name nt-introducer) (nt-key-ref-expand nt-id-key-g))
               (with-syntax ([(tmp-id) (generate-temporaries (list nt-id))])
                 (list
                  #`(define-for-syntax tmp-id
                      #,(nonterm-id-transformer nt-id nt-sym nt-def-name nt-space-name))
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
    [(parsed _ e) #'e]))
        
(define-for-syntax (make-ellipsis-transformer)
  (typeset-meta:make_Transformer
   (lambda (use-stx)
     #`(parsed #:rhombus/expr (tt "...")))))

(define (make-def-id redef? id extra-ids str-id index-str-in kind-str space extra-spaces nonterm-sym immed-space)
  (define str-id-e (syntax-e str-id))
  (cond
    [redef?
     (racketidfont
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
                     (symbol->string nonterm-sym)
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
     (define (make-content defn? [str str])
       ((if (eq? immed-space 'grammar) racketvarfont racketidfont)
        (make-id-element id str defn? #:space id-space #:suffix str+space)))
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
          (define ref-content (make-content #f index-str))
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
                                  (lambda (libs) (thing-index-desc name libs))))
                               tag
                               ref-content))))]
         [else content]))]))

(define-for-syntax (nonterm-id-transformer id sym nt-def-name nt-space-name)
  #`(make-nonterm-id-transformer
     (quote #,id)
     (quote #,sym)
     (quote-syntax #,(if (identifier? nt-def-name)
                         nt-def-name
                         (car (if (syntax? nt-def-name) (syntax-e nt-def-name) nt-def-name))))
     (quote #,(if (identifier? nt-def-name)
                  #f
                  (let ([l (if (syntax? nt-def-name) (syntax->list nt-def-name) nt-def-name)])
                    (if (pair? (cdr l))
                        (target-id-key-symbol (cadr l))
                        #f))))
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
         (make-id-element (quote-syntax #,sp-def-id) #,(symbol->string id-sym) #f
                          #:space '#,space-sym
                          #:suffix #,(if def-sub
                                         #`(list '#,def-sub '#,def-space-sym '#,(or sym id-sym))
                                         #`(list '#,def-space-sym '#,(or sym id-sym)))))))))

(define-for-syntax (make-meta-id-transformer id)
  (typeset-meta:make_Transformer
   (lambda (use-stx)
     #`(parsed #:rhombus/expr (racketvarfont #,(symbol->string (syntax-e id)))))))

(define-for-syntax (nt-key-expand nt-key-g)
  (define-values (root fields space-names)
    (syntax-parse nt-key-g
      #:datum-literals (op |.| parens group)
      [(_ root:identifier (~seq (op |.|) field:identifier) ... space:keyword)
       (values #'root
               (syntax->list #'(field ...))
               (full-space-names (string->symbol (keyword->string (syntax-e #'space)))))]
      [(_ root:identifier (~seq (op |.|) field:identifier) ...  (op |.|) (parens (group (op name:identifier))) space:keyword)
       (values #'root
               (syntax->list #'(field ... name))
               (full-space-names (string->symbol (keyword->string (syntax-e #'space)))))]
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
               (full-space-names (string->symbol (keyword->string (syntax-e #'space)))))]
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
     (values root
             space-name
             (make-intro space-name))]
    [else
     (define target+remains+space
       (resolve-name-ref space-names
                         (in-name-root-space root)
                         fields))
     (define space-name (caddr target+remains+space))
     (values (cons root (cons (car target+remains+space)
                              (cadr target+remains+space)))
             space-name
             (make-intro space-name))]))

(define-for-syntax (nt-key-ref-expand nt-key-g)
  (define-values (sym g)
    (syntax-parse nt-key-g
      #:datum-literals (op |.|)
      [(_ root (~seq (~and dot (op |.|)) field) ... name:identifier space:keyword)
       (values #'name #'(group root (~@ dot field) ... space))]
      [(_ root (~seq (~and dot (op |.|)) field) ...  name:identifier)
       (values #'name #'(group root (~@ dot field) ...))]
      [_ (values #f nt-key-g)]))
  (define-values (def-name space-name introducer) (nt-key-expand g))
  (let ([def-id (if (syntax? def-name) def-name (car def-name))]
        [introducer (if (syntax? def-name) introducer in-name-root-space)])
    (unless (identifier-label-binding (introducer def-id))
      (raise-syntax-error #f "no for-label binding the expected space" def-id)))
  (values sym def-name space-name introducer))

(define-for-syntax (extract-typeset t stx def-id-as-def space-name)
  (define (relocate to from-in from-property to-property)
    (define from (syntax-parse from-in
                   #:datum-literals (op)
                   [(op from) #'from]
                   [_ from-in]))
    (to-property (datum->syntax to
                                to
                                from)
                 (from-property from)))
  
  (define (subst name #:wrap? [wrap? #t] #:redef? [as-redef? #f])
    (cond
      [wrap?
       (define id (if (identifier? name) name (cadr (syntax->list name))))
       #`((op #,(relocate #'|#,| id syntax-raw-prefix-property syntax-raw-prefix-property))
          (#,(relocate #'parens id syntax-raw-suffix-property syntax-raw-tail-suffix-property)
           (group (parsed #:rhombus/expr
                          (let ([redef? #,as-redef?])
                            #,def-id-as-def)))))]
      [else #`(let ([redef? #,as-redef?])
                #,def-id-as-def)]))
  ((doc-transformer-extract-typeset t) stx space-name subst))

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
      [(= 1 (length strs)) (car strs)]
      [(= 2 (length strs)) (string-append (car strs) " and " (cadr strs))]
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
      #:datum-literals (group block op :: :~ =)
      [(group t . (~or* ((op (~or* :: :~ =)) . _)
                        ((block . _))
                        ()))
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
      #:datum-literals (brackets braces parens)
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
      [(parens b) (extract-binding-term #'b vars)]
      [id:identifier (add-metavariable vars #'id #f)]
      [_ vars]))
  (syntax-parse stx
    #:datum-literals (group block)
    [(~or* (group _:keyword (block g)) g)
     (extract-binding-group #'g vars)]
    [_ vars]))

(define-for-syntax (extract-group-metavariables g vars nonterm?)
  (syntax-parse g
    #:datum-literals (group)
    [(group t ...)
     (for/fold ([vars vars]) ([t (in-list (syntax->list #'(t ...)))])
       (extract-term-metavariables t vars nonterm?))]))

(define-for-syntax (extract-term-metavariables t vars nonterm?)
  (syntax-parse t
    #:datum-literals (parens brackets braces block quotes alts)
    [((~or parens brackets braces block quotes) g ...)
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
         [((~or parens brackets braces quotes block) g ...)
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
              [((~and tag (~or parens brackets braces quotes block)) g ...)
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
