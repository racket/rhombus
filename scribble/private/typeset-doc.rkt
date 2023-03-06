#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/syntax-local
                     enforest/hier-name-parse
                     (prefix-in typeset-meta: "typeset_meta.rhm")
                     shrubbery/property
                     rhombus/private/name-path-op
                     "add-space.rkt")
         "doc.rkt"
         (submod "doc.rkt" for-class)
         "typeset-help.rkt"
         "defining-element.rkt"
         shrubbery/print
         racket/list
         (only-in rhombus/private/name-root-ref
                  name-root-ref)
         (only-in rhombus/private/name-root-space
                  in-name-root-space
                  name-root-quote)
         (only-in (submod rhombus/private/module-path for-meta)
                  modpath)
         (only-in "rhombus.rhm"
                  rhombusblock_etc
                  [rhombus one-rhombus])
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
         define-nonterminal)

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
     (define space-names (for/list ([form (in-list forms)]
                                    [t (in-list transformers)])
                           ((doc-transformer-extract-space-sym t) form)))
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
                                                   "identifier to document has no for-label binding"
                                                   def-id))))
                         def-name))
     (define-values (nt-def-name nt-space-name nt-introducer)
       (if (attribute nt-key-g)
           (nt-key-expand #'nt-key-g)
           (values (car def-names) (car space-names) (car introducers))))
     (define def-id-as-defs (for/fold ([rev-as-defs '()] [seen #hash()] #:result (reverse rev-as-defs))
                                      ([immed-def-name (in-list def-names)]
                                       [immed-introducer (in-list introducers)]
                                       [immed-space-name (in-list space-names)])
                              (cond
                                [(not immed-def-name)
                                 (values (cons #f rev-as-defs)
                                         seen)]
                                [else
                                 (define def-name (if (eq? immed-space-name 'grammar)
                                                      ;; use key of a grammar non-terminal
                                                      nt-def-name
                                                      immed-def-name))
                                 (define introducer (if (eq? immed-space-name 'grammar)
                                                        nt-introducer
                                                        immed-introducer))
                                 (define space-name (if (eq? immed-space-name 'grammar)
                                                        nt-space-name
                                                        immed-space-name))
                                 (define def-id (if (identifier? def-name)
                                                    (introducer def-name)
                                                    (in-name-root-space (car (syntax-e def-name)))))
                                 (define str-id (if (identifier? def-name)
                                                    #f
                                                    (cadr (syntax->list def-name))))
                                 (define sym-path (cons
                                                   (if str-id
                                                       (syntax-e str-id)
                                                       null)
                                                   (if (eq? immed-space-name 'grammar)
                                                       (syntax-e immed-def-name)
                                                       null)))
                                 (define seen-key (cons (cons (syntax-e def-id) sym-path)
                                                        space-name))
                                 (define typeset-id #`(#,(if (hash-ref seen seen-key #f)
                                                             #'make-redef-id
                                                             #'make-def-id)
                                                       (quote-syntax #,def-id)
                                                       (quote-syntax #,str-id)
                                                       (quote #,space-name)
                                                       (quote #,(and (eq? immed-space-name 'grammar)
                                                                     immed-def-name))
                                                       (quote #,immed-space-name)))
                                 (values
                                  (cons (if (eq? immed-space-name 'grammar)
                                            typeset-id
                                            #`(defining-element
                                                #f
                                                #,typeset-id))
                                        rev-as-defs)
                                  (hash-set seen seen-key #t))])))
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
                        (extract-typeset t form def-id-as-def space-name)))
     (with-syntax ([(typeset ...) typesets]
                   [(kind-str ...) kind-strs])
       #`(let-syntax (#,@(for/list ([id (in-hash-values vars)])
                           #`[#,(typeset-meta:in_space id) (make-meta-id-transformer (quote-syntax #,id))])
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
         #,@(for/list ([nt-id (in-list (syntax->list #'(nt-id ...)))]
                       [nt-id-key-g (in-list (syntax->list #'(nt-id-key-g ...)))])
              (define-values (nt-sym nt-def-name nt-space-name nt-introducer) (nt-key-ref-expand nt-id-key-g))
              #`(define-syntax #,(typeset-meta:in_space nt-id)
                  #,(nonterm-id-transformer nt-id nt-sym nt-def-name nt-space-name))))]))

(define-for-syntax (make-ellipsis-transformer)
  (typeset-meta:make_Transformer
   (lambda (use-stx)
     #`(parsed (tt "...")))))

(define (make-def-id id str-id space nonterm-sym immed-space)
  (define str-id-e (syntax-e str-id))
  (define str (if (eq? immed-space 'grammar)
                  (symbol->string nonterm-sym)
                  (shrubbery-syntax->string (if str-id-e
                                                str-id
                                                id))))
  (define nonterm-suffix (if (eq? immed-space 'grammar)
                             (list nonterm-sym)
                             null))
  (define str+space (cond
                      [str-id-e
                       (append (list str-id-e space)
                               nonterm-suffix)]
                      [(null? nonterm-suffix) space]
                      [else (cons space nonterm-suffix)]))
  (define id-space (if str-id-e
                       ;; referring to a field of a namespace, so
                       ;; `id` is bound in the namespace space, not
                       ;; in `space`
                       'rhombus/namespace
                       space))
  (define (make-content defn?)
    ((if (eq? immed-space 'grammar) racketvarfont racketidfont)
     (make-id-element id str defn? #:space id-space #:suffix str+space)))
  (define content (annote-exporting-library (make-content #t)))
  (define target-maker (id-to-target-maker id #t #:space id-space #:suffix str+space))
  (cond
    [target-maker
     (define name (string->symbol str))
     (define ref-content (make-content #f))
     (target-maker content
                   (lambda (tag)
                     (if nonterm-sym
                         (target-element
                          #f
                          content
                          tag)
                         (toc-target2-element
                          #f
                          (index-element
                           #f
                           content
                           tag
                           (list (datum-intern-literal str))
                           (list ref-content)
                           (with-exporting-libraries
                             (lambda (libs) (thing-index-desc name libs))))
                          tag
                          ref-content))))]
    [else content]))

(define (make-redef-id id str-id space nonterm-sym immed-space)
  (define str-id-e (syntax-e str-id))
  (racketidfont
   (make-id-element id (shrubbery-syntax->string (if str-id-e str-id id)) #t
                    #:space space
                    #:suffix (if str-id-e
                                 (list str-id-e space)
                                 space))))

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
                        (cadr l)
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
        (racketvarfont
         (make-id-element (quote-syntax #,sp-def-id) #,(symbol->string id-sym) #f
                          #:space '#,space-sym
                          #:suffix #,(if def-sub
                                         #`(list '#,def-sub '#,def-space-sym '#,(or sym id-sym))
                                         #`(list '#,def-space-sym '#,(or sym id-sym)))))))))

(define-for-syntax (make-meta-id-transformer id)
  (typeset-meta:make_Transformer
   (lambda (use-stx)
     #`(parsed (racketvarfont #,(symbol->string (syntax-e id)))))))

(define-for-syntax (nt-key-expand nt-key-g)
  (define-values (root fields space-name)
    (syntax-parse nt-key-g
      #:datum-literals (op |.| parens group)
      [(_ root:identifier (~seq (op |.|) field:identifier) ... space:keyword)
       (values #'root
               (syntax->list #'(field ...))
               (full-space-name (string->symbol (keyword->string (syntax-e #'space)))))]
      [(_ root:identifier (~seq (op |.|) field:identifier) ...  (op |.|) (parens (group (op name:identifier))) space:keyword)
       (values #'root
               (syntax->list #'(field ... name))
               (full-space-name (string->symbol (keyword->string (syntax-e #'space)))))]
      [(_ root:identifier (~seq (op |.|) field:identifier) ...)
       (values #'root
               (syntax->list #'(field ...))
               #f)]
      [(_ root:identifier (~seq (op |.|) field:identifier) ... (op |.|) (parens (group (op name:identifier))))
       (values #'root
               (syntax->list #'(field ... name))
               #f)]
      [(_ (op name:identifier) space:keyword)
       (values #'name
               '()
               (full-space-name (string->symbol (keyword->string (syntax-e #'space)))))]
      [(_ (op name:identifier))
       (values #'name
               '()
               #f)]))
  (values (cond
            [(null? fields) root]
            [else
             (define target+remains
               (resolve-name-ref space-name
                                 (in-name-root-space root)
                                 fields))
             (cons root target+remains)])
          space-name
          (if space-name
              (make-interned-syntax-introducer space-name)
              (lambda (x) x))))

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
  
  (define (subst name #:wrap? [wrap? #t])
    (cond
      [wrap?
       (define id (if (identifier? name) name (cadr (syntax->list name))))
       #`((op #,(relocate #'|#,| id syntax-raw-prefix-property syntax-raw-prefix-property))
          (#,(relocate #'parens id syntax-raw-suffix-property syntax-raw-tail-suffix-property)
           (group (parsed #,def-id-as-def))))]
      [else def-id-as-def]))
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
