#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     (prefix-in typeset-meta: "typeset_meta.rhm")
                     shrubbery/property)
         racket/list
         (only-in (submod rhombus/private/import for-meta)
                  in-import-space)
         (only-in rhombus
                  def val fun :: |'| |.|)
         (only-in rhombus/macro
                  decl defn expr imp)
         (only-in "rhombus.rhm"
                  rhombusblock
                  [rhombus one-rhombus])
         (only-in rhombus/parse
                  rhombus-expression)
         (only-in scribble/manual
                  hspace
                  racketvarfont
                  racketidfont)
         (submod scribble/racket id-element)
         (only-in scribble/core
                  table
                  paragraph
                  element
                  index-element
                  toc-target2-element
                  plain)
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
         grammar)

(define-syntax (typeset-doc stx)
  (syntax-parse stx
    #:datum-literals (parens group brackets block)
    [(_ (parens (~optional (group #:literal (block (group literal-id ...) ...)))
                (group form ...) ...
                (group
                 (brackets content-group ...))))
     (define forms (syntax->list #'((group form ...) ...)))
     (define introducers (for/list ([form (in-list forms)])
                           (extract-introducer form)))
     (define def-ids (for/list ([form (in-list forms)]
                                [introducer (in-list introducers)])
                       (define def-id (extract-defined form))
                       (when def-id
                         (define s-def-id (introducer def-id))
                         (unless (identifier-binding s-def-id #f)
                           (raise-syntax-error 'doc
                                               "identifier to document has no for-label binding"
                                               s-def-id)))
                       def-id))
     (define wrap-defs (for/fold ([rev-wrap-defs '()] [seen #hasheq()] #:result (reverse rev-wrap-defs))
                                 ([def-id (in-list def-ids)]
                                  [introducer (in-list introducers)])
                         (cond
                           [(not def-id)
                            (values (cons #'(values) rev-wrap-defs)
                                    seen)]
                           [else
                            (values
                             (cons #`(let-syntax ([#,(typeset-meta:in_space def-id)
                                                   (#,(if (hash-ref seen (syntax-e def-id) #f)
                                                          #'make-redef-id-transformer
                                                          #'make-def-id-transformer)
                                                    (quote-syntax #,(introducer def-id)))]))
                                   rev-wrap-defs)
                             (hash-set seen (syntax-e def-id) #t))])))
     (define all-vars (for/fold ([vars #hasheq()]) ([form (in-list forms)])
                        (extract-metavariables form vars)))
     (define vars (for/fold ([vars all-vars]) ([id (in-list (syntax->list #'(~? (literal-id ... ...) ())))])
                    (hash-remove vars (syntax-e id))))
     (define typesets (map extract-typeset forms))
     (define kind-strs (map extract-kind-str forms))
     (with-syntax ([(typeset ...) typesets]
                   [((wrap-def ...) ...) wrap-defs]
                   [(kind-str ...) kind-strs])
       #`(let-syntax #,(for/list ([id (in-hash-values vars)])
                         #`[#,(typeset-meta:in_space id) (make-meta-id-transformer (quote-syntax #,id))])
           (list
            (table
             boxed-style
             (insert-labels
              (list
               (wrap-def ... typeset)
               ...)
              '(kind-str ...)))
            (rhombus-expression content-group)
            ...)))]))

(define-for-syntax (make-def-id-transformer id)
  (typeset-meta:Transformer
   (lambda (use-stx)
     #`(parsed (make-def-id (quote-syntax #,id))))))

(define-for-syntax (make-redef-id-transformer id)
  (typeset-meta:Transformer
   (lambda (use-stx)
     #`(parsed (make-redef-id (quote-syntax #,id))))))

(define (make-def-id id)
  (define (make-content defn?)
    (racketidfont
     (make-id-element id (symbol->string (syntax-e id)) defn?)))
  (define content (annote-exporting-library (make-content #t)))
  (define target-maker (id-to-target-maker id #t))
  (cond
    [target-maker
     (define name (syntax-e id))
     (define ref-content (make-content #f))
     (target-maker content
                   (lambda (tag)
                     (toc-target2-element
                      #f
                      (index-element
                       #f
                       content
                       tag
                       (list (datum-intern-literal (symbol->string name)))
                       (list ref-content)
                       (with-exporting-libraries
                         (lambda (libs) (thing-index-desc name libs))))
                      tag
                      ref-content)))]
    [else content]))

(define (make-redef-id id)
  (racketidfont
   (make-id-element id (symbol->string (syntax-e id)) #t)))

(define-for-syntax (make-meta-id-transformer id)
  (typeset-meta:Transformer
   (lambda (use-stx)
     #`(parsed (racketvarfont #,(symbol->string (syntax-e id)))))))

(define-for-syntax (extract-defined stx)
  (syntax-parse stx
    #:literals (def val fun :: defn decl imp |.| |'| grammar)
    #:datum-literals (parens group op modifier macro)
    [(group (~or def fun) id:identifier (parens g ...) . _) #'id]
    [(group (~or def val) id:identifier . _) #'id]
    [(group (~or defn decl) (op |.|) macro (op |'|) (parens (group id t ...))) #'id]
    [(group (~or imp) (op |.|) modifier (op |'|) (parens (group id t ...))) #'id]
    [(group grammar . _) #f]
    [_ (raise-syntax-error 'doc "unknown definition form" stx)]))

(define-for-syntax (add-metavariable vars id)
  (hash-set vars (syntax-e id) (or (hash-ref vars (syntax-e id) #f) id)))

(define-for-syntax (extract-metavariables stx vars)
  (syntax-parse stx
    #:literals (def val fun :: defn decl imp |.| |'| grammar)
    #:datum-literals (parens group op)
    [(group (~or def fun) id:identifier (parens g ...) . _)
     (for/fold ([vars vars]) ([g (in-list (syntax->list #'(g ...)))])
       (extract-binding-metavariables g vars))]
    [(group (~or def val) id:identifier . _) vars]
    [(group (~or defn decl imp) (op |.|) _ (op |'|) (parens (group id t ...)))
     (extract-group-metavariables #'(group t ...)  vars)]
    [(group grammar id (block g ...))
     (extract-groups-metavariables #'(g ...) (add-metavariable vars #'id))]
    [_ vars]))

(define-for-syntax (extract-binding-metavariables stx vars)
  (syntax-parse stx
    #:literals (def val fun ::)
    #:datum-literals (parens group op)
    [(group id (op ::) _) (add-metavariable vars #'id)]
    [_ vars]))

(define-for-syntax (extract-group-metavariables g vars)
  (syntax-parse g
    #:datum-literals (group)
    [(group t ...)
     (for/fold ([vars vars]) ([t (in-list (syntax->list #'(t ...)))])
       (extract-term-metavariables t vars))]))

(define-for-syntax (extract-term-metavariables t vars)
  (syntax-parse t
    [(tag g ...)
     #:when (memq (syntax-e #'tag) '(parens brackets braces block))
     (for/fold ([vars vars]) ([g (in-list (syntax->list #'(g ...)))])
       (extract-group-metavariables g vars))]
    [id:identifier (add-metavariable vars #'id)]
    [_ vars]))

(define-for-syntax (extract-groups-metavariables gs vars)
  (for/fold ([vars vars]) ([g (in-list (syntax->list gs))])
    (extract-group-metavariables g vars)))

(define-for-syntax (extract-typeset stx)
  (define (rb form #:options [options #'()])
    (with-syntax ([t-form form]
                  [t-block (syntax-raw-property
                            (datum->syntax #f 'block
                                           (syntax-parse form
                                             [(_ a . _) #'a]))
                            "")]
                  [(option ...) options])
      #'(rhombus-expression (group rhombusblock option ... (t-block t-form)))))
  (syntax-parse stx
    #:literals (expr defn decl imp |.| |'| grammar)
    #:datum-literals (parens group op)
    [(group (~or expr defn decl imp) (op |.|) _ (op |'|) (parens g)) (rb #'g)]
    [(group grammar id (block g ...))
     #`(typeset-grammar (rhombus-expression (group one-rhombus (parens (group id))))
                        #,@(for/list ([g (syntax->list #'(g ...))])
                             (rb g #:options #'((parens (group #:indent (block (group (parsed 0)))))))))]
    [_ (rb stx)]))

(define-for-syntax (extract-introducer stx)
  (syntax-parse stx
    #:literals (imp |.|)
    #:datum-literals (parens group op)
    [(group imp . _) in-import-space]
    [_ values]))

(define-for-syntax (extract-kind-str stx)
  (syntax-parse stx
    #:literals (defn decl expr imp |.| |'| grammar)
    #:datum-literals (parens group op macro)
    [(group decl . _) "declaration"]
    [(group defn . _) "definition"]
    [(group expr . _) "expression"]
    [(group imp . _) "import modifier"]
    [(group grammar . _) #f]
    [(group (~or def fun) id:identifier (parens . _) . _) "function"]
    [_ "value"]))

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

(define-syntax grammar "to be used in `doc`")

(define (typeset-grammar id . prods)
  (define (p c) (paragraph plain c))
  (define (sp s) (p (list (hspace 1) s (hspace 1))))
  (table
   plain
   (cons
    (list (p (hspace 2)) (p id) (sp "=") (car prods))
    (for/list ([prod (in-list (cdr prods))])
      (list (p (hspace 2)) (p "") (sp "|") prod)))))
