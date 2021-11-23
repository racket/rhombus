#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     (prefix-in typeset-meta: "typeset_meta.rhm")
                     shrubbery/property)
         racket/list
         (only-in rhombus
                  def val fun :: |'| |.|)
         (only-in rhombus/macro
                  decl defn expr)
         (only-in "rhombus.rhm"
                  rhombusblock)
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

(provide typeset-doc)

(define-syntax (typeset-doc stx)
  (syntax-parse stx
    #:datum-literals (parens group brackets)
    [(_ (parens (group form ...) ...
                (group
                 (brackets content-group ...))))
     (define forms (syntax->list #'((group form ...) ...)))
     (define def-ids (for/list ([form (in-list forms)])
                       (define def-id (extract-defined form))
                       (unless (identifier-binding def-id #f)
                         (raise-syntax-error 'doc
                                             "identifier to document has no for-label binding"
                                             def-id))
                       def-id))
     (define wrap-defs (for/fold ([rev-wrap-defs '()] [seen #hasheq()] #:result (reverse rev-wrap-defs))
                                 ([def-id (in-list def-ids)])
                         (cond
                           [(hash-ref seen (syntax-e def-id) #f)
                            (values (cons #'(values) rev-wrap-defs)
                                    seen)]
                           [else
                            (values (cons #`(let-syntax ([#,(typeset-meta:in_space def-id)
                                                          (make-def-id-transformer (quote-syntax #,def-id))]))
                                          rev-wrap-defs)
                                    (hash-set seen (syntax-e def-id) #t))])))
     (define vars (for/fold ([vars #hasheq()]) ([form (in-list forms)])
                    (extract-metavariables form vars)))
     (define t-forms (map extract-typeset forms))
     (define kind-strs (map extract-kind-str forms))
     (with-syntax ([(t-form ...) t-forms]
                   [(t-block ...) (for/list ([t-form (in-list t-forms)])
                                    (syntax-raw-property
                                     (datum->syntax #f 'block
                                                    (syntax-parse t-form
                                                      [(_ a . _) #'a]))
                                     ""))]
                   [((wrap-def ...) ...) wrap-defs]
                   [(kind-str ...) kind-strs])
       #`(let-syntax #,(for/list ([id (in-hash-values vars)])
                         #`[#,(typeset-meta:in_space id) (make-meta-id-transformer (quote-syntax #,id))])
           (list
            (table
             boxed-style
             (insert-labels
              (list
               (wrap-def ... (rhombus-expression (group rhombusblock (t-block t-form))))
               ...)
              '(kind-str ...)))
            (rhombus-expression content-group)
            ...)))]))

(define-for-syntax (make-def-id-transformer id)
  (typeset-meta:Transformer
   (lambda (use-stx)
     #`(parsed (make-def-id (quote-syntax #,id))))))

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

(define-for-syntax (make-meta-id-transformer id)
  (typeset-meta:Transformer
   (lambda (use-stx)
     #`(parsed (racketvarfont #,(symbol->string (syntax-e id)))))))

(define-for-syntax (extract-defined stx)
  (syntax-parse stx
    #:literals (def val fun :: defn decl |.| |'|)
    #:datum-literals (parens group op)
    [(group (~or def fun) id:identifier (parens g ...) . _) #'id]
    [(group (~or def val) id:identifier . _) #'id]
    [(group (~or defn decl) (op |.|) macro (op |'|) (parens (group id t ...))) #'id]
    [_ (raise-syntax-error 'doc "unknown definition form" stx)]))

(define-for-syntax (add-metavariable vars id)
  (hash-set vars (syntax-e id) (or (hash-ref vars (syntax-e id) #f) id)))

(define-for-syntax (extract-metavariables stx vars)
  (syntax-parse stx
    #:literals (def val fun :: defn decl |.| |'|)
    #:datum-literals (parens group op)
    [(group (~or def fun) id:identifier (parens g ...) . _)
     (for/fold ([vars vars]) ([g (in-list (syntax->list #'(g ...)))])
       (extract-binding-metavariables g vars))]
    [(group (~or def val) id:identifier . _) vars]
    [(group (~or defn decl) (op |.|) macro (op |'|) (parens (group id t ...)))
     (extract-group-metavariables #'(group t ...)  vars)]
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

(define-for-syntax (extract-typeset stx)
  (syntax-parse stx
    #:literals (defn decl |.| |'|)
    #:datum-literals (parens group op macro)
    [(group (~or defn decl) (op |.|) macro (op |'|) (parens g)) #'g]
    [_ stx]))

(define-for-syntax (extract-kind-str stx)
  (syntax-parse stx
    #:literals (defn decl expr |.| |'|)
    #:datum-literals (parens group op macro)
    [(group decl . _) "declaration"]
    [(group defn . _) "definition"]
    [(group expr . _) "expression"]
    [(group (~or def fun) id:identifier (parens . _) . _) "function"]
    [_ "value"]))

(define (insert-labels l lbls)
  (cond
    [(null? l) null]
    [else
     (map
      list
      (append
       ((add-background-label (car lbls))
        (list (car l)))
       (let loop ([l (cdr l)] [lbls (cdr lbls)])
         (cond
           [(null? l) null]
           [else
            (cons
             (paragraph plain (hspace 1))
             (append ((add-background-label (car lbls))
                      (list (car l)))
                     (loop (cdr l) (cdr lbls))))]))))]))
