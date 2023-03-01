#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/syntax-local
                     enforest/hier-name-parse
                     (prefix-in typeset-meta: "typeset_meta.rhm")
                     shrubbery/property
                     rhombus/private/name-path-op)
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

(provide typeset-doc)

(define-syntax (typeset-doc stx)
  (syntax-parse stx
    #:datum-literals (parens group brackets block)
    [(_ context
        (parens (~optional (group #:literal (block (group literal-id ...) ...)))
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
                           (if space-name
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
                           (define def-id (if (identifier? def-name)
                                              (introducer def-name)
                                              (in-name-root-space (car (syntax-e def-name)))))
                           (unless (identifier-binding def-id #f)
                             (raise-syntax-error 'doc
                                                 "identifier to document has no for-label binding"
                                                 def-id)))
                         def-name))
     (define def-id-as-defs (for/fold ([rev-as-defs '()] [seen #hash()] #:result (reverse rev-as-defs))
                                      ([def-name (in-list def-names)]
                                       [introducer (in-list introducers)]
                                       [space-name (in-list space-names)])
                              (cond
                                [(not def-name)
                                 (values (cons #f rev-as-defs)
                                         seen)]
                                [else
                                 (define def-id (if (identifier? def-name)
                                                    (introducer def-name)
                                                    (in-name-root-space (car (syntax-e def-name)))))
                                 (define str-id (if (identifier? def-name)
                                                    #f
                                                    (cadr (syntax->list def-name))))
                                 (define sym-path (if str-id
                                                      (syntax-e str-id)
                                                      null))
                                 (define seen-key (cons (cons (syntax-e def-id) sym-path)
                                                        space-name))
                                 (values
                                  (cons #`(defining-element
                                            #f
                                            (#,(if (hash-ref seen seen-key #f)
                                                   #'make-redef-id
                                                   #'make-def-id)
                                             (quote-syntax #,def-id)
                                             (quote-syntax #,str-id)
                                             (quote #,space-name)))
                                        rev-as-defs)
                                  (hash-set seen seen-key #t))])))
     (define all-vars (for/fold ([vars #hasheq()]) ([form (in-list forms)]
                                                    [t (in-list transformers)]
                                                    [space-name (in-list space-names)])
                        ((doc-transformer-extract-metavariables t) form space-name vars)))
     (define vars (for/fold ([vars all-vars]) ([id (in-list (syntax->list #'(~? (literal-id ... ...) ())))])
                    (hash-remove vars (syntax-e id))))
     (define typesets (for/list ([form (in-list forms)]
                                 [t (in-list transformers)]
                                 [def-id-as-def (in-list def-id-as-defs)]
                                 [space-name (in-list space-names)])
                        (extract-typeset t form def-id-as-def space-name)))
     (with-syntax ([(typeset ...) typesets]
                   [(kind-str ...) kind-strs])
       #`(let-syntax (#,@(for/list ([id (in-hash-values vars)])
                           #`[#,(typeset-meta:in_space id) (make-meta-id-transformer (quote-syntax #,id))])
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

(define-for-syntax (make-ellipsis-transformer)
  (typeset-meta:make_Transformer
   (lambda (use-stx)
     #`(parsed (tt "...")))))

(define (make-def-id id str-id space)
  (define str-id-e (syntax-e str-id))
  (define str (shrubbery-syntax->string (if str-id-e
                                            str-id
                                            id)))
  (define str+space (if str-id-e
                        (list str-id-e space)
                        space))
  (define id-space (if str-id-e
                       ;; referring to a field of a namespace, so
                       ;; `id` is bound in the namespace space, not
                       ;; in `space`
                       'rhombus/namespace
                       space))
  (define (make-content defn?)
    (racketidfont
     (make-id-element id str defn? #:space id-space #:suffix str+space)))
  (define content (annote-exporting-library (make-content #t)))
  (define target-maker (id-to-target-maker id #t #:space id-space #:suffix str+space))
  (cond
    [target-maker
     (define name (string->symbol str))
     (define ref-content (make-content #f))
     (target-maker content
                   (lambda (tag)
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
                      ref-content)))]
    [else content]))

(define (make-redef-id id str-id space)
  (define str-id-e (syntax-e str-id))
  (racketidfont
   (make-id-element id (shrubbery-syntax->string (if str-id-e str-id id)) #t
                    ;; FIXME: this should be `#:suffix`, not `#:space`
                    #:space (if str-id-e
                                (list str-id-e space)
                                space))))

(define-for-syntax (make-meta-id-transformer id)
  (typeset-meta:make_Transformer
   (lambda (use-stx)
     #`(parsed (racketvarfont #,(symbol->string (syntax-e id)))))))

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
  
  (define (subst name)
    (define id (if (identifier? name) name (cadr (syntax->list name))))
    #`((op #,(relocate #'|#,| id syntax-raw-prefix-property syntax-raw-prefix-property))
       (#,(relocate #'parens id syntax-raw-suffix-property syntax-raw-tail-suffix-property)
        (group (parsed #,def-id-as-def)))))
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
