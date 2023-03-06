#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     shrubbery/property
                     (prefix-in typeset-meta: "typeset_meta.rhm"))
         "doc.rkt"
         "typeset-help.rkt"
         racket/list
         rhombus/private/name-root
         (only-in rhombus/private/name-root-space
                  in-name-root-space)
         (only-in rhombus
                  :: |.| $
                  [= rhombus-=])
         (only-in "rhombus.rhm"
                  rhombusblock_etc
                  [rhombus one-rhombus])
         (only-in rhombus/parse
                  rhombus-expression)
         (only-in scribble/manual
                  hspace)
         (only-in scribble/core
                  table
                  paragraph
                  element
                  plain
                  style
                  table-cells))

(provide (for-space rhombus/namespace
                    space_meta_clause)
         (for-space rhombus/doc
                    grammar))

(define-name-root space_meta_clause
  #:fields
  ())

(define-syntax (define-doc stx)
  (syntax-parse stx
    [(_ id
        desc
        space-sym
        extract-name
        extract-metavariables
        extract-typeset)
     #'(begin
         (provide (for-space rhombus/doc id))
         (define-doc-syntax id
           (make-doc-transformer #:extract-desc (lambda (stx) desc)
                                 #:extract-space-sym (lambda (stx) 'space-sym)
                                 #:extract-name extract-name
                                 #:extract-metavariables extract-metavariables
                                 #:extract-typeset extract-typeset)))]))

(begin-for-syntax
  (define-splicing-syntax-class (identifier-target space-name)
    #:attributes (name)
    #:datum-literals (|.| op)
    (pattern (~seq root:identifier (~seq (op |.|) field:identifier) ...)
             #:do [(define target+remains (resolve-name-ref space-name
                                                            (in-name-root-space #'root)
                                                            (syntax->list #'(field ...))))]
             #:when target+remains
             #:attr name (datum->syntax #f (list #'root (car target+remains))))
    (pattern (~seq name:identifier)))
  (define-splicing-syntax-class (target space-name)
    #:attributes (name)
    #:datum-literals (|.| op parens group)
    (pattern (~seq root:identifier (~seq (op |.|) field:identifier) ... (op |.|) ((~and ptag parens) (group (op opname))))
             #:do [(define target+remains (resolve-name-ref space-name
                                                            (in-name-root-space #'root)
                                                            (syntax->list #'(field ... opname))
                                                            #:parens #'ptag))]
             #:when target+remains
             #:attr name (datum->syntax #f (list #'root (car target+remains))))
    (pattern (~seq (op id:identifier))
             #:attr name #'id)
    (pattern (~seq (~var id (identifier-target space-name)))
             #:attr name #'id.name)))

(define-for-syntax (head-extract-name stx space-name)
  (syntax-parse stx
    #:datum-literals (group)
    [(group _ (~var id (identifier-target space-name)) . _) #'id.name]))

(define-for-syntax (parens-extract-name stx space-name)
  (syntax-parse stx
    #:datum-literals (group parens)
    [(group _ (~var id (identifier-target space-name)) (parens . _) . _) #'id.name]))

(define-for-syntax (parens-extract-metavariables stx space-name vars)
  (syntax-parse stx
    #:datum-literals (parens group)
    [(group _ (~var id (identifier-target space-name)) (parens g ...) . _)
     (for/fold ([vars vars]) ([g (in-list (syntax->list #'(g ...)))])
       (extract-binding-metavariables g vars))]))

(define-for-syntax (identifier-macro-extract-name stx space-name)
  (syntax-parse stx
    #:datum-literals (group op quotes)
    [(group _ _ _ (quotes (group (~var id (identifier-target space-name)) . _))) #'id.name]
    [(group _ _ _ (quotes (~var id (identifier-target space-name)))) #'id.name]))

(define-for-syntax (operator-macro-extract-name stx space-name)
  (syntax-parse stx
    #:datum-literals ($ group op quotes)
    [(group _ _ _ (quotes (group (op $) _:identifier (~var id (target space-name)) . _))) #'id.name]
    [(group _ _ _ (quotes (group (~var id (target space-name)) . _))) #'id.name]
    [_ ((identifier-macro-extract-name space-name) stx)]))

(define-for-syntax (space-extract-name stx space-name)
  (syntax-parse stx
    #:datum-literals (group)
    [(group _ _ _ (~var id (identifier-target space-name))) #'id.name]))

(define-for-syntax (head-extract-metavariables stx space-name vars)
  vars)

(define-for-syntax (identifier-macro-extract-metavariables stx space-name vars)
  (syntax-parse stx
    #:datum-literals (group op quotes)
    [(group _ _ _ (quotes (group (~var _ (identifier-target space-name)) t ...)))
     (extract-pattern-metavariables #'(group t ...) vars)]
    [(group _ _ _ (quotes (~var _ (identifier-target space-name))))
     vars]))

(define-for-syntax (operator-macro-extract-metavariables stx space-name vars)
  (syntax-parse stx
    #:datum-literals ($ group op quotes)
    [(group _ _ _ (quotes (group (op $) t0:identifier (~var _ (target space-name)) t ...)))
     (extract-pattern-metavariables #'(group (op $) t0 t ...) vars)]
    [(group _ _ _ (quotes (group (~var _ (target space-name)) t ...)))
     (extract-pattern-metavariables #'(group t ...) vars)]
    [_ ((identifier-macro-extract-metavariables space-name) stx vars)]))

(define-for-syntax (space-extract-typeset stx space-name subst)
  (syntax-parse stx
    #:datum-literals (group)
    [(group _ _ _ (~var id (identifier-target space-name)) e ...)
     (rb #:at stx
         #`(group #,@(subst #'id.name) e ...))]))

(define-for-syntax (head-extract-typeset stx space-name subst)
  (syntax-parse stx
    #:datum-literals (group)
    [(group tag (~var id (identifier-target space-name)) e ...)
     (rb #:at stx
         #`(group tag #,@(subst #'id.name) e ...))]))

(define-for-syntax (identifier-macro-extract-typeset stx space-name subst)
  (syntax-parse stx
    #:datum-literals ($ group op quotes)
    [(group _ _ _ (quotes (~and g (group (~var id (identifier-target space-name)) e ...))))
     (rb #:at #'g
         #:pattern? #t
         #`(group #,@(subst #'id.name) e ...))]
    [(group _ _ _ (quotes (~var id (identifier-target space-name))))
     #`(paragraph plain #,(subst #'id.name))]))

(define-for-syntax (operator-macro-extract-typeset stx space-name subst)
  (syntax-parse stx
    #:datum-literals ($ group op quotes)
    [(group _ _ _ (quotes (~and g (group (~and $0 (op $)) e0:identifier (~var id (target space-name)) e ...))))
     (rb #:at #'g
         #:pattern? #t
         #`(group $0 e0 #,@(subst #'id.name) e ...))]
    [(group _ _ _ (quotes (~and g (group (~var id (target space-name)) e ...))))
     (rb #:at #'g
         #:pattern? #t
         #`(group #,@(subst #'id.name) e ...))]
    [_ ((identifier-macro-extract-typeset space-name) stx subst)]))

(define-doc space.enforest
  "space"
  rhombus/space
  space-extract-name
  (lambda (stx space-name vars) vars)
  space-extract-typeset)

(define-doc space.transform
  "space"
  rhombus/space
  space-extract-name
  (lambda (stx space-name vars) vars)
  space-extract-typeset)

(define-doc decl.nestable_macro
  "nestable declaration"
  #f
  identifier-macro-extract-name
  identifier-macro-extract-metavariables
  identifier-macro-extract-typeset)

(define-doc decl.macro
  "declaration"
  #f
  identifier-macro-extract-name
  identifier-macro-extract-metavariables
  identifier-macro-extract-typeset)

(define-doc defn.macro
  "definition"
  #f
  identifier-macro-extract-name
  identifier-macro-extract-metavariables
  identifier-macro-extract-typeset)

(define-doc expr.macro
  "expression"
  #f
  operator-macro-extract-name
  operator-macro-extract-metavariables
  operator-macro-extract-typeset)

(define-doc impo.modifier
  "import modifier"
  rhombus/impo
  operator-macro-extract-name
  operator-macro-extract-metavariables
  operator-macro-extract-typeset)

(define-doc modpath.macro
  "module path"
  rhombus/modpath
  operator-macro-extract-name
  operator-macro-extract-metavariables
  operator-macro-extract-typeset)

(define-doc expo.modifier
  "export modifier"
  rhombus/expo
  identifier-macro-extract-name
  identifier-macro-extract-metavariables
  identifier-macro-extract-typeset)

(define-doc impo.macro
  "import"
  rhombus/impo
  operator-macro-extract-name
  operator-macro-extract-metavariables
  operator-macro-extract-typeset)

(define-doc expo.macro
  "export"
  rhombus/expo
  operator-macro-extract-name
  operator-macro-extract-metavariables
  operator-macro-extract-typeset)

(define-doc bind.macro
  "binding operator"
  rhombus/bind
  operator-macro-extract-name
  operator-macro-extract-metavariables
  operator-macro-extract-typeset)

(define-doc annot.macro
  "annotation"
  rhombus/annot
  operator-macro-extract-name
  operator-macro-extract-metavariables
  operator-macro-extract-typeset)

(define-doc repet.macro
  "repetition"
  rhombus/repet
  operator-macro-extract-name
  operator-macro-extract-metavariables
  operator-macro-extract-typeset)

(define-doc reducer.macro
  "reducer"
  rhombus/reducer
  identifier-macro-extract-name
  identifier-macro-extract-metavariables
  identifier-macro-extract-typeset)

(define-doc for_clause.macro
  "for clause"
  rhombus/for_clause
  identifier-macro-extract-name
  identifier-macro-extract-metavariables
  identifier-macro-extract-typeset)

(define-doc class_clause.macro
  "class clause"
  rhombus/class_clause
  identifier-macro-extract-name
  identifier-macro-extract-metavariables
  identifier-macro-extract-typeset)

(define-doc interface_clause.macro
  "interface clause"
  rhombus/interface_clause
  identifier-macro-extract-name
  identifier-macro-extract-metavariables
  identifier-macro-extract-typeset)

(define-doc entry_point.macro
  "entry point"
  rhombus/entry_point
  identifier-macro-extract-name
  identifier-macro-extract-metavariables
  identifier-macro-extract-typeset)

(define-doc unquote_bind.macro
  "unquote binding"
  rhombus/unquote_bind
  operator-macro-extract-name
  operator-macro-extract-metavariables
  operator-macro-extract-typeset)

(define-doc syntax_class_clause.macro
  "syntax class clause"
  rhombus/syntax_class_clause
  identifier-macro-extract-name
  identifier-macro-extract-metavariables
  identifier-macro-extract-typeset)

(define-doc pattern_clause.macro
  "pattern clause"
  rhombus/pattern_clause
  identifier-macro-extract-name
  identifier-macro-extract-metavariables
  identifier-macro-extract-typeset)

(define-doc space_clause.macro
  "space clause"
  rhombus/space_clause
  identifier-macro-extract-name
  identifier-macro-extract-metavariables
  identifier-macro-extract-typeset)

(define-doc space_meta_clause.macro
  "space meta clause"
  rhombus/space_meta_clause
  identifier-macro-extract-name
  identifier-macro-extract-metavariables
  identifier-macro-extract-typeset)

(define-doc fun
  "function"
  #f
  parens-extract-name
  parens-extract-metavariables
  head-extract-typeset)

(define-doc def
  "function"
  #f
  head-extract-name
  head-extract-metavariables
  head-extract-typeset)
  
(define-doc operator
  "operator"
  #f
  (lambda (stx space-name)
    (syntax-parse stx
      #:datum-literals (group parens)
      [(group _ (parens (group (~var id (target space-name)) arg1)) . _) #'id.name]
      [(group _ (parens (group arg1 (~var id (target space-name)) arg2)) . _) #'id.name]))
  (lambda (stx space-name vars)
    (syntax-parse stx
      #:datum-literals (group parens)
      [(group _ (parens (group (~var _ (target space-name)) arg)) . _)
       (extract-binding-metavariables #'(group arg) vars)]
      [(group _ (parens (group arg0 (~var _ (target space-name)) arg1)) . _)
       (define vars0 (extract-binding-metavariables #'(group arg0) vars))
       (extract-binding-metavariables #'(group arg1) vars0)]))
  (lambda (stx space-name subst)
    (syntax-parse stx
      #:datum-literals (group parens)
      [(group tag ((~and p-tag parens) ((~and g-tag group) (~var id (target space-name)) arg)) e ...)
       (rb #:at stx
           #`(group tag (p-tag (g-tag #,@(subst #'id.name) arg)) e ...))]
      [(group tag ((~and p-tag parens) ((~and g-tag group) arg0 (~var id (target space-name)) arg1)) e ...)
       (rb #:at stx
           #`(group tag (p-tag (g-tag arg0 #,@(subst #'id.name) arg1)) e ...))])))

(define-doc syntax_class
  "syntax class"
  rhombus/stxclass
  (lambda (stx space-name)
    (syntax-parse stx
      #:datum-literals (group)
      [(group _ (~var id (identifier-target space-name)) . _) #'id.name]))
  (lambda (stx space-name vars)
    (syntax-parse stx
      #:datum-literals (group parens)
      [(group _ (~var _ (identifier-target space-name)) (parens g ...) . _)
       (for/fold ([vars vars]) ([g (in-list (syntax->list #'(g ...)))])
         (extract-binding-metavariables g vars))]
      [_ vars]))
  (lambda (stx space-name subst)
    (syntax-parse stx
      #:datum-literals (group)
      [(group _ (~var id (identifier-target space-name)) e ...)
       (rb #:at stx
           #`(group #,@(subst #'id.name) e ...))])))

(define-doc class
  "class"
  rhombus/class
  parens-extract-name
  parens-extract-metavariables
  head-extract-typeset)

(define-doc interface
  "interface"
  rhombus/class
  head-extract-name
  head-extract-metavariables
  head-extract-typeset)

(define-doc-syntax grammar
  (make-doc-transformer
   #:extract-desc (lambda (stx) #f)
   #:extract-space-sym (lambda (stx) 'grammar)
   #:extract-name (lambda (stx space-name)
                    (syntax-parse stx
                      #:datum-literals (group)
                      [(group _ id . _) #'id]))
   #:extract-metavariables
   (lambda (stx space-name vars)
     (syntax-parse stx
       #:datum-literals (group)
       [(group _ id) vars]
       [(group _ id b)
        (extract-pattern-metavariables #'(group b) vars)]))
   #:extract-typeset
   (lambda (stx space-name subst)
     (syntax-parse stx
       #:datum-literals (group)
       [(group grammar id)
        #`(paragraph plain #,(subst #'id #:wrap? #f))]
       [(group grammar id (block g ...))
        #`(typeset-grammar #,(subst #'id #:wrap? #f)
                           #,@(for/list ([g (in-list (syntax->list #'(g ...)))])
                                (syntax-parse g
                                  #:datum-literals (group)
                                  [(group t ...)
                                   (rb #'(group t ...)
                                       #:at g
                                       #:pattern? #t
                                       #:options #'((parens (group #:inset (block (group (parsed #f)))))))])))]))))

;; ----------------------------------------

(define-for-syntax (add-metavariable vars id)
  (hash-set vars (syntax-e id) (or (hash-ref vars (syntax-e id) #f) id)))

(define-for-syntax (extract-binding-metavariables stx vars)
  (syntax-parse stx
    #:literals (:: rhombus-=)
    #:datum-literals (parens group op)
    [(group lhs (op ::) . _) (extract-binding-metavariables #'(group lhs) vars)]
    [(group lhs (op rhombus-=) . _) (extract-binding-metavariables #'(group lhs) vars)]
    [(group (parens g)) (extract-binding-metavariables #'g vars)]
    [(group id:identifier) (add-metavariable vars #'id)]
    [_ vars]))

(define-for-syntax (extract-group-metavariables g vars)
  (syntax-parse g
    #:datum-literals (group)
    [(group t ...)
     (for/fold ([vars vars]) ([t (in-list (syntax->list #'(t ...)))])
       (extract-term-metavariables t vars))]))

(define-for-syntax (extract-term-metavariables t vars)
  (syntax-parse t
    #:datum-literals (parens brackets braces block quotes alts)
    [((~or parens brackets braces block quotes) g ...)
     (for/fold ([vars vars]) ([g (in-list (syntax->list #'(g ...)))])
       (extract-group-metavariables g vars))]
    [((~datum alts) b ...)
     (for/fold ([vars vars]) ([b (in-list (syntax->list #'(b ...)))])
       (extract-term-metavariables b vars))]
    [id:identifier
     (if (identifier-binding (typeset-meta:in_space #'id))
         vars
         (add-metavariable vars #'id))]
    [_ vars]))

(define-for-syntax (extract-pattern-metavariables g vars)
  (syntax-parse g
    #:datum-literals (group block)
    [(group t ...)
     (for/fold ([vars vars] [after-$? #f] #:result vars) ([t (in-list (syntax->list #'(t ...)))])
       (syntax-parse t
         #:datum-literals (op parens brackets braces block quotes alts)
         #:literals ($)
         [(op $) (values vars #t)]
         [_:identifier (if after-$?
                           (values (extract-term-metavariables t vars) #f)
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

(define-for-syntax (rb form
                       #:at [at-form form]
                       #:pattern? [pattern? #f]
                       #:options [options #'((parens (group #:inset (block (group (parsed #f))))))])
  (with-syntax ([t-form (if pattern?
                            (drop-pattern-escapes form)
                            form)]
                [t-block (syntax-raw-property
                          (datum->syntax #f 'block
                                         (syntax-parse at-form
                                           #:datum-literals (op)
                                           [(_ (op a) . _) #'a]
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
              #:datum-literals (op parens brackets braces quotes block alts)
              #:literals ($)
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

(define (typeset-grammar id . prods)
  (define (p c) (paragraph plain c))
  (define (sp s) (p (list (hspace 1) s (hspace 1))))
  (table
   (style #f (list (table-cells (for/list ([prod (in-list prods)])
                                  (define bl (style #f '(top)))
                                  (list bl bl bl)))))
   (cons
    (list (p id) (sp "=") (car prods))
    (for/list ([prod (in-list (cdr prods))])
      (list (p "") (sp "|") prod)))))
