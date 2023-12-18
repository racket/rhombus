#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         (rename-in "typeset-doc.rkt"
                    [doc-typeset-rhombusblock rb])
         "typeset-help.rkt"
         rhombus/private/name-root
         (only-in rhombus/private/name-root-space
                  in-name-root-space)
         (submod "rhombus-spacer.rhm" for_doc) ; for spacer bindings
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
        namespace
        desc
        space-sym ; id, #f, or expression for procedure
        extract-name
        extract-metavariables
        extract-typeset)
     #`(begin
         (provide (for-space rhombus/doc id)
                  #,@(if (syntax-e #'namespace)
                         #'((for-space rhombus/namespace
                                       namespace))
                         #'()))
         #,@(if (syntax-e #'namespace)
                #'((require (only-space-in rhombus/namespace
                                           (rename-in rhombus/meta
                                                      [namespace namespace]))))
                #'())
         (define-doc-syntax id
           (make-doc-transformer #:extract-desc (lambda (stx) desc)
                                 #:extract-space-sym #,(if (or (identifier? #'space-sym)
                                                               (boolean? (syntax-e #'space-sym)))
                                                           #'(lambda (stx) 'space-sym)
                                                           #'space-sym)
                                 #:extract-name extract-name
                                 #:extract-metavariables extract-metavariables
                                 #:extract-typeset extract-typeset)))]
    [(_ id
        desc
        space-sym
        extract-name
        extract-metavariables
        extract-typeset)
     #'(define-doc id #f
         desc
         space-sym
         extract-name
         extract-metavariables
         extract-typeset)]))

(begin-for-syntax
  (define-splicing-syntax-class (identifier-target space-name)
    #:attributes (name)
    #:datum-literals (|.| op)
    (pattern (~seq root:identifier (~seq (op |.|) field:identifier) ...)
             #:do [(define target+remains+space (resolve-name-ref (list space-name)
                                                                  (in-name-root-space #'root)
                                                                  (syntax->list #'(field ...))))]
             #:when target+remains+space
             #:attr name (datum->syntax #f (list #'root (car target+remains+space))))
    (pattern (~seq name:identifier)))
  (define-splicing-syntax-class (target space-name)
    #:attributes (name)
    #:datum-literals (|.| op parens group)
    (pattern (~seq root:identifier (~seq (op |.|) field:identifier) ... (op |.|) ((~and ptag parens) (group (op opname))))
             #:do [(define target+remains+space (resolve-name-ref (list space-name)
                                                                  (in-name-root-space #'root)
                                                                  (syntax->list #'(field ... opname))
                                                                  #:parens #'ptag))]
             #:when target+remains+space
             #:attr name (datum->syntax #f (list #'root (car target+remains+space))))
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

(define-for-syntax (parens-extract-metavariables stx space-name vars #:just-parens? [just-parens? #f])
  (define (extract-groups stx)
    (define-syntax-class maybe-kw-opt
      #:attributes (g)
      #:datum-literals (group block)
      (pattern (group _:keyword (block :maybe-opt)))
      (pattern :maybe-opt))
    (define-syntax-class maybe-opt
      #:datum-literals (group block op =)
      (pattern ((~and tag group) t ... (op =) . _)
        #:with g #'(tag t ...))
      (pattern ((~and tag group) t ... (block . _))
        #:with g #'(tag t ...))
      (pattern g))
    (define-syntax-class and
      #:datum-literals (group op &)
      (pattern ((~and tag group) (op &) . g)))
    (define-syntax-class kw-and
      #:datum-literals (group op ~&)
      (pattern ((~and tag group) (op ~&) . g)))
    (syntax-parse stx
      #:datum-literals (group op [ooo ...])
      [(~or* (~and (pre-g ... dot-g (group (op ooo)))
                   (~parse (kw-opt-g:maybe-kw-opt ...) #'(pre-g ...))
                   (~parse gs #'(kw-opt-g.g ... dot-g)))
             (~and (pre-g ... and:and (~optional kw-and:kw-and))
                   (~parse (kw-opt-g:maybe-kw-opt ...) #'(pre-g ...))
                   (~parse gs #'(kw-opt-g.g ... (and.tag . and.g) (~? (kw-and.tag . kw-and.g)))))
             (~and (pre-g ... kw-and:kw-and (~optional and:and))
                   (~parse (kw-opt-g:maybe-kw-opt ...) #'(pre-g ...))
                   (~parse gs #'(kw-opt-g.g ... (kw-and.tag . kw-and.g) (~? (and.tag . and.g)))))
             (~and (kw-opt-g:maybe-kw-opt ...)
                   (~parse gs #'(kw-opt-g.g ...))))
       (for/fold ([vars vars]) ([g (in-list (syntax->list #'gs))])
         (extract-binding-metavariables g vars))]))
  (if just-parens?
      (syntax-parse stx
        #:datum-literals (parens)
        [(parens . gs)
         (extract-groups #'gs)])
      (syntax-parse stx
        #:datum-literals (parens group)
        [(group _ (~var _ (identifier-target space-name)) (parens . gs) . _)
         (extract-groups #'gs)])))

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
    [_ (identifier-macro-extract-name stx space-name)]))

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
    [_ (identifier-macro-extract-metavariables stx space-name vars)]))

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
    [_ (identifier-macro-extract-typeset stx space-name subst)]))

(define-doc space.enforest space
  "space"
  rhombus/space
  space-extract-name
  (lambda (stx space-name vars) vars)
  space-extract-typeset)

(define-doc space.transform space
  "space"
  rhombus/space
  space-extract-name
  (lambda (stx space-name vars) vars)
  space-extract-typeset)

(define-doc decl.nestable_macro decl
  "nestable declaration"
  rhombus/decl
  identifier-macro-extract-name
  identifier-macro-extract-metavariables
  identifier-macro-extract-typeset)

(define-doc decl.macro decl
  "declaration"
  rhombus/decl
  identifier-macro-extract-name
  identifier-macro-extract-metavariables
  identifier-macro-extract-typeset)

(define-doc defn.macro defn
  "definition"
  rhombus/defn
  identifier-macro-extract-name
  identifier-macro-extract-metavariables
  identifier-macro-extract-typeset)

(define-doc expr.macro expr
  "expression"
  #f
  operator-macro-extract-name
  operator-macro-extract-metavariables
  operator-macro-extract-typeset)

(define-doc impo.modifier impo
  "import modifier"
  rhombus/impo
  operator-macro-extract-name
  operator-macro-extract-metavariables
  operator-macro-extract-typeset)

(define-doc modpath.macro #f
  "module path"
  rhombus/modpath
  operator-macro-extract-name
  operator-macro-extract-metavariables
  operator-macro-extract-typeset)

(define-doc expo.modifier expo
  "export modifier"
  rhombus/expo
  identifier-macro-extract-name
  identifier-macro-extract-metavariables
  identifier-macro-extract-typeset)

(define-doc impo.macro impo
  "import"
  rhombus/impo
  operator-macro-extract-name
  operator-macro-extract-metavariables
  operator-macro-extract-typeset)

(define-doc expo.macro expo
  "export"
  rhombus/expo
  operator-macro-extract-name
  operator-macro-extract-metavariables
  operator-macro-extract-typeset)

(define-doc bind.macro bind
  "binding operator"
  rhombus/bind
  operator-macro-extract-name
  operator-macro-extract-metavariables
  operator-macro-extract-typeset)

(define-doc annot.macro annot
  "annotation"
  rhombus/annot
  operator-macro-extract-name
  operator-macro-extract-metavariables
  operator-macro-extract-typeset)

(define-doc repet.macro repet
  "repetition"
  rhombus/repet
  operator-macro-extract-name
  operator-macro-extract-metavariables
  operator-macro-extract-typeset)

(define-doc reducer.macro reducer
  "reducer"
  rhombus/reducer
  operator-macro-extract-name
  operator-macro-extract-metavariables
  operator-macro-extract-typeset)

(define-doc for_clause.macro for_clause
  "for clause"
  rhombus/for_clause
  identifier-macro-extract-name
  identifier-macro-extract-metavariables
  identifier-macro-extract-typeset)

(define-doc class_clause.macro class_clause
  "class clause"
  rhombus/class_clause
  identifier-macro-extract-name
  identifier-macro-extract-metavariables
  identifier-macro-extract-typeset)

(define-doc interface_clause.macro interface_clause
  "interface clause"
  rhombus/interface_clause
  identifier-macro-extract-name
  identifier-macro-extract-metavariables
  identifier-macro-extract-typeset)

(define-doc entry_point.macro entry_point
  "entry point"
  rhombus/entry_point
  identifier-macro-extract-name
  identifier-macro-extract-metavariables
  identifier-macro-extract-typeset)

(define-doc unquote_bind.macro unquote_bind
  "unquote binding"
  rhombus/unquote_bind
  operator-macro-extract-name
  operator-macro-extract-metavariables
  operator-macro-extract-typeset)

(define-doc syntax_class_clause.macro syntax_class_clause
  "syntax class clause"
  rhombus/syntax_class_clause
  identifier-macro-extract-name
  identifier-macro-extract-metavariables
  identifier-macro-extract-typeset)

(define-doc pattern_clause.macro pattern_clause
  "pattern clause"
  rhombus/pattern_clause
  identifier-macro-extract-name
  identifier-macro-extract-metavariables
  identifier-macro-extract-typeset)

(define-doc space_clause.macro space_clause
  "space clause"
  rhombus/space_clause
  identifier-macro-extract-name
  identifier-macro-extract-metavariables
  identifier-macro-extract-typeset)

(define-doc space_meta_clause.macro space_meta_clause
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

(define-for-syntax (build-dotted root name)
  (define target+remains+space (resolve-name-ref (list #f) root (list name)))
  (unless target+remains+space
    (raise-syntax-error #f "no label binding" root name))
  (define target (car target+remains+space))
  (datum->syntax #f (list root
                          ;; 'raw property used to typeset object
                          (datum->syntax target (syntax-e target) name name)
                          ;; string for key, index, and search:
                          (format "~a.~a"
                                  (syntax-e root)
                                  (syntax-e name)))))

(define-for-syntax (method-extract-name stx space-name #:property? [property? #f])
  (syntax-parse stx
    #:datum-literals (group parens alts block :: |.| op)
    [(~and (~fail #:unless property?)
           (group _ (alts (block (group (parens (group _ (op ::) class)) (op |.|) name . _)) . _)))
     (build-dotted #'class #'name)]
    [(group _ (parens (group _ (op ::) class)) (op |.|) name . _)
     (build-dotted #'class #'name)]))

(define-for-syntax (method-extract-metavariables stx space-name vars #:property? [property? #f])
  (syntax-parse stx
    #:datum-literals (group parens alts block :: |.| op)
    [(~and (~fail #:unless property?)
           (group _ (alts (block (group (parens (group self (op ::) _)) (op |.|) . _)) . more)))
     (define vars+self (add-metavariable vars #'self #f))
     (syntax-parse #'more
       #:datum-literals (group parens block :: |.| := op)
       [((block (group (parens (group _ (op ::) _)) (op |.|) _ (op :=) . rhs)) . _)
        (extract-binding-metavariables #'(group . rhs) vars+self)]
       [_ vars+self])]
    [(group _ (parens (group self (op ::) _)) (op |.|) name . more)
     (define vars+self (add-metavariable vars #'self #f))
     (syntax-parse #'more
       #:datum-literals (parens)
       [((~and p (parens . _)) . _)
        (parens-extract-metavariables #'p space-name vars+self #:just-parens? #t)]
       [_ vars+self])]))

(define-for-syntax (method-extract-typeset stx space-name subst #:property? [property? #f])
  (syntax-parse stx
    #:datum-literals (group parens alts block :: |.| op)
    [(~and (~fail #:unless property?)
           (group tag ((~and a-tag alts)
                       ((~and b-tag block)
                        ((~and g-tag group)
                         (~and lhs (parens (group _ (op ::) _)))
                         (~and dot (op |.|))
                         name . more))
                       ((~and b2-tag block)
                        ((~and g2-tag group)
                         (~and lhs2 (parens (group _ (op ::) _)))
                         (~and dot2 (op |.|))
                         name2 . more2)))))
     (rb #:at stx
         #`(group as_class_clause
                  tag (a-tag
                       (b-tag (g-tag lhs dot #,@(subst #'name) . more))
                       (b2-tag (g2-tag lhs2 dot2 #,@(subst #'name2 #:redef? #t) . more2)))))]
    [(group tag (~and lhs (parens (group _ (op ::) _))) (~and dot (op |.|)) name . more)
     (rb #:at stx
         #`(group as_class_clause
                  tag lhs dot #,@(subst #'name) . more))]))

(define-doc method
  "method"
  #f
  method-extract-name
  method-extract-metavariables
  method-extract-typeset)

(define-doc property
  "property"
  #f
  (lambda (stx space-name)
    (method-extract-name stx space-name #:property? #t))
  (lambda (stx space-name vars)
    (method-extract-metavariables stx space-name vars #:property? #t))
  (lambda (stx space-name subst)
    (method-extract-typeset stx space-name subst #:property? #t)))

(define-doc dot
  "expression"
  #f
  (lambda (stx space-name)
    (syntax-parse stx
      #:datum-literals (group parens :: |.| op)
      [(group _ (parens (group _ (op ::) class)) (op |.|) name . _)
       (build-dotted #'class #'name)]))
  (lambda (stx space-name vars)
    (syntax-parse stx
      #:datum-literals (group parens :: |.| op)
      [(group _ (parens (group _ (op ::) _)) (op |.|) _ . more)
       (extract-pattern-metavariables #'(group . more) vars)]))
  (lambda (stx space-name subst)
    (syntax-parse stx
      #:datum-literals (group parens :: |.| op)
      [(group tag (~and lhs (parens (group _ (op ::) _))) (~and dot (op |.|)) name . more)
       (rb #:at stx
           #:pattern? #t
           #`(group as_class_clause
                    tag lhs dot #,@(subst #'name) . more))])))

(define-doc def
  "value"
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
      [(group tag (~var id (identifier-target space-name)) e ...)
       (rb #:at stx
           #`(group tag #,@(subst #'id.name) e ...))])))

(define-doc class
  "class"
  (lambda (stx)
    '(rhombus/class rhombus/annot))
  parens-extract-name
  (lambda (stx space-name vars)
    (class-body-extract-metavariables
     stx
     space-name
     (parens-extract-metavariables stx space-name vars)))
  head-extract-typeset)

(define-for-syntax (class-body-extract-metavariables stx space-name vars)
  (syntax-parse stx
    #:datum-literals (block parens)
    [(group _ _ (parens . _) (block g ...))
     (for/fold ([vars vars]) ([g (in-list (syntax->list #'(g ...)))])
       (syntax-parse g
         #:datum-literals (constructor)
         [(_ constructor . more)
          (syntax-parse #'more
            #:datum-literals (alts block group parens)
            [((~and p (parens . _)) . _)
             (parens-extract-metavariables #'p space-name vars #:just-parens? #t)]
            [((alts (block (group (~and p (parens . _))) . _) ...))
             (for/fold ([vars vars]) ([p (in-list (syntax->list #'(p ...)))])
               (parens-extract-metavariables p space-name vars #:just-parens? #t))])]
         [_ vars]))]
    [_ vars]))

(define-doc interface
  "interface"
  (lambda (stx)
    '(rhombus/class rhombus/annot))
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
                                       #:options #'((parens (group #:inset (block (group (parsed #:rhombus/expr #f)))))))])))]))))

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
