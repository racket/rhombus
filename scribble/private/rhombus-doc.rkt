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
  #f
  identifier-macro-extract-name
  identifier-macro-extract-metavariables
  identifier-macro-extract-typeset)

(define-doc decl.macro decl
  "declaration"
  #f
  identifier-macro-extract-name
  identifier-macro-extract-metavariables
  identifier-macro-extract-typeset)

(define-doc defn.macro defn
  "definition"
  #f
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

(begin-for-syntax
  (define (build-dotted root name)
    (define target+remains (resolve-name-ref #f root (list name)))
    (unless target+remains
      (raise-syntax-error #f "no label binding" root name))
    (define target (car target+remains))
    (datum->syntax #f (list root
                            ;; 'raw property used to typeset object
                            (datum->syntax target (syntax-e target) name name)
                            ;; string for key, index, and search:
                            (format "~a.~a"
                                    (syntax-e root)
                                    (syntax-e name)))))

  (define method-extract-name
    (lambda (stx space-name)
      (syntax-parse stx
        #:datum-literals (group parens alts block :: |.| op)
        [(group _ (parens (group _ (op ::) class)) (op |.|) name . _)
         (build-dotted #'class #'name)]
        [(group _ (alts (block (group (parens (group _ (op ::) class)) (op |.|) name . _)) . _))
         (build-dotted #'class #'name)]
        [_ (parens-extract-name stx space-name)])))

  (define method-extract-metavariables
    (lambda (stx space-name vars)
      (syntax-parse stx
        #:datum-literals (group parens alts block :: |.| := op)
        [(group tag (parens (group self (op ::) class)) (op |.|) name (~and p (parens . _)) . _)
         (parens-extract-metavariables #'(group tag name p) space-name
                                       (add-metavariable vars #'self))]
        [(group tag (parens (group self (op ::) class)) (op |.|) name . _)
         (add-metavariable vars #'self)]
        [(group _ (alts (block (group (parens (group self (op ::) class)) (op |.|) name . _))
                        (block (group (parens (group _ (op ::) _)) (op |.|) _ (op :=) . rhs))))
         (extract-binding-metavariables #'(group . rhs)
                                        (add-metavariable vars #'self))]
        [(group _ (alts (block (group (parens (group self (op ::) class)) (op |.|) name . _)) . _))
         (add-metavariable vars #'self)]
        [_ (parens-extract-metavariables stx space-name vars)])))

  (define method-extract-typeset
    (lambda (stx space-name subst)
      (syntax-parse stx
        #:datum-literals (group parens alts block :: |.| op)
        [(group tag ((~and p-tag parens) (group lhs (~and cc (op ::)) class)) (~and dot (op |.|)) name . more)
         (rb #:at stx
             #`(group tag (p-tag (group lhs cc class)) dot #,@(subst #'name) . more))]
        [(group tag ((~and a-tag alts)
                     ((~and b-tag block)
                      ((~and g-tag group)
                       ((~and p-pag parens) (group lhs (~and cc (op ::)) class)) (~and dot (op |.|)) name . more))
                     ((~and b2-tag block)
                      ((~and g2-tag group)
                       ((~and p2-pag parens) (group lhs2 (~and cc2 (op ::)) class2)) (~and dot2 (op |.|)) name2 . more2))
                     ...))
         (with-syntax ([((name2 ...) ...)
                        (for/list ([name2 (in-list (syntax->list #'(name2 ...)))])
                          (subst name2 #:redef? #t))])
           (rb #:at stx
               #`(group tag (a-tag
                             (b-tag
                              (g-tag
                               (p-pag (group lhs cc class)) dot #,@(subst #'name) . more))
                             (b2-tag
                              (g2-tag
                               (p2-pag (group lhs2 cc2 class2)) dot name2 ... . more2))
                             ...))))]
        [_ (head-extract-typeset stx space-name subst)]))))
  
(define-doc method
  "method"
  #f
  method-extract-name
  method-extract-metavariables
  method-extract-typeset)

(define-doc property
  "property"
  #f
  method-extract-name
  method-extract-metavariables
  method-extract-typeset)

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
      [(group _ (parens (group self (op ::) class)) (op |.|) name . more)
       (extract-pattern-metavariables #'(group . more)
                                      vars)]))
  (lambda (stx space-name subst)
    (syntax-parse stx
      #:datum-literals (group parens :: |.| op)
      [(group tag ((~and p-tag parens) (group self cc class)) dot name . more)
       (rb #:at stx
           #`(group tag (p-tag (group self cc class)) dot #,@(subst #'name) . more))])))

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
      [(group _ (~var id (identifier-target space-name)) e ...)
       (rb #:at stx
           #`(group #,@(subst #'id.name) e ...))])))

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
         #:datum-literals (constructor parens)
         [(_ constructor (~and p (parens . _)))
          (parens-extract-metavariables #'(group ctr ctr p) space-name vars)]
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
                                       #:options #'((parens (group #:inset (block (group (parsed #f)))))))])))]))))

;; ----------------------------------------

(define-for-syntax (add-metavariable vars id)
  (hash-set vars (syntax-e id) (or (hash-ref vars (syntax-e id) #f) id)))

(define-for-syntax (extract-binding-metavariables stx vars)
  (syntax-parse stx
    #:literals (:: rhombus-=)
    #:datum-literals (parens group op)
    [(group _:keyword (block g)) (extract-binding-metavariables #'g vars)]
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
