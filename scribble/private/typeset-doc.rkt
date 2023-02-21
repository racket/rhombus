#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     (prefix-in typeset-meta: "typeset_meta.rhm")
                     shrubbery/property)
         "typeset-help.rkt"
         "defining-element.rkt"
         shrubbery/print
         racket/list
         (only-in rhombus/private/name-root-space
                  in-name-root-space
                  name-root-quote)
         (only-in (submod rhombus/private/module-path for-meta)
                  modpath)
         (only-in rhombus
                  def fun operator interface :: |.| $
                  [= rhombus-=]
                  syntax_class class)
         (only-in rhombus/meta
                  space decl defn expr impo expo annot repet bind reducer for_clause
                  class_clause interface_clause entry_point unquote_bind
                  syntax_class_clause pattern_clause space_clause
                  ;; for-syntax:
                  space_meta_clause)
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

(provide typeset-doc
         grammar
         specsubform)

(define-syntax (typeset-doc stx)
  (syntax-parse stx
    #:datum-literals (parens group brackets block)
    [(_ context
        (parens (~optional (group #:literal (block (group literal-id ...) ...)))
                ((~and group-tag group) form ...) ...
                (group
                 (brackets content-group ...))))
     (define forms (map (lambda (stx) (datum->syntax #f (syntax-e stx)))
                        (syntax->list #'((group-tag form ...) ...))))
     (define space-names (for/list ([form (in-list forms)])
                           (extract-space-name form)))
     (define introducers (for/list ([space-name (in-list space-names)])
                           (if space-name
                               (let ([intro (make-interned-syntax-introducer space-name)])
                                 (lambda (x)
                                   (intro x 'add)))
                               values)))
     (define def-names (for/list ([form (in-list forms)]
                                  [space-name (in-list space-names)]
                                  [introducer (in-list introducers)])
                         (define def-name (extract-defined form space-name))
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
                                                    [space-name (in-list space-names)])
                        (extract-metavariables form vars space-name)))
     (define vars (for/fold ([vars all-vars]) ([id (in-list (syntax->list #'(~? (literal-id ... ...) ())))])
                    (hash-remove vars (syntax-e id))))
     (define typesets (for/list ([form (in-list forms)]
                                 [def-id-as-def (in-list def-id-as-defs)]
                                 [space-name (in-list space-names)])
                        (extract-typeset form def-id-as-def space-name)))
     (define kind-strs (map extract-kind-str forms))
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
  ;; FIXME: current `#:space` arguments should really be `#:suffix`,
  ;; and supply just the space with `#:space`; waiting a little while
  ;; to let Racket releases include that functionality
  (define (make-content defn?)
    (racketidfont
     (make-id-element id str defn? #:space str+space)))
  (define content (annote-exporting-library (make-content #t)))
  (define target-maker (id-to-target-maker id #t #:space str+space))
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
                    #:space (if str-id-e
                                (list str-id-e space)
                                space))))

(define-for-syntax (make-meta-id-transformer id)
  (typeset-meta:make_Transformer
   (lambda (use-stx)
     #`(parsed (racketvarfont #,(symbol->string (syntax-e id)))))))

(begin-for-syntax
  (define-syntax-rule (def-space :sp sp)
    (define-syntax-class :sp
      (pattern id:identifier
               #:when (free-identifier=? (in-name-root-space #'id) (name-root-quote sp)))))
  (define-syntax-rule (def-space+1 :sp sp)
    (define-syntax-class :sp
      (pattern id:identifier
               #:when (free-identifier=? (in-name-root-space #'id) (name-root-quote sp)
                                         (+ 1 (syntax-local-phase-level))))))
  (def-space :decl decl)
  (def-space :defn defn)
  (def-space :expr expr)
  (def-space :bind bind)
  (def-space :annot annot)
  (def-space :repet repet)
  (def-space :reducer reducer)
  (def-space :expo expo)
  (def-space :impo impo)
  (def-space :modpath modpath)
  (def-space :for_clause for_clause)
  (def-space :class_clause class_clause)
  (def-space :interface_clause interface_clause)
  (def-space :entry_point entry_point)
  (def-space :unquote_bind unquote_bind)
  (def-space :syntax_class_clause syntax_class_clause)
  (def-space :pattern_clause pattern_clause)
  (def-space :space space)
  (def-space :space_clause space_clause)
  (def-space+1 :space_meta_clause space_meta_clause)

  (define-splicing-syntax-class operator-macro-head
    #:literals (def fun)
    #:datum-literals (op macro modifier |.|)
    (pattern (~seq (~or _::expr _::bind _::annot _::repet
                        _::expo _::impo _::modpath
                        _::unquote_bind)
                   (op |.|)
                   macro))
    (pattern (~seq (~or _::expo _::impo) (op |.|) modifier))
    (pattern (~seq def)))
  (define-splicing-syntax-class identifier-macro-head
    #:datum-literals (op modifier macro nestable_macro |.|)
    (pattern (~seq (~or _::defn _::decl _::expr _::annot _::repet _::bind _::reducer _::expo _::modpath
                        _::for_clause _::class_clause _::interface_clause _::entry_point
                        _::unquote_bind _::syntax_class_clause _::pattern_clause
                        _::space_clause _::space_meta_clause)
                   (op |.|) macro))
    (pattern (~seq (~or _::decl)
                   (op |.|) nestable_macro))
    (pattern (~seq (~or _::impo _::expo) (op |.|) modifier))
    (pattern (~seq def)))
  (define-splicing-syntax-class specsubform-head
    #:literals (specsubform)
    #:datum-literals (|.|)
    (pattern (~seq _ (op |.|) specsubform))
    (pattern specsubform))
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

(define-for-syntax (extract-defined stx space-name)
  (syntax-parse stx
    #:literals (def fun interface operator grammar syntax_class class)
    #:datum-literals (|.| $ parens group op
                          modifier class_clause interface_clause quotes
                          enforest transform)
    [(group _::space _ _ (~var id (identifier-target space-name))) #'id.name]
    [(group (~or fun class) (~var id (identifier-target space-name)) (parens . _) . _) #'id.name]
    [(group (~or def interface) (~var id (identifier-target space-name)) . _) #'id.name]
    [(group (~or operator) (parens (group (~var id (target space-name)) arg1)) . _) #'id.name]
    [(group (~or operator) (parens (group arg1 (~var id (target space-name)) arg2)) . _) #'id.name]
    [(group _:operator-macro-head (quotes (group (op $) _:identifier (~var id (target space-name)) . _))) #'id.name]
    [(group _:operator-macro-head (quotes (group (~var id (target space-name)) . _))) #'id.name]
    [(group _:identifier-macro-head (quotes (group (~var id (identifier-target space-name)) . _))) #'id.name]
    [(group _:identifier-macro-head (quotes (~var id (identifier-target space-name)))) #'id.name]
    [(group syntax_class (~var id (identifier-target space-name)) . _) #'id.name]
    [(group _::space (op |.|) (~or enforest transform) (~var id (identifier-target space-name)) . _) #'id.name]
    [(group _:specsubform-head . _) #f]
    [(group grammar . _) #f]
    [_ (raise-syntax-error 'doc "unknown definition form" stx)]))

(define-for-syntax (add-metavariable vars id)
  (hash-set vars (syntax-e id) (or (hash-ref vars (syntax-e id) #f) id)))

(define-for-syntax (extract-metavariables stx vars space-name)
  (syntax-parse stx
    #:literals (def fun class operator grammar specsubform syntax_class)
    #:datum-literals (parens group op quotes)
    [(group (~or _::space) . _) vars]
    [(group (~or fun class) (~var id (identifier-target space-name)) (parens g ...) . _)
     (for/fold ([vars vars]) ([g (in-list (syntax->list #'(g ...)))])
       (extract-binding-metavariables g vars))]
    [(group (~or def) (~var id (identifier-target space-name)) . _) vars]
    [(group (~or operator) (parens (group (~var _ (target space-name)) arg)) . _)
     (extract-binding-metavariables #'(group arg) vars)]
    [(group (~or operator) (parens (group arg0 (~var _ (target space-name)) arg1)) . _)
     (define vars0 (extract-binding-metavariables #'(group arg0) vars))
     (extract-binding-metavariables #'(group arg1) vars0)]
    [(group _:operator-macro-head (quotes (group (op $) t0:identifier (~var _ (target space-name)) t ...)))
     (extract-pattern-metavariables #'(group (op $) t0 t ...) vars)]
    [(group _:operator-macro-head (quotes (group (~var _ (target space-name)) t ...)))
     (extract-pattern-metavariables #'(group t ...) vars)]
    [(group _:identifier-macro-head (quotes (group (~var _ (identifier-target space-name)) t ...)))
     (extract-pattern-metavariables #'(group t ...) vars)]
    [(group _:identifier-macro-head (quotes (~var _ (identifier-target space-name))))
     vars]
    [(group _:specsubform-head (quotes g))
     (extract-pattern-metavariables #'g vars)]
    [(group syntax_class (~var _ (identifier-target space-name)) (parens g ...) . _)
     (for/fold ([vars vars]) ([g (in-list (syntax->list #'(g ...)))])
       (extract-binding-metavariables g vars))]
    [(group grammar id b)
     (extract-pattern-metavariables #'(group b) (add-metavariable vars #'id))]
    [_ vars]))

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
    [id:identifier (add-metavariable vars #'id)]
    [_ vars]))

(define-for-syntax (extract-pattern-metavariables g vars)
  (syntax-parse g
    #:datum-literals (group)
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

(define-for-syntax (extract-typeset stx def-id-as-def space-name)
  (define (rb form
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
  (syntax-parse stx
    #:literals (def fun class interface syntax_class operator |$| grammar)
    #:datum-literals (parens group op quotes |.|)
    [(group (~and tag (~or def fun class interface)) (~var id (identifier-target space-name)) e ...)
     (rb #:at stx
         #`(group tag #,@(subst #'id.name) e ...))]
    [(group (~and tag operator) ((~and p-tag parens) ((~and g-tag group) (~var id (target space-name)) arg)) e ...)
     (rb #:at stx
         #`(group tag (p-tag (g-tag #,@(subst #'id.name) arg)) e ...))]
    [(group (~and tag operator) ((~and p-tag parens) ((~and g-tag group) arg0 (~var id (target space-name)) arg1)) e ...)
     (rb #:at stx
         #`(group tag (p-tag (g-tag arg0 #,@(subst #'id.name) arg1)) e ...))]
    [(group _:operator-macro-head (quotes (~and g (group (~and $0 (op $)) e0:identifier (~var id (target space-name)) e ...))))
     (rb #:at #'g
         #:pattern? #t
         #`(group $0 e0 #,@(subst #'id.name) e ...))]
    [(group _:operator-macro-head (quotes (~and g (group (~var id (target space-name)) e ...))))
     (rb #:at #'g
         #:pattern? #t
         #`(group #,@(subst #'id.name) e ...))]
    [(group _:identifier-macro-head (quotes (~and g (group (~var id (identifier-target space-name)) e ...))))
     (rb #:at #'g
         #:pattern? #t
         #`(group #,@(subst #'id.name) e ...))]
    [(group _:identifier-macro-head (quotes (~var id (identifier-target space-name))))
     #`(paragraph plain #,def-id-as-def)]
    [(group syntax_class (~var id (identifier-target space-name)) e ...)
     (rb #:at stx
         #`(group #,@(subst #'id.name) e ...))]
    [(group _::space _ _ (~var id (identifier-target space-name)) e ...)
     (rb #:at stx
         #`(group #,@(subst #'id.name) e ...))]
    [(group _:specsubform-head (quotes g))
     (rb #:at #'g #:pattern? #t #'g)]
    [(group grammar id (block g ...))
     #`(typeset-grammar (rhombus-expression (group one-rhombus (parens (group id))))
                        #,@(for/list ([g (in-list (syntax->list #'(g ...)))])
                             (syntax-parse g
                               #:datum-literals (group)
                               [(group t ...)
                                (rb #'(group t ...)
                                    #:at g
                                    #:pattern? #t
                                    #:options #'((parens (group #:inset (block (group (parsed #f)))))))])))]
    [_ (rb stx)]))

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

(define-for-syntax (extract-space-name stx)
  (syntax-parse stx
    #:literals (syntax_class class interface)
    #:datum-literals (parens group op)
    [(group _::space . _) 'rhombus/space]
    [(group _::impo . _) 'rhombus/impo]
    [(group _::expo . _) 'rhombus/expo] ; one space currently used for both exports and modifiers
    [(group _::modpath . _) 'rhombus/modpath]
    [(group _::annot . _) 'rhombus/annot]
    [(group _::repet . _) 'rhombus/repet]
    [(group _::reducer . _) 'rhombus/reducer]
    [(group _::for_clause . _) 'rhombus/for_clause]
    [(group _::class_clause . _) 'rhombus/class_clause]
    [(group _::interface_clause . _) 'rhombus/interface_clause]
    [(group _::entry_point . _) 'rhombus/entry_point]
    [(group _::unquote_bind . _) 'rhombus/unquote_bind]
    [(group _::syntax_class_clause . _) 'rhombus/syntax_class_clause]
    [(group _::pattern_clause . _) 'rhombus/pattern_clause]
    [(group _::space_clause . _) 'rhombus/space_clause]
    [(group _::space_meta_clause . _) 'rhombus/space_meta_clause]
    [(group _::bind . _) 'rhombus/bind]
    [(group interface . _) 'rhombus/class]
    [(group class . _) 'rhombus/class]
    [(group syntax_class . _) 'rhombus/stxclass]
    [_ #f]))

(define-for-syntax (extract-kind-str stx)
  (syntax-parse stx
    #:literals (def fun operator syntax_class interface grammar class)
    #:datum-literals (parens group op quotes modifier macro nestable_macro)
    [(group _::space . _) "space"]
    [(group _::decl _ nestable_macro . _) "nestable declaration"]
    [(group _::decl . _) "declaration"]
    [(group _::defn . _) "definition"]
    [(group _::expr . _) "expression"]
    [(group _::impo . _) "import modifier"]
    [(group _::modpath . _) "module path"]
    [(group _::expo _ modifier . _) "export modifier"]
    [(group _::expo _ macro . _) "export"]
    [(group _::annot . _) "annotation"]
    [(group _::repet . _) "repetition"]
    [(group _::reducer . _) "reducer"]
    [(group _::for_clause . _) "for clause"]
    [(group _::class_clause . _) "class clause"]
    [(group _::interface_clause . _) "interface clause"]
    [(group _::entry_point . _) "entry point"]
    [(group _::unquote_bind . _) "unquote binding"]
    [(group _::syntax_class_clause . _) "syntax class clause"]
    [(group _::pattern_clause  . _) "pattern clause"]
    [(group _::space_clause  . _) "space clause"]
    [(group _::space_meta_clause  . _) "space meta clause"]
    [(group _::bind . _) "binding operator"]
    ;; after expr, bind, etc. so that expr.subspecform gets "expression" not #f
    [(group _:specsubform-head . _) #f]
    [(group grammar . _) #f]
    [(group (~or def fun) id:identifier (parens . _) . _) "function"]
    [(group (~or def) (quotes . _) . _) "expression"]
    [(group operator . _) "operator"]
    [(group syntax_class . _) "syntax class"]
    [(group class . _) "class"]
    [(group interface . _) "interface"]
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

(define-syntax specsubform "to be used in `doc`")

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
