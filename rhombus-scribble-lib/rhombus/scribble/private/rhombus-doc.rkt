#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     shrubbery/property
                     shrubbery/print
                     enforest/name-parse
                     enforest/deprecated
                     rhombus/private/enforest
                     rhombus/private/name-path-op
                     racket/list
                     rhombus/private/pack)
         (rename-in "typeset-doc.rkt"
                    [doc-typeset-rhombusblock rb])
         (submod "doc.rkt" for-class)
         (lib "shrubbery/render/private/typeset-help.rkt")
         rhombus/private/name-root
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
                    grammar
                    non_target
                    operator_order))

(module+ for_doc_transformer
  (provide
   (for-syntax extract-name
               extract-identifier-name

               head-extract-name
               parens-extract-name
               identifier-macro-extract-name
               operator-macro-extract-name

               head-extract-metavariables
               parens-extract-metavariables
               identifier-macro-extract-metavariables
               operator-macro-extract-metavariables

               head-extract-typeset
               parens-extract-typeset
               identifier-macro-extract-typeset
               operator-macro-extract-typeset)))

(define-name-root space_meta_clause
  #:fields
  ())

(define-syntax (define-doc stx)
  (syntax-parse stx
    [(_ id
        (~optional (~or* #f [from namespace] namespace))
        desc ; string or expression for procedure
        space-sym ; id, #f, or expression for procedure
        extract-name
        extract-metavariables
        (~optional (~seq #:spacer-infos extract-spacer-infos))
        extract-typeset)
     #`(begin
         (provide (for-space rhombus/doc id)
                  (~? (for-space rhombus/namespace
                                 namespace)))
         (~? (require (only-space-in rhombus/namespace
                                     (only-in (~? from rhombus/meta)
                                              [namespace namespace]))))
         (define-doc-syntax id
           (make-doc-transformer #:extract-desc #,(if (string? (syntax-e #'desc))
                                                      #`(lambda (stx) desc)
                                                      #`desc)
                                 #:extract-space-sym #,(if (or (identifier? #'space-sym)
                                                               (boolean? (syntax-e #'space-sym)))
                                                           #'(lambda (stx) 'space-sym)
                                                           #'space-sym)
                                 #:extract-sort-order space-to-sort-order
                                 #:extract-name extract-name
                                 #:extract-metavariables extract-metavariables
                                 #:extract-spacer-infos (~? extract-spacer-infos none-extract-spacer-infos)
                                 #:extract-typeset extract-typeset)))]))

(define-for-syntax (space-to-sort-order stx spcs)
  (for/list ([spc/s (in-list spcs)])
    (define spc (if (list? spc/s)
                    (car spc/s)
                    spc/s))
    (case spc
      [(rhombus/class) 0]
      [(#f) 1]
      [(rhombus/defn) 2]
      [(rhombus/decl) 3]
      [(rhombus/annot)4]
      [(rhombus/bind) 5]
      [(rhombus/unquote_bind) 6]
      [(rhombus/repet) 7]
      [(rhombus/reducer) 8]
      [(rhombus/stxclass) 9]
      [(rhombus/space) 10]
      [(rhombus/impo) 11]
      [(rhombus/expo) 12]
      [(rhombus/modpath) 13]
      [(rhombus/entry_point) 14]
      [(rhombus/immediate_callee) 15]
      [(rhombus/for_clause) 16]
      [(rhombus/class_clause) 17]
      [(rhombus/interface_clause) 18]
      [(rhombus/veneer_clause) 19]
      [(rhombus/syntax_class_clause) 20]
      [(rhombus/pattern_clause) 21]
      [(rhombus/space_clause) 22]
      [(rhombus/space_meta_clause) 23]
      [(rhombus/key_comp) 24]
      [(rhombus/operator_order) 25]
      [(rhombus/doc) 26]
      [else 100])))

(begin-for-syntax
  (define-splicing-syntax-class :doc-form
    #:datum-literals (op |.|)
    (pattern (~seq _:identifier))
    (pattern (~seq (~seq _:identifier (op |.|)) ... _:identifier)))

  (define (resolved->typeset resolved raw)
    (define root (hash-ref resolved 'root #f))
    (define roots (hash-ref resolved 'roots #f))
    (define root-syms (hash-ref resolved 'root-syms #f))
    (define target (hash-ref resolved 'target))
    (define def-ht
      (cond
        [root (let* ([ht (hash 'root root
                               'target target)]
                     [ht (if roots
                             (hash-set ht 'roots roots)
                             ht)]
                     [ht (if root-syms
                             (hash-set ht 'root-syms root-syms)
                             ht)]
                     [ht (if raw
                             (hash-set ht 'raw (hash-ref resolved 'raw))
                             ht)])
                ht)]
        [else target]))
    (define target-raw (syntax-raw-property target))
    (define raw-prefix (hash-ref resolved 'raw-prefix #f))
    (cond
      [raw-prefix
       (let* ([ht (if (identifier? def-ht)
                      (hash 'target def-ht)
                      def-ht)]
              [ht (hash-set ht 'raw_prefix raw-prefix)])
         ht)]
      [else
       def-ht]))

  (define-splicing-syntax-class (identifier-target space-name #:raw [raw #f])
    #:attributes (name sym)
    #:datum-literals (|.| op)
    (pattern (~seq root:identifier (~seq (op |.|) field:identifier) ...)
             #:do [(define resolved (resolve-name-ref (list space-name)
                                                      #'root
                                                      (syntax->list #'(field ...))
                                                      #:raw raw))]
             #:when resolved
             #:attr name (resolved->typeset resolved raw)
             #:attr sym (car (reverse (syntax->datum #'(root field ...)))))
    (pattern (~seq name:identifier)
             #:attr sym (syntax-e #'name)))
  (define-splicing-syntax-class (target space-name)
    #:attributes (name sym)
    #:datum-literals (|.| op parens group)
    (pattern (~seq root:identifier (~seq (op |.|) field:identifier) ... (op |.|) ((~and ptag parens) (group (op opname))))
             #:do [(define resolved (resolve-name-ref (list space-name)
                                                      #'root
                                                      (syntax->list #'(field ... opname))
                                                      #:parens #'ptag))]
             #:when resolved
             #:attr name (resolved->typeset resolved #f)
             #:attr sym (syntax->datum #'opname))
    (pattern (~seq (op name:identifier))
             #:attr sym (syntax-e #'name))
    (pattern (~seq (~var || (identifier-target space-name))))))

(define-for-syntax (none-extract-spacer-infos stx space-names)
  (map (lambda (x) #f) space-names))

(define-for-syntax (extract-name stx space-name)
  (syntax-parse (unpack-group stx #f #f)
    #:datum-literals (group)
    [(group (~var id (target space-name))) (attribute id.name)]
    [_ #f]))

(define-for-syntax (extract-identifier-name stx space-name)
  (syntax-parse (unpack-group stx #f #f)
    #:datum-literals (group)
    [(group (~var id (identifier-target space-name))) (attribute id.name)]
    [_ #f]))

(define-for-syntax (head-extract-name stx space-name)
  (syntax-parse (unpack-group stx #f #f)
    #:datum-literals (group)
    [(group _::doc-form (~var id (identifier-target space-name)) . _) (attribute id.name)]))

(define-for-syntax (head-dot-head-extract-name stx space-name)
  (syntax-parse stx
    #:datum-literals (group op |.|)
    [(group _ (op |.|) _ (~var id (identifier-target space-name)) . _) (attribute id.name)]))

(define-for-syntax (parens-extract-name stx space-name)
  (syntax-parse (unpack-group stx #f #f)
    #:datum-literals (group parens)
    [(group _::doc-form (~var id (identifier-target space-name)) (parens . _) . _) (attribute id.name)]))

(define-for-syntax (do-parens-extract-metavariables gs space-name vars)
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
  (define norm-gs
    (syntax-parse gs
      #:datum-literals (group op [ooo ...])
      [(pre-g ... dot-g (group (op ooo)))
       #:with (kw-opt-g:maybe-kw-opt ...) #'(pre-g ...)
       #'(kw-opt-g.g ... dot-g)]
      [(pre-g ... and:and (~optional kw-and:kw-and))
       #:with (kw-opt-g:maybe-kw-opt ...) #'(pre-g ...)
       #'(kw-opt-g.g ... (and.tag . and.g) (~? (kw-and.tag . kw-and.g)))]
      [(pre-g ... kw-and:kw-and (~optional and:and))
       #:with (kw-opt-g:maybe-kw-opt ...) #'(pre-g ...)
       #'(kw-opt-g.g ... (kw-and.tag . kw-and.g) (~? (and.tag . and.g)))]
      [(kw-opt-g:maybe-kw-opt ...)
       #'(kw-opt-g.g ...)]))
  (for/fold ([vars vars]) ([g (in-list (syntax->list norm-gs))])
    (extract-binding-metavariables g vars)))

(define-for-syntax (parens-extract-metavariables stx space-name vars)
  (syntax-parse (unpack-group stx #f #f)
    #:datum-literals (parens group)
    [(group _::doc-form (~var _ (identifier-target space-name)) (parens . gs) . _)
     (do-parens-extract-metavariables #'gs space-name vars)]
    [_ vars]))

(define-for-syntax (parens-extract-spacer-infos stx space-names)
  (syntax-parse (unpack-group stx #f #f)
    #:datum-literals (parens group op ::)
    [(group _::doc-form (~var _ (identifier-target (caar space-names))) (parens . gs) (op ::) . ret)
     (ret-extract-spacer-infos #'ret)]
    [_ #f]))

(define-for-syntax (target->dotted-identifier name sym)
  (cond
    [(identifier? name)
     name]
    [else
     (define ht (if (syntax? name) (syntax-e name) name))
     (define t (hash-ref ht 'target #f))
     (define root (hash-ref ht 'root #f))
     (define root-syms (hash-ref ht 'root-syms #'#f))
     (if root
         (hash 'id t 'sym sym 'root_id root 'root_syms root-syms)
         t)]))

(define-for-syntax (ret-extract-spacer-infos ret)
  (syntax-parse ret
    #:datum-literals (block alts)
    [(id:identifier)
     (hash 'result_annotation #'id)]
    [(id:identifier (block . _))
     (hash 'result_annotation #'id)]
    [(id:identifier (alts . _))
     (hash 'result_annotation #'id)]
    [((~var id (identifier-target 'rhombus/annot)))
     (hash 'result_annotation (target->dotted-identifier (attribute id.name) (attribute id.sym)))]
    [_
     #f]))

(define-for-syntax (identifier-macro-extract-name stx space-name)
  (syntax-parse (unpack-group stx #f #f)
    #:datum-literals (group op quotes parens)
    [(group _::doc-form (quotes (group (parens (group (~var id (identifier-target space-name)))) . _) . _)) (attribute id.name)]
    [(group _::doc-form (quotes (group (~var id (identifier-target space-name)) . _) . _)) (attribute id.name)]
    [(group _::doc-form (quotes (~var id (identifier-target space-name)))) (attribute id.name)]
    [_ #f]))

(define-for-syntax (operator-macro-extract-name stx space-name)
  (syntax-parse (unpack-group stx #f #f)
    #:datum-literals ($ group op quotes)
    [(group _::doc-form (quotes (group (op $) _:identifier (~var id (target space-name)) . _))) (attribute id.name)]
    [(group _::doc-form (quotes (group (~var id (target space-name)) . _))) (attribute id.name)]
    [_ (identifier-macro-extract-name stx space-name)]))

(define-for-syntax (space-extract-name stx space-name)
  (syntax-parse stx
    #:datum-literals (group)
    [(group _::doc-form (~var id (identifier-target space-name))) (attribute id.name)]))

(define-for-syntax (head-extract-metavariables stx space-name vars)
  vars)

(define-for-syntax (identifier-macro-extract-metavariables stx space-name vars)
  (syntax-parse (unpack-group stx #f #f)
    #:datum-literals (group op quotes parens)
    [(group _::doc-form (quotes (group (parens (group (~var _ (identifier-target space-name)))) t ...)
                                (group t2 ...)
                                ...))
     (extract-pattern-metavariables #'(group t ... t2 ... ...) vars)]
    [(group _::doc-form (quotes (group (~var _ (identifier-target space-name)) t ...)
                                (group t2 ...)
                                ...))
     (extract-pattern-metavariables #'(group t ... t2 ... ...) vars)]
    [(group _::doc-form (quotes (~var _ (identifier-target space-name))))
     vars]
    [_ vars]))

(define-for-syntax (operator-macro-extract-metavariables stx space-name vars)
  (syntax-parse (unpack-group stx #f #f)
    #:datum-literals ($ group op quotes)
    [(group _::doc-form (quotes (group (op $) t0:identifier (~var _ (target space-name)) t ...)))
     (extract-pattern-metavariables #'(group (op $) t0 t ...) vars)]
    [(group _::doc-form (quotes (group (~var _ (target space-name)) t ...)))
     (extract-pattern-metavariables #'(group t ...) vars)]
    [_ (identifier-macro-extract-metavariables stx space-name vars)]))

(define-for-syntax (space-extract-typeset stx space-name subst)
  (syntax-parse stx
    #:datum-literals (group)
    [(group _::doc-form (~var id (identifier-target space-name)) e ...)
     (rb #:at stx
         #`(group #,@(subst (attribute id.name)) e ...))]))

(define-for-syntax (head-extract-typeset stx space-name subst)
  (syntax-parse stx
    #:datum-literals (group)
    [(group tag::doc-form (~var id (identifier-target space-name)) e ...)
     (rb #:at stx
         #`(group (~@ . tag) #,@(subst (attribute id.name)) e ...))]))

(define-for-syntax (head-dot-head-extract-typeset stx space-name subst)
  (syntax-parse stx
    #:datum-literals (group)
    [(group tag dot tag2 (~var id (identifier-target space-name)) e ...)
     (rb #:at stx
         #`(group tag dot tag2 #,@(subst (attribute id.name)) e ...))]))

(define-for-syntax (parens-extract-typeset stx space-name subst)
  (head-extract-typeset stx space-name subst))

(define-for-syntax (identifier-macro-extract-typeset stx space-name subst)
  (syntax-parse (unpack-group stx #f #f)
    #:datum-literals ($ group op quotes)
    [(group _::doc-form (quotes (~and g (group (parens (group (~var id (identifier-target space-name)))) e ...))))
     ;; just one group; don't keep `group` tag prefix and suffix
     (rb #:at #'g
         #:pattern? #t
         #`(group #,@(subst (attribute id.name)) e ...))]
    [(group _::doc-form (quotes (~and g (group (~var id (identifier-target space-name)) e ...))))
     ;; just one group; don't keep `group` tag prefix and suffix
     (rb #:at #'g
         #:pattern? #t
         #`(group #,@(subst (attribute id.name)) e ...))]
    [(group _::doc-form (quotes (~and g ((~and g-tag group) (~var id (identifier-target space-name)) e ...)) g2 ...))
     (rb #:at #'g
         #:pattern? #t
         #`(multi
            (g-tag #,@(subst (attribute id.name)) e ...)
            g2
            ...))]
    [(group _::doc-form (quotes (~var id (identifier-target space-name))))
     #`(paragraph plain #,(subst (attribute id.name)))]))

(define-for-syntax (operator-macro-extract-typeset stx space-name subst)
  (syntax-parse (unpack-group stx #f #f)
    #:datum-literals ($ group op quotes)
    [(group _::doc-form (quotes (~and g (group (~and $0 (op $)) e0:identifier (~var id (target space-name)) e ...))) . more)
     (rb #:at #'g
         #:pattern? #t
         #`(group $0 e0 #,@(subst (attribute id.name)) e ...))]
    [(group _::doc-form (quotes (~and g (group (~var id (target space-name)) e ...))))
     (rb #:at #'g
         #:pattern? #t
         #`(group #,@(subst (attribute id.name)) e ...))]
    [_ (identifier-macro-extract-typeset stx space-name subst)]))

(define-doc meta.bridge meta
  "bridge"
  #f
  head-dot-head-extract-name
  (lambda (stx space-name vars) vars)
  head-dot-head-extract-typeset)

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

(define-doc defn.sequence_macro defn
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

(define-for-syntax (annotation-prune-fallback proc)
  (lambda (stx . args)
    (apply proc
           (syntax-parse stx
             #:datum-literals (group block)
             [((~and tag group) head ... (block (group #:method_fallback rhs ...)))
              #'(tag head ...)]
             [_ stx])
           args)))

(define-for-syntax (annotation-extract-spacer-infos stx space-names)
  (syntax-parse stx
    #:datum-literals (group block)
    [(group head ... (block (group #:method_fallback (~var id (identifier-target 'rhombus/annot)))))
     (hash 'method_fallback (target->dotted-identifier (attribute id.name) (attribute id.sym)))]
    [(group head ... (block (group #:method_fallback (block (group (~var id (identifier-target 'rhombus/annot)))))))
     (hash 'method_fallback (target->dotted-identifier (attribute id.name) (attribute id.sym)))]
    [(group head ... (block (~and fallback (group #:method_fallback . _))))
     (raise-syntax-error #f
                         "invalid method-fallback clause"
                         stx
                         #'fallback)]
    [_ #f]))

(define-doc annot.macro annot
  "annotation"
  rhombus/annot
  (annotation-prune-fallback operator-macro-extract-name)
  (annotation-prune-fallback operator-macro-extract-metavariables)
  #:spacer-infos annotation-extract-spacer-infos
  (annotation-prune-fallback operator-macro-extract-typeset))

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

(define-doc veneer_clause.macro veneer_clause
  "veneer clause"
  rhombus/veneer_clause
  identifier-macro-extract-name
  identifier-macro-extract-metavariables
  identifier-macro-extract-typeset)

(define-doc entry_point.macro entry_point
  "entry point"
  rhombus/entry_point
  identifier-macro-extract-name
  identifier-macro-extract-metavariables
  identifier-macro-extract-typeset)

(define-doc immediate_callee.macro immediate_callee
  "immediate callee"
  rhombus/immediate_callee
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

(define-doc key_comp.def key_comp
  "map configuration"
  rhombus/key_comp
  operator-macro-extract-name
  operator-macro-extract-metavariables
  operator-macro-extract-typeset)

(define-doc fun
  "function"
  #f
  parens-extract-name
  parens-extract-metavariables
  #:spacer-infos parens-extract-spacer-infos
  head-extract-typeset)

(define-for-syntax (build-dotted root #:prefix [names #'()] name)
  (define resolved (resolve-name-ref (list #f) root (append (syntax->list names) (list name))))
  (unless resolved
    (raise-syntax-error #f "no label binding" root name))
  (define raw-target (hash-ref resolved 'target))
  (define target (datum->syntax raw-target (syntax-e raw-target) name name))
  (define r-root (hash-ref resolved 'root))
  (cond
    [r-root
     (hash 'root r-root
           ;; 'raw property used to typeset object
           'target target
           ;; string for key, index, and search:
           'raw (hash-ref resolved 'raw))]
    [else target]))

(define-for-syntax (dotted-to-identifier head tail)
  (cond
    [(null? (syntax-e tail))
     head]
    [else
     (define resolved (resolve-name-ref (list #f) head (syntax->list tail)))
     (and resolved (hash-ref resolved 'target))]))

(begin-for-syntax
  (define-splicing-syntax-class :dotted-class
    #:attributes (head tail)
    #:datum-literals (op |.|)
    (pattern (~seq head:identifier)
             #:with tail #'())
    (pattern (~seq head:identifier (~seq (op |.|) tail-elem:identifier) ...)
             #:with tail #'(tail-elem ...))))

(define-for-syntax (method-extract-name stx space-name #:property? [property? #f])
  (syntax-parse stx
    #:datum-literals (group parens alts block :: |.| op)
    [(~and (~fail #:unless property?)
           (group _ (alts (block (group (parens (group _ (op ::) class::dotted-class)) (op |.|) name . _)) . _)))
     (build-dotted #'class.head #:prefix #'class.tail #'name)]
    [(group _ (parens (group _ (op ::) class::dotted-class)) (op |.|) name . _)
     (build-dotted #'class.head #:prefix #'class.tail #'name)]
    ;; allow plain-function form, useful when a default export is replaced
    [_ (parens-extract-name stx space-name)]))

(define-for-syntax (method-extract-metavariables stx space-name vars #:property? [property? #f])
  (define (add-annot id class-id)
    (if class-id
        (syntax-property id 'annot_id class-id)
        id))
  (syntax-parse stx
    #:datum-literals (group parens alts block :: |.| op)
    [(~and (~fail #:unless property?)
           (group _ (alts (block (group (parens (group self (op ::) class::dotted-class)) (op |.|) . _)) . more)))
     (define class-id (dotted-to-identifier #'class.head #'class.tail))
     (define vars+self (add-metavariable vars (add-annot #'self class-id) #f))
     (syntax-parse #'more
       #:datum-literals (group parens block :: |.| := op)
       [((block (group (parens (group _ (op ::) . _)) (op |.|) _ (op :=) . rhs)) . _)
        (extract-binding-metavariables #'(group . rhs) vars+self)]
       [_ vars+self])]
    [(group _ (parens (group self (op ::) class::dotted-class)) (op |.|) name . more)
     (define class-id (dotted-to-identifier #'class.head #'class.tail))
     (define vars+self (add-metavariable vars (add-annot #'self class-id) #f))
     (syntax-parse #'more
       #:datum-literals (parens)
       [((parens . gs) . _)
        (do-parens-extract-metavariables #'gs space-name vars+self)]
       [_ vars+self])]
    ;; allow plain-function form
    [_ (parens-extract-metavariables stx space-name vars)]))

(define-for-syntax (method-extract-typeset stx space-name subst #:property? [property? #f])
  (define (def-class stx)
    (syntax-parse stx
      [(parens (group id colons . cs))
       #`(parens (group id colons #,@(for/list ([c (in-list (syntax->list #'cs))])
                                       (syntax-parse c
                                         #:datum-literals (op)
                                         [(op c)
                                          #`(op #,(syntax-property #'c 'typeset-define #t #t))]
                                         [_ (syntax-property c 'typeset-define #t #t)]))))]))
  (syntax-parse stx
    #:datum-literals (group parens alts block :: |.| op)
    [(~and (~fail #:unless property?)
           (group tag ((~and a-tag alts)
                       ((~and b-tag block)
                        ((~and g-tag group)
                         (~and lhs (parens (group _ (op ::) . _)))
                         (~and dot (op |.|))
                         name . more))
                       ((~and b2-tag block)
                        ((~and g2-tag group)
                         (~and lhs2 (parens (group _ (op ::) . _)))
                         (~and dot2 (op |.|))
                         name2 . more2)))))
     (rb #:at stx
         #`(group as_class_clause
                  tag (a-tag
                       (b-tag (g-tag #,(def-class #'lhs) dot #,@(subst #'name) . more))
                       (b2-tag (g2-tag #,(def-class #'lhs2) dot2 #,@(subst #'name2 #:as_redef #t) . more2)))))]
    [(group tag (~and lhs (parens (group _ (op ::) . _))) (~and dot (op |.|)) name . more)
     (rb #:at stx
         #`(group as_class_clause
                  tag #,(def-class #'lhs) dot #,@(subst #'name) . more))]
    ;; allow plain-function form
    [(group tag (~var id (identifier-target space-name)) e ...)
     (rb #:at stx
         #`(group as_class_clause tag #,@(subst (attribute id.name)) e ...))]))

(define-for-syntax (method-extract-spacer-infos stx space-names #:property? [property? #f])
  (syntax-parse stx
    #:datum-literals (group parens alts block :: |.| op)
    [(~and (~fail #:unless property?)
           (group _ (alts (block (group (parens (group self (op ::) . ret)) (op |.|) . _)) . more)))
     (ret-extract-spacer-infos #'ret)]
    [(~and (~fail #:unless property?)
           (group _ (parens (group self (op ::) . _)) (op |.|) name (op ::) . ret))
     (ret-extract-spacer-infos #'ret)]
    [(group _  (parens . _) (op |.|) name (parens . _) (op ::) . ret)
     (ret-extract-spacer-infos #'ret)]
    [_
     (parens-extract-spacer-infos stx space-names)]))

(define-doc method
  "method"
  #f
  method-extract-name
  method-extract-metavariables
  #:spacer-infos method-extract-spacer-infos
  method-extract-typeset)

(define-doc property
  "property"
  #f
  (lambda (stx space-name)
    (method-extract-name stx space-name #:property? #t))
  (lambda (stx space-name vars)
    (method-extract-metavariables stx space-name vars #:property? #t))
  #:spacer-infos (lambda (stx space-names)
                   (method-extract-spacer-infos stx space-names #:property? #t))
  (lambda (stx space-name subst)
    (method-extract-typeset stx space-name subst #:property? #t)))

(define-doc dot
  "expression"
  #f
  (lambda (stx space-name)
    (syntax-parse stx
      #:datum-literals (group parens :: |.| op)
      [(group _ (parens (group _ (op ::) class::dotted-class)) (op |.|) name . _)
       (build-dotted #'class.head #:prefix #'class.tail #'name)]))
  (lambda (stx space-name vars)
    (syntax-parse stx
      #:datum-literals (group parens :: |.| op)
      [(group _ (parens (group id (op ::) . _)) (op |.|) _ . more)
       (extract-pattern-metavariables #'(group . more) (add-metavariable vars #'id #f))]))
  (lambda (stx space-name subst)
    (syntax-parse stx
      #:datum-literals (group parens :: |.| op)
      [(group tag (~and lhs (parens (group _ (op ::) . _))) (~and dot (op |.|)) name . more)
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

(define-doc Parameter.def [rhombus Parameter]
  "context parameter"
  #f
  (lambda (stx space-name)
    (syntax-parse stx
      #:datum-literals (group |.| op)
      [(group _ (op |.|) _ (~var id (identifier-target space-name)) . _) (attribute id.name)]))
  (lambda (stx space-name vars) vars)
  (lambda (stx space-name subst)
    (syntax-parse stx
      #:datum-literals (group |.| op)
      [(group ns (~and dot (op |.|)) tag (~var id (identifier-target space-name)) e ...)
       (rb #:at stx
           #`(group ns dot tag #,@(subst (attribute id.name)) e ...))])))

(define-doc operator
  "operator"
  (lambda (stx)
    '(#f rhombus/repet))
  (lambda (stx space-name)
    (syntax-parse stx
      #:datum-literals (group parens)
      [(group _ (parens (group (~var id (target space-name)) arg1)) . _) (attribute id.name)]
      [(group _ (parens (group arg1 (~var id (target space-name)) arg2)) . _) (attribute id.name)]))
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
           #`(group tag (p-tag (g-tag #,@(subst (attribute id.name)) arg)) e ...))]
      [(group tag ((~and p-tag parens) ((~and g-tag group) arg0 (~var id (target space-name)) arg1)) e ...)
       (rb #:at stx
           #`(group tag (p-tag (g-tag arg0 #,@(subst (attribute id.name)) arg1)) e ...))])))

(define-doc operator_order.def operator_order
  "operator order"
  rhombus/operator_order
  head-dot-head-extract-name
  (lambda (stx space-name vars) vars)
  head-dot-head-extract-typeset)

(define-doc operator_order.def_set operator_order
  "operator order set"
  rhombus/operator_order
  head-dot-head-extract-name
  (lambda (stx space-name vars) vars)
  head-dot-head-extract-typeset)

(define-doc syntax_class
  "syntax class"
  rhombus/stxclass
  (lambda (stx space-name)
    (syntax-parse stx
      #:datum-literals (group)
      [(group _ (~var id (identifier-target space-name)) . _) (attribute id.name)]))
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
           #`(group tag #,@(subst (attribute id.name)) e ...))])))

(define-for-syntax (class-extract-descs stx)
  (syntax-parse stx
    #:datum-literals (group block parens)
    [(group _ ... (parens field ...) . _)
     (cons "class"
           (for/list ([field (in-list (syntax->list #'(field ...)))])
             "function"))]))

(define-for-syntax (class-extract-space-names stx)
  (syntax-parse stx
    #:datum-literals (group block parens)
    [(group _ ... (parens field ...) . tail)
     (define (unless-none name space-names)
       (syntax-parse #'tail
         #:datum-literals (block)
         [((block g ...))
          (if (for/or ([g (in-list (syntax->list #'(g ...)))])
                (define id
                  (syntax-parse g
                    #:datum-literals (group block)
                    [(group id #:none) #'id]
                    [(group id (block (group #:none))) #'id]
                    [_ #'#f]))
                (eq? (syntax-e id) name))
              null
              space-names)]
         [_ space-names]))
     (cons (append (unless-none 'expression (unless-none 'constructor '(#f)))
                   (unless-none 'annotation '(rhombus/annot))
                   (unless-none 'binding '(rhombus/bind))
                   '(rhombus/class))
           (for/list ([field (in-list (syntax->list #'(field ...)))])
             (list #f)))]))

(define-for-syntax (class-extract-names stx space-name)
  (syntax-parse stx
    #:datum-literals (group block parens)
    [(group _ (~var id (identifier-target space-name)) (parens field ...) . _)
     (cons
      (attribute id.name)
      (for/list ([field (in-list (syntax->list #'(field ...)))])
        (define (field-name->name sym)
          (syntax-parse (append (syntax->list #'id) (list #'(op |.|) sym))
            [((~var sym (identifier-target #f #:raw (symbol->string (syntax-e sym)))))
             (attribute sym.name)]))
        (syntax-parse field
          #:datum-literals (group block mutable)
          [(group mutable id:identifier . _)
           (field-name->name #'id)]
          [(group id:identifier . _)
           (field-name->name #'id)]
          [(group :keyword (block (group mutable id:identifier . _)))
           (field-name->name #'id)]
          [(group :keyword (block (group id:identifier . _)))
           (field-name->name #'id)])))]))

(define-for-syntax (class-body-extract-metavariables stx space-name vars)
  (syntax-parse stx
    #:datum-literals (group block parens)
    [(group _ _ ... (parens . _) (block g ...))
     (for/fold ([vars vars]) ([g (in-list (syntax->list #'(g ...)))])
       (syntax-parse g
         #:datum-literals (constructor)
         [(_ constructor . more)
          (syntax-parse #'more
            #:datum-literals (alts block group parens)
            [(#:none) vars]
            [((block (group #:none))) vars]
            [((parens . gs) . _)
             (do-parens-extract-metavariables #'gs space-name vars)]
            [((alts (block (group (parens . gs)) . _) ...))
             (for/fold ([vars vars]) ([gs (in-list (syntax->list #'(gs ...)))])
               (do-parens-extract-metavariables gs space-name vars))])]
         [_ vars]))]
    [_ vars]))

(define-for-syntax (class-extract-typeset stx space-names substs)
  (syntax-parse stx
    #:datum-literals (group parens)
    [(group tag (~var id (identifier-target (car space-names)))
            ((~and p-tag parens) field ...)
            e ...)
     (rb #:at stx
         #`(group tag #,@((car substs) (attribute id.name))
                  (p-tag #,@(for/list ([field (in-list (syntax->list #'(field ...)))]
                                       [subst (in-list (cdr substs))])
                              (syntax-parse field
                                #:datum-literals (group mutable block)
                                [((~and tag group) (~and mut mutable) id:identifier . r)
                                 #`(tag mut #,@(subst #'id #:as_meta #t) . r)]
                                [((~and tag group) id:identifier . r)
                                 #`(tag #,@(subst #'id #:as_meta #t) . r)]
                                [((~and tag group) kw:keyword ((~and blk block) ((~and tag2 group) (~and mut mutable) id:identifier . r)))
                                 #`(tag kw (blk (tag2 mut #,@(subst #'id #:as_meta #t) . r)))]
                                [((~and tag group) kw:keyword ((~and blk block) ((~and tag2 group) id:identifier . r)))
                                 #`(tag kw (blk (tag2 #,@(subst #'id #:as_meta #t) . r)))])))
                  e ...))]))

(define-for-syntax (implements-extract-spacer-infos stx space-names)
  (cons
   (syntax-parse stx
     #:datum-literals (group block implements extends)
     [(group _ ... (block _ ... (group (~or implements extends) (~var id (identifier-target 'rhombus/annot))) _ ...))
      (hash 'method_fallback (target->dotted-identifier (attribute id.name) (attribute id.sym)))]
     [(group _ ... (block _ ... (group (~or implements extends) (block (group (~var id (identifier-target 'rhombus/annot))))) _ ...))
      (hash 'method_fallback (target->dotted-identifier (attribute id.name) (attribute id.sym)))]
     [_ #f])
   (map (lambda (s) #f) (cdr space-names))))

(define-doc class
  class-extract-descs
  (lambda (stx)
    (class-extract-space-names stx))
  class-extract-names
  (lambda (stx space-name vars)
    (class-body-extract-metavariables
     stx
     space-name
     (parens-extract-metavariables stx space-name vars)))
  #:spacer-infos implements-extract-spacer-infos
  class-extract-typeset)

(define-doc interface
  "interface"
  (lambda (stx)
    '(rhombus/class rhombus/annot))
  head-extract-name
  head-extract-metavariables
  #:spacer-infos implements-extract-spacer-infos
  head-extract-typeset)

(define-doc veneer
  "veneer"
  (lambda (stx)
    '(rhombus/class rhombus/annot))
  head-extract-name
  head-extract-metavariables
  #:spacer-infos implements-extract-spacer-infos
  head-extract-typeset)

(begin-for-syntax
  (define-splicing-syntax-class :enum-rhs
    #:datum-literals (group block alts)
    (pattern (~seq (block clause ...)))
    (pattern (~seq (alts (block clause) ...)))))

(define-for-syntax (enum-extract-descs stx)
  (syntax-parse stx
    #:datum-literals (group block)
    [(group _ ...
            rhs::enum-rhs)
     (with-syntax ([((sym ...) ...) (enum-extract-syms #'(rhs.clause ...))])
       (cons "enumeration"
             (for/list ([sym (in-list (syntax->list #'(sym ... ...)))])
               "value")))]
    [_ (list "enumeration")]))

(define-for-syntax (enum-extract-space-names stx)
  (syntax-parse stx
    #:datum-literals (group block)
    [(group _ ...
            rhs::enum-rhs)
     (with-syntax ([((sym ...) ...) (enum-extract-syms #'(rhs.clause ...))])
       (cons (list 'rhombus/annot)
             (for/list ([sym (in-list (syntax->list #'(sym ... ...)))])
               (list #f 'rhombus/bind))))]
    [_ (list (list 'rhombus/annot))]))

(define-for-syntax (enum-extract-names stx space-name)
  (syntax-parse stx
    #:datum-literals (group block)
    [(group _ (~var id (identifier-target space-name))
            rhs::enum-rhs)
     (with-syntax ([((sym ...) ...) (enum-extract-syms #'(rhs.clause ...))])
       (cons (attribute id.name)
             (for/list ([sym (in-list (syntax->list #'(sym ... ...)))])
               (syntax-parse (append (syntax->list #'id) (list #'(op |.|) sym))
                 [((~var sym (identifier-target #f #:raw (symbol->string (syntax-e sym)))))
                  (attribute sym.name)]))))]
    [(group _ (~var id (identifier-target space-name)))
     (list (attribute id.name))]))

(define-for-syntax (enum-extract-typeset stx space-namess substs)
  (syntax-parse stx
    #:datum-literals (group block)
    [(group form (~var id (identifier-target (car space-namess)))
            rhs::enum-rhs)
     (with-syntax ([((sym ...) ...) (enum-extract-syms #'(rhs.clause ...))])
       (with-syntax ([(((def-sym ...) ...) ...)
                      (let loop ([symss (syntax->list #'((sym ...) ...))]
                                 [substs (cdr substs)])
                        (cond
                          [(null? symss) null]
                          [else
                           (let iloop ([syms (syntax->list (car symss))]
                                       [substs substs]
                                       [accum null])
                             (cond
                               [(null? syms) (cons (reverse accum)
                                                   (loop (cdr symss) substs))]
                               [else
                                (iloop (cdr syms) (cdr substs) (cons ((car substs) (car syms))
                                                                     accum))]))]))])
         (with-syntax ([(new-clause ...)
                        (for/list ([clause (in-list (syntax->list #'(rhs.clause ...)))]
                                   [def-syms (in-list (syntax->list #'(((def-sym ...) ...) ...)))])
                          (syntax-parse clause
                            [(group-tag _:identifier ...)
                             (with-syntax ([((def-sym ...) ...) def-syms])
                               #'(group-tag def-sym ... ...))]
                            [_ clause]))])
           (syntax-parse #'rhs
             #:datum-literals (block)
             [(((~and block block-tag) _ ...))
              (rb #:at stx
                  #`(group form #,@((car substs) (attribute id.name))
                           (block-tag new-clause ...)))]
             [((alts-tag (block-tag . _) ...))
              (rb #:at stx
                  #`(group form #,@((car substs) (attribute id.name))
                           (alts-tag (block-tag new-clause) ...)))]))))]
    [(group form (~var id (identifier-target (car space-namess))))
     (rb #:at stx
         #`(group form #,@((car substs) (attribute id.name))))]))

(define-for-syntax (enum-extract-syms clauses)
  (for/list ([clause (in-list (syntax->list clauses))])
    (syntax-parse clause
      #:datum-literals (group)
      [(group sym:identifier ...) #'(sym ...)]
      [_ #'()])))

(define-doc enum defn
  (begin enum-extract-descs)
  (begin enum-extract-space-names)
  enum-extract-names
  (lambda (stx space-name vars) vars)
  enum-extract-typeset)

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
       [(group _ id) (extract-term-metavariables #'id vars #t)]
       [(group _ id b)
        (let ([vars  (extract-term-metavariables #'id vars #t)])
          (extract-pattern-metavariables #'(group b) vars))]))
   #:extract-typeset
   (lambda (stx space-name subst)
     (let retry ([stx stx])
       (syntax-parse stx
         #:datum-literals (group alts block)
         [(group grammar id)
          #`(paragraph plain #,(subst #'id #:as_wrap #f))]
         [(group grammar id (block g ...))
          (warn-deprecated! 'grammar-block-instead-of-alts "15-JAN-2026")
          (retry #'(group grammar id (alts (block g) ...)))]
         [(group grammar id (alts (block g) ...))
          #`(typeset-grammar #,(subst #'id #:as_wrap #f)
                             #,@(for/list ([g (in-list (syntax->list #'(g ...)))])
                                  (syntax-parse g
                                    #:datum-literals (group)
                                    [(group t ...)
                                     (rb #'(group t ...)
                                         #:at g
                                         #:pattern? #t
                                         #:options #'(parens (group #:inset (block (group (parsed #:rhombus/expr #f))))))])))])))))

(define-doc-syntax non_target
  (let ()
    (define (trim stx)
      (syntax-parse stx
        #:datum-literals (group block)
        [(group _ (block g)) #'g]))
    (define (bounce stx sel)
      (syntax-parse stx
        #:datum-literals (group block)
        [(group _ (block (group . (~var name (:hier-name-seq in-name-root-space in-doc-space name-path-op name-root-ref)))))
         (define v (syntax-local-value (in-doc-space #'name.name) (lambda () #f)))
         (unless (doc-transformer? v)
           (raise-syntax-error #f "cannot find doc transformer" stx))
         (sel v)]))
    (make-doc-transformer
     #:extract-desc
     (lambda (stx)
       ((bounce stx doc-transformer-extract-desc) (trim stx)))
     #:extract-space-sym
     (lambda (stx)
       ((bounce stx doc-transformer-extract-space-sym) (trim stx)))
     #:extract-name
     (lambda (stx space-name)
       (define defs ((bounce stx doc-transformer-extract-defined) (trim stx) space-name))
       (if (list? defs)
           (map (lambda (def) #f) defs)
           #f))
     #:extract-metavariables
     (lambda (stx space-name vars)
       ((bounce stx doc-transformer-extract-metavariables) (trim stx) space-name vars))
     #:extract-typeset
     (lambda (stx space-name subst)
       ((bounce stx doc-transformer-extract-typeset) (trim stx) space-name subst)))))

(define-doc-syntax operator_order
  (make-doc-transformer
   #:extract-desc
   (lambda (stx) (list #f))
   #:extract-space-sym
   (lambda (stx) (list #f))
   #:extract-name
   (lambda (stx space-name) (list #f))
   #:extract-metavariables
   (lambda (stx space-name vars)
     vars)
   #:extract-typeset
   (lambda (stx space-name subst)
     (syntax-parse stx
       #:datum-literals (group block)
       [(group _ (block option0 option ...))
        (rb #:at #'option0
            #:options #'(parens (group #:inset (block (group (parsed #:rhombus/expr #f))))
                                (group #:space (block (group rhombus/operator_order #f))))
            #'(multi option0 option ...))]))))

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

(define-doc doc
  "doc entry"
  rhombus/doc
  identifier-macro-extract-name
  identifier-macro-extract-metavariables
  identifier-macro-extract-typeset)
