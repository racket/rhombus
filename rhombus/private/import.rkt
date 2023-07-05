#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     racket/symbol
                     enforest
                     enforest/operator
                     enforest/transformer
                     enforest/property
                     enforest/syntax-local
                     enforest/name-parse
                     enforest/hier-name-parse
                     enforest/proc-name
                     syntax/private/modcollapse-noctc
                     "srcloc.rkt"
                     "name-path-op.rkt"
                     "introducer.rkt"
                     "macro-result.rkt"
                     "import-invert.rkt"
                     "tag.rkt"
                     "id-binding.rkt"
                     "operator-parse.rkt"
                     "srcloc-span.rkt")
         "enforest.rkt"
         "name-root.rkt"
         "name-root-space.rkt"
         "name-root-ref.rkt"
         (submod "module-path.rkt" for-import-export)
         "definition.rkt"
         "dot.rkt"
         (submod "dot.rkt" for-dot-provider)
         "space.rkt"
         "space-parse.rkt"
         "parens.rkt"
         "import-lower-require.rkt"
         "import-from-namespace.rkt"
         "space-in.rkt"
         (only-in "implicit.rkt"
                  #%literal)
         (only-in "arithmetic.rkt"
                  [/ rhombus/])
         "dotted-sequence-parse.rkt")

(provide (for-space rhombus/defn
                    import)

         (for-space rhombus/impo
                    #%juxtapose
                    #%literal
                    (rename-out [rhombus/ /]
                                [rhombus-file file]
                                [rhombus-lib lib]
                                [rhombus-self self]
                                [rhombus-parent parent]
                                [rhombus-! !]
                                [rhombus. |.|])
                    as
                    open
                    expose
                    rename
                    only
                    except
                    only_space
                    except_space
                    meta
                    meta_label))

(module+ for-meta
  (provide (for-syntax import-modifier
                       import-modifier-block
                       in-import-space
                       :import
                       :prefix-op+import+tail
                       :infix-op+import+tail
                       :import-modifier)
           define-import-syntax)
  (begin-for-syntax
    (provide (property-out import-prefix-operator)
             (property-out import-infix-operator)
             import-prefix+infix-operator)))

(begin-for-syntax
  (property import-prefix-operator prefix-operator)
  (property import-infix-operator infix-operator)

  (struct import-prefix+infix-operator (prefix infix)
    #:property prop:import-prefix-operator (lambda (self) (import-prefix+infix-operator-prefix self))
    #:property prop:import-infix-operator (lambda (self) (import-prefix+infix-operator-infix self)))

  (property import-modifier transformer)
  (property import-modifier-block transformer)

  (define in-import-space (make-interned-syntax-introducer/add 'rhombus/impo))

  (define (check-import-result form proc)
    (unless (syntax? form) (raise-bad-macro-result (proc-name proc) "import" form))
    form)

  (define (make-identifier-import id)
    (unless (module-path? (syntax-e id))
      (raise-syntax-error 'import
                          "not a valid module path element, and not bound as a namespace"
                          id))
    id)

  (define name-root-portal-ref
    (make-name-root-ref #:non-portal-ref (lambda (ns id tail)
                                           (values ns (list* #'dotted-path. id tail)))))

  (define-rhombus-enforest
    #:syntax-class :import
    #:prefix-more-syntax-class :prefix-op+import+tail
    #:infix-more-syntax-class :infix-op+import+tail
    #:desc "import"
    #:operator-desc "import operator"
    #:parsed-tag #:rhombus/impo
    #:in-space in-import-space
    #:prefix-operator-ref import-prefix-operator-ref
    #:infix-operator-ref import-infix-operator-ref
    #:check-result check-import-result
    #:make-identifier-form make-identifier-import
    #:name-root-ref name-root-portal-ref)

  (define (make-import-modifier-ref transform-in req)
    ;; "accessor" closes over `req`:
    (lambda (v)
      (define mod (or (import-modifier-ref v)
                      (import-modifier-block-ref v)))
      (and mod
           (transformer (lambda (stx)
                          ((transformer-proc mod) (transform-in req) stx))))))

  (define-rhombus-transform
    #:syntax-class (:import-modifier parsed-req)
    #:desc "import modifier"
    #:parsed-tag #:rhombus/impo
    #:in-space in-import-space
    #:transformer-ref (make-import-modifier-ref transform-in (syntax-parse parsed-req
                                                               #:datum-literals (parsed)
                                                               [(parsed #:rhombus/impo req) #'req]
                                                               [_ (raise-arguments-error
                                                                   'import_meta.ParsedModifier
                                                                   "given import to modify is not parsed"
                                                                   "base import" parsed-req)]))
    #:accept-parsed? #t)

  (define (extract-prefixes r #:require-identifier? require-identifier?)
    (let extract ([r r] [accum null])
      (syntax-parse r
        #:datum-literals (rename-in only-in except-in expose-in rhombus-prefix-in for-meta for-label
                                    only-space-in only-spaces-in except-spaces-in
                                    submod)
        [#f (reverse accum)]
        [((~or rename-in only-in except-in expose-in for-label only-spaces-in except-spaces-in) mp . _)
         (extract #'mp accum)]
        [((~and tag rhombus-prefix-in) mp name ctx)
         (if (or (identifier? #'name)
                 (not (syntax-e #'name))
                 (eq? (syntax-e #'name) '#:none))
             (extract #'mp (cons r accum))
             (if require-identifier?
                 (raise-syntax-error #f
                                     "expected an identifier"
                                     #'name)
                 (syntax-parse #'name
                   [(_ op) (extract #'mp (cons #'(tag mp op ctx)
                                               accum))])))]
        [((~or for-meta only-space-in) _ mp) (extract #'mp accum)]
        [_ (raise-syntax-error 'import
                               "don't know how to extract module path"
                               r)])))

  (define (extract-prefix+open-id mp r #:require-identifier? [require-identifier? #t])
    (define prefixes (extract-prefixes r #:require-identifier? require-identifier?))
    (define (extract-string-prefix mp)
      (datum->syntax
       mp
       (string->symbol
        (regexp-replace #rx"[.].*$"
                        (regexp-replace #rx"^.*/" (syntax-e mp) "")
                        ""))
       mp))
    (cond
      [(null? prefixes)
       (values
        (syntax-parse mp
          #:datum-literals (lib import-root import-dotted import-spaces file submod reimport singleton quote)
          [_:string (extract-string-prefix mp)]
          [_:identifier (datum->syntax
                         mp
                         (string->symbol (regexp-replace #rx"^.*/"
                                                         (symbol->string (syntax-e mp))
                                                         ""))
                         mp)]
          [(lib str) (extract-string-prefix #'str)]
          [(import-root id . _) #'id]
          [(import-dotted _ id) #'id]
          [(import-spaces _ mp . _) #'mp]
          [(reimport id . _) #'id]
          [(singleton _ id) #'id]
          [(file str) (let-values ([(base name dir?) (split-path (syntax-e #'str))])
                        (datum->syntax
                         mp
                         (string->symbol (path->string (path-replace-suffix name #"")))))]
          [(submod _ ... id) #'id]
          [(submod ".") (datum->syntax mp 'self)]
          [(submod "..") (datum->syntax mp 'parent)]
          [(quote id) #'id]
          [_
           (raise-syntax-error 'import
                               "don't know how to extract default prefix"
                               mp)])
        #f)]
      [(null? (cdr prefixes))
       (syntax-parse (car prefixes)
         [(_ mp name ctx) (values #'name (and (syntax-e #'ctx) #'ctx))])]
      [else
       (raise-syntax-error 'import
                           "second prefix specification not allowed"
                           ;; `as` and `open` put original text on `rhombus-prefix-in`
                           (syntax-parse (car prefixes)
                             [(form _ _ _) #'form]))]))

  (define (apply-modifiers mods r-parsed)
    (cond
      [(null? mods) r-parsed]
      [else
       (syntax-parse (car mods)
         #:datum-literals (group)
         [(~var im (:import-modifier #`(parsed #:rhombus/impo #,r-parsed)))
          (apply-modifiers (cdr mods) #'im.parsed)]
         [(group form . _)
          (raise-syntax-error #f
                              "not an import modifier"
                              #'form)])]))

  (define-syntax-class :modified-imports
    #:datum-literals (group block)
    (pattern (group mod-id:identifier mod-arg ... (block imp ...))
             #:when (syntax-local-value* (in-import-space #'mod-id) import-modifier-ref)
             #:attr mod #'(group mod-id mod-arg ...))))

(define-defn-syntax import
  (definition-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ (block r ...))
        #`((rhombus-import #,stx () r ...))]
       [(_ r ...)
        #`((rhombus-import #,stx () (#,group-tag r ...)))]))))

(define-syntax (rhombus-import stx)
  ;; handle one import group at a time, so it can import
  ;; transformers that are used for later imports
  (syntax-parse stx
    [(_ _ _) #'(begin)]
    [(_ orig mods mi::modified-imports . more)
     #`(begin
         (rhombus-import orig (mi.mod . mods) mi.imp)
         ...
         (rhombus-import orig mods . more))]
    [(_ orig mods r::import . more)
     ;; apply modifiers, but then flip around to extract
     ;; module path from the modifiers
     (define r-parsed (apply-modifiers (reverse (syntax->list #'mods))
                                       #'r.parsed))
     (define-values (mod-path-stx r-stx) (import-invert r-parsed #'orig #'r))
     #`(begin
         (rhombus-import-one #hasheq() #f #,mod-path-stx #,r-stx (no-more wrt-placeholder dotted-placeholder))
         (rhombus-import orig mods . more))]))

;; Uses a continuation form to thread through the module path that
;; accessed-via-dots module paths are relative to and to thread
;; through a dotted path prefix. When a dot prefix is used, then we
;; get an `import-spaces` form, which can have a mixture of modules
;; (to re-import), namespaces, and singletons (i.e., a specific single
;; export), and we have to sort out that mixture here.
(define-syntax (rhombus-import-one stx)
  (syntax-parse stx
    #:datum-literals (import-dotted import-root import-spaces singleton)
    [(_ wrt dotted (import-dotted mod-path id) r k)
     (maybe-convert-dotted-to-expose
      #'mod-path #'id #'r
      (lambda (mod-path r)
        #`(rhombus-import-one wrt dotted #,mod-path #,r k))
      (lambda ()
        ;; need to handle `import-dotted` in a right-associative way, but it
        ;; was parsed as left-associative
        (define (make-id after-mp id)
          (datum->syntax (extract-mod-path after-mp) (syntax-e id) id id))
        (let loop ([mod-path #'mod-path]
                   [k (lambda (after-mp)
                        #`(rhombus-import-dotted-one wrt dotted #,(make-id after-mp #'id) id #f r k))])
          (syntax-parse mod-path
            #:datum-literals (import-dotted)
            [(import-dotted next-mod-path next-id)
             (loop #'next-mod-path
                   (lambda (after-mp)
                     (define new-id (make-id after-mp #'next-id))
                     (define new-next-id ((make-syntax-introducer) #'next-id))
                     #`(rhombus-import-dotted-one wrt dotted #,new-id #,new-next-id #t (rhombus-prefix-in #f #f #f)
                                                  #,(k new-next-id))))]
            [_
             (define m-mod-path (intro-mod-path mod-path (make-syntax-introducer)))
             #`(rhombus-import-one wrt dotted #,m-mod-path (rhombus-prefix-in #f #f #f)
                                   #,(k m-mod-path))]))))]
    [(_ wrt dotted (import-spaces dot-name ir ...) r (k-form wrt-placeholder dotted-placeholder . k-args))
     ;; Split into the three types of imports:
     (define-values (mods orig-mods namesps sings) (split-imports (syntax->list #'(ir ...))))
     ;; For each space where the import is a module, collapse
     ;; module paths to deal with import chains
     (define new-wrt
       (for/fold ([wrt (syntax-e #'wrt)])
                 ([mod (in-list mods)])
         (syntax-parse mod
           [(space mp)
            (hash-set (if (eq? '#:all (syntax-e #'space))
                          #hasheq()
                          wrt)
                      (syntax-e #'space)
                      (collapse-path #'mp
                                     (or (hash-ref wrt (syntax-e #'space) #f)
                                         (hash-ref wrt '#:all #'#f))))])))
     (define new-dotted (if (syntax-e #'dot-name)
                            (string->symbol (string-append
                                             (if (syntax-e #'dotted)
                                                 (string-append
                                                  (symbol->immutable-string (syntax-e #'dotted))
                                                  ".")
                                                 "")
                                             (symbol->immutable-string (syntax-e #'dot-name))))
                            #'dotted))
     ;; Mixtures of modules and non-module or multiple module paths
     ;; are allowed. To handle the mixture, we thread a consistent
     ;; `covered-ht` through both module and non-module imports.
     ;; Meanwhile, report an error for any work on content that's
     ;; applied to a singleton import by itself:
     (unless (or (pair? mods) (pair? namesps))
       (convert-require-from-namespace #'r #hasheq() #hasheq() #f #t #f #t))
     ;; module (re-)imports
     (define-values (mod-forms covered-ht)
       (let loop ([orig-mods orig-mods] [rev-mod-forms '()] [covered-ht #hasheq()])
         (cond
           [(null? orig-mods) (values (reverse rev-mod-forms) covered-ht)]
           [else
            (define orig-mod (car orig-mods))
            (define-values (mod-form new-covered-ht)
              (syntax-parse orig-mod
                [(space mp)
                 (define-values (prefix open-id) (extract-prefix+open-id #'mp #'r))
                 ;; The name-root expansion of import prefixes is handled by
                 ;; `name-root-ref`, which recognizes `(portal <id> (import ....))`
                 ;; forms generated by `lower-require-clause`
                 (define mp/maybe-r (hash-ref new-wrt (syntax-e #'space)))
                 (define-values (use-mp inner-r)
                   (if (vector? mp/maybe-r)
                       (values (vector-ref mp/maybe-r 0) (vector-ref mp/maybe-r 1))
                       (values mp/maybe-r #f)))
                 (lower-require-clause inner-r
                                       (if (eq? (syntax-e #'space) '#:all)
                                           #'r
                                           #`(only-space-in space r))
                                       use-mp
                                       (and (syntax-e prefix)
                                            prefix)
                                       open-id
                                       covered-ht
                                       (or (pair? (cdr orig-mods))
                                           (pair? namesps)))]))
            (loop (cdr orig-mods) (cons mod-form rev-mod-forms) new-covered-ht)])))
     ;; namespaces
     (define namesp-forms
       (let loop ([irs namesps] [covered-ht covered-ht])
         (cond
           [(null? irs) null]
           [else
            (syntax-parse (car irs)
              [(space im)
               (define-values (form new-covered-ht) (imports-from-namespace #'im #'r covered-ht (pair? (cdr irs))
                                                                            #'dotted
                                                                            (let ([sp (syntax-e #'space)])
                                                                              (if (eq? sp '#:all)
                                                                                  #f
                                                                                  sp))))
               (cons form
                     (loop (cdr irs) new-covered-ht))])])))
     #`(begin
         #,@mod-forms
         #,@namesp-forms
         ;; singletons
         #,@(for/list ([sing (in-list sings)])
              (syntax-parse sing
                #:datum-literals (singleton reimport)
                [(space (~and mp (singleton id as-id)))
                 (define-values (prefix ignored-open-id)
                   (extract-prefix+open-id #'mp #'r #:require-identifier? #f)) ; `as` "prefix" is really a rename
                 (when (eq? '#:none (syntax-e prefix))
                   (raise-syntax-error 'import
                                       "cannot rename a dotted import to `~none`"
                                       prefix))
                 (cond
                   [(syntax-e prefix)
                    (cond
                      [(space-excluded? (syntax-e #'space) #'r)
                       #'(begin)]
                      [else
                       (define intro (if (syntax-e #'space)
                                         (make-interned-syntax-introducer/add (syntax-e #'space))
                                         (lambda (x) x)))
                       (import-singleton #'id (intro prefix))])]
                   [(or (pair? mods) (pair? namesps))
                    #'(begin)]
                   [else
                    (raise-syntax-error #f
                                        "cannot open binding that is not a namespace"
                                        #'id)])]))
         (k-form #,new-wrt #,new-dotted . k-args))]
    [(_ wrt dotted mp r k)
     #'(rhombus-import-one wrt dotted (import-spaces #f (#:all mp)) r k)]))

(define-syntax (rhombus-import-dotted-one stx)
  (syntax-parse stx
    [(_ wrt dotted lookup-id id as-ns? r k)
     #`(rhombus-import-one wrt dotted #,(bound-identifier-as-import stx #'lookup-id #'id #t (syntax-e #'as-ns?)) r k)]))

(define-for-syntax (maybe-convert-dotted-to-expose mod-path id r convert-k fail-k)
  (syntax-parse mod-path
    #:datum-literals (import-dotted)
    [(import-dotted . _)
     ;; cannot handle nested dots
     (fail-k)]
    [_
     ;; phase shifting, space constraints, and `as` are all consistent
     ;; with both a singleton import or a namespace import, so we can
     ;; convert a dotted path to a non-dotted form, which lets us handle
     ;; phase shifts (which are otherwise disallowed)
     (define-values (new-r meta)
       (let loop ([r r] [prefixed? #f])
         (syntax-parse r
           #:datum-literals (rename-in only-in except-in expose-in rhombus-prefix-in for-meta for-label
                                       only-space-in only-spaces-in except-spaces-in
                                       submod)
           [#f (values #`(expose-in #,r #,id) #f)]
           [((~and tag (~or only-spaces-in except-spaces-in)) mp . tail)
            (define-values (new-r meta) (loop #'mp prefixed?))
            (values (and new-r #`(tag #,new-r . tail))
                    meta)]
           [((~and tag (~or for-label)) mp . tail)
            (define-values (new-r meta) (loop #'mp prefixed?))
            (values (and new-r #`(tag #,new-r . tail))
                    #'tag)]
           [((~and tag rhombus-prefix-in) mp name ctx)
            (define-values (new-r meta) (loop #'mp #t))
            (values (and (identifier? #'name)
                         (not prefixed?)
                         new-r
                         #`(rename-in #,new-r [#,id name]))
                    meta)]
           [((~and tag (~or only-space-in)) sp mp)
            (define-values (new-r meta) (loop #'mp prefixed?))
            (values (and new-r
                         #`(tag sp #,new-r))
                    meta)]
           [((~and tag (~or for-meta)) m mp)
            (define-values (new-r meta) (loop #'mp prefixed?))
            (values (and new-r
                         #`(tag m #,new-r))
                    #'tag)]
           [((~or only-in except-in expose-in rename-in) mp . _)
            (define-values (new-r meta) (loop #'mp prefixed?))
            (values #f meta)]
           [_ (error "oops ~s" r)])))
     (when (and meta (not new-r))
       (raise-syntax-error #f "phase shifting not supported with a dotted module path"
                           meta
                           mod-path))
     (if new-r
         (convert-k mod-path #`(rhombus-prefix-in #,new-r #:none #f))
         (fail-k))]))

(define-for-syntax (bound-identifier-as-import stx lookup-id id as-field? as-ns?)
  (define space+maps
    (for/list ([space-sym (in-list (if as-ns?
                                       (list 'rhombus/namespace)
                                       (cons #f (syntax-local-module-interned-scope-symbols))))]
               #:do[(define intro (if space-sym
                                      (make-interned-syntax-introducer/add space-sym)
                                      (lambda (id) id)))
                    (define space-id (intro lookup-id))
                    (define i (and (or (not as-field?)
                                       (identifier-distinct-binding* space-id (intro id)))
                                   (or (not space-sym)
                                       (identifier-distinct-binding* space-id lookup-id))
                                   (or (syntax-local-value* space-id import-root-ref)
                                       (and (identifier-distinct-binding* space-id (if as-field? (intro id) lookup-id))
                                            'other))))]
               #:when i)
      (cond
        [(eq? i 'other)
         (list space-sym
               #`(singleton #,space-id #,(if as-ns? id (intro id))))]
        [else
         (unless (eq? space-sym 'rhombus/namespace)
           (error "internal error: namespace or import at strange space" space-sym))
         (list '#:all
               (syntax-parse i
                 #:datum-literals (parsed nspace)
                 [(parsed mod-path parsed-r)
                  (define-values (mp r) (import-invert (syntax-local-introduce (transform-in #'parsed-r)) #f #f))
                  #`(reimport #,id #,mp #,r)]
                 [(nspace . _) #`(import-root #,id #,i #,space-id)]))])))
  (cond
    [(null? space+maps)
     (cond
       [as-field?
        (raise-syntax-error #f
                            (if as-ns?
                                "not provided as a namespace"
                                "not provided")
                            id)]
       [else
        (raise-syntax-error #f
                            "not bound as a namespace"
                            stx
                            id)])]
    [else
     #`(import-spaces #,id #,@space+maps)]))

(define-for-syntax (intro-mod-path mod-path intro)
  ;; avoid introducing a `map` that is stored in an `import-root` module path
  (let loop ([mod-path mod-path])
    (syntax-parse mod-path
      #:datum-literals (import-spaces import-root singleton)
      [(import-spaces id (s mp) ...)
       #:with (i-mp ...) (map loop (syntax->list #'(mp ...)))
       #`(import-spaces id (s i-mp) ...)]
      [(import-root id map space-id)
       #`(import-root #,(intro #'id) map space-id)]
      [(singleton lookup-id id)
       #`(singleton lookup-id #,(intro #'id))]
      [_ (intro mod-path)])))

;; extracts one syntax object just introduced by `intro-mod-path`:
(define-for-syntax (extract-mod-path mod-path)
  (let loop ([mod-path mod-path])
    (syntax-parse mod-path
      #:datum-literals (import-spaces import-root singleton)
      [(import-spaces id (s mp) . _)
       (loop #'mp)]
      [(import-spaces id) #f]
      [(import-root id map space-id)
       #'id]
      [(singleton _ id)
       #'id]
      [_ mod-path])))

(define-for-syntax (split-imports irs)
  (let loop ([irs irs] [rev-mods '()] [rev-orig-mods '()] [rev-namesps '()] [rev-sings '()])
    (cond
      [(null? irs) (values (reverse rev-mods) (reverse rev-orig-mods) (reverse rev-namesps) (reverse rev-sings))]
      [else
       (define ir (car irs))
       (syntax-parse ir
         #:datum-literals (import-root singleton)
         [(_ (import-root . _))
          (loop (cdr irs) rev-mods rev-orig-mods (cons ir rev-namesps) rev-sings)]
         [(_ (singleton . _))
          (loop (cdr irs) rev-mods rev-orig-mods rev-namesps (cons ir rev-sings))]
         [(space mp)
          (define adj-ir #`(space #,(convert-symbol-module-path #'mp)))
          (loop (cdr irs) (cons adj-ir rev-mods) (cons ir rev-orig-mods) rev-namesps rev-sings)])])))

(define-for-syntax (imports-from-namespace im r-parsed covered-ht accum? dotted-id only-space-sym)
  (define open-all-spaces? (not only-space-sym))
  (syntax-parse im
    #:datum-literals (import-root nspace)
    [(import-root id (nspace orig-id _ [key val . rule] ...) lookup-id)
     (define-values (prefix open-id) (extract-prefix+open-id #'id r-parsed))
     (define bound-prefix (string-append (symbol->immutable-string (syntax-e #'id))
                                         "."))
     (define extension-ht
       ;; get bindings from outside the namespace with names that start "id."
       (for*/fold ([ht #hasheq()]) ([space-sym (in-list (if open-all-spaces?
                                                            (cons #f (syntax-local-module-interned-scope-symbols))
                                                            (list only-space-sym)))]
                                    #:do [(define intro (if space-sym
                                                            (make-interned-syntax-introducer/add space-sym)
                                                            (lambda (x) x)))]
                                    [sym (in-list (syntax-bound-symbols (syntax-local-introduce
                                                                         (intro (out-of-name-root-space #'lookup-id)))
                                                                        (syntax-local-phase-level)
                                                                        ;; need exact-scopes binding with dotted:
                                                                        (syntax-e dotted-id)))]
                                    #:do [(define str (symbol->immutable-string sym))]
                                    #:when (and (> (string-length str) (string-length bound-prefix))
                                                (string=? bound-prefix (substring str 0 (string-length bound-prefix))))
                                    #:do [(define ext-id (intro
                                                          (out-of-name-root-space (datum->syntax #'lookup-id sym #'id))))]
                                    #:when (identifier-extension-binding? ext-id #'lookup-id))
         (define field-sym (string->symbol
                            (substring
                             (symbol->immutable-string sym)
                             (string-length bound-prefix))))
         (define id+spaces (hash-ref ht field-sym null))
         (cond
           [(ormap (lambda (id+space)
                     (free-identifier=? (car id+space) ext-id))
                   id+spaces)
            ;; already found a binding (must be in the default space) that covers this one
            ht]
           [else
            (hash-set ht field-sym (cons (cons ext-id space-sym) id+spaces))])))
     (define-values (ht expose-ht new-covered-ht as-is?)
       (convert-require-from-namespace
        r-parsed
        (let ([ht (for/hasheq ([key (in-list (syntax->list #'(key ...)))]
                               [val (in-list (syntax->list #'(val ...)))]
                               [rule (in-list (syntax->list #'(rule ...)))]
                               #:when (syntax-e key))
                    (values (syntax-e key) (expose-spaces-with-rule val rule)))])
          (for/fold ([ht ht]) ([(k id+spaces) (in-hash extension-ht)])
            (define val-id+spaces (let ([l (expose-spaces (hash-ref ht k '()) only-space-sym)])
                                    (cond
                                      [(null? l) '()]
                                      [else
                                       (define to-replace (for/hasheq ([id+space (in-list id+spaces)])
                                                            (values (cdr id+space) #t)))
                                       (for/list ([id+space (in-list id+spaces)]
                                                  #:unless (hash-ref to-replace (cdr id+space) #f))
                                         id+space)])))
            (hash-set ht k (append val-id+spaces id+spaces))))
        covered-ht
        accum?
        #f
        only-space-sym
        #f))
     (define (extension-shadows? key space-sym)
       (for/or ([id+space (in-list (hash-ref extension-ht key '()))])
         (define space (cdr id+space))
         (or (not space) (eq? space space-sym))))
     (define (already-bound? id as-id)
       (or (bound-identifier=? id as-id)
           (and (identifier-binding id (syntax-local-phase-level) #t #t)
                (free-identifier=? id as-id))))     
     (values
      #`(begin
          ;; non-exposed
          #,@(if (identifier? prefix)
                 #`(#,@(cond
                         [as-is?
                          (let ([new-id (in-name-root-space
                                         (datum->syntax #'id (syntax-e prefix) #'id))])
                            (if (already-bound? new-id #'lookup-id)
                                '()
                                #`((define-syntax #,new-id
                                     (make-rename-transformer (quote-syntax lookup-id)))
                                   ;; Additional imports for namespace extensions
                                   #,@(for*/list ([(field-sym ext-id+spaces) (in-hash extension-ht)]
                                                  [ext-id+space (in-list ext-id+spaces)]
                                                  #:do [(define ext-id (car ext-id+space))
                                                        (define intro (let ([space (cdr ext-id+space)])
                                                                        (if space
                                                                            (make-interned-syntax-introducer/add space)
                                                                            (lambda (x) x))))
                                                        (define new-id
                                                          (intro
                                                           (datum->syntax #'id
                                                                          (string->symbol
                                                                           (string-append (symbol->immutable-string (syntax-e prefix))
                                                                                          "."
                                                                                          (symbol->immutable-string field-sym)))
                                                                          #'id)))]
                                                  #:unless (already-bound? new-id ext-id))
                                        #`(define-syntax #,new-id
                                            (make-rename-transformer (quote-syntax #,ext-id)))))))]
                         [else
                          (with-syntax ([(root-id) (generate-temporaries #'(id))])
                            #`((define-name-root root-id
                                 #:orig-id orig-id
                                 #:fields
                                 #,(for/list ([(key val) (in-hash ht)])
                                     (cond
                                       [(identifier? val)
                                        ;; simple case: propagate all available spaces
                                        #`[#,key #,val]]
                                       [else
                                        ;; generate space-specific bindings
                                        #`[#,key
                                           #,(if (pair? val) (caar val) key) ; will be ignored, but needed for right shape
                                           #:space #,(for/list ([id+space (in-list val)])
                                                       #`[#,(cdr id+space) #,(car id+space)])
                                           ;; no other spaces:
                                           #:only]])))
                               ;; add rename transformer for the prefix
                               (define-syntax #,(in-name-root-space (datum->syntax #'id (syntax-e prefix) #'id))
                                 (make-rename-transformer (quote-syntax #,(in-name-root-space #'root-id))))))]))
                 null)
          ;; exposed, including extensions inside the namespace
          #,@(let ([expose-ht (if (syntax-e prefix)
                                  (close-over-extensions expose-ht ht)
                                  expose-ht)])
               (for/list ([key (in-hash-keys (if (syntax-e prefix)
                                                 expose-ht
                                                 ht))]
                          #:do [(define val/id (hash-ref ht key))
                                (define val (if (identifier? val/id)
                                                (find-identifer-in-spaces val/id only-space-sym)
                                                val/id))
                                (define spaces (cond
                                                 [(identifier? val) (list only-space-sym)]
                                                 [else (map cdr val)]))
                                (define v-ids (cond
                                                [(identifier? val) (list val)]
                                                [else (map car val)]))]
                          [v-id (in-list v-ids)]
                          [space-sym (in-list spaces)]
                          #:unless (extension-shadows? key space-sym)
                          #:do [(define intro (if space-sym
                                                  (make-interned-syntax-introducer space-sym)
                                                  (lambda (x mode) x)))]
                          #:do [(define space-v-id (intro v-id 'add))]
                          #:when (identifier-binding* space-v-id)
                          #:do [(define new-id (intro (if (syntax-e prefix)
                                                          ;; identifier must be explicitly `expose`d, so use
                                                          ;; the identifier from that declaration:
                                                          (hash-ref expose-ht key)
                                                          ;; use the identifier for `open` or supplied
                                                          ;; with `open`
                                                          (if open-id
                                                              (datum->syntax open-id key open-id)
                                                              ;; as a fallback, use the module name,
                                                              ;; which is needed for `.`-based chaining
                                                              (datum->syntax #'id key #'id)))
                                                      'add))])
                 #`(define-syntax #,new-id
                     (make-rename-transformer (quote-syntax #,space-v-id)))))
          ;; exposed extensions from outside the namespace
          #,@(for*/list ([key (in-hash-keys (if (syntax-e prefix) expose-ht ht))]
                         #:do [(define id+keep-spaces
                                 (and (syntax-e prefix) 
                                      (hash-ref ht key)))
                               (define keep-spaces
                                 (and id+keep-spaces
                                      (pair? id+keep-spaces)
                                      (for/hasheq ([id+space (in-list id+keep-spaces)])
                                        (values (cdr id+space) #t))))]
                         [id+space (in-list (hash-ref extension-ht key '()))]
                         #:do [(define as-id (car id+space))
                               (define space-sym (cdr id+space))
                               (define intro (if space-sym
                                                 (make-interned-syntax-introducer space-sym)
                                                 (lambda (x mode) x)))
                               (define new-id (intro (datum->syntax #'id key #'id) 'add))]
                         #:when (or (not keep-spaces)
                                    (hash-ref keep-spaces space-sym #f))
                         #:unless (already-bound? new-id as-id))
               #`(define-syntax #,new-id
                   (make-rename-transformer (quote-syntax #,as-id)))))
      new-covered-ht)]))

(define-for-syntax (import-singleton id as-id)
  (cond
    [(bound-identifier=? id as-id)
     #'(begin)]
    [else
     #`(define-syntax #,as-id (make-rename-transformer (quote-syntax #,id)))]))

;; "done" continuation for `rhombus-import-one`
(define-syntax (no-more stx) #'(begin))

;; return can be a vector to supply an inner `r` to go with an collapsed `mp`
(define-for-syntax (collapse-path mp wrt)
  (define-values (unwrapped-mp return)
    (syntax-parse mp
      #:datum-literals (reimport)
      [(reimport _ mp r) (values #'mp (lambda (mp) (vector mp #'r)))]
      [_ (values mp values)]))
  (define plain-mp (syntax-parse unwrapped-mp
                     #:datum-literals (only-space-in)
                     [(only-space-in _ mp) #'mp]
                     [_ unwrapped-mp]))
  (return
   (cond
     [(not (syntax-e wrt)) plain-mp]
     [else (datum->syntax plain-mp
                          (collapse-module-path (syntax->datum plain-mp) (syntax->datum wrt))
                          plain-mp
                          plain-mp)])))

(define-syntax (define-import-syntax stx)
  (syntax-parse stx
    [(_ name:id rhs)
     (quasisyntax/loc stx
       (define-syntax #,(in-import-space #'name) rhs))]))

(define-import-syntax #%juxtapose
  (import-infix-operator
   #'#%juxtapose
   '((default . weaker))
   'macro
   (lambda (form1 stx)
     (syntax-parse stx
       #:datum-literals (block group)
       [(_ (block mod ...))
        (values (apply-modifiers (syntax->list #'(mod ...))
                                 form1)
                #'())]
       [(_ mod-id:identifier mod-arg ... (block mod ...))
        #:when (syntax-local-value* (in-import-space #'mod-id) import-modifier-ref)
        (values (apply-modifiers (syntax->list #'((group mod-id mod-arg ...) mod ...))
                                 form1)
                #'())]
       [(_ mod ...)
        (values (apply-modifiers (syntax->list #'((group mod ...)))
                                 form1)
                #'())]))
   'left))

(define-import-syntax #%literal
  (make-module-path-literal-operator import-prefix-operator))

(define-import-syntax rhombus/
  (make-module-path-/-operator import-infix-operator))

(define-import-syntax rhombus-file
  (make-module-path-file-operator import-prefix-operator))

(define-import-syntax rhombus-lib
  (make-module-path-lib-operator import-prefix-operator))

(define-import-syntax rhombus-!
  (make-module-path-submod-operator import-infix-operator))

(define-import-syntax rhombus-self
  (make-module-path-submod-same-operator import-prefix-operator))

(define-import-syntax rhombus-parent
  (make-module-path-submod-up-operator import-prefix-operator))

(define-for-syntax infix-path-dot
  (import-infix-operator
   #'rhombus.
   '((default . weaker))
   'macro
   ;; infix
   (lambda (form1 stx)
     (syntax-parse stx
       #:datum-literals (group)
       [(_ id:identifier . tail)
        (values #`(import-dotted #,form1 id)
                #'tail)]
       [(_ (_::parens (group id::operator)) . tail)
        (values #`(import-dotted #,form1 id.name)
                #'tail)]
       [else
        (raise-syntax-error #f "not ready, yet" stx)]))
   'left))

(define-import-syntax rhombus.
  (import-prefix+infix-operator
   (import-prefix-operator
    #'rhombus.
    '((default . weaker))
    'macro
    (lambda (stx)
      (syntax-parse stx
        [(_ id:identifier . tail)
         (values (bound-identifier-as-import stx #'id #'id #f #t)
                 #'tail)]
        [_
         (raise-syntax-error #f
                             "expected an identifier"
                             stx)])))
   infix-path-dot))

(define-syntax dotted-path. infix-path-dot)

(define-import-syntax as
  (import-modifier
   (lambda (req stx)
     (syntax-parse stx
       [(form-id (~and kw #:none))
        (datum->syntax #f
                       (list (relocate-span #'rhombus-prefix-in (list #'form-id #'kw))
                             req
                             #'kw
                             #f))]
       [(form-id name::name)
        (datum->syntax #f
                       (list (relocate-span #'rhombus-prefix-in (list #'form-id #'name.name))
                             req
                             #'name.name
                             #f))]))))

(begin-for-syntax
  (define-syntax-class :as-id
    #:description "`as`"
    (pattern as-id:identifier
             #:when (free-identifier=? (in-import-space #'as) (in-import-space #'as-id)))))
    
(define-import-syntax rename
  (import-modifier-block
   (lambda (req stx)
     (syntax-parse stx
       #:datum-literals (block)
       [(_ int::name _::as-id ext::name)
        (datum->syntax req
                       (list* #'rename-in req #'([int.name ext.name]))
                       req)]
       [(_ (_::block (group int::name _::as-id ext::name)
                     ...))
        (datum->syntax req
                       (list* #'rename-in req #'([int.name ext.name] ...))
                       req)]))))

(define-import-syntax only
  (import-modifier-block
   (lambda (req stx)
     (syntax-parse stx
       #:datum-literals (block group)
       [(_ name::name)
        (datum->syntax req
                       (list* #'only-in req #'(name.name))
                       req)]
       [(_ (_::block (group name::name ...) ...))
        (datum->syntax req
                       (list* #'only-in req #'(name.name ... ...))
                       req)]))))

(define-import-syntax except
  (import-modifier-block
   (lambda (req stx)
     (syntax-parse stx
       #:datum-literals (block group)
       [(_ name::name)
        (datum->syntax req
                       (list* #'except-in req #'(name.name))
                       req)]
       [(_ (_::block (group name::name ...) ...))
        (datum->syntax req
                       (list* #'except-in req #'(name.name ... ...))
                       req)]))))

(define-import-syntax only_space
  (import-modifier-block
   (lambda (req stx)
     (with-syntax ([(name ...)
                    (syntax-parse stx
                      #:datum-literals (block group)
                      [(_ (_::block (group . g) ...))
                       (parse-space-names stx #'(g ...))]
                      [(_  . g)
                       (parse-space-names stx #'(g))])])
       (datum->syntax req
                      (list* #'only-spaces-in req #'(name ...))
                      req)))))

(define-import-syntax except_space
  (import-modifier-block
   (lambda (req stx)
     (with-syntax ([(name ...)
                    (syntax-parse stx
                      #:datum-literals (block group)
                      [(_ (_::block (group . g) ...))
                       (parse-space-names stx #'(g ...))]
                      [(_ . g) (parse-space-names stx #'(g))])])
       (datum->syntax req
                      (list* #'except-spaces-in req #'(name ...))
                      req)))))

(define-import-syntax open
  (import-modifier
   (lambda (req stx)
     (syntax-parse stx
       #:datum-literals (block group)
       [(_ #:scope_like id:identifier)
        (datum->syntax #f (list (relocate-span #'rhombus-prefix-in (syntax->list stx))
                                req
                                #f
                                #'id))]
       [(form-id)
        (datum->syntax #f (list (relocate* #'rhombus-prefix-in #'form-id)
                                req
                                #f
                                #f))]))))

(define-import-syntax expose
  (import-modifier-block
   (lambda (req stx)
     (syntax-parse stx
       #:datum-literals (block group)
       [(_ name::name)
        (datum->syntax req
                       (list* #'expose-in req #'(name.name))
                       req)]
       [(_ (_::block (group name::name ...) ...))
        (datum->syntax req
                       (list* #'expose-in req #'(name.name ... ...))
                       req)]))))

(define-import-syntax meta
  (import-modifier
   (lambda (req stx)
     (syntax-parse stx
       [(form phase)
        (define ph (syntax-e #'phase))
        (unless (exact-integer? ph)
          (raise-syntax-error #f "not a valid phase" stx #'phase))
        (datum->syntax #f (list (relocate-span (syntax/loc #'form for-meta)
                                               (list #'form #'phase))
                                #'phase
                                req))]
       [(form) 
        (datum->syntax #f (list (relocate* (syntax/loc #'form for-meta)
                                           #'form)
                                #'1
                                req))]))))

(define-import-syntax meta_label
  (import-modifier
   (lambda (req stx)
     (syntax-parse stx
       [(form) 
        (datum->syntax #f (list (relocate* (syntax/loc #'form for-meta)
                                           #'form)
                                #f
                                req))]))))
