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
                     "realm.rkt"
                     "import-invert.rkt"
                     "tag.rkt"
                     "id-binding.rkt")
         "enforest.rkt"
         "name-root.rkt"
         "name-root-space.rkt"
         "name-root-ref.rkt"
         (submod "module-path.rkt" for-import-export)
         "definition.rkt"
         "dot.rkt"
         (submod "dot.rkt" for-dot-provider)
         "space.rkt"
         "parens.rkt"
         "import-lower-require.rkt"
         "import-from-namespace.rkt"
         "space-in.rkt"
         (only-in "implicit.rkt"
                  #%literal)
         (only-in "arithmetic.rkt"
                  [/ rhombus/])
         "dotted-sequence-parse.rkt")

(provide import

         (for-space rhombus/impo
                    #%juxtapose
                    #%literal
                    (rename-out [rhombus/ /]
                                [rhombus-file file]
                                [rhombus-lib lib]
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
                       in-import-space)
           define-import-syntax))

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
    (unless (syntax? form) (raise-result-error* (proc-name proc) rhombus-realm "Syntax" form))
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
    #:desc "import"
    #:operator-desc "import operator"
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
    #:syntax-class (:import-modifier req)
    #:desc "import modifier"
    #:in-space in-import-space
    #:transformer-ref (make-import-modifier-ref transform-in req))

  (define (extract-prefixes r)
    (let extract ([r r] [accum null])
      (syntax-parse r
        #:datum-literals (rename-in only-in except-in expose-in rhombus-prefix-in for-meta for-label
                                    only-space-in only-spaces-in except-spaces-in)
        [#f (reverse accum)]
        [((~or rename-in only-in except-in expose-in for-label only-spaces-in except-spaces-in) mp . _)
         (extract #'mp accum)]
        [(rhombus-prefix-in mp name) (extract #'mp (cons r accum))]
        [((~or for-meta only-space-in) _ mp) (extract #'mp accum)]
        [_ (raise-syntax-error 'import
                               "don't know how to extract module path"
                               r)])))

  (define (extract-prefix mp r)
    (define prefixes (extract-prefixes r))
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
       (syntax-parse mp
         #:datum-literals (lib import-root import-dotted import-spaces file submod reimport singleton)
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
         [(submod _ id) #'id]
         [_ (raise-syntax-error 'import
                                "don't know how to extract default prefix"
                                mp)])]
      [(null? (cdr prefixes))
       (syntax-parse (car prefixes)
         [(_ mp name) #'name])]
      [else
       (raise-syntax-error 'import
                           "second prefix specification not allowed"
                           (cadr prefixes))]))

  (define (apply-modifiers mods r-parsed)
    (cond
      [(null? mods) r-parsed]
      [else
       (syntax-parse (car mods)
         #:datum-literals (group)
         [(~var im (:import-modifier r-parsed))
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

(define-syntax import
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
    [(_ wrt dotted ((~literal import-dotted) mod-path id) r k)
     #:do [(define intro (make-syntax-introducer))]
     #:with m-mod-path (intro-mod-path #'mod-path intro)
     #:with m-id (intro #'id)
     (check-allowed-for-dotted #'r)
     #`(begin
         (rhombus-import-one wrt dotted m-mod-path (rhombus-prefix-in #f #f)
                             (rhombus-import-dotted-one wrt dotted m-id id r k)))]
    [(_ wrt dotted (import-spaces dot-name ir ...) r (k-form write-placeholder dotted-placeholder . k-args))
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
       (convert-require-from-namespace #'r #hasheq() #hasheq() #f #t #f))
     ;; module re-imports
     (define-values (mod-forms covered-ht)
       (let loop ([orig-mods orig-mods] [rev-mod-forms '()] [covered-ht #hasheq()])
         (cond
           [(null? orig-mods) (values (reverse rev-mod-forms) covered-ht)]
           [else
            (define orig-mod (car orig-mods))
            (define-values (mod-form new-covered-ht)
              (syntax-parse orig-mod
                [(space mp)
                 (define prefix (extract-prefix #'mp #'r))
                 ;; The name-root expansion of import prefixes is handled by
                 ;; `name-root-ref`, which recognizes `(portal <id> (import ....))`
                 ;; forms generated by `lower-require-clause`
                 (lower-require-clause (if (eq? (syntax-e #'space) '#:all)
                                           #'r
                                           #'(only-space-in space r))
                                       (hash-ref new-wrt (syntax-e #'space))
                                       (and (syntax-e prefix)
                                            prefix)
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
                                                                            (syntax-e #'space)))
               (cons form
                     (loop (cdr irs) new-covered-ht))])])))
     #`(begin
         #,@mod-forms
         #,@namesp-forms
         ;; singletons
         #,@(for/list ([sing (in-list sings)])
              (syntax-parse sing
                #:datum-literals (singleton)
                [(space (~and mp (singleton id as-id)))
                 (define prefix (extract-prefix #'mp #'r)) ; `as` "prefix" is really a rename
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
    [(_ wrt dotted lookup-id id r k)
     #`(rhombus-import-one wrt dotted #,(bound-identifier-as-import stx #'lookup-id #'id #t) r k)]))

(define-for-syntax (bound-identifier-as-import stx lookup-id id as-field?)
  (define space+maps
    (for/list ([space-sym (in-list (cons #f (syntax-local-module-interned-scope-symbols)))]
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
               #`(singleton #,space-id #,(intro id)))]
        [else
         (unless (eq? space-sym 'rhombus/namespace)
           (error "internal error: namespace or import at strange space" space-sym))
         (list #f
               (syntax-parse i
                 #:datum-literals (parsed map)
                 [(parsed mod-path parsed-r) #`(reimport #,id #,(syntax-local-introduce (transform-in #'parsed-r)))]
                 [(map . _) #`(import-root #,id #,i #,space-id)]))])))
  (cond
    [(null? space+maps)
     (if as-field?
         (raise-syntax-error #f
                             (string-append "not provided as a namespace")
                             id)
         (raise-syntax-error #f
                             (string-append "not bound as a namespace")
                             stx
                             id))]
    [else
     #`(import-spaces #,id #,@space+maps)]))

(define-for-syntax (intro-mod-path mod-path intro)
  ;; avoid introducing a `map` that is stored in an `import-root` module path
  (let loop ([mod-path mod-path])
    (syntax-parse mod-path
      #:datum-literals (import-spaces import-root)
      [(import-spaces id (s mp) ...)
       #:with (i-mp ...) (map loop (syntax->list #'(mp ...)))
       #`(import-spaces id (s i-mp) ...)]
      [(import-root id map space-id)
       #`(import-root #,(intro #'id) map space-id)]
      [_ (intro mod-path)])))

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
    #:datum-literals (import-root map)
    [(import-root id (map orig-id _ [key val] ...) lookup-id)
     (define prefix (extract-prefix #'id r-parsed))
     (define bound-prefix (string-append (symbol->immutable-string (syntax-e #'id))
                                         "."))
     (define extension-ht
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
                               #:when (syntax-e key))
                    (values (syntax-e key) val))])
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
        only-space-sym))
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
          #,@(if (syntax-e prefix)
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
                          ;; if spaces were reduced, then we'll need to introduce
                          ;; fresh rename transformers to map to them; note that `ht`
                          ;; includes additional bindings from namespace extensions
                          (define key-rhs-ht
                            (for/hasheq ([(key val) (in-hash ht)]
                                         #:when (cond
                                                  [(identifier? val)
                                                   ;; no space modifier fetched the list of binding spaces,
                                                   ;; so the spaces certainly weren't reduced
                                                   #f]
                                                  [(and (cdar val)
                                                        (null? (cdr val)))
                                                   ;; only one non-default space remaining, so we can use that identifier
                                                   #f]
                                                  [else
                                                   ;; make a new name, to which we'll add each space
                                                   #t]))
                              (values key ((make-syntax-introducer) (datum->syntax #f key)))))
                          (with-syntax ([(root-id) (generate-temporaries #'(id))])
                            #`((define-name-root root-id
                                 #:orig-id orig-id
                                 #:fields
                                 #,(for/list ([(key val) (in-hash ht)])
                                     #`[#,key #,(let ([v (hash-ref key-rhs-ht key val)])
                                                  (if (pair? v)
                                                      (caar v)
                                                      v))]))
                               ;; add rename transformers needed to implement space pruning
                               #,@(for/list ([(key rhs) (in-hash key-rhs-ht)]
                                             #:do [(define val (hash-ref ht key))]
                                             [id+space (in-list val)])
                                    (define space (cdr id+space))
                                    #`(define-syntax #,(if space
                                                           ((make-interned-syntax-introducer space) rhs)
                                                           rhs)
                                        (make-rename-transformer (quote-syntax #,(car id+space)))))
                               ;; add rename transformer for the prefix
                               (define-syntax #,(in-name-root-space (datum->syntax #'id (syntax-e prefix) #'id))
                                 (make-rename-transformer (quote-syntax #,(in-name-root-space #'root-id))))))]))
                 null)
          ;; exposed
          #,@(for/list ([key (in-hash-keys (if (syntax-e prefix) expose-ht ht))]
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
                        #:do [(define new-id (intro (datum->syntax #'id key #'id) 'add))])
               #`(define-syntax #,new-id
                   (make-rename-transformer (quote-syntax #,space-v-id))))
          ;; exposed extensions
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

(define-for-syntax (collapse-path mp wrt)
  (define unwrapped-mp (syntax-parse mp
                         #:datum-literals (reimport)
                         [(reimport _ mp) #'mp]
                         [_ mp]))
  (define plain-mp (syntax-parse unwrapped-mp
                     #:datum-literals (only-space-in)
                     [(only-space-in _ mp) #'mp]
                     [_ unwrapped-mp]))
  (cond
    [(not (syntax-e wrt)) plain-mp]
    [else (datum->syntax plain-mp
                         (collapse-module-path (syntax->datum plain-mp) (syntax->datum wrt))
                         plain-mp
                         plain-mp)]))
  
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

(define-for-syntax infix-path-dot
  (import-infix-operator
   #'rhombus.
   '((default . weaker))
   'macro
   ;; infix
   (lambda (form1 stx)
     (syntax-parse stx
       [(_ id:identifier . tail)
        (values #`(import-dotted #,form1 id)
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
         (values (bound-identifier-as-import stx #'id #'id #f)
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
       [(_ name:identifier)
        (datum->syntax req
                       (list #'rhombus-prefix-in req #'name)
                       req)]))))

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
       [(_ (_::block (group name::name ...) ...))
        (datum->syntax req
                       (list* #'only-in req #'(name.name ... ...))
                       req)]))))

(define-import-syntax except
  (import-modifier-block
   (lambda (req stx)
     (syntax-parse stx
       #:datum-literals (block group)
       [(_ (_::block (group name::name ...) ...))
        (datum->syntax req
                       (list* #'except-in req #'(name.name ... ...))
                       req)]))))

(define-for-syntax (parse-space-names stx gs)
  (apply
   append
   (for/list ([g (in-list (syntax->list gs))])
     (let loop ([g g])
       (syntax-parse g
         [() null]
         [(~var h (:hier-name-seq in-name-root-space in-space-space name-path-op name-root-ref))
          (define sp (syntax-local-value* (in-space-space #'h.name) space-name-ref))
          (unless (space-name? sp)
            (raise-syntax-error #f
                                "not bound as a space"
                                stx
                                #'h.name))
          (cons (space-name-symbol sp)
                (loop #'h.tail))])))))

(define-import-syntax only_space
  (import-modifier-block
   (lambda (req stx)
     (syntax-parse stx
       #:datum-literals (block group)
       [(_ (_::block (group . g) ...))
        (with-syntax ([(name ...) (parse-space-names stx #'(g ...))])
          (datum->syntax req
                         (list* #'only-spaces-in req #'(name ...))
                         req))]))))

(define-import-syntax except_space
  (import-modifier-block
   (lambda (req stx)
     (syntax-parse stx
       #:datum-literals (block group)
       [(_ (_::block (group . g) ...))
        (with-syntax ([(name ...) (parse-space-names stx #'(g ...))])
          (datum->syntax req
                         (list* #'except-spaces-in req #'(name ...))
                         req))]))))

(define-import-syntax open
  (import-modifier
   (lambda (req stx)
     (syntax-parse stx
       #:datum-literals (block group)
       [(_)
        (datum->syntax req
                       (list #'rhombus-prefix-in req #f)
                       req)]))))

(define-import-syntax expose
  (import-modifier-block
   (lambda (req stx)
     (syntax-parse stx
       #:datum-literals (block group)
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
        (datum->syntax req (list (syntax/loc #'form for-meta) #'phase req) req)]
       [(form) 
        (datum->syntax req (list (syntax/loc #'form for-meta) #'1 req) req)]))))

(define-import-syntax meta_label
  (import-modifier
   (lambda (req stx)
     (syntax-parse stx
       [(form) 
        (datum->syntax req (list (syntax/loc #'form for-meta) #f req) req)]))))
