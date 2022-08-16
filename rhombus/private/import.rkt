#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest
                     enforest/operator
                     enforest/transformer
                     enforest/property
                     enforest/syntax-local
                     enforest/name-parse
                     enforest/proc-name
                     syntax/modcollapse
                     "srcloc.rkt"
                     "name-path-op.rkt"
                     "introducer.rkt"
                     "realm.rkt"
                     "import-invert.rkt")
         "name-root.rkt"
         "name-root-ref.rkt"
         (submod "module-path.rkt" for-import-export)
         "definition.rkt"
         "dot.rkt"
         (submod "dot.rkt" for-dot-provider)
         "lower-require.rkt"
         "import-from-root.rkt"
         (only-in "implicit.rkt"
                  #%literal)
         (only-in "arithmetic.rkt"
                  [/ rhombus/]))

(provide import

         (for-space rhombus/import
                    #%juxtapose
                    #%literal
                    (rename-out [rhombus/ /]
                                [rhombus-file file]
                                [rhombus-lib lib]
                                [rhombus. |.|])
                    as
                    open
                    expose
                    rename
                    only
                    except
                    for_meta
                    for_label))

(module+ for-meta
  (provide (for-syntax import-modifier
                       import-modifier-block
                       in-import-space)))

(begin-for-syntax
  (property import-prefix-operator prefix-operator)
  (property import-infix-operator infix-operator)

  (struct import-prefix+infix-operator (prefix infix)
    #:property prop:import-prefix-operator (lambda (self) (import-prefix+infix-operator-prefix self))
    #:property prop:import-infix-operator (lambda (self) (import-prefix+infix-operator-infix self)))


  (property import-modifier transformer)
  (property import-modifier-block transformer)

  (define in-import-space (make-interned-syntax-introducer/add 'rhombus/import))

  (define (check-import-result form proc)
    (unless (syntax? form) (raise-result-error* (proc-name proc) rhombus-realm "Syntax" form))
    form)

  (define (make-identifier-import id)
    (unless (module-path? (syntax-e id))
      (raise-syntax-error 'import
                          "not a valid module path element, and not bound as a name root"
                          id))
    id)

  (define-enforest
    #:syntax-class :import
    #:desc "import"
    #:operator-desc "import operator"
    #:in-space in-import-space
    #:name-path-op name-path-op
    #:name-root-ref name-root-ref
    #:name-root-ref-root name-root-ref-root
    #:prefix-operator-ref import-prefix-operator-ref
    #:infix-operator-ref import-infix-operator-ref
    #:check-result check-import-result
    #:make-identifier-form make-identifier-import)

  (define (make-import-modifier-ref transform-in req)
    ;; "accessor" closes over `req`:
    (lambda (v)
      (define mod (or (import-modifier-ref v)
                      (import-modifier-block-ref v)))
      (and mod
           (transformer (lambda (stx)
                          ((transformer-proc mod) (transform-in req) stx))))))

  (define-transform
    #:syntax-class (:import-modifier req)
    #:desc "import modifier"
    #:in-space in-import-space
    #:name-path-op name-path-op
    #:name-root-ref name-root-ref
    #:name-root-ref-root name-root-ref-root
    #:transformer-ref (make-import-modifier-ref transform-in req))

  (define (extract-prefixes r)
    (let extract ([r r] [accum null])
      (syntax-parse r
        #:datum-literals (rename-in only-in except-in expose-in rhombus-prefix-in for-meta for-label only-space-in)
        [#f (reverse accum)]
        [((~or rename-in only-in except-in expose-in for-label) mp . _) (extract #'mp accum)]
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
         #:datum-literals (lib import-root import-dotted import-spaces file reimport singleton)
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
         [(import-spaces mp . _) #'mp]
         [(reimport id . _) #'id]
         [(singleton _ id) #'id]
         [(file str) (let-values ([(base name dir?) (split-path (syntax-e #'str))])
                       (datum->syntax
                        mp
                        (string->symbol (path->string (path-replace-suffix name #"")))))]
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
        #'((rhombus-import () r ...))]))))

(define-syntax (rhombus-import stx)
  ;; handle one import group at a time, so it can import
  ;; transformers that are used for later imports
  (syntax-parse stx
    [(_ _) #'(begin)]
    [(_ mods mi::modified-imports . more)
     #`(begin
         (rhombus-import (mi.mod . mods) mi.imp)
         ...
         (rhombus-import mods . more))]
    [(_ mods r::import . more)
     ;; apply modifiers, but then flip around to extract
     ;; module path from the modifiers
     (define r-parsed (apply-modifiers (reverse (syntax->list #'mods))
                                       #'r.parsed))
     (define-values (mod-path-stx r-stx) (import-invert r-parsed))
     #`(begin
         (rhombus-import-one #f #,mod-path-stx #,r-stx (no-more wrt-placeholder))
         (rhombus-import mods . more))]))

;; Uses a continuation form to thread through the module path that
;; accessed-via-dots module paths are relative to
(define-syntax (rhombus-import-one stx)
  (syntax-parse stx
    #:datum-literals (import-dotted import-root import-spaces singleton)
    [(_ wrt ((~literal import-dotted) mod-path id) r k)
     #:with m-mod-path (syntax-local-introduce #'mod-path)
     #:with m-id (syntax-local-introduce #'id)
     #`(begin
         (rhombus-import-one wrt m-mod-path (rhombus-prefix-in #f #f)
                             (rhombus-import-dotted-one wrt m-id id r k)))]
    [(_ wrt (import-spaces ir ...) r (k-form write-placeholder . k-args))
     (define-values (mods hiers sings) (split-imports (syntax->list #'(ir ...))))
     ;; Mixtures of modules and non-module or multiple module paths
     ;; could be handled by unpacking the module imports into a hierarchical
     ;; name, effectively giving up on the optimization of expanding to
     ;; a plain `require`. There's an issue with shading bindings, though.
     (unless (or (null? mods) (null? hiers))
       (raise-syntax-error #f
                           "cannot handle mixture of module and nested"
                           stx))
     (define new-mp
       (cond
         [(null? mods) #'wrt]
         [else
          (syntax-parse (car mods)
            [(_ mp)
             (for ([mod (in-list (cdr mods))])
               (unless (equal? (cadr (syntax->datum mod)) (syntax->datum #'mp))
                 (raise-syntax-error #f
                                     "cannot handle mixture of module paths"
                                     stx)))
             (collapse-path #'mp #'wrt)])]))
     (unless (or (pair? mods) (pair? hiers))
       ;; report error for any work on content applied to a singleton:
       (convert-require-from-root #'r #hasheq() #hasheq() #f))
     #`(begin
         ;; module re-imports
         #,@(for/list ([mod (in-list mods)])
              (syntax-parse (car mods)
                [(space mp)
                 (define prefix (extract-prefix #'mp #'r))
                 (define intro (if (syntax-e prefix)
                                   (make-syntax-introducer)
                                   values))
                 ;; The name-root expansion of import prefixes is handled by
                 ;; `name-root-ref`, which recognizes `(portal <id> (import ....))`
                 ;; forms generated by `lower-require-clause`
                 (lower-require-clause (if (eq? (syntax-e #'space) '#:all)
                                           #'r
                                           #'(only-space-in space r))
                                       new-mp
                                       (and (syntax-e prefix)
                                            prefix))]))
         ;; hierarchical name s
         #,@(let loop ([irs hiers] [covered-ht #hasheq()])
              (cond
                [(null? irs) null]
                [else
                 (syntax-parse (car irs)
                   [(_ im)
                    (define-values (form new-covered-ht) (imports-from-root #'im #'r covered-ht (pair? (cdr irs))))
                    (cons form
                          (loop (cdr irs) new-covered-ht))])]))
         ;; singletons
         #,@(for/list ([sing (in-list sings)])
              (syntax-parse sing
                #:datum-literals (singleton)
                [(_ (~and mp (singleton id as-id)))
                 (define prefix (extract-prefix #'mp #'r))
                 (cond
                   [(syntax-e prefix)
                    (import-singleton #'id prefix)]
                   [(or (pair? mods) (pair? hiers))
                    #'(begin)]
                   [else
                    (raise-syntax-error #f
                                        "cannot open binding that is not a name root"
                                        #'id)])]))
         (k-form #,new-mp . k-args))]
    [(_ wrt mp r k)
     #'(rhombus-import-one wrt (import-spaces (#:all mp)) r k)]))

(define-syntax (rhombus-import-dotted-one stx)
  (syntax-parse stx
    [(_ wrt lookup-id id r k)
     #`(rhombus-import-one wrt #,(name-root-as-import stx #'lookup-id #'id #t) r k)]))

(define-for-syntax (name-root-as-import stx lookup-id id as-field?)
  (define space+maps
    (for/list ([space-sym (in-list (cons #f (syntax-local-module-interned-scope-symbols)))]
               #:do[(define intro (if space-sym
                                      (make-interned-syntax-introducer/add space-sym)
                                      (lambda (id) id)))
                    (define space-id (intro lookup-id))
                    (define i (and (or (not as-field?)
                                       (identifier-distinct-binding space-id (intro id)))
                                   (or (not space-sym)
                                       (identifier-distinct-binding space-id lookup-id))
                                   (or (syntax-local-value* space-id import-root-ref)
                                       (and (identifier-binding space-id)
                                            'other))))]
               #:when i)
      (list space-sym
            (if (eq? i 'other)
                #`(singleton #,space-id #,(intro id))
                (syntax-parse i
                  #:datum-literals (parsed map)
                  [(parsed mod-path parsed-r) #`(reimport #,id #,(syntax-local-introduce (transform-in #'parsed-r)))]
                  [(map . _) #`(import-root #,(intro id) #,i)])))))
  (cond
    [(null? space+maps)
     (if as-field?
         (raise-syntax-error #f
                             (string-append "not provided as a name root")
                             id)
         (raise-syntax-error #f
                             (string-append "not bound as a name root")
                             stx
                             id))]
    [else
     #`(import-spaces #,@space+maps)]))

(define-for-syntax (split-imports irs)
  (let loop ([irs irs] [rev-mods '()] [rev-hiers '()] [rev-sings '()])
    (cond
      [(null? irs) (values (reverse rev-mods) (reverse rev-hiers) (reverse rev-sings))]
      [else
       (define ir (car irs))
       (syntax-parse ir
         #:datum-literals (import-root singleton)
         [(_ (import-root . _))
          (loop (cdr irs) rev-mods (cons ir rev-hiers) rev-sings)]
         [(_ (singleton . _))
          (loop (cdr irs) rev-mods rev-hiers (cons ir rev-sings))]
         [_
          (loop (cdr irs) (cons ir rev-mods) rev-hiers rev-sings)])])))

(define-for-syntax (imports-from-root im r-parsed covered-ht accum?)
  (syntax-parse im
    #:datum-literals (import-root map)
    [(import-root id (map orig-id [key val] ...))
     (define prefix (extract-prefix #'id r-parsed))
     (define-values (ht expose-ht new-covered-ht)
       (convert-require-from-root
        r-parsed
        (for/hasheq ([key (in-list (syntax->list #'(key ...)))]
                     [val (in-list (syntax->list #'(val ...)))]
                     #:when (syntax-e key))
          (values (syntax-e key) val))
        covered-ht
        accum?))
     (values
      #`(begin
          #,@(if (syntax-e prefix)
                 (with-syntax ([(root-id) (generate-temporaries #'(id))])
                   #`((define-name-root root-id
                        #:root-as-rename #,(for/or ([key (in-list (syntax->list #'(key ...)))]
                                                    [val (in-list (syntax->list #'(val ...)))]
                                                    #:when (not (syntax-e key)))
                                             val)
                        #:fields
                        #,(for/list ([(key val) (in-hash ht)])
                            #`[#,key #,val]))
                      (define-syntax #,(datum->syntax #'id (syntax-e prefix) #'id)
                        (make-rename-transformer (quote-syntax root-id)))))
                 null)
          #,@(for/list ([key (in-hash-keys (if (syntax-e prefix) expose-ht ht))])
               (define val (hash-ref ht key))
               #`(define-syntax #,(datum->syntax #'id key #'id)
                   (make-rename-transformer (quote-syntax #,val)))))
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
  (define plain-mp (syntax-parse mp
                     #:datum-literals (reimport)
                     [(reimport _ mp) #'mp]
                     [_ mp]))
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

(define-import-syntax rhombus.
  (import-prefix+infix-operator
   (import-prefix-operator
    #'rhombus.
    '((default . weaker))
    'macro
    (lambda (stx)
      (syntax-parse stx
        [(_ id:identifier . tail)
         (values (name-root-as-import stx #'id #'id #f)
                 #'tail)]
        [_
         (raise-syntax-error #f
                             "expected an identifier"
                             stx)])))
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
    'left)))

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
       [(_ (block (group int::name _::as-id ext::name)
                  ...))
        (datum->syntax req
                       (list* #'rename-in req #'([int.name ext.name] ...))
                       req)]))))

(define-import-syntax only
  (import-modifier-block
   (lambda (req stx)
     (syntax-parse stx
       #:datum-literals (block group)
       [(_ (block (group name::name ...) ...))
        (datum->syntax req
                       (list* #'only-in req #'(name.name ... ...))
                       req)]))))

(define-import-syntax except
  (import-modifier-block
   (lambda (req stx)
     (syntax-parse stx
       #:datum-literals (block group)
       [(_ (block (group name::name ...) ...))
        (datum->syntax req
                       (list* #'except-in req #'(name.name ... ...))
                       req)]))))

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
       [(_ (block (group name::name ...) ...))
        (datum->syntax req
                       (list* #'expose-in req #'(name.name ... ...))
                       req)]))))

(define-import-syntax for_meta
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

(define-import-syntax for_label
  (import-modifier
   (lambda (req stx)
     (syntax-parse stx
       [(form) 
        (datum->syntax req (list (syntax/loc #'form for-meta) #f req) req)]))))
