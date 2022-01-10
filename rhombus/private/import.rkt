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
                     "srcloc.rkt"
                     "name-path-op.rkt"
                     "introducer.rkt"
                     "realm.rkt")
         "name-root-ref.rkt"
         (submod "module-path.rkt" for-import-export)
         "declaration.rkt"
         "dot.rkt"
         (submod "dot.rkt" for-dot-provider)
         "lower-require.rkt"
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
                                [rhombus-lib lib])
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

  (property import-modifier transformer)
  (property import-modifier-block transformer)

  (define in-import-space (make-interned-syntax-introducer/add 'rhombus/import))

  (define (check-import-result form proc)
    (unless (syntax? form) (raise-result-error* (proc-name proc) rhombus-realm "Syntax" form))
    form)

  (define (make-identifier-import id)
    (unless (module-path? (syntax-e id))
      (raise-syntax-error 'import
                          "not a valid module path element"
                          id))
    id)

  (define-enforest
    #:syntax-class :import
    #:desc "import"
    #:operator-desc "import operator"
    #:in-space in-import-space
    #:name-path-op name-path-op
    #:name-root-ref name-root-ref
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
    #:transformer-ref (make-import-modifier-ref transform-in req))

  (define (extract-module-path-and-prefixes r)
    (let extract ([r r] [accum null])
      (define (done) (values r (reverse accum)))
      (syntax-parse r
        [_:string (done)]
        [_:identifier (done)]
        [((~literal file) _) (done)]
        [((~literal lib) _) (done)]
        [((~literal rename-in) mp . _) (extract #'mp accum)]
        [((~literal only-in) mp . _) (extract #'mp accum)]
        [((~literal except<-in) mp . _) (extract #'mp accum)]
        [((~literal expose-in) mp . _) (extract #'mp accum)]
        [((~literal rhombus-prefix-in) mp name) (extract #'mp (cons r accum))]
        [((~literal for-meta) _ mp) (extract #'mp accum)]
        [_ (raise-syntax-error 'import
                               "don't know how to extract module path"
                               r)])))

  (define (extract-module-path r)
    (define-values (mp prefixes) (extract-module-path-and-prefixes r))
    mp)

  (define (extract-prefix r)
    (define-values (mp prefixes) (extract-module-path-and-prefixes r))
    (cond
      [(null? prefixes)
       (syntax-parse mp
         [_:string (datum->syntax
                    mp
                    (string->symbol
                     (regexp-replace #rx"[.].*$"
                                     (regexp-replace #rx"^.*/" (syntax-e mp) "")
                                     ""))
                    mp)]
         [_:identifier (datum->syntax
                        mp
                        (string->symbol (regexp-replace #rx"^.*/"
                                                        (symbol->string (syntax-e mp))
                                                        ""))
                        mp)]
         [((~literal lib) str) (extract-prefix #'str)]
         [((~literal file) str) (let-values ([(base name dir?) (split-path (syntax-e #'str))])
                                  (datum->syntax
                                   mp
                                   (string->symbol (path-replace-suffix name #""))))]
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
  (declaration-transformer
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
     (define r-parsed (apply-modifiers (reverse (syntax->list #'mods))
                                       #'r.parsed))
     (define prefix (extract-prefix r-parsed))
     (define intro (if (syntax-e prefix)
                       (make-syntax-introducer)
                       values))
     ;; The name-root expansion of import prefixes is handled by
     ;; `name-root-ref`
     #`(begin
         (#%require #,@(lower-require-clause r-parsed
                                             (and (syntax-e prefix)
                                                  prefix)))
         (rhombus-import mods . more))]))
  
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
