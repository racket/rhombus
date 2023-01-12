#lang racket/base
(require (for-syntax racket/base
                     racket/provide-transform
                     racket/phase+space
                     racket/symbol
                     syntax/parse/pre
                     enforest
                     enforest/operator
                     enforest/property
                     enforest/transformer
                     enforest/name-parse
                     enforest/hier-name-parse
                     enforest/proc-name
                     enforest/syntax-local
                     "name-path-op.rkt"
                     "srcloc.rkt"
                     "introducer.rkt"
                     "realm.rkt"
                     "tag.rkt")
         "name-root-ref.rkt"
         "declaration.rkt"
         "nestable-declaration.rkt"
         "dotted-sequence-parse.rkt"
         (submod "module-path.rkt" for-import-export))

(provide export

         (for-space rhombus/expo
                    rename
                    as
                    except
                    meta
                    meta_label
                    names
                    all_from
                    |.|
                    #%juxtapose))

(module+ for-meta
  (provide (for-syntax export-modifier
                       in-export-space)
           define-export-syntax))

(begin-for-syntax
  (property export-prefix-operator prefix-operator)
  (property export-infix-operator infix-operator)

  (property export-modifier transformer)

  (define in-export-space (make-interned-syntax-introducer/add 'rhombus/expo))

  (define (check-export-result form proc)
    (unless (syntax? form) (raise-result-error* (proc-name proc) rhombus-realm "Syntax" form))
    form)

  (define (make-identifier-export id)
    #`(all-spaces-out #,id))

  (define-enforest
    #:syntax-class :export
    #:infix-more-syntax-class :export-infix-op+form+tail
    #:desc "export"
    #:operator-desc "export operator"
    #:in-space in-export-space
    #:name-path-op name-path-op
    #:name-root-ref name-root-ref
    #:name-root-ref-root name-root-ref-root
    #:prefix-operator-ref export-prefix-operator-ref
    #:infix-operator-ref export-infix-operator-ref
    #:check-result check-export-result
    #:make-identifier-form make-identifier-export
    #:make-operator-form make-identifier-export)

  (define (make-export-modifier-ref transform-in ex)
    ;; "accessor" closes over `ex`:
    (lambda (v)
      (define mod (export-modifier-ref v))
      (and mod
           (transformer (lambda (stx)
                          ((transformer-proc mod) (transform-in ex) stx))))))

  (define-transform
    #:syntax-class (:export-modifier req)
    #:desc "export modifier"
    #:in-space in-export-space
    #:name-path-op name-path-op
    #:name-root-ref name-root-ref
    #:name-root-ref-root name-root-ref-root
    #:transformer-ref (make-export-modifier-ref transform-in req))

  (define-syntax-class :modified-export
    #:datum-literals (group block)
    (pattern (group mod-id:identifier mod-arg ... (block exp ...))
             #:when (syntax-local-value* (in-export-space #'mod-id) export-modifier-ref)
             #:with (e::modified-export ...) #'(exp ...)
             #:with (~var ex (:export-modifier #'(combine-out e.parsed ...))) #'(group mod-id mod-arg ...)
             #:attr parsed #'ex.parsed)
    (pattern e0::export
             #:attr parsed #'e0.parsed))

  (define (apply-modifiers mods e-parsed)
    (cond
      [(null? mods) e-parsed]
      [else
       (syntax-parse (car mods)
         #:datum-literals (group)
         [(~var ex (:export-modifier e-parsed))
          (apply-modifiers (cdr mods) #'ex.parsed)]
         [(group form . _)
          (raise-syntax-error #f
                              "not an export modifier"
                              #'form)])])))

(define-syntax all-spaces-out
  (make-provide-transformer
   (lambda (stx phase+spaces)
     (define phases (if (null? phase+spaces)
                        (list 0)
                        (hash-keys
                         (for/hash ([p+s (in-list phase+spaces)])
                           (values (phase+space-phase p+s) #t)))))
     (apply
      append
      (for/list ([stx (in-list (cdr (syntax->list stx)))])
        (define-values (id out-id)
          (syntax-parse stx
            [[id out-id] (values #'id #'out-id)]
            [id (values #'id #'id)]))
        (define (make-export phase space id [as-sym (syntax-e out-id)])
          (export id
                  as-sym
                  (phase+space phase space)
                  #f ; not protected
                  stx))
        (define (adjust-prefix sym prefix)
          (if (eq? (syntax-e id) (syntax-e out-id))
              sym
              (string->symbol (string-append
                               (symbol->immutable-string (syntax-e out-id))
                               "."
                               (substring (symbol->immutable-string sym) (string-length prefix))))))
        (apply
         append
         (for/list ([phase (in-list phases)])
           (define space+ids
             (for*/list ([sym (in-list (cons #f (syntax-local-module-interned-scope-symbols)))]
                         [(intro) (in-value (if sym
                                                (make-interned-syntax-introducer sym)
                                                (lambda (x) x)))]
                         [(space-id) (in-value (intro id))]
                         #:when (and (identifier-binding space-id)
                                     (or (not sym)
                                         (not (free-identifier=? id space-id)))))
               (cons sym space-id)))
           (when (null? space+ids)
             (raise-syntax-error 'export
                                 "identifier is not defined or imported"
                                 id))
           (apply
            append
            (for/list ([space+id (in-list space+ids)])
              (define space (car space+id))
              (define int-id (cdr space+id))
              (append
               (list (make-export phase space int-id))
               (cond
                 [(extensible-name-root (list int-id))
                  ;; also export any extensions
                  (define prefix (format "~a." (symbol->string (syntax-e int-id))))
                  (define intro (if space
                                    (make-interned-syntax-introducer space)
                                    (lambda (x) x)))
                  (for/list ([sym (in-list (syntax-bound-symbols (intro int-id)))]
                             #:do [(define str (symbol->immutable-string sym))]
                             #:when (and (> (string-length str) (string-length prefix))
                                         (string=? prefix (substring str 0 (string-length prefix)))
                                         (identifier-extension-binding? (datum->syntax (intro int-id) sym) (intro int-id)))
                             #:when (or (not space)
                                        (identifier-distinct-binding (datum->syntax (intro int-id) sym)
                                                                     (datum->syntax int-id sym)
                                                                     phase)))
                    (make-export phase space (datum->syntax int-id sym int-id) (adjust-prefix sym prefix)))]
                 [else null])))))))))))

(define-syntax export
  (nestable-declaration-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (block)
       [(_ (block e::modified-export ...))
        #`((provide e.parsed ...))]
       [(_ term ...)
        #:with e::modified-export #`(#,group-tag term ...)
        #`((provide e.parsed))]))))

(define-syntax (define-export-syntax stx)
  (syntax-parse stx
    [(_ name:id rhs)
     (quasisyntax/loc stx
       (define-syntax #,(in-export-space #'name) rhs))]))

(begin-for-syntax
  (define-syntax-class :as-id
    #:description "`as`"
    (pattern as-id:identifier
             #:when (free-identifier=? (in-export-space #'as) (in-export-space #'as-id))))

  (define-syntax-class :renaming
    #:datum-literals (group)
    (pattern (group . (~var int (:hier-name-seq values name-path-op name-root-ref)))
             #:with (_::as-id ext::name) #'int.tail
             #:attr int-name #'int.name
             #:attr ext-name #'ext.name)))

(define-export-syntax as
  (export-prefix-operator
   #'rename
   '((default . stronger))
   'macro
   (lambda (stx)
     (syntax-parse stx
       [(self . _)
        (raise-syntax-error #f
                            "allowed only in `rename`"
                            #'self)]))))

(define-export-syntax rename
  (export-prefix-operator
   #'rename
   '((default . stronger))
   'macro
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (block)
       [(_ (block r::renaming ...)
           . tail)
        (values #`(all-spaces-out [r.int-name r.ext-name] ...)
                #'tail)]))))

(define-export-syntax except
  (export-modifier
   (lambda (ex stx)
     (syntax-parse stx
       #:datum-literals (block)
       [(_ (block e::export ...))
        #`(except-out #,ex e.parsed ...)]))))
     
(define-export-syntax meta
  (export-modifier
   (lambda (ex stx)
     (syntax-parse stx
       [(form phase)
        (define ph (syntax-e #'phase))
        (unless (exact-integer? ph)
          (raise-syntax-error #f "not a valid phase" stx #'phase))
        (datum->syntax ex (list (syntax/loc #'form for-meta) #'phase ex) ex)]
       [(form) 
        (datum->syntax ex (list (syntax/loc #'form for-meta) #'1 ex) ex)]))))

(define-export-syntax meta_label
  (export-modifier
   (lambda (ex stx)
     (syntax-parse stx
       [(form) 
        (datum->syntax ex (list (syntax/loc #'form for-meta) #f ex) ex)]))))
     
(define-export-syntax names
  (export-prefix-operator
   #'names
   '((default . stronger))
   'macro
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (block)
       [(_ (block (group name::name ...) ...)
           . tail)
        (values #`(combine-out (all-spaces-out name.name) ... ...)
                #'tail)]))))

(define-export-syntax all_from
  (export-prefix-operator
   #'all_from
   '((default . stronger))
   'macro
   (lambda (stx)
     (parameterize ([current-module-path-context 'export])
       (syntax-parse stx
         #:datum-literals (parens group op |.|)
         [(_ (parens (group (op |.|) . (~var name (:hier-name-seq values name-path-op name-root-ref))))
             . tail)
          (values
           (cond
             [(syntax-local-value* #'name.name import-root-ref)
              => (lambda (i)
                   (define form
                     (syntax-parse i
                       #:datum-literals (parsed map)
                       [(parsed mod-path parsed-r) #`(all-from-out #,(relocate #'name.name #'mod-path))]
                       [(map _ _ [key val] ...) #`(rename-out #,@(for/list ([key (in-list (syntax->list #'(key ...)))]
                                                                            [val (in-list (syntax->list #'(val ...)))]
                                                                            #:when (syntax-e key))
                                                                   #`[#,val #,key]))]))
                   (unless (null? (syntax-e #'name.tail))
                     (raise-syntax-error #f
                                         "unexpected after `.`"
                                         #'name-tail))
                   form)]
             [else
              (raise-syntax-error #f
                                  "not bound as a name root"
                                  #'name.name)])
           #'tail)]
         [(_ (parens mod-path::module-path)
             . tail)
          (values #`(all-from-out #,(convert-symbol-module-path #'mod-path.parsed))
                  #'tail)])))))

(define-export-syntax #%juxtapose
  (export-infix-operator
   #'#%juxtapose
   '((default . weaker))
   'macro
   (lambda (form1 stx)
     (syntax-parse stx
       #:datum-literals (block group)
       [(_ (block mod ...) . tail)
        (values (apply-modifiers (syntax->list #'(mod ...))
                                 form1)
                #'tail)]
       [e::export-infix-op+form+tail
        (values #`(combine-out #,form1
                               e.parsed)
                #'e.tail)]))
   'left))

(define-export-syntax |.|
  (export-infix-operator
   #'rename
   '((default . stronger))
   'macro
   (lambda (form stx)
     (syntax-parse stx
       #:datum-literals (op)
       [((op form-id) . _)
        (raise-syntax-error #f
                            "allowed here only as a name-path separator, used as an operator"
                            #'form-id)]))
   'left))
