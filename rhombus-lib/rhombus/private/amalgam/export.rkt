#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/operator
                     enforest/property
                     enforest/transformer
                     enforest/name-parse
                     enforest/hier-name-parse
                     enforest/proc-name
                     enforest/syntax-local
                     "srcloc.rkt"
                     "name-path-op.rkt"
                     "introducer.rkt"
                     "tag.rkt"
                     "macro-result.rkt"
                     (for-syntax racket/base))
         "enforest.rkt"
         "all-spaces-out.rkt"
         "only-spaces-out.rkt"
         "name-root-ref.rkt"
         "name-root-space.rkt"
         "declaration.rkt"
         "nestable-declaration.rkt"
         (submod "module-path.rkt" for-import-export)
         "space-parse.rkt"
         "parens.rkt")

(provide (for-space rhombus/decl
                    export)

         (for-space rhombus/expo
                    rename
                    as
                    except
                    meta
                    meta_label
                    only_space
                    except_space
                    names
                    all_from
                    all_defined
                    |.|
                    #%juxtapose))

(module+ for-meta
  (provide (for-syntax export-modifier
                       in-export-space
                       expo-quote
                       :export
                       :export-prefix-op+form+tail
                       :export-infix-op+form+tail
                       :export-modifier)
           define-export-syntax)
  (begin-for-syntax
    (provide (property-out export-prefix-operator)
             (property-out export-infix-operator)
             export-prefix+infix-operator)))

(begin-for-syntax
  (property export-prefix-operator prefix-operator)
  (property export-infix-operator infix-operator)

  (struct export-prefix+infix-operator (prefix infix)
    #:property prop:export-prefix-operator (lambda (self) (export-prefix+infix-operator-prefix self))
    #:property prop:export-infix-operator (lambda (self) (export-prefix+infix-operator-infix self)))

  (property export-modifier transformer)

  (define in-export-space (make-interned-syntax-introducer/add 'rhombus/expo))
  (define-syntax (expo-quote stx)
    (syntax-case stx ()
      [(_ id) #`(quote-syntax #,((make-interned-syntax-introducer 'rhombus/expo) #'id))]))

  (define (check-export-result form proc)
    (unless (syntax? form) (raise-bad-macro-result (proc-name proc) "export" form))
    form)

  (define namespace-syntax-property-key (gensym 'namespace))

  (define (make-identifier-export id)
    (cond
      [(syntax-property id namespace-syntax-property-key)
       => (lambda (ns-id)
            (define outs
              (name-root-all-out ns-id id (cons #f (syntax-local-module-interned-scope-symbols))))
            #`(combine-out #,@(for/list ([id+space-sym (in-list outs)])
                                (define id (car id+space-sym))
                                (define space-sym (cdr id+space-sym))
                                #`(only-spaces-out (all-spaces-out #,id) #,space-sym))))]
      [else #`(all-spaces-out #,id)]))
  (define (export-extension-combine prefix field-id id)
    (syntax-property field-id namespace-syntax-property-key prefix))

  (define name-root-export-ref
    (make-name-root-ref #:binding-ref (lambda (v)
                                        (or (export-prefix-operator-ref v)
                                            (export-infix-operator-ref v)))
                        #:binding-extension-combine export-extension-combine))

  (define-rhombus-enforest
    #:syntax-class :export
    #:prefix-more-syntax-class :export-prefix-op+form+tail
    #:infix-more-syntax-class :export-infix-op+form+tail
    #:desc "export"
    #:operator-desc "export operator"
    #:parsed-tag #:rhombus/expo
    #:in-space in-export-space
    #:prefix-operator-ref export-prefix-operator-ref
    #:infix-operator-ref export-infix-operator-ref
    #:check-result check-export-result
    #:make-identifier-form make-identifier-export
    #:make-operator-form make-identifier-export
    #:name-root-ref name-root-export-ref)

  (define (make-export-modifier-ref parsed-ex)
    ;; "accessor" closes over unpacked `parsed-ex`
    (let ([ex (syntax-parse parsed-ex
                #:datum-literals (parsed)
                [(parsed #:rhombus/expo req) (syntax-local-introduce #'req)]
                [_ (raise-arguments-error
                    'export_meta.ParsedModifier
                    "given export to modify is not parsed"
                    "base export" parsed-ex)])])
      (lambda (v)
        (define mod (export-modifier-ref v))
        (and mod
             (transformer (lambda (stx ignored-ex)
                            ((transformer-proc mod) (syntax-local-introduce ex) stx)))))))

  (define name-root-export-modifier-ref
    (make-name-root-ref #:binding-ref export-modifier-ref
                        #:binding-extension-combine export-extension-combine))

  (define-rhombus-transform
    #:syntax-class (:export-modifier parsed-ex)
    #:desc "export modifier"
    #:parsed-tag #:rhombus/expo
    #:in-space in-export-space
    #:transformer-ref (make-export-modifier-ref parsed-ex)
    #:name-root-ref name-root-export-modifier-ref)

  (define-syntax-class :modified-export
    #:attributes (parsed)
    #:datum-literals (group)
    (pattern (group mod-id:identifier mod-arg ... (_::block exp ...))
             #:when (syntax-local-value* (in-export-space #'mod-id) export-modifier-ref)
             #:with (e::modified-export ...) #'(exp ...)
             #:with (~var || (:export-modifier #'(parsed #:rhombus/expo (combine-out e.parsed ...))))
             #`(#,group-tag mod-id mod-arg ...))
    (pattern ::export))

  (define (apply-modifiers mods e-parsed)
    (cond
      [(null? mods) e-parsed]
      [else
       (syntax-parse (car mods)
         #:datum-literals (group)
         [(~var ex (:export-modifier #`(parsed #:rhombus/expo #,e-parsed)))
          (apply-modifiers (cdr mods) #'ex.parsed)]
         [(group form . _)
          (raise-syntax-error #f
                              "not an export modifier"
                              #'form)])])))

(define-decl-syntax export
  (nestable-declaration-transformer
   (lambda (stx name-prefix)
     (syntax-parse stx
       [(_ (_::block e::modified-export ...))
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
    #:attributes (name)
    #:description "the literal `as`"
    #:opaque
    (pattern ::name
             #:when (free-identifier=? (in-export-space #'name)
                                       (expo-quote as))))

  (define-syntax-class :renaming
    #:datum-literals (group)
    (pattern (group . (~var int (:hier-name-seq in-name-root-space values name-path-op name-root-ref)))
             #:with (_::as-id ext::name) #'int.tail
             #:with int-name #'int.name
             #:with ext-name #'ext.name)))

(define-export-syntax as
  (export-prefix-operator
   #f
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
   #f
   '((default . stronger))
   'macro
   (lambda (stx)
     (syntax-parse stx
       [(_ (_::block g ...))
        #:with ([r.int-name r.ext-name] ...)
        (for/list ([g (in-list (syntax->list #'(g ...)))])
          (syntax-parse g
            [r::renaming #'[r.int-name r.ext-name]]))
        (values #`(all-spaces-out [r.int-name r.ext-name] ...)
                #'())]
       [(_ t ...)
        #:with r::renaming #'(group t ...)
        (values #`(all-spaces-out [r.int-name r.ext-name])
                #'())]))))

(define-export-syntax except
  (export-modifier
   (lambda (ex stx)
     (syntax-parse stx
       [(_ (_::block e::export ...))
        #`(except-out #,ex e.parsed ...)]
       [(_ term ...)
        #:with e::export #'(group term ...)
        #`(except-out #,ex e.parsed)]))))

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

(define-export-syntax only_space
  (export-modifier
   (lambda (ex stx)
     (define (build form spaces-stx)
       (define spaces (parse-space-names stx spaces-stx))
       (datum->syntax ex (list* (syntax/loc form only-spaces-out) ex spaces) ex))
     (syntax-parse stx
       #:datum-literals (group)
       [(form space ...)
        (build #'form #'((space ...)))]
       [(form (_::block (group space ...)
                        ...))
        (build #'form #'((space ...) ...))]))))

(define-export-syntax except_space
  (export-modifier
   (lambda (ex stx)
     (define (build form spaces-stx)
       (define spaces (parse-space-names stx spaces-stx))
       (datum->syntax ex (list* (syntax/loc form except-spaces-out) ex spaces) ex))
     (syntax-parse stx
       #:datum-literals (group)
       [(form space ...)
        (build #'form #'((space ...)))]
       [(form (_::block (group space ...)
                        ...))
        (build #'form #'((space ...) ...))]))))

(define-export-syntax names
  (export-prefix-operator
   #f
   '((default . stronger))
   'macro
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (group)
       [(_ (_::block (group name::name ...) ...)
           . tail)
        (values #`(combine-out (all-spaces-out name.name) ... ...)
                #'tail)]))))

(define-export-syntax all_from
  (export-prefix-operator
   #f
   '((default . stronger))
   'macro
   (lambda (stx)
     (parameterize ([current-module-path-context 'export])
       (syntax-parse stx
         #:datum-literals (group op |.|)
         [(_ (_::parens (group (op |.|) . (~var name (:hier-name-seq in-name-root-space values name-path-op name-root-ref))))
             . tail)
          (values
           (cond
             [(syntax-local-value* (in-name-root-space #'name.name) import-root-ref)
              => (lambda (i)
                   (define form
                     (syntax-parse i
                       #:datum-literals (parsed nspace)
                       [(parsed mod-path parsed-r)
                        #`(all-from-out #,(relocate #'name.name #'mod-path))]
                       [(nspace _ _ [key val . rule] ...)
                        (define keys (syntax->list #'(key ...)))
                        (define vals (syntax->list #'(val ...)))
                        (define rules (syntax->list #'(rule ...)))
                        (define all-spaces
                          #`(all-spaces-out #,@(for/list ([key (in-list keys)]
                                                          [val (in-list vals)]
                                                          [rule (in-list rules)]
                                                          #:when (and (syntax-e key)
                                                                      (null? (syntax-e rule))))
                                                 #`[#,val #,key])))
                        (cond
                          [(for/and ([rule (in-list rules)]) (null? (syntax-e rule)))
                           ;; simple case: can group all together
                           all-spaces]
                          [else
                           ;; individual cases to handle spaces
                           #`(combine-out
                              #,all-spaces
                             #,@(for/list ([key (in-list keys)]
                                           [val (in-list vals)]
                                           [rule (in-list rules)]
                                           #:when (and (syntax-e key)
                                                       (pair? (syntax-e rule))))
                                  (let loop ([rule rule])
                                    (syntax-parse rule
                                      [(#:space ([space space-id] ...) . rule-rest)
                                       #`(combine-out
                                          (only-spaces-out space-id space)
                                          ...
                                          #,(loop #'rule-rest))]
                                      [((~and mode (~or* #:only #:except))  space ...)
                                       #`(#,(if (eq? (syntax-e #'mode) '#:only)
                                                #'only-spaces-out
                                                #'except-spaces-out)
                                          (all-spaces-out [#,val #,key])
                                          space ...)]))))])]))
                   (unless (null? (syntax-e #'name.tail))
                     (raise-syntax-error #f
                                         "unexpected after `.`"
                                         #'name.tail))
                   form)]
             [else
              (raise-syntax-error #f
                                  "not bound as a name root"
                                  #'name.name)])
           #'tail)]
         [(_ (_::parens mod-path::module-path)
             . tail)
          (values #`(all-from-out #,(convert-symbol-module-path #'mod-path.parsed))
                  #'tail)])))))

(define-export-syntax all_defined
  (export-prefix-operator
   #f
   '((default . stronger))
   'macro
   (lambda (stx)
     (syntax-parse stx
       [(form #:scope_like id:identifier . tail)
        (values (datum->syntax #'id (list #'all-spaces-defined-out) #'form #'form)
                #'tail)]
       [(form . tail)
        (values (datum->syntax #'form (list #'all-spaces-defined-out) #'form #'form)
                #'tail)]))))

(define-export-syntax #%juxtapose
  (export-infix-operator
   #f
   '((default . weaker))
   'macro
   (lambda (form1 stx)
     (syntax-parse stx
       [(_ (_::block mod ...) . tail)
        (values (apply-modifiers (syntax->list #'(mod ...))
                                 form1)
                #'tail)]
       [(_ . tail)
        #:with (~var e (:export-infix-op+form+tail (expo-quote #%juxtapose))) #'(group . tail)
        (values #`(combine-out #,form1
                               e.parsed)
                #'e.tail)]))
   'left))

(define-export-syntax |.|
  (export-infix-operator
   #f
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
