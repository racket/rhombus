#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest/syntax-local
                     enforest/hier-name-parse
                     "srcloc.rkt"
                     "name-path-op.rkt"
                     "introducer.rkt")
         "definition.rkt"
         "expression.rkt"
         "binding.rkt"
         (submod "binding-syntax.rkt" for-class)
         (for-syntax "class-transformer.rkt")
         "annotation.rkt"
         (submod "annotation.rkt" for-class)
         (submod "annotation-syntax.rkt" for-class)
         (submod "dot.rkt" for-dot-provider)
         "call-result-key.rkt"
         "composite.rkt"
         "assign.rkt"
         "static-info.rkt"
         "name-root.rkt"
         "name-root-ref.rkt"
         "dotted-sequence-parse.rkt"
         "realm.rkt"
         "parens.rkt"
         "parse.rkt"
         "implicit.rkt"
         "error.rkt"
         "class-clause.rkt"
         "entry-point.rkt")

(provide (rename-out [rhombus-class class])
         extends
         internal
         constructor
         binding
         annotation
         final
         nonfinal
         authentic)

(begin-for-syntax
  (struct class-desc (final? class:id constructor-id fields constructor-makers))
  (define (class-desc-ref v) (and (class-desc? v) v))

  (define in-class-desc-space (make-interned-syntax-introducer/add 'rhombus/class))

  (define-syntax-class :field
    #:datum-literals (group op)
    #:literals (mutable)
    (pattern (group (~optional (~and mutable (~var mutable))
                               #:defaults ([mutable #'#f]))
                    name:identifier
                    (~optional c::inline-annotation))
             #:attr predicate (if (attribute c)
                                  #'c.predicate
                                  #'#f)
             #:attr annotation-str (if (attribute c)
                                       #'c.annotation-str
                                       #'#f)
             #:attr static-infos (if (attribute c)
                                     #'c.static-infos
                                     #'())))

  (define-syntax-class :not-parens
    #:datum-literals (parens)
    (pattern (~not (parens . _))))

  (define-splicing-syntax-class :options-block
    #:datum-literals (block group parens)
    (pattern (~seq)
             #:attr (form 1) '())
    (pattern (~seq (_::block form ...))))

  (define (check-duplicate-field-names stxes fields super)
    (let ([ht (for/hasheq ([field (in-list (if super (class-desc-fields super) '()))])
                (values (car field) 'super))])
      (for/fold ([ht ht]) ([field (in-list fields)])
        (define prev (hash-ref ht (syntax-e field) #f))
        (when prev
          (raise-syntax-error #f
                              (if (eq? prev 'super)
                                  "field name already exists in superclass"
                                  "duplicate field name")
                              stxes
                              field))
        (hash-set ht (syntax-e field) #t)))))

(define-syntax rhombus-class
  (definition-transformer
   (lambda (stxes)
     (syntax-parse stxes
       #:datum-literals (group block)
       [(_ name-seq::dotted-identifier-sequence ((~datum parens) field::field ...)
           options::options-block)
        #:with full-name::dotted-identifier #'name-seq
        #:with name #'full-name.name
        (define options (parse-options stxes #'(options.form ...)))
        (define parent-name (hash-ref options 'extends #f))
        (define super (and parent-name
                           (or (syntax-local-value* (in-class-desc-space parent-name) class-desc-ref)
                               (raise-syntax-error #f "not a class name" stxes parent-name))))
        (define final? (hash-ref options 'final? (not super)))
        (define authentic? (hash-ref options 'authentic? #f))
        (define internal-id (hash-ref options 'internal #f))
        (define (maybe-use-internal-id id clause-name)
          (cond
            [(syntax-e id) id]
            [internal-id internal-id]
            [else (raise-syntax-error #f
                                      (format "no `internal` clause, and no maker name in `~a` clause"
                                              clause-name)
                                      stxes)]))
        (define constructor-id (let ([id (hash-ref options 'constructor-id #f)])
                                 (and id
                                      (maybe-use-internal-id id 'constructor))))
        (define binding-id (let ([b (hash-ref options 'binding #f)])
                             (and b
                                  (maybe-use-internal-id (car b) 'binding))))
        (define annotation-id (let ([b (hash-ref options 'annotation #f)])
                                (and b
                                     (maybe-use-internal-id (car b) 'annotation))))
        (when super
          (when (class-desc-final? super)
            (raise-syntax-error #f
                                "superclass is final and cannot be extended"
                                stxes
                                parent-name))
          (when (and (class-desc-constructor-makers super)
                     (not constructor-id))
            (raise-syntax-error #f
                                "superclass does not use the default constructor, so a subclass needs `~constructor`"
                                stxes
                                parent-name)))
        (define fields (syntax->list #'(field.name ...)))
        (check-duplicate-field-names stxes fields super)
        (define-values (immutable-fields mutable-fields)
          (for/fold ([imm '()] [m '()] #:result (values (reverse imm) (reverse m)))
                    ([field (in-list fields)]
                     [mutable (syntax->list #'(field.mutable ...))])
            (if (syntax-e mutable)
                (values imm (cons field m))
                (values (cons field imm) m))))
        (define intro (make-syntax-introducer))
        (with-syntax ([name? (datum->syntax #'name (string->symbol (format "~a?" (syntax-e #'name))) #'name)]
                      [(class:name) (generate-temporaries #'(name))]
                      [(make-name) (generate-temporaries #'(name))]
                      [(core-bind-name) (if (hash-ref options 'binding #f)
                                            (generate-temporaries #'(name))
                                            #'(name))]
                      [(core-ann-name) (if (hash-ref options 'annotation #f)
                                           (generate-temporaries #'(name))
                                           #'(name))]
                      [name-instance (intro (datum->syntax #'name (string->symbol (format "~a.instance" (syntax-e #'name))) #'name))]
                      [(name-field ...) (for/list ([field (in-list fields)])
                                          (intro
                                           (datum->syntax field
                                                          (string->symbol (format "~a.~a"
                                                                                  (syntax-e #'name)
                                                                                  (syntax-e field)))
                                                          field)))]
                      [(set-name-field! ...) (for/list ([field (in-list mutable-fields)])
                                               (intro
                                                (datum->syntax field
                                                               (string->symbol (format "set-~a.~a!"
                                                                                       (syntax-e #'name)
                                                                                       (syntax-e field)))
                                                               field)))]
                      [cnt (length fields)]
                      [(field-index ...) (for/list ([field (in-list fields)]
                                                    [i (in-naturals)])
                                           i)]
                      [(immutable-field-index ...) (for/list ([field (in-list immutable-fields)]
                                                              [i (in-naturals)])
                                                     i)]
                      [(mutable-field ...) mutable-fields]
                      [(mutable-field-index ...) (for/list ([mutable (syntax->list #'(field.mutable ...))]
                                                            [i (in-naturals)]
                                                            #:when (syntax-e mutable))
                                                   i)]
                      [((super-field-name super-name-field super-field-static-infos) ...) (if super
                                                                                              (class-desc-fields super)
                                                                                              '())])
          (with-syntax ([constructor-name (if constructor-id
                                              (car (generate-temporaries #'(name)))
                                              #'make-name)]
                        [constructor-maker-name (and (or (not final?)
                                                         super)
                                                     constructor-id
                                                     (car (generate-temporaries #'(maker))))])
            (append
             (list
              #`(define-values (class:name make-name name? name-field ... set-name-field! ...)
                  (let-values ([(class:name name name? name-ref name-set!)
                                (make-struct-type 'name
                                                  #,(and super
                                                         (class-desc-class:id super))
                                                  cnt 0 #f
                                                  (list (cons prop:field-name->accessor
                                                              (cons '(field.name ...)
                                                                    (hasheq (~@ 'super-field-name super-name-field)
                                                                            ...)))
                                                        #,@(if final?
                                                               (list #'(cons prop:sealed #t))
                                                               '())
                                                        #,@(if authentic?
                                                               (list #'(cons prop:authentic #t))
                                                               '()))
                                                  #f #f
                                                  '(immutable-field-index ...)
                                                  #,(build-guard-expr fields
                                                                      (syntax->list #'(field.predicate ...))
                                                                      (map syntax-e
                                                                           (syntax->list #'(field.annotation-str ...)))))])
                    (values class:name name name?
                            (make-struct-field-accessor name-ref field-index 'name-field 'name 'rhombus)
                            ...
                            (make-struct-field-mutator name-set! mutable-field-index 'set-name-field! 'name 'rhombus)
                            ...))))
             (if constructor-id
                 (cond
                   [(and final?
                         (not super))
                    (list
                     #`(define constructor-name
                         (let-syntax ([#,constructor-id (make-rename-transformer (quote-syntax make-name))])
                           (let ([name (wrap-constructor
                                        name make-name name?
                                        #,(hash-ref options 'constructor-rhs))])
                             name))))]
                   [else
                    (list
                     #`(define constructor-maker-name
                         (lambda (#,constructor-id)
                           (let ([name (wrap-constructor
                                        name make-name name?
                                        #,(hash-ref options 'constructor-rhs))])
                             name)))
                     #`(define constructor-name
                         #,(cond
                             [super
                              (compose-constructor
                               #'make-name
                               #`([#,(length fields) constructor-maker-name]
                                  #,@(or (class-desc-constructor-makers super)
                                         (list (length (class-desc-fields super))))))]
                             [else #'(constructor-maker-name make-name)])))])
                 null)
             (list
              #`(define-binding-syntax core-bind-name
                  (binding-transformer
                   #'name
                   (make-composite-binding-transformer #,(symbol->string (syntax-e #'name))
                                                       (quote-syntax name?)
                                                       #:static-infos (quote-syntax ((#%dot-provider name-instance)))
                                                       (list (quote-syntax name-field) ...)
                                                       #:accessor->info? #t
                                                       (list (quote-syntax field.static-infos) ...)))))
             (cond
               [(hash-ref options 'binding #f)
                => (lambda (bind)
                     (define into (make-syntax-introducer))
                     (list
                      #`(define-binding-syntax #,(intro binding-id) (make-rename-transformer
                                                                     (quote-syntax #,(in-binding-space #'core-bind-name))))
                      #`(define-binding-syntax name
                          (wrap-class-transformer name #,(intro (cadr bind)) make-binding-prefix-operator))))]
               [else null])
             (list
              #'(define-annotation-constructor core-ann-name
                  ([accessors (list (quote-syntax name-field) ...)])
                  (quote-syntax name?)
                  (quote-syntax ((#%dot-provider name-instance)))
                  cnt
                  (make-class-instance-predicate accessors)
                  (make-class-instance-static-infos accessors)))
             (cond
               [(hash-ref options 'annotation #f)
                => (lambda (ann)
                     (define into (make-syntax-introducer))
                     (list
                      #`(define-annotation-syntax #,(intro annotation-id) (make-rename-transformer
                                                                           (quote-syntax #,(in-annotation-space #'core-ann-name))))
                      #`(define-annotation-syntax name
                          (wrap-class-transformer name #,(intro (cadr ann)) make-annotation-prefix-operator))))]
               [else null])
             (list
              #`(define-class-desc-syntax name
                  (class-desc #,final?
                              (quote-syntax class:name)
                              (quote-syntax name)
                              (list (list 'super-field-name (quote-syntax super-name-field) (quote-syntax super-field-static-infos))
                                    ...
                                    (list 'field.name (quote-syntax name-field) (quote-syntax field.static-infos))
                                    ...)
                              #,(and (syntax-e #'constructor-maker-name)
                                     #`(quote-syntax ([#,(length fields) constructor-maker-name]
                                                      #,@(or (and super
                                                                  (or (class-desc-constructor-makers super)
                                                                      (list (length (class-desc-fields super)))))
                                                             '()))))))
              #'(define-name-root name
                  #:root (class-expression-transformer (quote-syntax name) (quote-syntax constructor-name))
                  #:fields ([field.name name-field] ...))
              #'(define-dot-provider-syntax name-instance
                  (dot-provider (make-handle-class-instance-dot (quote-syntax name))))
              #'(define-static-info-syntax constructor-name (#%call-result ((#%dot-provider name-instance))))
              #'(begin
                  (define-static-info-syntax/maybe* name-field (#%call-result field.static-infos))
                  ...)))))]))))

(define-for-syntax (class-expression-transformer id make-id)
  (expression-transformer
   id
   (lambda (stx)
     (syntax-parse stx
       [(_ . tail) (values make-id #'tail)]))))

(define-for-syntax (build-guard-expr fields predicates annotation-strs)
  (and (for/or ([predicate (in-list predicates)])
         (syntax-e predicate))
       #`(lambda (#,@fields who)
           (values #,@(for/list ([field (in-list fields)]
                                 [predicate (in-list predicates)]
                                 [annotation-str (in-list annotation-strs)])
                        (cond
                          [(not (syntax-e predicate)) field]
                          [else #`(if (#,predicate #,field)
                                      #,field
                                      (raise-argument-error* who
                                                             rhombus-realm
                                                             '#,annotation-str
                                                             #,field))]))))))

(define-for-syntax (make-class-instance-predicate accessors)
  (lambda (arg predicate-stxs)
    #`(and #,@(for/list ([acc (in-list accessors)]
                         [pred (in-list predicate-stxs)])
                #`(#,pred (#,acc #,arg))))))

(define-for-syntax (make-class-instance-static-infos accessors)
  (lambda (static-infoss)
    (for/list ([acc (in-list accessors)]
               [static-infos (in-list static-infoss)])
      #`(#,acc #,static-infos))))

;; dot provider for a class instance used before a `.`
(define-for-syntax ((make-handle-class-instance-dot name) form1 dot field-id)
  (define desc (syntax-local-value* (in-class-desc-space name) class-desc-ref))
  (unless desc (error "cannot find annotation binding for instance dot provider"))
  (define accessor-id+static-infos
    (for/or ([field+acc (in-list (class-desc-fields desc))])
      (and (eq? (car field+acc) (syntax-e field-id))
           (cdr field+acc))))
  (cond
    [accessor-id+static-infos
     (define accessor-id (car accessor-id+static-infos))
     (define e (datum->syntax (quote-syntax here)
                              (list (relocate field-id accessor-id) form1)
                              (span-srcloc form1 field-id)
                              #'dot))

     (define static-infos (cadr accessor-id+static-infos))
     (define more-static-infos (syntax-local-static-info form1 accessor-id))
     (define all-static-infos (if more-static-infos
                                  (datum->syntax #f
                                                 (append (syntax->list more-static-infos)
                                                         static-infos))
                                  static-infos))

     (wrap-static-info* e all-static-infos)]
    [else #f]))

(define-syntax (define-class-desc-syntax stx)
  (syntax-parse stx
    [(_ id:identifier rhs)
     #`(define-syntax #,(in-class-desc-space #'id)
         rhs)]))

(define-syntax (define-static-info-syntax/maybe* stx)
  (syntax-parse stx
    [(_ id (_)) #'(begin)]
    [(_ id rhs ...) #'(define-static-info-syntax id rhs ...)]))

(define-syntax (wrap-constructor stx)
  (syntax-parse stx
    [(_ name constructor-id predicate-id g)
     #:do [(define adjustments (entry-point-adjustments
                                '()
                                (lambda (body)
                                  #`(let ([r #,body])
                                      (if (predicate-id r)
                                          r
                                          #,(quasisyntax/loc #'g
                                              (raise-constructor-result-error 'name r)))))
                                #f))]
     #:with (~var lam (:entry-point adjustments)) #'g
     #'lam.parsed]))

(define (raise-constructor-result-error who val)
  (raise-contract-error who
                        (string-append "constructor result does not match annotation\n"
                                       "  result: ~v")
                        val))

(define-for-syntax (makers->pair-expressions stx)
  (and stx
       (syntax-parse stx
         [() '()]
         [([count id] . rest)
          (cons #'(cons count id) (makers->pair-expressions #'rest))]
         [(count) #'count])))

(define-for-syntax (compose-constructor real-make makers)
  (let loop ([makers makers]
             [final-make real-make])
    (syntax-parse makers
      [(count:exact-integer)
       ;; represents some number of ancestors that use the default protocol;
       ;; we don't expect to get here, though, since the last case below
       #`(procedure-reduce-arity
          #,final-make
          count)]
      [([count id])
       ;; root ancestor has a custom protocol
       #`(id #,final-make)]
      [([count id] . rest)
       ;; look ahead by one so we can get the arity right:
       (syntax-parse #'rest
         [(ancestor-count:exact-integer)
          (define args (generate-temporaries (for/list ([i (syntax-e #'count)]) i)))
          (define ancestor-args (generate-temporaries (for/list ([i (syntax-e #'ancestor-count)]) i)))
          #`(id
             (lambda #,ancestor-args
               (lambda #,args
                 (#,final-make #,@ancestor-args #,@args))))]
         [([_ ancestor-id] . _)
          (define args (car (generate-temporaries '(args))))
          #`(id
             (make-keyword-procedure-like
              ancestor-id
              (lambda (kws kw-args . next-args)
                (lambda #,args
                  (keyword-apply #,(loop #'rest
                                         #`(lambda ancestor-args
                                             (apply #,final-make (append ancestor-args #,args))))
                                 kws kw-args
                                 next-args)))))])])))

(define (make-keyword-procedure-like make-proc gen-proc)
  (define proc (make-proc void)) ; we know that `make-proc` immediately returns a procedure
  (define-values (allow-kws req-kws) (procedure-keywords proc))
  (procedure-reduce-keyword-arity-mask (make-keyword-procedure gen-proc)
                                       (procedure-arity-mask proc)
                                       allow-kws
                                       req-kws))

;; ----------------------------------------

(define-for-syntax (parse-options orig-stx forms)
  (syntax-parse forms
    #:context orig-stx
    [(clause::class-clause ...)
     (define clauses (apply append (map syntax->list (syntax->list #'(clause.parsed ...)))))
     (define (extract-rhs b)
       (syntax-parse b
         [(_::block g) #'g]
         [else
          (raise-syntax-error #f
                              "expected a single entry point in block body"
                              b)]))
     (let loop ([clauses clauses] [options #hasheq()])
       (cond
         [(null? clauses) options]
         [else
          (define clause (car clauses))
          (define new-options
            (syntax-parse clause
              #:literals (extends constructor final nonfinal authentic binding annotation)
              [(extends id)
               (when (hash-has-key? options 'extends)
                 (raise-syntax-error #f "redundant superclass clause" orig-stx clause))
               (hash-set options 'extends #'id)]
              [(internal id)
               (when (hash-has-key? options 'internal)
                 (raise-syntax-error #f "redundant internal-name clause" orig-stx clause))
               (hash-set options 'internal #'id)]
              [(constructor id block)
               (when (hash-has-key? options 'constructor-id)
                 (raise-syntax-error #f "redundant constructor clause" orig-stx clause))
               (hash-set (hash-set options 'constructor-id #'id)
                         'constructor-rhs
                         (extract-rhs #'block))]
              [(binding core-name block)
               (when (hash-has-key? options 'binding)
                 (raise-syntax-error #f "redundant binding clause" orig-stx clause))
               (hash-set options 'binding (list #'core-name (extract-rhs #'block)))]
              [(annotation core-name block)
               (when (hash-has-key? options 'annotation)
                 (raise-syntax-error #f "redundant annotation clause" orig-stx clause))
               (hash-set options 'annotation (list #'core-name (extract-rhs #'block)))]
              [(final)
               (when (hash-has-key? options 'final?)
                 (raise-syntax-error #f "redundant finality clause" orig-stx clause))
               (hash-set options 'final? #t)]
              [(nonfinal)
               (when (hash-has-key? options 'final?)
                 (raise-syntax-error #f "redundant finality clause" orig-stx clause))
               (hash-set options 'final? #f)]
              [(authentic)
               (when (hash-has-key? options 'authentic?)
                 (raise-syntax-error #f "redundant authenticity clause" orig-stx clause))
               (hash-set options 'authentic? #t)]
              [_
               (raise-syntax-error #f "unrecognized clause" orig-stx clause)]))
          (loop (cdr clauses) new-options)]))]))

(define-syntax extends
  (class-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ (~seq form ...))
        #:with (~var id (:hier-name-seq in-class-desc-space name-path-op name-root-ref)) #'(form ...)
        #:with () #'id.tail
        #'[(extends id.name)]]))))

(define-syntax internal
  (class-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ name:identifier)
        #'[(internal name)]]))))

(define-syntax constructor
  (class-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (group)
       [(_ (_::parens (group make:identifier))
           (~and (_::block . _)
                 constructor-block))
        #`[(constructor make constructor-block)]]
       [(_ (~and (_::block . _)
                 constructor-block))
        #`[(constructor #f constructor-block)]]))))

(define-syntax binding
  (class-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (group)
       [(_ (_::parens (group core:identifier))
           (~and (_::block . _)
                 binding-block))
        #`[(binding core binding-block)]]
       [(_ (~and (_::block . _)
                 binding-block))
        #`[(binding #f binding-block)]]))))

(define-syntax annotation
  (class-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (group)
       [(_ (_::parens (group core:identifier))
           (~and (_::block . _)
                 annotation-block))
        #`[(annotation core annotation-block)]]
       [(_ (~and (_::block . _)
                 annotation-block))
        #`[(annotation #f annotation-block)]]))))

(define-syntax nonfinal
  (class-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_) #`[(nonfinal)]]))))

(define-syntax final
  (class-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_) #`[(final)]]))))

(define-syntax authentic
  (class-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_) #`[(authentic)]]))))
