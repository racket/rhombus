#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/syntax-local
                     "srcloc.rkt"
                     "class-parse.rkt"
                     "interface-parse.rkt"
                     "veneer-parse.rkt"
                     "class-method-result.rkt"
                     "statically-str.rkt")
         "class-method.rkt"
         (submod "dot.rkt" for-dot-provider)
         "dotted-sequence-parse.rkt"
         "export-check.rkt"
         "parens.rkt"
         "static-info.rkt"
         "expression.rkt"
         (only-in (submod "expr-macro.rkt" for-define)
                  make-expression-prefix-operator)
         (only-in "repetition.rkt"
                  in-repetition-space
                  repetition-transformer
                  identifier-repetition-use
                  :repetition-info
                  :infix-op+repetition-use+tail)
         "compound-repetition.rkt"
         (submod "assign.rkt" for-assign) (only-in "assign.rkt" :=)
         "op-literal.rkt"
         "name-root.rkt"
         "parse.rkt"
         (submod "function-parse.rkt" for-call)
         "class-transformer.rkt"
         "class-dot-transformer.rkt"
         "is-static.rkt"
         "realm.rkt")

(provide (for-syntax build-class-dot-handling
                     build-interface-dot-handling

                     extract-all-dot-names
                     add-super-dot-providers)
         no-dynamic-dot-syntax)

(define-for-syntax (build-class-dot-handling method-mindex method-vtable method-results final?
                                             has-private? method-private method-private-inherit
                                             exposed-internal-id internal-of-id
                                             expression-macro-rhs intro constructor-given-name
                                             exported-of internal-exported-of
                                             dot-provider-rhss parent-dot-providers
                                             names
                                             #:veneer? [veneer? #f])
  (with-syntax ([(name name-extends tail-name
                       name? name-convert constructor-name name-instance name-ref name-of
                       make-internal-name internal-name-instance dot-provider-name
                       indirect-static-infos dot-providers
                       [public-field-name ...] [private-field-name ...] [field-name ...]
                       [public-name-field ...] [name-field ...]
                       [public-name-field/mutate ...]
                       [dot-id ...]
                       [private-field-desc ...]
                       [ex ...]
                       base-ctx scope-ctx)
                 names])
    (register-field-check #'(base-ctx scope-ctx ex ...))
    (define-values (method-names method-impl-ids method-defns)
      (method-static-entries method-mindex method-vtable method-results #'name-ref #'name? final? #f))
    (with-syntax ([(method-name ...) method-names]
                  [(method-id ...) method-impl-ids]
                  [(dot-rhs ...) dot-provider-rhss]
                  [(dot-rhs-id ...) (map (make-syntax-introducer) (syntax->list #'(dot-id ...)))]
                  [(dot-class-id ...) (map (make-syntax-introducer) (syntax->list #'(dot-id ...)))])
      (append
       method-defns
       (append
        ;; build `(define-syntax name ....)`, but supporting `name-extends`
        (cond
          [expression-macro-rhs
           (list
            (build-syntax-definition/maybe-extension
             #f #'name #'name-extends
             (wrap-class-transformer #'name #'tail-name
                                     (intro expression-macro-rhs)
                                     #'make-expression-prefix-operator
                                     "class")))]
          [veneer?
           (cons
            #`(define constructor-name
                (let ([name (lambda (v)
                              #,(cond
                                  [(syntax-e #'name-convert)
                                   #`(name-convert v 'name)]
                                  [(syntax-e #'name?)
                                   #`(begin
                                       (name? v 'name)
                                       v)]
                                  [else
                                   #`v]))])
                  name))
            (build-syntax-definitions/maybe-extension
             (list #f 'rhombus/repet) #'name #'name-extends
             #`(class-expression-transformers (quote-syntax name) (quote-syntax constructor-name) #t)))]
          [(and constructor-given-name
                (not (free-identifier=? #'name constructor-given-name)))
           (list
            (build-syntax-definition/maybe-extension
             #f #'name #'name-extends
             #'no-constructor-transformer))]
          [else
           (build-syntax-definitions/maybe-extension
            (list #f 'rhombus/repet) #'name #'name-extends
            #`(class-expression-transformers (quote-syntax name) (quote-syntax constructor-name)))])
        (list
         #`(define-name-root name
             #:extends name-extends
             #:fields ([public-field-name public-name-field/mutate]
                       ...
                       [method-name method-id]
                       ...
                       [dot-id dot-class-id]
                       ...
                       ex ...
                       #,@(if exported-of
                              #`([#,exported-of name-of])
                              null))))
        (syntax->list
         #`((define-syntaxes (dot-class-id dot-rhs-id)
              (let ([dot-id dot-rhs])
                (values (wrap-class-dot-via-class dot-id (quote-syntax dot-id)
                                                  (quote-syntax name?) (quote-syntax dot-providers))
                        dot-id)))
            ...))
        (maybe-dot-provider-definition #'(dot-rhs-id ...) #'dot-provider-name parent-dot-providers)
        (list
         #`(define-dot-provider-syntax name-instance
             (dot-provider #,(let ([default #`(make-handle-class-instance-dot (quote-syntax name)
                                                                              #hasheq()
                                                                              #hasheq())])
                               (if (syntax-e #'dot-provider-name)
                                   #`(compose-dot-providers
                                      (quote-syntax dot-provider-name)
                                      #,default)
                                   default)))))
        (if exposed-internal-id
            (with-syntax ([([private-method-name private-method-id private-method-id/intf/prop] ...)
                           (for/list ([(sym id/prop) (in-hash method-private)])
                             (define id (if (pair? id/prop) (car id/prop) id/prop))
                             (define id/intf/prop
                               (cond
                                 [(and (not final?)
                                       (hash-ref method-private-inherit sym #f))
                                  => (lambda (vec)
                                       ;; Override of private implemented
                                       (if (identifier? id/prop)
                                           vec
                                           (list vec)))]
                                 [else id/prop]))
                             (list sym id id/intf/prop))])
              (list
               #`(define-syntaxes (#,exposed-internal-id #,(in-repetition-space exposed-internal-id))
                   (class-expression-transformers (quote-syntax name) (quote-syntax make-internal-name)))
               #`(define-name-root #,exposed-internal-id
                   #:fields ([field-name name-field]
                             ...
                             [method-name method-id]
                             ...
                             [private-method-name private-method-id]
                             ...
                             #,@(if internal-exported-of
                                    #`([#,internal-exported-of #,internal-of-id])
                                    null)))
               #`(define-dot-provider-syntax internal-name-instance
                   (dot-provider (make-handle-class-instance-dot (quote-syntax name)
                                                                 (hasheq
                                                                  (~@ 'private-field-name
                                                                      private-field-desc)
                                                                  ...)
                                                                 (hasheq
                                                                  (~@ 'private-method-name
                                                                      (quote-syntax private-method-id/intf/prop))
                                                                  ...))))))
            null))))))

(define-for-syntax (build-interface-dot-handling method-mindex method-vtable method-results
                                                 internal-name
                                                 expression-macro-rhs
                                                 dot-provider-rhss parent-dot-providers
                                                 names)
  (with-syntax ([(name name-extends tail-name
                       name? name-instance name-ref static-name-ref
                       internal-name-instance internal-name-ref
                       dot-provider-name [dot-id ...]
                       dot-providers
                       [ex ...]
                       base-ctx scope-ctx)
                 names])
    (register-field-check #'(base-ctx scope-ctx ex ...))
    (define-values (method-names method-impl-ids method-defns)
      (method-static-entries method-mindex method-vtable method-results #'static-name-ref #'name? #f #t))
    (with-syntax ([(method-name ...) method-names]
                  [(method-id ...) method-impl-ids]
                  [(dot-rhs ...) dot-provider-rhss]
                  [(dot-rhs-id ...) (map (make-syntax-introducer) (syntax->list #'(dot-id ...)))]
                  [(dot-intf-id ...) (map (make-syntax-introducer) (syntax->list #'(dot-id ...)))])
      (append
       method-defns
       (list
        (build-syntax-definition/maybe-extension
         #f #'name #'name-extends
         (cond
           [expression-macro-rhs
            (wrap-class-transformer #'name #'tail-name
                                    ((make-syntax-introducer) expression-macro-rhs)
                                    #'make-expression-prefix-operator
                                    "interface")]
           [else #'no-constructor-transformer]))
        #`(define-name-root name
            #:extends name-extends
            #:fields ([method-name method-id]
                      ...
                      [dot-id dot-intf-id]
                      ...
                      ex ...)))
       (syntax->list
        #`((define-syntaxes (dot-intf-id dot-rhs-id)
             (let ([dot-id dot-rhs])
               (values (wrap-class-dot-via-class dot-id (quote-syntax dot-id)
                                                 (quote-syntax name?) (quote-syntax dot-providers))
                       dot-id)))
           ...))
       (maybe-dot-provider-definition #'(dot-rhs-id ...) #'dot-provider-name parent-dot-providers)
       (list
        #`(define-dot-provider-syntax name-instance
             (dot-provider #,(let ([default #'(make-handle-class-instance-dot (quote-syntax name)
                                                                              #hasheq()
                                                                              #hasheq())])

                               (if (syntax-e #'dot-provider-name)
                                   #`(compose-dot-providers
                                      (quote-syntax dot-provider-name)
                                      #,default)
                                   default)))))
       (if internal-name
           (list
            #`(define-dot-provider-syntax internal-name-instance
                (dot-provider (make-handle-class-instance-dot (quote-syntax #,internal-name) #hasheq() #hasheq()))))
           null)))))

(define-for-syntax (maybe-dot-provider-definition dot-rhs-ids dot-provider-name parent-dot-providers)
  (if (and (syntax-e dot-provider-name)
           (or (pair? (syntax-e dot-rhs-ids))
               (and (pair? parent-dot-providers)
                    (pair? (cdr parent-dot-providers)))))
      (list
       #`(define-syntax #,dot-provider-name
           (compose-dot-providers
            #,@(if (pair? (syntax-e dot-rhs-ids))
                   (list #`(wrap-class-dot-provider-transformers
                            (quote-syntax #,dot-rhs-ids)))
                   null)
            #,@(for/list ([name (in-list parent-dot-providers)])
                 #`(quote-syntax #,name)))))
      null))

(define-for-syntax (method-static-entries method-mindex method-vtable method-results name-ref-id name?-id
                                          final? via-interface?)
  (for/fold ([names '()] [ids '()] [defns '()])
            ([(name mix) (in-hash method-mindex)])
    (cond
      [(and (not (mindex-final? mix))
            (not final?))
       (define proc-id (car (generate-temporaries (list name))))
       (define stx-id (car (generate-temporaries (list name))))
       (values (cons name names)
               (cons stx-id ids)
               (list* #`(define #,proc-id
                          (make-method-accessor '#,name #,name-ref-id #,(mindex-index mix)))
                      #`(define-syntax #,stx-id
                          (make-method-accessor-transformer '#,name
                                                            (quote-syntax #,name-ref-id)
                                                            #,(mindex-index mix)
                                                            (quote-syntax #,proc-id)
                                                            #,(let ([ids (hash-ref method-results name #f)])
                                                                (and ids
                                                                     #`(quote-syntax #,(car ids))))))
                      defns))]
      [(or
        ;; if the method is inherited, we need to check whether
        ;; a provided instance is the subclass, not a superclass
        (mindex-inherited? mix)
        ;; if the method is in an interface, then even if it's final,
        ;; we need to check using `name-ref-id`, because...
        via-interface?)
       (define proc-id (vector-ref method-vtable (mindex-index mix)))
       (define stx-id (car (generate-temporaries (list name))))
       (values (cons name names)
               (cons stx-id ids)
               (list*
                #`(define-syntax #,stx-id
                    (make-method-checked-static-transformer '#,name
                                                            (quote-syntax #,name?-id)
                                                            (quote-syntax #,proc-id)))
                defns))]
      [else
       (values (cons name names)
               (cons (vector-ref method-vtable (mindex-index mix)) ids)
               defns)])))

(define (make-method-accessor name ref idx)
  (procedure-rename
   (make-keyword-procedure
    (lambda (kws kw-args obj . args)
      (keyword-apply (method-ref ref obj idx) kws kw-args obj args))
    (lambda (obj . args)
      (apply (method-ref ref obj idx) obj args)))
   name))

(define-for-syntax (make-method-accessor-transformer name name-ref-id idx proc-id ret-info-id)
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (op)
       [(rator-id (~and args (tag::parens self arg ...)) . tail)
        #:when (not (syntax-parse #'self [(_ kw:keyword . _) #t] [_ #f]))
        (define obj-id #'this)
        (define rator #`(method-ref #,name-ref-id #,obj-id #,idx))
        (define r (and ret-info-id
                       (syntax-local-method-result ret-info-id)))
        (define-values (call new-tail to-anon-function?)
          (parse-function-call rator (list obj-id) #`(#,obj-id (tag arg ...))
                               #:static? (is-static-context? #'tag)
                               #:rator-stx (datum->syntax #f name #'rator-id)
                               #:rator-arity (and r (method-result-arity r))
                               #:can-anon-function? #t))
        (values (let ([call #`(let ([#,obj-id (rhombus-expression self)])
                                #,call)])
                  (if (and r (not to-anon-function?))
                      (wrap-static-info* call (method-result-static-infos r))
                      call))
                #'tail)]
       [(head (tag::parens) . _)
        #:when (is-static-context? #'tag)
        (raise-syntax-error #f
                            (string-append "wrong number of arguments in function call" statically-str)
                            (datum->syntax #f name #'head))]
       [(_ . tail)
        (values proc-id #'tail)]))))

(define-for-syntax (make-method-checked-static-transformer name name?-id proc-id)
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (op)
       [(rator-id (~and args (tag::parens self arg ...)) . tail)
        #:when (not (syntax-parse #'self [(_ kw:keyword . _) #t] [_ #f]))
        (define obj-id #'this)
        (define-values (call new-tail to-anon-function?)
          (parse-function-call proc-id (list obj-id) #'(#,obj-id (tag arg ...))
                               #:static? (is-static-context? #'tag)
                               #:rator-stx (datum->syntax #f name #'rator-id)))
        (values (wrap-static-info*
                 #`(let ([#,obj-id (rhombus-expression self)])
                     (unless (#,name?-id #,obj-id) (raise-not-an-instance '#,name #,obj-id))
                     #,(unwrap-static-infos call))
                 (extract-static-infos call))
                #'tail)]
       [(head (tag::parens) . _)
        #:when (is-static-context? #'tag)
        (raise-syntax-error #f
                            (string-append "wrong number of arguments in function call" statically-str)
                            (datum->syntax #f name #'head))]
       [(_ . tail)
        (values #`(wrap-object-check #,proc-id '#,name #,name?-id)
                #'tail)]))))

(define (wrap-object-check proc name name?)
  (define-values (req-kws allowed-kws) (procedure-keywords proc))
  (cond
    [(null? allowed-kws)
     (procedure-reduce-arity-mask (lambda (obj . args)
                                    (unless (name? obj) (raise-not-an-instance name obj))
                                    (apply proc obj args))
                                  (procedure-arity-mask proc)
                                  name)]
    [else
     (procedure-reduce-keyword-arity-mask (make-keyword-procedure
                                           (lambda (kws kw-args obj . args)
                                             (unless (name? obj) (raise-not-an-instance name obj))
                                             (keyword-apply proc kws kw-args obj args)))
                                          (procedure-arity-mask proc)
                                          req-kws
                                          allowed-kws
                                          name)]))
(define-for-syntax (check-static stx)
  (unless (is-static-context/tail? stx)
    (raise-syntax-error #f
                        "not allowed in a dynamic context"
                        stx)))

(define-for-syntax (class-expression-transformers id make-id [static-only? #f])
  (values
   (expression-transformer
    (lambda (stx)
      (when static-only? (check-static stx))
      (syntax-parse stx
        [(head . tail) (values (relocate-id #'head make-id)
                               #'tail)])))
   (repetition-transformer
    (lambda (stx)
      (when static-only? (check-static stx))
      (syntax-parse stx
        [(head . tail) (values (identifier-repetition-use (relocate-id #'head make-id))
                               #'tail)])))))

(define-for-syntax (desc-ref-id desc)
  (cond
    [(class-desc? desc)
     (class-desc-ref-id desc)]
    [(interface-desc? desc)
     (interface-desc-ref-id desc)]
    [else
     #f]))

;; dot provider for a class instance used before a `.`
(define-for-syntax ((make-handle-class-instance-dot name internal-fields internal-methods)
                    form1 dot field-id
                    tail more-static? repetition?
                    success failure)
  (define desc (syntax-local-value* (in-class-desc-space name) (lambda (v)
                                                                 (or (class-desc-ref v)
                                                                     (interface-desc-ref v)
                                                                     (veneer-desc-ref v)))))
  (unless desc (error "cannot find annotation binding for instance dot provider"))
  (define (do-field fld)
    (cond
      [repetition?
       ;; let dot-provider dispatcher handle repetition construction:
       (failure)]
      [else
       (define accessor-id (field-desc-accessor-id fld))
       (syntax-parse tail
         [assign::assign-op-seq
          #:when (syntax-e (field-desc-mutator-id fld))
          (define-values (assign-expr tail) (build-assign
                                             (attribute assign.op)
                                             #'assign.op-name
                                             #'assign.name
                                             #`(lambda ()
                                                 (#,(relocate field-id accessor-id) obj))
                                             #`(lambda (v)
                                                 (#,(field-desc-mutator-id fld) obj v))
                                             #'obj
                                             #'assign.tail))
          (success #`(let ([obj #,form1])
                       #,assign-expr)
                   tail)]
         [_
          (define e (relocate+reraw
                     (respan (datum->syntax #f (list form1 dot field-id)))
                     (datum->syntax (quote-syntax here)
                                    (list (relocate field-id accessor-id) form1)
                                    #f
                                    #'dot)))
          (define static-infos (field-desc-static-infos fld))
          (define more-static-infos (syntax-local-static-info form1 accessor-id))
          (define all-static-infos (if more-static-infos
                                       (datum->syntax #f
                                                      (append (syntax->list more-static-infos)
                                                              static-infos))
                                       static-infos))
          (success (wrap-static-info* e all-static-infos)
                   tail)])]))
  (define (do-method pos/id*
                     #:result-id [ret-info-id #f]
                     #:nonfinal? [nonfinal? #f]
                     #:property? [property? #f]
                     #:shape-arity [shape-arity #f]
                     #:add-check [add-check #f]
                     #:ref-id [ref-id (desc-ref-id desc)])
    (define-values (args new-tail)
      (if property?
          (syntax-parse tail
            [(_:::=-expr . tail)
             #:when (not repetition?)
             (define-values (e-parsed e-tail)
               (syntax-parse #'(group  . tail)
                 [(~var e (:infix-op+expression+tail #':=))
                  (values #'e.parsed #'e.tail)]))
             (values (relocate+reraw e-parsed
                                     #`(#,(no-srcloc #'parens)
                                        (group (parsed #:rhombus/expr #,e-parsed))))
                     e-tail)]
            [_ (values (no-srcloc #'(parens)) tail)])
          (syntax-parse tail
            #:datum-literals (op)
            [((~and args (::parens arg ...)) . tail)
             (values #'args #'tail)]
            [_
             (values #f tail)])))
    (define pos/id
      (cond
        [(identifier? pos/id*) pos/id*]
        [nonfinal? pos/id*] ; dynamic dispatch
        [else (vector-ref (syntax-e (objects-desc-method-vtable desc)) pos/id*)]))
    (cond
      [args
       (define obj-id #'obj)
       (define (checked-wrap-call e extra-rands)
         (define form1 (car extra-rands))
         (relocate+reraw form1
                         #`(let ([#,obj-id #,form1])
                             #,@(if add-check
                                    (list (add-check obj-id))
                                    null)
                             #,e)))
       ;; Define `wrap-rator` and `wrap-obj-e` instead of just `rator`
       ;; and `obj-e` so that we can push the relavant operators into
       ;; the body of a `for` that is generated for a repetition.
       ;; In the case that a method is extracted from a vtable, we
       ;; need to have a single `form1` result bound to use in both
       ;; the rator and the added extra rand (that supplies "self").
       (define-values (wrap-call rator wrap-rator wrap-obj-e arity static-infos)
         (cond
           [(identifier? pos/id)
            (cond
              [add-check
               (values checked-wrap-call
                       pos/id
                       (lambda (rator extra-rands) rator)
                       (lambda (extra-rand extra-rands) obj-id)
                       #f
                       #'())]
              [else
               (values (lambda (e extra-rands) e)
                       pos/id
                       (lambda (rator extra-rands) rator)
                       (lambda (extra-rand extra-rands) extra-rand)
                       #f
                       #'())])]
           [else
            (define r (and ret-info-id
                           (syntax-local-method-result ret-info-id)))
            (define static-infos
              (or (and ret-info-id
                       (method-result-static-infos r))
                  #'()))
            (values checked-wrap-call
                    #'placeholder
                    (lambda (placeholder extra-rands) #`(method-ref #,ref-id #,obj-id #,pos/id))
                    (lambda (extra-rand extra-rands) obj-id)
                    (or shape-arity
                        (and r (method-result-arity r)))
                    static-infos)]))
       (define-values (call-stx empty-tail to-anon-function?)
         (parse-function-call (if repetition? (identifier-repetition-use rator) rator)
                              (list form1)
                              #`(#,form1 #,args)
                              #:wrap-call wrap-call
                              #:wrap-rator wrap-rator
                              #:wrap-extra-rand wrap-obj-e
                              #:rator-stx field-id
                              #:srcloc (respan #`(#,form1 #,field-id #,args))
                              #:static? more-static?
                              #:repetition? repetition?
                              #:rator-arity arity
                              #:rator-kind (if property? 'property 'method)
                              #:result-static-infos static-infos
                              #:can-anon-function? (not repetition?)))
       (success call-stx
                new-tail)]
      [else
       (when (and more-static?
                  (not property?))
         (raise-syntax-error #f
                             (string-append "method must be called" statically-str)
                             field-id))
       (cond
         [(identifier? pos/id)
          (success #`(curry-method #,pos/id #,form1) new-tail)]
         [else
          (success #`(method-curried-ref #,ref-id #,form1 #,pos/id) new-tail)])]))
  (define (make-interface-check desc name)
    (lambda (obj-id)
      #`(void (#,(interface-desc-ref-id desc) #,obj-id))))
  (cond
    [(and (class-desc? desc)
          (for/or ([field+acc (in-list (class-desc-fields desc))])
            (and (eq? (field-desc-name field+acc) (syntax-e field-id))
                 field+acc)))
     => (lambda (fld) (do-field fld))]
    [(hash-ref (objects-desc-method-map desc) (syntax-e field-id) #f)
     => (lambda (pos)
          (define shape (vector-ref (objects-desc-method-shapes desc) pos))
          (define shape-symbol (and shape (if (vector? shape) (vector-ref shape 0) shape)))
          (define shape-arity (and shape (vector? shape) (vector-ref shape 1)))
          (define non-final? (or (box? shape-symbol) (and (pair? shape-symbol) (box? (car shape-symbol)))))
          (do-method (if (veneer-desc? desc)
                         ;; always static:
                         (vector-ref (syntax-e (objects-desc-method-vtable desc)) pos)
                         ;; dynamic:
                         pos)
                     #:result-id (hash-ref (objects-desc-method-result desc) (syntax-e field-id) #f)
                     #:nonfinal? non-final?
                     #:property? (pair? shape-symbol)
                     #:shape-arity shape-arity
                     ;; corner case: final method in interface called through
                     ;; non-internal needs a check that the argument is not merely
                     ;; an internal instance, since the implementation checks only
                     ;; for an internal instance
                     #:add-check (and (interface-desc? desc)
                                      (not (interface-internal-desc? desc))
                                      (not non-final?)
                                      (make-interface-check desc field-id))))]
    [(hash-ref internal-fields (syntax-e field-id) #f)
     => (lambda (fld) (do-field fld))]
    [(hash-ref internal-methods (syntax-e field-id) #f)
     => (lambda (id/intf/property)
          (define property? (pair? (syntax-e id/intf/property)))
          (define id/intf (if property?
                              (car (syntax-e id/intf/property))
                              id/intf/property))
          (cond
            [(identifier? id/intf)
             (do-method id/intf #:property? property?)]
            [else
             (define-values (ref-id pos result-id) (unpack-intf-ref id/intf))
             (do-method pos #:result-id result-id #:property? property? #:nonfinal? #t #:ref-id ref-id)]))]
    [(hash-ref (get-private-table desc) (syntax-e field-id) #f)
     => (lambda (id/intf/fld)
          (cond
            [(identifier? id/intf/fld)
             (do-method id/intf/fld)]
            [(and (syntax? id/intf/fld) (pair? (syntax-e id/intf/fld)))
             (cond
               [(vector? (syntax-e (car (syntax-e id/intf/fld))))
                (define-values (ref-id pos result-id) (unpack-intf-ref (car (syntax-e id/intf/fld))))
                (do-method pos #:result-id result-id #:property? #t #:nonfinal? #t #:ref-id ref-id)]
               [else
                (do-method (car (syntax-e id/intf/fld)) #:property? #t)])]
            [(syntax? id/intf/fld)
             (define-values (ref-id pos result-id) (unpack-intf-ref id/intf/fld))
             (do-method pos #:result-id result-id #:nonfinal? #t #:ref-id ref-id)]
            [else
             (do-field id/intf/fld)]))]
    [(and more-static?
          (not repetition?))
     (raise-syntax-error #f
                         (string-append "no such field or method" statically-str)
                         field-id)]
    [else (failure)]))

(define-for-syntax no-constructor-transformer
  (expression-transformer
   (lambda (stx)
     (raise-syntax-error #f "cannot be used as an expression" stx))))

(define (no-dynamic-dot-syntax id)
  (lambda (obj)
    (raise-arguments-error* id rhombus-realm
                            "dynamic use of dot syntax disallowed"
                            "object" obj)))

(define-for-syntax (extract-all-dot-names ids-stx supers)
  (apply
   append
   (map syntax-e (syntax->list ids-stx))
   (for/list ([super (in-list supers)]
              #:when super)
     (objects-desc-dots super))))

;; A `#%dot-provider` value can be a list of dot-provider identifieries,
;; which is useful for intersection to reflect a shared super interface.
;; We only try to do that in cases of single-inheritance, however --- and
;; only when it will stay single inheritance, so that's why we ignore
;; `interfaces`.
(define-for-syntax (add-super-dot-providers name-instance super interfaces)
  (cond
    [super
     (define dot-providers
       (cond
         [(class-desc? super) (class-desc-instance-dot-providers super)]
         [(veneer-desc? super) (veneer-desc-instance-dot-providers super)]
         [else (error "unknown super")]))
     (cons name-instance (if (identifier? dot-providers) (list dot-providers) dot-providers))]
    [else name-instance]))
