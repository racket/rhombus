#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/syntax-local
                     "srcloc.rkt"
                     "class-parse.rkt"
                     "interface-parse.rkt"
                     "veneer-parse.rkt"
                     "class-method-result.rkt"
                     "statically-str.rkt"
                     "maybe-as-original.rkt")
         racket/unsafe/undefined
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
         "realm.rkt"
         "name-prefix.rkt"
         "static-info.rkt"
         "indirect-static-info-key.rkt")

(provide (for-syntax build-class-dot-handling
                     build-interface-dot-handling

                     extract-all-dot-names
                     add-super-dot-providers)
         no-dynamic-dot-syntax)

(define-for-syntax (build-class-dot-handling method-mindex method-orig-names method-vtable method-results replaced-ht final?
                                             has-private? method-private method-private-inherit
                                             exposed-internal-id internal-of-id
                                             expression-macro-rhs intro constructor-given-name
                                             exported-of internal-exported-of
                                             dot-provider-rhss parent-dot-providers
                                             names
                                             #:veneer? [veneer? #f])
  (with-syntax ([(name reflect-name name-extends tail-name
                       name? name-convert constructor-name name-instance name-ref name-of
                       make-internal-name internal-name-instance dot-provider-name
                       indirect-static-infos dot-providers
                       [all-public-field-name ...] [private-field-name ...] [field-name ...]
                       [public-name-field ...] [name-field ...]
                       [all-public-name-field/mutate ...]
                       [dot-id ...]
                       [private-field-desc ...]
                       [ex ...]
                       base-ctx scope-ctx)
                 names])
    (register-field-check #'(base-ctx scope-ctx ex ...))
    (define-values (method-names method-impl-ids method-defns)
      (method-static-entries method-mindex method-orig-names method-vtable method-results replaced-ht
                             #'name-ref #'name? final? #f #'reflect-name))
    (with-syntax ([((public-field-name public-name-field/mutate) ...)
                   (filter-replaced replaced-ht #'(all-public-field-name ...) #'(all-public-name-field/mutate ...))]
                  [(method-name ...) method-names]
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
           (cond
             [(eq? '#:none (syntax-e expression-macro-rhs))
              null]
             [else
              (list
               (build-syntax-definition/maybe-extension
                #f #'name #'name-extends
                (if (eq? '#:error (syntax-e expression-macro-rhs))
                    #'no-constructor-transformer
                    (wrap-class-transformer #'name #'tail-name
                                            (intro expression-macro-rhs)
                                            #'make-expression-prefix-operator
                                            "class"))))])]
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
                                                                              #f
                                                                              #hasheq()
                                                                              #hasheq())])
                               (if (syntax-e #'dot-provider-name)
                                   #`(compose-dot-providers
                                      (quote-syntax dot-provider-name)
                                      #,default)
                                   default)))))
        (if exposed-internal-id
            (with-syntax ([([private-method-name private-method-id private-method-id/intf/prop] ...)
                           (extract-private-method-names method-private
                                                         method-private-inherit
                                                         final?)])
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
                                                                 #t
                                                                 (hasheq
                                                                  (~@ 'private-field-name
                                                                      private-field-desc)
                                                                  ...)
                                                                 (hasheq
                                                                  (~@ 'private-method-name
                                                                      (quote-syntax private-method-id/intf/prop))
                                                                  ...))))))
            null))))))

(define-for-syntax (extract-private-method-names method-private method-private-inherit final?)
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
    (list sym id id/intf/prop)))

(define-for-syntax (build-interface-dot-handling method-mindex method-orig-names method-vtable method-results replaced-ht
                                                 internal-name
                                                 expression-macro-rhs
                                                 dot-provider-rhss parent-dot-providers
                                                 names)
  (with-syntax ([(name reflect-name name-extends tail-name
                       name? name-instance name-ref static-name-ref
                       internal-name-instance internal-name-ref
                       dot-provider-name [dot-id ...]
                       dot-providers
                       [ex ...]
                       base-ctx scope-ctx)
                 names])
    (register-field-check #'(base-ctx scope-ctx ex ...))
    (define-values (method-names method-impl-ids method-defns)
      (method-static-entries method-mindex method-orig-names method-vtable method-results replaced-ht
                             #'static-name-ref #'name? #f #t #'reflect-name))
    (with-syntax ([(method-name ...) method-names]
                  [(method-id ...) method-impl-ids]
                  [(dot-rhs ...) dot-provider-rhss]
                  [(dot-rhs-id ...) (map (make-syntax-introducer) (syntax->list #'(dot-id ...)))]
                  [(dot-intf-id ...) (map (make-syntax-introducer) (syntax->list #'(dot-id ...)))])
      (append
       method-defns
       (cond
         [(and expression-macro-rhs
               (eq? '#:none (syntax-e expression-macro-rhs)))
          null]
         [else
          (list
           (build-syntax-definition/maybe-extension
            #f #'name #'name-extends
            (cond
              [(and expression-macro-rhs
                    (not (eq? '#:error (syntax-e expression-macro-rhs))))
               (wrap-class-transformer #'name #'tail-name
                                       ((make-syntax-introducer) expression-macro-rhs)
                                       #'make-expression-prefix-operator
                                       "interface")]
              [else #'no-constructor-transformer])))])
       (list
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
                                                                             #f
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
                (dot-provider (make-handle-class-instance-dot (quote-syntax #,internal-name)
                                                              #t
                                                              #hasheq()
                                                              #hasheq()))))
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

(define-for-syntax (method-static-entries method-mindex method-orig-names method-vtable method-results replaced-ht
                                          name-ref-id name?-id final? via-interface? reflect-name)
  (for/fold ([names '()] [ids '()] [defns '()])
            ([(name mix) (in-hash method-mindex)]
             #:unless (hash-ref replaced-ht name #f))
    (define prefixed-name (add-name-prefix reflect-name (datum->syntax #f name)))
    (define final-method? (or (mindex-final? mix)
                              final?))
    (define make-rator
      (cond
        [(not final-method?)
         (define idx (mindex-index mix))
         (lambda (obj-id)
           #`(method-ref #,name-ref-id #,obj-id #,idx))]
        [else
         (lambda (obj-id) (vector-ref method-vtable (mindex-index mix)))]))
    (define proc-id (maybe-as-original (car (generate-temporaries (list name)))
                                       (hash-ref method-orig-names (mindex-index mix) #f)))
    (define new-defns
      (append
       (list
        #`(define #,proc-id #,(gen-wrapper-at-arity (mindex-arity mix) make-rator prefixed-name
                                                    (and (or (mindex-inherited? mix)
                                                             (not final-method?)
                                                             via-interface?)
                                                         name?-id)
                                                    (datum->syntax
                                                     #f
                                                     (or (mindex-reflect-name mix)
                                                         prefixed-name)))))
       (let ([si-id
              ;; piggy-back on method implement's static info, but if there is
              ;; no implementation (because it's abstract), then the method-result
              ;; name will also be used for static info
              (let ([id (vector-ref method-vtable (mindex-index mix))])
                (if (eq? id '#:abstract)
                    (let ([ids (hash-ref method-results name #f)])
                      (and ids (car ids)))
                    id))])
         (if si-id
             (list
              #`(define-static-info-syntax #,proc-id (#%indirect-static-info #,si-id)))
             null))
       defns))
    (values (cons name names)
            (cons proc-id ids)
            new-defns)))

(define-for-syntax (filter-replaced replaced-ht names proc-ids)
  (for/list ([name (in-list (syntax->list names))]
             [proc-id (in-list (syntax->list proc-ids))]
             #:unless (hash-ref replaced-ht (syntax-e name) #f))
    (list name proc-id)))

(define-for-syntax (gen-wrapper-at-arity a make-rator new-proc-id maybe-name?-id reflect-name)
  (define mask (cond
                 [(not a) -2]
                 [(integer? a) a]
                 [else (car a)]))
  (define allowed-kws (and a (if (integer? a) null (caddr a))))
  (define (n-args n) (for/list ([i (in-range n)])
                       (string->symbol (format "arg~a" i))))
  (define (check obj-id)
    (if maybe-name?-id
        #`(unless (#,maybe-name?-id #,obj-id) (raise-not-an-instance '#,reflect-name #,obj-id))
        #'(void)))
  (cond
    [(null? allowed-kws)
     (define proc #`(case-lambda
                      #,@(let loop ([mask mask] [n 0])
                           (cond
                             [(= mask 0) '()]
                             [(= mask (bitwise-not (sub1 (arithmetic-shift 1 n))))
                              ;; accept n or more
                              (define args (n-args n))
                              #`([(#,@args . rest)
                                  #,(check (car args))
                                  (apply #,(make-rator (car args)) #,@args rest)])]
                             [(not (zero? (bitwise-and mask (arithmetic-shift 1 n))))
                              (define args (n-args n))
                              (cons #`[#,args
                                       #,(check (car args))
                                       (#,(make-rator (car args)) . #,args)]
                                    (loop (- mask (arithmetic-shift 1 n)) (add1 n)))]
                             [else
                              (loop mask (add1 n))]))))
     (syntax-property proc 'inferred-name (syntax-e reflect-name))]
    [(or (not allowed-kws)
         ;; implemented with multiple keyword cases?
         (not (zero? (bitwise-and mask (sub1 (arithmetic-shift 1 (sub1 (integer-length mask))))))))
     #`(procedure-reduce-keyword-arity-mask (make-keyword-procedure
                                             (lambda (kws kw-args obj . args)
                                               #,(check #'obj)
                                               (keyword-apply #,(make-rator #'obj) kws kw-args obj args)))
                                            #,mask
                                            '#,(if a (cadr a) '())
                                            '#,allowed-kws
                                            '#,reflect-name)]
    [else
     (define args (n-args (- (integer-length mask) (if (negative? mask) 0 1))))
     (define reqd-kw (for/hash ([kw (in-list (cadr a))])
                       (values kw #t)))
     (define kw-formal-args (apply append
                                   (for/list ([kw (in-list allowed-kws)]
                                              [i (in-naturals)])
                                     (define arg (string->symbol (format "kw~a" i)))
                                     (list kw (if (hash-ref reqd-kw kw #f)
                                                  arg
                                                  (list arg #'unsafe-undefined))))))
     (define kw-args (for/list ([kw/arg (in-list kw-formal-args)])
                       (if (pair? kw/arg) (car kw/arg) kw/arg)))
     (define proc (if (negative? mask)
                      #`(lambda (#,@args #,@kw-formal-args . rest)
                          #,(check (car args))
                          (apply #,(make-rator (car args)) #,@args #,@kw-args rest))
                      #`(lambda (#,@args #,@kw-formal-args)
                          #,(check (car args))
                          (#,(make-rator (car args)) #,@args #,@kw-args))))
     (syntax-property proc 'inferred-name (syntax-e reflect-name))]))

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
(define-for-syntax ((make-handle-class-instance-dot name do-allow-protected? internal-fields internal-methods)
                    form1 dot field-id
                    tail more-static? repetition?
                    success failure)
  (define desc (syntax-local-value* (in-class-desc-space name) objects-desc-ref))
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
          (success #`(let ([obj #,(discard-static-infos form1)])
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
                         #`(let ([#,obj-id #,(discard-static-infos form1)])
                             #,@(if add-check
                                    (list (add-check obj-id))
                                    null)
                             #,e)))
       ;; Define `wrap-rator` and `wrap-obj-e` instead of just `rator`
       ;; and `obj-e` so that we can push the relevant operators into
       ;; the body of a `for` that is generated for a repetition.
       ;; In the case that a method is extracted from a vtable, we
       ;; need to have a single `form1` result bound to use in both
       ;; the rator and the added extra rand (that supplies "self").
       (define-values (wrap-call rator wrap-rator wrap-obj-e arity static-infos)
         (cond
           [(identifier? pos/id)
            (define id (maybe-as-original pos/id field-id))
            (cond
              [add-check
               (values checked-wrap-call
                       id
                       (lambda (rator extra-rands) rator)
                       (lambda (extra-rand extra-rands) obj-id)
                       #f
                       #'())]
              [else
               (values (lambda (e extra-rands) e)
                       id
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
          (success #`(curry-method #,pos/id #,(discard-static-infos form1)) new-tail)]
         [else
          (success #`(method-curried-ref #,ref-id #,(discard-static-infos form1) #,pos/id) new-tail)])]))
  (define (make-interface-check desc name)
    (lambda (obj-id)
      #`(void (#,(interface-desc-ref-id desc) #,obj-id))))
  (define (allow-protected?)
    (or do-allow-protected?
        (get-private-table desc
                           #:fail-v #f
                           #:allow-super-for (syntax-e field-id))))
  (cond
    [(and (class-desc? desc)
          (or (for/or ([field+acc (in-list (class-desc-fields desc))])
                (and (eq? (field-desc-name field+acc) (syntax-e field-id))
                     field+acc))
              (and (class-desc-all-fields desc)
                   (for/or ([a-field (in-list (class-desc-all-fields desc))]
                            #:when (protect? a-field))
                     (define fd (protect-v a-field))
                     (and (eq? (car fd) (syntax-e field-id))
                          (allow-protected?)
                          fd)))))
     => (lambda (fld) (do-field fld))]
    [(hash-ref (objects-desc-method-map desc) (syntax-e field-id) #f)
     => (lambda (pos)
          (define shape (vector-ref (objects-desc-method-shapes desc) pos))
          (define shape-ex (if (vector? shape) (vector-ref shape 0) shape))
          (define protected? (protect? shape-ex))
          (define shape-symbol (if (protect? shape-ex) (protect-v shape-ex) shape-ex))
          (define shape-arity (and (vector? shape) (vector-ref shape 1)))
          (define non-final? (or (box? shape-symbol) (and (pair? shape-symbol) (box? (car shape-symbol)))))
          (cond
            [(and protected?
                  (not (allow-protected?)))
             (failure)]
            [else
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
                                         (make-interface-check desc field-id)))]))]
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
