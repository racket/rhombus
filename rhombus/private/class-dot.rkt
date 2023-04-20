#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/syntax-local
                     shrubbery/property
                     "srcloc.rkt"
                     "tag.rkt"
                     "class-parse.rkt"
                     "interface-parse.rkt"
                     "statically-str.rkt")
         "class-method.rkt"
         "class-method-result.rkt"
         (submod "dot.rkt" for-dot-provider)
         "parens.rkt"
         "static-info.rkt"
         "expression.rkt"
         (only-in (submod "expr-macro.rkt" for-define)
                  make-expression-prefix-operator)
         (only-in "repetition.rkt"
                  in-repetition-space
                  repetition-transformer
                  identifier-repetition-use)
         "assign.rkt"
         "op-literal.rkt"
         "name-root.rkt"
         "parse.rkt"
         (submod "function-parse.rkt" for-call)
         (for-syntax "class-transformer.rkt")
         "class-dot-transformer.rkt"
         "is-static.rkt"
         "realm.rkt")

(provide (for-syntax build-class-dot-handling
                     build-interface-dot-handling

                     make-handle-class-instance-dot

                     extract-all-dot-names)
         no-dynamic-dot-syntax)

(define-for-syntax (build-class-dot-handling method-mindex method-vtable method-results final?
                                             has-private? method-private exposed-internal-id internal-of-id
                                             expression-macro-rhs intro constructor-given-name
                                             export-of? dot-provider-rhss parent-dot-providers
                                             names)
  (with-syntax ([(name name? constructor-name name-instance name-ref name-of
                       make-internal-name internal-name-instance dot-provider-name
                       [public-field-name ...] [private-field-name ...] [field-name ...]
                       [public-name-field ...] [name-field ...]
                       [dot-id ...]
                       [private-field-desc ...]
                       [ex ...])
                 names])
    (define-values (method-names method-impl-ids method-defns)
      (method-static-entries method-mindex method-vtable method-results #'name-ref final?))
    (with-syntax ([(method-name ...) method-names]
                  [(method-id ...) method-impl-ids]
                  [(dot-rhs ...) dot-provider-rhss]
                  [(dot-rhs-id ...) (map (make-syntax-introducer) (syntax->list #'(dot-id ...)))]
                  [(dot-class-id ...) (map (make-syntax-introducer) (syntax->list #'(dot-id ...)))])
      (append
       method-defns
       (append
        (list
         (cond
           [expression-macro-rhs
            #`(define-syntax name
                (wrap-class-transformer name
                                        #,(intro expression-macro-rhs)
                                        make-expression-prefix-operator
                                        "class"))]
           [(and constructor-given-name
                 (not (free-identifier=? #'name constructor-given-name)))
            #`(define-syntax name
                no-constructor-transformer)]
           [else
            #`(define-syntaxes (name #,(in-repetition-space #'name))
                (class-expression-transformers (quote-syntax name) (quote-syntax constructor-name)))])
         #`(define-name-root name
             #:fields ([public-field-name public-name-field]
                       ...
                       [method-name method-id]
                       ...
                       [dot-id dot-class-id]
                       ...
                       ex ...
                       #,@(if export-of?
                              #`([of name-of])
                              null))))
        (syntax->list
         #`((define-syntaxes (dot-class-id dot-rhs-id)
              (let ([dot-id dot-rhs])
                (values (wrap-class-dot-via-class dot-id (quote-syntax dot-id)
                                                  (quote-syntax name?) (quote-syntax name-instance))
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
        (if exposed-internal-id
            (with-syntax ([([private-method-name private-method-id private-method-id/prop] ...)
                           (for/list ([(sym id/prop) (in-hash method-private)])
                             (define id (if (pair? id/prop) (car id/prop) id/prop))
                             (list sym id id/prop))])
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
                             [of #,internal-of-id]))
               #`(define-dot-provider-syntax internal-name-instance
                   (dot-provider (make-handle-class-instance-dot (quote-syntax name)
                                                                 (hasheq
                                                                  (~@ 'private-field-name
                                                                      private-field-desc)
                                                                  ...)
                                                                 (hasheq
                                                                  (~@ 'private-method-name
                                                                      (quote-syntax private-method-id/prop))
                                                                  ...))))))
            null))))))

(define-for-syntax (build-interface-dot-handling method-mindex method-vtable method-results 
                                                 internal-name
                                                 expression-macro-rhs
                                                 dot-provider-rhss parent-dot-providers
                                                 names)
  (with-syntax ([(name name? name-instance name-ref
                       internal-name-instance internal-name-ref
                       dot-provider-name [dot-id ...]
                       [ex ...])
                 names])
    (define-values (method-names method-impl-ids method-defns)
      (method-static-entries method-mindex method-vtable method-results #'name-ref #f))
    (with-syntax ([(method-name ...) method-names]
                  [(method-id ...) method-impl-ids]
                  [(dot-rhs ...) dot-provider-rhss]
                  [(dot-rhs-id ...) (map (make-syntax-introducer) (syntax->list #'(dot-id ...)))]
                  [(dot-intf-id ...) (map (make-syntax-introducer) (syntax->list #'(dot-id ...)))])
      (append
       method-defns
       (list
        #`(define-syntax name
            #,(cond
                [expression-macro-rhs
                 #`(wrap-class-transformer name
                                           #,((make-syntax-introducer) expression-macro-rhs)
                                           make-expression-prefix-operator
                                           "interface")]
                [else #'no-constructor-transformer]))
        #`(define-name-root name
            #:fields ([method-name method-id]
                      ...
                      [dot-id dot-intf-id]
                      ...
                      ex ...)))
        (syntax->list
         #`((define-syntaxes (dot-intf-id dot-rhs-id)
              (let ([dot-id dot-rhs])
                (values (wrap-class-dot-via-class dot-id (quote-syntax dot-id)
                                                  (quote-syntax name?) (quote-syntax name-instance))
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
               ((length parent-dot-providers) . > . 1)))
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

(define-for-syntax (method-static-entries method-mindex method-vtable method-results name-ref-id final?)
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
                                                            (quote-syntax #,stx-id)
                                                            (quote-syntax #,name-ref-id)
                                                            #,(mindex-index mix)
                                                            (quote-syntax #,proc-id)
                                                            #,(let ([ids (hash-ref method-results name #f)])
                                                                (and ids
                                                                     #`(quote-syntax #,(car ids))))))
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

(define-for-syntax (make-method-accessor-transformer name id name-ref-id idx proc-id ret-info-id)
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (op)
       [(rator-id (~and args (tag::parens self arg ...)) . tail)
        (define obj-id #'this)
        (define rator #`(method-ref #,name-ref-id #,obj-id #,idx))
        (define r (and ret-info-id
                       (syntax-local-method-result ret-info-id)))
        (define-values (call new-tail)
          (parse-function-call rator (list obj-id) #`(#,obj-id (tag arg ...))
                               #:static? (is-static-call-context? #'tag)
                               #:rator-stx (datum->syntax #f name #'rator-id)
                               #:rator-arity (and r (method-result-arity r))))
        (values (let ([call #`(let ([#,obj-id (rhombus-expression self)])
                                #,call)])
                  (if r
                      (wrap-static-info* call (method-result-static-infos r))
                      call))
                #'tail)]
       [(head (tag::parens) . _)
        #:when (is-static-call-context? #'tag)
        (raise-syntax-error #f
                            (string-append "wrong number of arguments in function call" statically-str)
                            (datum->syntax #f name #'head))]
       [(_ . tail)
        (values proc-id #'tail)]))))

(define-for-syntax (class-expression-transformers id make-id)
  (values
   (expression-transformer
    (lambda (stx)
      (syntax-parse stx
        [(head . tail) (values (relocate-id #'head make-id)
                               #'tail)])))
   (repetition-transformer
    (lambda (stx)
      (syntax-parse stx
        [(head . tail) (values (identifier-repetition-use (relocate-id #'head make-id))
                               #'tail)])))))

(define-for-syntax (desc-method-shapes desc)
  (if (class-desc? desc)
      (class-desc-method-shapes desc)
      (interface-desc-method-shapes desc)))

(define-for-syntax (desc-method-vtable desc)
  (if (class-desc? desc)
      (class-desc-method-vtable desc)
      (interface-desc-method-vtable desc)))

(define-for-syntax (desc-method-map desc)
  (if (class-desc? desc)
      (class-desc-method-map desc)
      (interface-desc-method-map desc)))

(define-for-syntax (desc-method-result desc)
  (if (class-desc? desc)
      (class-desc-method-result desc)
      (interface-desc-method-result desc)))

(define-for-syntax (desc-ref-id desc)
  (if (class-desc? desc)
      (class-desc-ref-id desc)
      (interface-desc-ref-id desc)))

;; dot provider for a class instance used before a `.`
(define-for-syntax ((make-handle-class-instance-dot name internal-fields internal-methods)
                    form1 dot field-id
                    tail more-static?
                    success failure)
  (define desc (syntax-local-value* (in-class-desc-space name) (lambda (v)
                                                                 (or (class-desc-ref v)
                                                                     (interface-desc-ref v)))))
  (unless desc (error "cannot find annotation binding for instance dot provider"))
  (define (do-field fld)
    (define accessor-id (field-desc-accessor-id fld))
    (define-values (op-id assign-rhs new-tail)
      (syntax-parse tail
        [(_:::=-expr . tail)
         #:when (syntax-e (field-desc-mutator-id fld))
         #:with (~var e (:infix-op+expression+tail #':=)) #'(group . tail)
         (values (field-desc-mutator-id fld)
                 #'e.parsed
                 #'e.tail)]
        [_
         (values accessor-id
                 #f
                 tail)]))
    (define e (datum->syntax (quote-syntax here)
                             (append (list (relocate field-id op-id) form1)
                                     (if assign-rhs
                                         (list field-id)
                                         null))
                             (span-srcloc form1 field-id)
                             #'dot))
    (define full-e
      (cond
        [assign-rhs #`(let ([#,field-id #,assign-rhs])
                        #,e)]
        [else e]))
    
    (define static-infos (field-desc-static-infos fld))
    (define more-static-infos (syntax-local-static-info form1 accessor-id))
    (define all-static-infos (if more-static-infos
                                 (datum->syntax #f
                                                (append (syntax->list more-static-infos)
                                                        static-infos))
                                 static-infos))
    (success (wrap-static-info* full-e all-static-infos)
             new-tail))
  (define (do-method pos/id* ret-info-id nonfinal? property? shape-arity)
    (define-values (args new-tail)
      (if property?
          (syntax-parse tail
            [(_:::=-expr . tail)
             #:with (~var e (:infix-op+expression+tail #':=)) #'(group  . tail)
             (values #`(#,(no-srcloc #'parens) (group (parsed e.parsed)))
                     #'e.tail)]
            [_ (values #'(parens) tail)])
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
        [else (vector-ref (syntax-e (desc-method-vtable desc)) pos/id*)]))
    (cond
      [args
       (define-values (rator obj-e arity wrap)
         (cond
           [(identifier? pos/id)
            (values pos/id form1 #f (lambda (e) e))]
           [else
            (define obj-id #'obj)
            (define r (and ret-info-id
                           (syntax-local-method-result ret-info-id)))
            (define static-infos
              (or (and ret-info-id
                       (method-result-static-infos r))
                  #'()))
            (values #`(method-ref #,(desc-ref-id desc) #,obj-id #,pos/id)
                    obj-id
                    (or shape-arity
                        (and r (method-result-arity r)))
                    (lambda (e)
                      (define call-e #`(let ([#,obj-id #,form1])
                                         #,e))
                      (if (pair? (syntax-e static-infos))
                          (wrap-static-info* call-e static-infos)
                          call-e)))]))
       (define-values (call-stx empty-tail)
         (parse-function-call rator (list obj-e) #`(#,obj-e #,args)
                              #:rator-stx field-id
                              #:static? more-static?
                              #:rator-arity arity
                              #:rator-kind (if property? 'property 'method)))
       (success (wrap call-stx)
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
          (success #`(method-curried-ref #,(desc-ref-id desc) #,form1 #,pos/id) new-tail)])]))
  (cond
    [(and (class-desc? desc)
          (for/or ([field+acc (in-list (class-desc-fields desc))])
            (and (eq? (field-desc-name field+acc) (syntax-e field-id))
                 field+acc)))
     => (lambda (fld) (do-field fld))]
    [(hash-ref (desc-method-map desc) (syntax-e field-id) #f)
     => (lambda (pos)
          (define shape (vector-ref (desc-method-shapes desc) pos))
          (define shape-symbol (and shape (if (vector? shape) (vector-ref shape 0) shape)))
          (define shape-arity (and shape (vector? shape) (vector-ref shape 1)))
          (do-method pos
                     (hash-ref (desc-method-result desc) (syntax-e field-id) #f)
                     ;; non-final?
                     (or (box? shape-symbol) (and (pair? shape-symbol) (box? (car shape-symbol))))
                     ;; property?
                     (pair? shape-symbol)
                     shape-arity))]
    [(hash-ref internal-fields (syntax-e field-id) #f)
     => (lambda (fld) (do-field fld))]
    [(hash-ref internal-methods (syntax-e field-id) #f)
     => (lambda (id/property)
          (define id (if (identifier? id/property)
                         id/property
                         (car (syntax-e id/property))))
          (define property? (pair? (syntax-e id/property)))
          (do-method id #f #f property? #f))]
    [(hash-ref (get-private-table desc) (syntax-e field-id) #f)
     => (lambda (id/fld)
          (if (identifier? id/fld)
              (do-method id/fld #f #f #f #f)
              (do-field id/fld)))]
    [more-static?
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
     (if (class-desc? super)
         (class-desc-dots super)
         (interface-desc-dots super)))))
