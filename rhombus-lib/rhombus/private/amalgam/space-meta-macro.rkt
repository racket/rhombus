#lang racket/base
(require (for-syntax racket/base
                     racket/symbol
                     syntax/parse/pre
                     "expose.rkt")
         racket/symbol
         syntax/parse/pre
         enforest/property
         enforest/operator
         (only-in (submod enforest/operator
                          for-parse)
                  information-about-bindings)
         enforest/transformer
         enforest/proc-name
         "name-path-op.rkt"
         "srcloc.rkt"
         (for-template "enforest.rkt")
         "introducer.rkt"
         "space-provide.rkt"
         "pack.rkt"
         (submod "syntax-class-primitive.rkt" for-syntax-class)
         (submod "syntax-class-primitive.rkt" for-syntax-class-syntax)
         (submod "syntax-object.rkt" for-quasiquote)
         (for-syntax racket/base)
         "name-root.rkt"
         "name-start-syntax-class.rkt"
         "space.rkt"
         "parse.rkt"
         "forwarding-sequence.rkt"
         "export-check.rkt"
         "space-meta-clause.rkt"
         "define-arity.rkt"
         "call-result-key.rkt"
         (submod "space-meta-clause-primitive.rkt" for-space-meta-macro)
         (submod "namespace.rkt" for-exports)
         "macro-result.rkt"
         (for-template (only-in "space.rkt" space-name))
         (submod "annotation.rkt" for-class)
         "syntax-wrap.rkt"
         "sentinel-declaration.rkt"
         "annotation-failure.rkt"
         "origin.rkt"
         "lookup-space.rkt")

(provide enforest-meta
         transform-meta
         (for-space rhombus/space
                    space_meta_clause)
         (for-space rhombus/annot
                    SpaceMeta))

(define+provide-space space_meta_clause rhombus/space_meta_clause
  #:fields
  ())

(define-syntax (enforest-meta stx)
  (syntax-parse stx
    #:datum-literals (group)
    [(_  name:identifier base-stx scope-stx names clauses)
     #`(rhombus-mixed-nested-forwarding-sequence
        (enforest-meta-finish [#,stx base-stx scope-stx name #t . names]) rhombus-meta-enforest
        (enforest-meta-body-step . clauses))]))

(define-syntax (transform-meta stx)
  (syntax-parse stx
    #:datum-literals (group)
    [(_  name:identifier base-stx scope-stx names clauses)
     #`(rhombus-mixed-nested-forwarding-sequence
        (enforest-meta-finish [#,stx base-stx scope-stx name #f . names]) rhombus-meta-enforest
        (enforest-meta-body-step . clauses))]))

(define-syntax enforest-meta-body-step
  (lambda (stx)
    (syntax-parse stx
      [(_) #'(begin)]
      [(_ . forms)
       ;; Like `class-body-step` in "class-step.rkt"
       (if (ormap (lambda (e) (or (nestable-declaration? e)
                                  (space-meta-clause? e)))
                  (syntax->list #'forms))
           #'(enforest-meta-body-step/to-clause-or-decl . forms)
           #'(quote-syntax (rhombus-meta-enforest (#:post-forms ((rhombus-nested
                                                                  #f
                                                                  #f
                                                                  . forms))))
                           #:local))])))

(define-syntax enforest-meta-body-step/to-clause-or-decl
  (lambda (stx)
    (syntax-parse stx
      [(_ form . rest)
       #:with clause::space-meta-clause (syntax-local-introduce #'form)
       (syntax-parse (syntax-local-introduce #'clause.parsed)
         #:datum-literals (group parsed)
         [((group (parsed #:rhombus/space_meta_clause p)) ...)
          #`(begin p ... (enforest-meta-body-step . rest))]
         [(form ...)
          #`(enforest-meta-body-step form ...  (group sentinel_declaration) . rest)])]
      [(_ form . rest)
       #`(rhombus-top-step
          #,(if (nestable-declaration? #'form)
                #'enforest-meta-body-step
                #'enforest-meta-body-step/to-clause-or-decl)
          #f
          #f
          #f
          ()
          form . rest)]
      [(_) #'(begin)])))

(define-syntax enforest-meta-finish
  (lambda (stx)
    (syntax-parse stx
      [(_ [orig-stx base-stx init-scope-stx
           meta-name enforest?
           name
           space-path-name
           make-prefix-operator make-infix-operator make-prefix+infix-operator
           (extra-kw ...)]
          [#:ctx forward-base-ctx forward-ctx]
          exports
          [option stx-params]
          ...)
       #:with scope-stx ((make-syntax-delta-introducer #'forward-ctx #'forward-base-ctx) #'init-scope-stx)
       (define options (parse-space-meta-clause-options #'orig-stx (syntax-e #'transformer?) #'(option ...)))
       (define class-name (hash-ref options '#:syntax_class #'#f))
       (define class-arguments (hash-ref options '#:syntax_class_arguments #f))
       (when class-arguments
         (unless (= (length class-arguments) (length (syntax->list #'(extra-kw ...))))
           (raise-syntax-error #f "syntax class argument count does not match macro definer's keyword count" class-name)))
       (define prefix-more-class-name (hash-ref options '#:syntax_class_prefix_more #'#f))
       (define infix-more-class-name (hash-ref options '#:syntax_class_infix_more #'#f))
       (define name-start-class-name (hash-ref options '#:syntax_class_name_start #'#f))
       (define space-reflect-name (hash-ref options '#:reflection #'#f))
       (define desc (hash-ref options '#:desc #f))
       (define desc-operator (hash-ref options '#:operator_desc #'"operator"))
       (define parsed-tag (string->keyword (symbol->immutable-string (syntax-e #'space-path-name))))
       (define pack-id (hash-ref options '#:parsed_packer #'#f))
       (define unpack-id (hash-ref options '#:parsed_unpacker #'#f))
       (define pack-and-unpack? (or (syntax-e pack-id) (syntax-e unpack-id)))
       (define make-macro-result (or (let ([e (hash-ref options '#:parsed_checker #f)])
                                       (and e
                                            (syntax-parse #'orig-stx
                                              [(form-name . _)
                                               #`(let ([parse_checker (fill-out-parse-checker-keywords 'form-name #,e)])
                                                   (lambda (name tag recur)
                                                     (lambda (form-in proc reloc origins env)
                                                       (parse_checker form-in
                                                                      #:transform proc
                                                                      #:recur recur
                                                                      #:relocate (lambda (stx)
                                                                                   (transfer-origins
                                                                                    origins
                                                                                    (relocate+reraw-shrubbery reloc stx)))
                                                                      #:syntax_class_arguments env))))])))
                                     (if pack-and-unpack?
                                         #'make-macro-result/recur
                                         #'make-macro-result)))
       (define identifier-transformer (hash-ref options '#:identifier_transformer #`(id-syntax-error #,(if desc
                                                                                                           #'form-desc
                                                                                                           #f))))
       (define private-kws (hash-ref options '#:private #hasheq()))
       (define post-forms (hash-ref options '#:post-forms null))
       (define expose (make-expose #'scope-stx #'base-stx))
       (define exs (parse-exports #'(combine-out . exports) expose))
       (check-distinct-exports (exports->names exs)
                               class-name prefix-more-class-name infix-more-class-name name-start-class-name
                               #'orig-stx)
       (register-field-check #`(base-ctx scope-ctx . #,exs))
       (define (build-pack-and-unpack)
         #`((maybe-skip
             #,pack-id
             (define/arity (#,pack-id stx)
               #:static-infos ((#%call-result (#,(quote-syntax unsyntax) (get-syntax-static-infos))))
               #,(with-syntax ([parsed-tag-kw parsed-tag])
                   #`(let ([t (unpack-term stx '#,pack-id #f)])
                       (relocate+reraw t #`(parsed parsed-tag-kw #,t))))))
            (maybe-skip
             #,unpack-id
             (define/arity (#,unpack-id stx [fail-k #f])
               #:static-infos ((#%call-result (#,(quote-syntax unsyntax) (get-syntax-static-infos))))
               (unpack-parsed '#,parsed-tag stx fail-k)))))
       (with-syntax ([:base-decl (if class-arguments
                                     #`(:base #,@class-arguments)
                                     #':base)]
                     [base-arity-mask (if class-arguments
                                          (arithmetic-shift 1 (length class-arguments))
                                          #f)]
                     [more-arity-mask (if class-arguments
                                          (arithmetic-shift 1 (+ 1 (length class-arguments)))
                                          2)]
                     [unpack-parsed*/tag (if (syntax-e unpack-id)
                                             #`(unpack-parsed* '#,parsed-tag)
                                             #'unpack-term*)])
         (define (build-name-start-syntax-class)
           (if (syntax-e name-start-class-name)
               (with-syntax ([name (quote-syntax name)])
                 #`((define-syntax-class-syntax #,name-start-class-name (make-syntax-class #':name-start
                                                                                           #:auto-args #'(in-new-space)
                                                                                           #:kind 'group
                                                                                           #:fields #'((name name #f 0 unpack-term* stx)
                                                                                                       (head #f head tail unpack-tail-list* stx)
                                                                                                       (tail #f tail tail unpack-tail-list* stx))))))
               null))
         (cond
           [(syntax-e #'enforest?)
            #`(begin
                (define-name-root #,(expose #'meta-name)
                  #:fields
                  (#,@(filter-missing-or-private
                       private-kws
                       #`([#,class-name #,class-name #:syntax_class]
                          [#,prefix-more-class-name #,prefix-more-class-name #:syntax_class_prefix_more]
                          [#,infix-more-class-name #,infix-more-class-name #:syntax_class_infix_more]
                          [#,name-start-class-name #,name-start-class-name #:syntax_class_name_start]
                          [#,space-reflect-name #,space-reflect-name #:reflection]
                          [#,pack-id #,pack-id #:parsed_packer]
                          [#,unpack-id #,unpack-id #:parsed_unpacker]))
                   . #,exs))
                (define in-new-space (make-interned-syntax-introducer/add 'space-path-name))
                #,@(if desc
                       #`((define form-desc #,desc))
                       null)
                (property new-prefix-operator prefix-operator)
                (property new-infix-operator infix-operator)
                (struct new-prefix+infix-operator (prefix infix)
                  #:property prop:new-prefix-operator (lambda (self) (new-prefix+infix-operator-prefix self))
                  #:property prop:new-infix-operator (lambda (self) (new-prefix+infix-operator-infix self)))
                (define-rhombus-enforest
                  #:syntax-class :base-decl
                  #:enforest parse-group
                  #:prefix-more-syntax-class :prefix-more
                  #:infix-more-syntax-class :infix-more
                  #:desc #,(if desc #'form-desc "form")
                  #:operator-desc #,desc-operator
                  #:parsed-tag #,parsed-tag
                  #:in-space in-new-space
                  #:prefix-operator-ref new-prefix-operator-ref
                  #:infix-operator-ref new-infix-operator-ref
                  #:make-identifier-form #,identifier-transformer)
                (maybe-skip
                 #,class-name
                 (define-syntax-class-syntax #,class-name (make-syntax-class #':base
                                                                             #:kind 'group
                                                                             #:arity-mask base-arity-mask
                                                                             #:fields #'((parsed parsed parsed 0 unpack-parsed*/tag stx))
                                                                             #:root-swap '(parsed . group))))
                (maybe-skip
                 #,prefix-more-class-name
                 (define-syntax-class-syntax #,prefix-more-class-name (make-syntax-class #':prefix-more
                                                                                         #:kind 'group
                                                                                         #:fields #'((parsed parsed #f 0 unpack-parsed*/tag stx)
                                                                                                     (tail #f tail tail unpack-tail-list* stx))
                                                                                         #:root-swap '(parsed . group)
                                                                                         #:arity-mask more-arity-mask)))
                (maybe-skip
                 #,infix-more-class-name
                 (define-syntax-class-syntax #,infix-more-class-name (make-syntax-class #':infix-more
                                                                                        #:kind 'group
                                                                                        #:fields #'((parsed parsed #f 0 unpack-parsed*/tag stx)
                                                                                                    (tail #f tail tail unpack-tail-list* stx))
                                                                                        #:root-swap '(parsed . group)
                                                                                        #:arity-mask more-arity-mask)))
                #,@(build-name-start-syntax-class)
                (define macro-result (#,make-macro-result
                                      (quote name)
                                      (quote #,parsed-tag)
                                      (lambda (e env)
                                        (syntax-local-introduce
                                         (apply parse-group (syntax-local-introduce e) env)))))
                (define make-prefix-operator (make-make-prefix-operator new-prefix-operator
                                                                        (quote #,(and pack-and-unpack? parsed-tag))
                                                                        macro-result))
                (define make-infix-operator (make-make-infix-operator new-infix-operator
                                                                      (quote #,(and pack-and-unpack? parsed-tag))
                                                                      macro-result))
                (define make-prefix+infix-operator new-prefix+infix-operator)
                #,@(build-pack-and-unpack)
                (maybe-skip
                 #,space-reflect-name
                 (define #,space-reflect-name (space-name 'space-path-name)))
                #,@post-forms)]
           [else
            #`(begin
                (define-name-root #,(expose #'meta-name)
                  #:fields
                  #,(filter-missing-or-private
                     private-kws
                     #`([#,class-name #,class-name #:syntax_class]
                        [#,space-reflect-name #,space-reflect-name #:reflection]
                        [#,name-start-class-name #,name-start-class-name #:syntax_class_name_start]
                        . #,exs)))
                (define in-new-space (make-interned-syntax-introducer/add 'space-path-name))
                (maybe-skip
                 #,class-name
                 (property new-transformer transformer))
                (maybe-skip
                 #,class-name
                 (define-rhombus-transform
                   #:syntax-class :base-decl
                   #:transform parse-group
                   #:desc #,desc
                   #:parsed-tag #,parsed-tag
                   #:in-space in-new-space
                   #:transformer-ref new-transformer-ref))
                (maybe-skip
                 #,class-name
                 (define-syntax-class-syntax #,class-name (make-syntax-class #':base
                                                                             #:kind 'group
                                                                             #:fields #'((parsed parsed parsed 0 unpack-term* stx))
                                                                             #:root-swap '(parsed . group)
                                                                             #:arity-mask base-arity-mask)))
                #,@(build-name-start-syntax-class)
                (maybe-skip
                 #,class-name
                 (define macro-result (#,make-macro-result
                                       (quote name)
                                       (quote #,parsed-tag)
                                       (lambda (e env)
                                         (syntax-local-introduce
                                          (apply parse-group (syntax-local-introduce e) env))))))
                (maybe-skip
                 #,class-name
                 (define make-prefix-operator (make-make-transformer 'name new-transformer macro-result)))
                #,@(build-pack-and-unpack)
                (maybe-skip
                 #,space-reflect-name
                 (define #,space-reflect-name (space-name 'space-path-name)))
                #,@post-forms)]))])))

(define-for-syntax (filter-missing-or-private private-kws flds)
  (for/list ([fld (in-list (syntax->list flds))]
             #:when (syntax-parse fld
                      [[#f . _] #f]
                      [[_ _ kw]
                       #:when (hash-ref private-kws (syntax-e #'kw) #f)
                       #f]
                      [_ #t]))
    (syntax-parse fld
      [[a b . _] #'[a b]])))

(define (tag form parsed-tag)
  ;; input to macro might or might not be tagged; it's not tagged when
  ;; the enforest engine found a `parsed` term
  (if parsed-tag
      (syntax-parse form
        #:datum-literals (parsed)
        [(parsed tag _)
         #:when (eq? (syntax-e #'tag) parsed-tag)
         form]
        [_
         (datum->syntax #f (list 'parsed parsed-tag form))])
      form))

(define ((make-make-prefix-operator new-prefix-operator parsed-tag macro-result) order prec protocol proc)
  (new-prefix-operator
   order
   prec
   protocol
   (cond
     [(eq? protocol 'automatic)
      (procedure-rename
       (lambda (form stx . more)
         (define result (apply proc (tag form parsed-tag) stx more))
         (macro-result result
                       proc
                       (datum->syntax #f (list stx form))
                       (list form)
                       more))
       (object-name proc))]
     [else
      (procedure-rename
       (lambda (tail . more)
         (finish (lambda () (syntax-parse tail
                              [(head . tail)
                               (apply proc (pack-tail #'tail #:after #'head) #'head more)]))
                 proc
                 macro-result
                 tail
                 null
                 more))
       (object-name proc))])))

(define ((make-make-infix-operator new-infix-operator parsed-tag macro-result) order prec protocol proc assc)
  (new-infix-operator
   order
   prec
   protocol
   (cond
     [(eq? protocol 'automatic)
      (procedure-rename
       (lambda (form1 form2 stx . more)
         (define result (apply proc (tag form1 parsed-tag) (tag form2 parsed-tag) stx more))
         (macro-result result
                       proc
                       (datum->syntax #f (list form1 stx form2))
                       (list form1 form2)
                       more))
       (object-name proc))]
     [else
      (procedure-rename
       (lambda (form1 tail . more)
         (finish
          (lambda () (syntax-parse tail
                       [(head . tail) (apply proc (tag form1 parsed-tag) (pack-tail #'tail #:after #'head) #'head more)]))
          proc
          macro-result
          (cons form1 tail)
          (list form1)
          more))
       (object-name proc))])
   assc))

(define ((make-make-transformer name new-transformer macro-result) proc)
  (new-transformer
   (lambda (stx . more)
     (syntax-parse stx
       [(head . tail)
        (macro-result (apply proc (pack-tail #'tail) #'head more)
                      proc
                      #f
                      null
                      more)]))))

(define (finish thunk proc macro-result reloc-stx origins more)
  (define-values (form new-tail reloc)
    (call-with-values
     thunk
     (case-lambda
       [(form new-tail) (values form new-tail #f)]
       [(form) (values form #'(group) (datum->syntax #f reloc-stx))])))
  (values (macro-result form
                        proc
                        reloc
                        origins
                        more)
          (unpack-tail new-tail proc #f)))

(define (make-macro-result name parsed-tag recur)
  (make-macro-result/recur name parsed-tag #f))

(define ((make-macro-result/recur name parsed-tag recur) form-in proc reloc origins env)
  (define form (syntax-unwrap form-in))
  (unless (syntax? form)
    (raise-bad-macro-result (proc-name proc) (symbol->immutable-string name) form-in))
  (define result
    (if recur
        (syntax-parse form
          #:datum-literals (parsed)
          [(parsed tag e)
           #:when (eq? (syntax-e #'tag) parsed-tag)
           #'e]
          [_
           (cond
             [(unpack-tail form #f #f)
              => (lambda (g) (recur g env))]
             [else
              (raise-bad-macro-result (proc-name proc) "single-group syntax object" #:syntax-for? #f form)])])
        form))
  ;; unlike expressions, we need to keep lifting origin information out, so that's
  ;; why `transfer-origins` is here while it's not in "expr-macro.rkt"
  (transfer-origins
   origins
   (relocate+reraw-shrubbery (or reloc form) result)))

(define (fill-out-parse-checker-keywords who proc)
  (define-values (reqds allowed) (if (procedure? proc)
                                     (procedure-keywords proc)
                                     (values null null)))
  (define avail '(#:transform #:recur #:relocate #:syntax_class_arguments))
  (unless (and (procedure? proc)
               (procedure-arity-includes? proc 1 #t)
               (for/and ([req (in-list reqds)])
                 (memq req avail)))
    (raise-annotation-failure who
                              proc
                              (string-append "satisfying(fun (f :: Function):\n"
                                             "             let (_, [req, ...], _) = Function.arity(f)\n"
                                             "             let avail = { #'~transform, #'~recur, #'~relocate, #'~syntax_class_arguments }\n"
                                             "             {req, ...}.subtract(avail) == {})")))
  (lambda (form #:transform transform
                #:recur recur-in
                #:relocate relocate
                #:syntax_class_arguments syntax_class_arguments)
    (define recur (lambda (e [env '()])
                    (let ([e (unpack-tail (unpack-group e who e) #f #f)])
                      (recur-in e env))))
    (define-values (kws kw-args)
      (for/lists (kws kw-args) ([kw (in-list avail)]
                                [arg (in-list (list transform recur relocate syntax_class_arguments))]
                                #:when (or (not allowed) (memq kw allowed)))
        (values kw arg)))
    (keyword-apply proc kws kw-args form null)))

(define-for-syntax (check-distinct-exports ex-ht
                                           class-name prefix-more-class-name infix-more-class-name name-start-class-name
                                           orig-stx)
  (define (check id what)
    (when (and (syntax-e id)
               (hash-ref ex-ht (syntax-e id) #f))
      (raise-syntax-error #f
                          (string-append "exported name conflicts with exported " what)
                          orig-stx
                          (hash-ref ex-ht (syntax-e id) #f))))
  (check class-name "syntax class name")
  (check prefix-more-class-name "prefix-more syntax class name")
  (check infix-more-class-name "infix-more syntax class name")
  (check name-start-class-name "name-start syntax class name"))

(define (id-syntax-error desc)
  (lambda (id)
    (raise-syntax-error #f
                        (if desc
                            (string-append "unbound " desc " identifier")
                            "unbound identifier")
                        id
                        #f
                        null
                        (information-about-bindings id lookup-space-description))))

(define-syntax (maybe-skip stx)
  (syntax-parse stx
    [(_ #f . _) #'(begin)]
    [(_ _ def) #'def]))

(define-annotation-syntax SpaceMeta
  (identifier-annotation space-name? ()))

(define (unpack-parsed tag stx [fail-k #f])
  (syntax-parse (unpack-term stx #f #f)
    #:datum-literals (parsed)
    [(parsed kw:keyword form)
     #:when (eq? (syntax-e #'kw) tag)
     #'form]
    [_ #:when fail-k (fail-k stx)]))
