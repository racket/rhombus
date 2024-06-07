#lang racket/base
(require (for-syntax racket/base
                     racket/symbol
                     syntax/parse/pre
                     "expose.rkt")
         racket/symbol
         syntax/parse/pre
         enforest/property
         enforest/operator
         enforest/transformer
         enforest/proc-name
         (for-template "enforest.rkt")
         "introducer.rkt"
         "space-provide.rkt"
         "pack.rkt"
         (submod "syntax-class-primitive.rkt" for-syntax-class)
         (submod "syntax-object.rkt" for-quasiquote)
         (for-syntax racket/base)
         "implicit.rkt" ;; needed for `#%body`
         "name-root.rkt"
         "space.rkt"
         "parse.rkt"
         "forwarding-sequence.rkt"
         "space-meta-clause.rkt"
         "define-arity.rkt"
         "call-result-key.rkt"
         (submod "space-meta-clause-primitive.rkt" for-space-meta-macro)
         (submod "namespace.rkt" for-exports)
         "macro-result.rkt"
         (for-template (only-in "space.rkt" space-name))
         (submod "annotation.rkt" for-class))

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
      [(_ form . rest)
       #:with clause::space-meta-clause (syntax-local-introduce #'form)
       (syntax-parse (syntax-local-introduce #'clause.parsed)
         #:datum-literals (group parsed)
         [((group (parsed #:rhombus/space_meta_clause p)) ...)
          #`(begin p ... (enforest-meta-body-step . rest))]
         [(form ...)
          #`(enforest-meta-body-step form ... . rest)])]
      [(_ form . rest)
       #`(rhombus-top-step
          enforest-meta-body-step
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
       (define space-reflect-name (hash-ref options '#:reflection #'#f))
       (define desc (hash-ref options '#:desc #'"form"))
       (define desc-operator (hash-ref options '#:operator_desc #'"operator"))
       (define parsed-tag (string->keyword (symbol->immutable-string (syntax-e #'space-path-name))))
       (define pack-id (hash-ref options '#:parsed_packer #'#f))
       (define unpack-id (hash-ref options '#:parsed_unpacker #'#f))
       (define pack-and-unpack? (or (syntax-e pack-id) (syntax-e unpack-id)))
       (define macro-result (hash-ref options '#:parsed_checker
                                      #`(make-check-syntax (quote name)
                                                           (quote #,parsed-tag)
                                                           #,(and pack-and-unpack?
                                                                  #`(lambda (e env) (apply parse-group e env))))))
       (define identifier-transformer (hash-ref options '#:identifier_transformer #'values))
       (define expose (make-expose #'scope-stx #'base-stx))
       (define exs (parse-exports #'(combine-out . exports) expose))
       (check-distinct-exports (exports->names exs)
                               class-name prefix-more-class-name infix-more-class-name
                               #'orig-stx)
       (define (build-pack-and-unpack)
         #`((maybe-skip
             #,pack-id
             (define/arity (#,pack-id stx)
               #:static-infos ((#%call-result (#,(quote-syntax unsyntax) (get-syntax-static-infos))))
               #,(with-syntax ([parsed-tag-kw parsed-tag])
                   #`#`(parsed parsed-tag-kw #,(unpack-term stx '#,pack-id #f)))))
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
                                          2)])
         (cond
           [(syntax-e #'enforest?)
            #`(begin
                (define-name-root #,(expose #'meta-name)
                  #:fields
                  (#,@(filter-missing
                       #`([#,class-name _class-name]
                          [#,prefix-more-class-name _prefix-more-class-name]
                          [#,infix-more-class-name _infix-more-class-name]
                          [#,space-reflect-name _space]
                          [#,pack-id #,pack-id]
                          [#,unpack-id #,unpack-id]))
                   . #,exs))
                (define in-new-space (make-interned-syntax-introducer/add 'space-path-name))
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
                  #:desc #,desc
                  #:operator-desc #,desc-operator
                  #:parsed-tag #,parsed-tag
                  #:in-space in-new-space
                  #:prefix-operator-ref new-prefix-operator-ref
                  #:infix-operator-ref new-infix-operator-ref
                  #:check-result #,macro-result
                  #:make-identifier-form #,identifier-transformer)
                (define-syntax _class-name (make-syntax-class #':base
                                                              #:kind 'group
                                                              #:arity base-arity-mask
                                                              #:fields #'((parsed parsed parsed 0 unpack-term*))
                                                              #:root-swap '(parsed . group)))
                (define-syntax _prefix-more-class-name (make-syntax-class #':prefix-more
                                                                          #:kind 'group
                                                                          #:fields #'((parsed parsed #f 0 unpack-term*)
                                                                                      (tail #f tail tail unpack-tail-list*))
                                                                          #:root-swap '(parsed . group)
                                                                          #:arity more-arity-mask))
                (define-syntax _infix-more-class-name (make-syntax-class #':infix-more
                                                                         #:kind 'group
                                                                         #:fields #'((parsed parsed #f 0 unpack-term*)
                                                                                     (tail #f tail tail unpack-tail-list*))
                                                                         #:root-swap '(parsed . group)
                                                                         #:arity more-arity-mask))
                (define make-prefix-operator (make-make-prefix-operator new-prefix-operator
                                                                        (quote #,(and pack-and-unpack? parsed-tag))))
                (define make-infix-operator (make-make-infix-operator new-infix-operator
                                                                      (quote #,(and pack-and-unpack? parsed-tag))))
                (define make-prefix+infix-operator new-prefix+infix-operator)
                #,@(build-pack-and-unpack)
                (maybe-skip
                 #,space-reflect-name
                 (define _space (space-name 'space-path-name))))]
           [else
            #`(begin
                (define-name-root #,(expose #'meta-name)
                  #:fields
                  #,(filter-missing
                     #`([#,class-name _class-name]
                        [#,space-reflect-name _space]
                        . #,exs)))
                (define in-new-space (make-interned-syntax-introducer/add 'space-path-name))
                (maybe-skip
                 class-name
                 (property new-transformer transformer))
                (maybe-skip
                 class-name
                 (define-rhombus-transform
                   #:syntax-class :base-decl
                   #:transform parse-group
                   #:desc #,desc
                   #:parsed-tag #,parsed-tag
                   #:in-space in-new-space
                   #:transformer-ref new-transformer-ref
                   #:check-result #,macro-result))
                (maybe-skip
                 class-name
                 (define-syntax _class-name (make-syntax-class #':base
                                                               #:kind 'group
                                                               #:fields #'((parsed parsed parsed 0 unpack-term*))
                                                               #:root-swap '(parsed . group)
                                                               #:arity base-arity-mask)))
                (maybe-skip
                 class-name
                 (define make-prefix-operator (make-make-transformer 'name new-transformer)))
                #,@(build-pack-and-unpack)
                (maybe-skip
                 #,space-reflect-name
                 (define _space (space-name 'space-path-name))))]))])))

(define-for-syntax (filter-missing flds)
  (for/list ([fld (in-list (syntax->list flds))]
             #:when (syntax-parse fld
                      [[#f . _] #f]
                      [_ #t]))
    fld))

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

(define ((make-make-prefix-operator new-prefix-operator parsed-tag) prec protocol proc)
  (new-prefix-operator
   prec
   protocol
   (cond
     [(eq? protocol 'automatic)
      (if parsed-tag
          (procedure-rename
           (lambda (form stx . more) (apply proc (tag form parsed-tag) stx more))
           (object-name proc))
          proc)]
     [else
      (procedure-rename
       (lambda (tail . more)
         (finish (lambda () (syntax-parse tail
                              [(head . tail) (apply proc (pack-tail #'tail #:after #'head) #'head more)]))
                 proc))
       (object-name proc))])))

(define ((make-make-infix-operator new-infix-operator parsed-tag) prec protocol proc assc)
  (new-infix-operator
   prec
   protocol
   (cond
     [(eq? protocol 'automatic)
      (if parsed-tag
          (procedure-rename
           (lambda (form1 form2 stx . more) (apply proc (tag form1 parsed-tag) (tag form2 parsed-tag) stx more))
           (object-name proc))
          proc)]
     [else
      (procedure-rename
       (lambda (form1 tail . more)
         (finish
          (lambda () (syntax-parse tail
                       [(head . tail) (apply proc (tag form1 parsed-tag) (pack-tail #'tail #:after #'head) #'head more)]))
          proc))
       (object-name proc))])
   assc))

(define ((make-make-transformer name new-transformer) proc)
  (new-transformer
   (lambda (stx . more)
     (syntax-parse stx
       [(head . tail) (apply proc (pack-tail #'tail) #'head more)]))))

(define (finish thunk proc)
  (define-values (form new-tail)
    (call-with-values
     thunk
     (case-lambda
       [(form new-tail) (values form new-tail)]
       [(form) (values form #'(group))])))
  (values form
          (unpack-tail new-tail proc #f)))

(define ((make-check-syntax name parsed-tag recur) form proc . env)
  (unless (syntax? form)
    (raise-bad-macro-result (proc-name proc) (symbol->immutable-string name) form))
  (if recur
      (syntax-parse form
        #:datum-literals (parsed)
        [(parsed tag e)
         #:when (eq? (syntax-e #'tag) parsed-tag)
         form]
        [_
         (cond
           [(unpack-tail form #f #f)
            => (lambda (g) (recur g env))]
           [else
            (raise-bad-macro-result (proc-name proc) (symbol->immutable-string name) form)])])
      form))

(define-for-syntax (check-distinct-exports ex-ht
                                           class-name prefix-more-class-name infix-more-class-name
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
  (check infix-more-class-name "infix-more syntax class name"))

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
