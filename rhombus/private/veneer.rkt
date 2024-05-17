#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/syntax-local
                     shrubbery/print
                     "class-parse.rkt"
                     "veneer-parse.rkt"
                     (submod "veneer-meta.rkt" for-class)
                     "interface-parse.rkt"
                     "srcloc.rkt"
                     "tag.rkt"
                     "annotation-string.rkt")
         "provide.rkt"
         "forwarding-sequence.rkt"
         "definition.rkt"
         (submod "annotation.rkt" for-class)
         "static-info.rkt"
         "binding.rkt"
         "veneer-clause.rkt"
         "class-clause-parse.rkt"
         "class-clause-tag.rkt"
         "class-step.rkt"
         "class-dot.rkt"
         "class-static-info.rkt"
         "class-method.rkt"
         "class-top-level.rkt"
         "dotted-sequence-parse.rkt"
         "dot-provider-key.rkt"
         "parens.rkt"
         (submod "namespace.rkt" for-exports)
         "class-able.rkt"
         "if-blocked.rkt")

(provide (for-spaces (rhombus/defn)
                     veneer))

(define-defn-syntax veneer
  (definition-transformer
    (lambda (stxes)
      (syntax-parse stxes
        #:datum-literals (group block)
        [(_ name-seq::dotted-identifier-sequence (tag::parens (group (~literal this) ann-op::annotate-op ann-term ...+))
            options::options-block)
         #:with full-name::dotted-identifier #'name-seq
         #:with name #'full-name.name
         #:with name-extends #'full-name.extends
         #:with tail-name #'full-name.tail-name
         #:with orig-stx stxes
         (define body #'(options.form ...))
         (define intro (make-syntax-introducer #t))
         ;; The shape of `finish-data` is recognzied in `veneer-annotation+finish`
         ;; and "veneer-meta.rkt"
         (define finish-data #`([orig-stx base-stx #,(intro #'scope-stx)
                                          name name-extends tail-name
                                          #,(attribute ann-op.check?) ann-op.name (ann-term ...)]
                                ;; data accumulated from parsed clauses:
                                ()))
         #`(#,(cond
                [(null? (syntax-e body))
                 #`(veneer-annotation+finish #,finish-data [#:ctx base-stx base-stx] ())]
                [else
                 #`(rhombus-mixed-nested-forwarding-sequence
                    (veneer-annotation+finish #,finish-data) rhombus-class
                    (veneer-body-step #,finish-data . #,(intro body)))]))]))))

(define-class-body-step veneer-body-step
  :veneer-clause
  veneer-expand-data
  class-clause-accum)

;; First phase of `veneer` output: bind the annotation form, so it can be used
;; in body fields
(define-syntax veneer-annotation+finish
  (lambda (stx)
    (syntax-parse stx
      [(_ ([orig-stx base-stx init-scope-stx
                     name name-extends tail-name
                     check? ann-op-name ann-terms]
           . _)
          [#:ctx forward-base-ctx forward-ctx]
          exports
          [option stx-params] ...)
       #:with scope-stx ((make-syntax-delta-introducer #'forward-ctx #'forward-base-ctx) #'init-scope-stx)
       (define options (parse-annotation-options #'orig-stx #'(option ...) #'(stx-params ...)))
       (define parent-name (hash-ref options 'extends #f))
       (define super (and parent-name
                          (or (syntax-local-value* (in-class-desc-space parent-name) veneer-desc-ref)
                              (raise-syntax-error #f "not a veneer name" #'orig-stx parent-name))))

       (define interface-names (reverse (hash-ref options 'implements '())))
       (define interfaces (interface-names->interfaces #'orig-stx interface-names
                                                       #:for-veneer? #t))
       (define private-interfaces (interface-set-diff
                                   (interface-names->interfaces #'orig-stx (hash-ref options 'private-implements '()))
                                   (interface-names->interfaces #'orig-stx (hash-ref options 'public-implements '()))))

       (define annotation-rhs (hash-ref options 'annotation-rhs #f))
       (define expression-macro-rhs (hash-ref options 'expression-rhs #f))

       (define intro (make-syntax-introducer))

       (define converter? (or (hash-ref options 'converter? #f)
                              (and super
                                   (veneer-desc-convert-id super))))

       (define-values (call-statinfo-indirect-id
                       index-statinfo-indirect-id
                       index-set-statinfo-indirect-id
                       append-statinfo-indirect-id
                       compare-statinfo-indirect-id

                       super-call-statinfo-indirect-id

                       static-infos-id
                       static-infos-exprs
                       instance-static-infos

                       indirect-static-infos
                       internal-indirect-static-infos)
         (extract-instance-static-infoss #'name options super interfaces private-interfaces intro))

       (with-syntax ([name-instance (intro (datum->syntax #'name (string->symbol (format "~a.instance" (syntax-e #'name))) #'name))]
                     [internal-name-instance #f]
                     [name? (datum->syntax #'name (string->symbol (format "~a?" (syntax-e #'name))) #'name)]
                     [name-convert (and converter?
                                        (datum->syntax #'name (string->symbol (format "~a-convert" (syntax-e #'name))) #'name))]
                     [name-of (intro (datum->syntax #'name (string->symbol (format "~a-of" (syntax-e #'name))) #'name))]
                     [call-statinfo-indirect call-statinfo-indirect-id]
                     [index-statinfo-indirect index-statinfo-indirect-id]
                     [index-set-statinfo-indirect index-set-statinfo-indirect-id]
                     [append-statinfo-indirect append-statinfo-indirect-id]
                     [compare-statinfo-indirect compare-statinfo-indirect-id]
                     [super-call-statinfo-indirect super-call-statinfo-indirect-id]
                     [indirect-static-infos indirect-static-infos]
                     [instance-static-infos instance-static-infos])
         (values
          #`(begin
              #,@(top-level-declare #'(name?))
              #,@(build-instance-static-infos-defs static-infos-id static-infos-exprs)
              #,@(build-veneer-annotation converter? super interfaces
                                          #'(name name? name-convert
                                                  name-instance indirect-static-infos))
              (veneer-finish
               [orig-stx base-stx scope-stx
                         name name-extends tail-name
                         name? name-convert check?
                         name-instance
                         call-statinfo-indirect index-statinfo-indirect index-set-statinfo-indirect
                         append-statinfo-indirect compare-statinfo-indirect
                         super-call-statinfo-indirect
                         indirect-static-infos
                         instance-static-infos
                         ann-terms ann-op-name]
               exports
               [option stx-params] ...))))])))

(define-syntax veneer-finish
  (lambda (stx)
    (syntax-parse stx
      [(_ [orig-stx base-stx scope-stx
                    name name-extends tail-name
                    name? name-convert check?
                    name-instance
                    call-statinfo-indirect index-statinfo-indirect index-set-statinfo-indirect
                    append-statinfo-indirect compare-statinfo-indirect
                    super-call-statinfo-indirect
                    indirect-static-infos
                    instance-static-infos
                    ann-terms ann-op-name]
          exports
          [option stx-params] ...)
       #:with ann::annotation #'(group . ann-terms)
       (define stxes #'orig-stx)
       (define options (parse-options #'orig-stx #'(option ...) #'(stx-params ...)))
       (define parent-name (hash-ref options 'extends #f))
       (define super (and parent-name
                          (syntax-local-value* (in-class-desc-space parent-name) veneer-desc-ref)))
       (define interface-names (reverse (hash-ref options 'implements '())))
       (define-values (all-interfaces interfaces) (interface-names->interfaces stxes interface-names
                                                                               #:results values))
       (define private-interfaces (interface-set-diff
                                   (interface-names->interfaces stxes (hash-ref options 'private-implements '()))
                                   (interface-names->interfaces stxes (hash-ref options 'public-implements '()))))
       (define expression-macro-rhs (hash-ref options 'expression-rhs #f))
       (define annotation-rhs (hash-ref options 'annotation-rhs #f))

       (define converter? (or (hash-ref options 'converter? #f)
                              (and super
                                   (veneer-desc-convert-id super))))

       (define expose (make-expose #'scope-stx #'base-stx))

       (define intro (make-syntax-introducer))

       (define dots (hash-ref options 'dots '()))
       (define dot-provider-rhss (map cdr dots))
       (define parent-dot-providers
         (for/list ([parent (in-list (if super (cons super interfaces) interfaces))]
                    #:do [(define dp (objects-desc-dot-provider parent))]
                    #:when dp)
           dp))

       (define final? #t)

       (define added-methods (reverse (hash-ref options 'methods '())))
       (define-values (method-mindex   ; symbol -> mindex
                       method-names    ; index -> symbol-or-identifier
                       method-vtable   ; index -> function-identifier or '#:abstract
                       method-results  ; symbol -> nonempty list of identifiers; first one implies others
                       method-private  ; symbol -> identifier or (list identifier)
                       method-decls    ; symbol -> identifier, intended for checking distinct
                       abstract-name)  ; #f or identifier for a still-abstract method
         (extract-method-tables stxes added-methods super interfaces private-interfaces final? #f))

       (check-fields-methods-dots-distinct stxes #hasheq() method-mindex method-names method-decls dots)
       (check-consistent-unimmplemented stxes final? abstract-name #'name)

       (define exs (parse-exports #'(combine-out . exports) expose))
       (check-exports-distinct stxes exs null method-mindex dots)

       (define has-private?
         ((hash-count method-private) . > . 0))

       (define-values (indexable? here-indexable? public-indexable?)
         (able-method-status 'get super interfaces method-mindex method-vtable method-private))
       (define-values (setable? here-setable? public-setable?)
         (able-method-status 'set super interfaces method-mindex method-vtable method-private))
       (define-values (appendable? here-appendable? public-appendable?)
         (able-method-status 'append super interfaces method-mindex method-vtable method-private))
       (define-values (comparable? here-comparable? public-comparable?)
         (able-method-status 'compare super interfaces method-mindex method-vtable method-private
                             #:name 'compare_to))

       (define (temporary template)
         ((make-syntax-introducer) (datum->syntax #f (string->symbol (format template (syntax-e #'name))))))

       (with-syntax ([(export ...) exs])
         (with-syntax ([constructor-name (and (not expression-macro-rhs)
                                              (car (generate-temporaries (list #'name))))]
                       [(super-name* ...) (if super #'(super-name) '())]
                       [(interface-name ...) interface-names]
                       [(dot-id ...) (map car dots)]
                       [dot-provider-name (or (and (or (pair? dot-provider-rhss)
                                                       ((length parent-dot-providers) . > . 1))
                                                   (temporary "dot-provider-~a"))
                                              (and (pair? parent-dot-providers)
                                                   (car parent-dot-providers)))]
                       [representation-static-infos (extract-representation-static-infos #'ann.parsed)]
                       [name?/checked (if (syntax-e #'check?) #'name? #f)]
                       [dot-providers (add-super-dot-providers #'name-instance super interfaces)])
           (define defns
             (reorder-for-top-level
              (append
               (build-veneer-predicate-or-converter super converter?
                                                    #'(name name? name-convert check?
                                                            ann.parsed ann-terms ann-op-name
                                                            name-instance indirect-static-infos))
               (build-methods #:veneer-vtable method-vtable
                              method-results
                              added-methods method-mindex method-names method-private
                              #f #f
                              #'(name #f #|<- not `name-instance`|# name?/checked name-convert #f
                                      prop-methods-ref
                                      representation-static-infos ;; instead of `indirect-static-infos`
                                      []
                                      []
                                      []
                                      []
                                      []
                                      []
                                      [super-name* ... interface-name ...]
                                      []))
               ;; includes defining the namespace and constructor name:
               (build-class-dot-handling #:veneer? #t
                                         method-mindex method-vtable method-results final?
                                         has-private? method-private #f #f
                                         expression-macro-rhs intro #f
                                         #f
                                         #f
                                         dot-provider-rhss parent-dot-providers
                                         #`(name name-extends tail-name
                                                 name?/checked name-convert
                                                 constructor-name name-instance name-ref name-of
                                                 #f #f dot-provider-name
                                                 indirect-static-infos dot-providers
                                                 [] [] []
                                                 [] []
                                                 []
                                                 [dot-id ...]
                                                 []
                                                 [export ...]
                                                 base-stx scope-stx))
               (build-class-static-infos #:veneer? #t
                                         #f
                                         super
                                         #f
                                         null
                                         null
                                         null
                                         null
                                         #'(name constructor-name name-instance
                                                 #f #f
                                                 indirect-static-infos
                                                 dot-providers #f
                                                 []
                                                 []
                                                 []
                                                 []
                                                 []))
               (build-veneer-desc super options
                                  parent-name interface-names all-interfaces private-interfaces
                                  method-mindex method-names method-vtable method-results method-private dots
                                  public-indexable?
                                  public-setable?
                                  public-appendable?
                                  public-comparable?
                                  #'(name name-extends class:name constructor-maker-name name-defaults name-ref
                                          name? name-convert check? converter?
                                          dot-provider-name prefab-guard-name
                                          instance-static-infos dot-providers))
               (build-method-results added-methods
                                     method-mindex method-vtable method-private
                                     method-results
                                     final?
                                     #'prop-methods-ref
                                     #f #f
                                     #'index-statinfo-indirect indexable?
                                     #'index-set-statinfo-indirect setable?
                                     #'append-statinfo-indirect appendable?
                                     #'compare-statinfo-indirect comparable?
                                     #'super-call-statinfo-indirect
                                     #:checked-append? #f
                                     #:checked-compare? #f))))
           #`(begin . #,defns)))])))

(define-for-syntax (build-veneer-annotation converter? super interfaces names)
  (with-syntax ([(name name? name-convert
                       name-instance indirect-static-infos)
                 names])
    (with-syntax ([dot-providers (add-super-dot-providers #'name-instance super interfaces)])
      (cond
        [(not converter?)
         (list
          #`(define-annotation-syntax name
              (identifier-annotation name?
                                     ((#%dot-provider dot-providers)
                                      . indirect-static-infos)
                                     #:static-only)))]
        [else
         (list
          #`(define-annotation-syntax name
              (identifier-binding-annotation #,(binding-form #'converter-binding-infoer
                                                             #'(name name-convert val))
                                             val
                                             ((#%dot-provider dot-providers)
                                              . indirect-static-infos)
                                             #:static-only)))]))))

(define-syntax (converter-binding-infoer stx)
  (syntax-parse stx
    [(_ static-infos (name name-convert val))
     (binding-info (shrubbery-syntax->string #'name)
                   #'val
                   #'()
                   #'((val (0)))
                   #'converter-matcher
                   #'converter-committer
                   #'converter-binder
                   #'(name-convert convert-committer converted-val val))]))

(define-syntax (converter-matcher stx)
  (syntax-parse stx
    [(_ arg-id (name-convert convert-committer converted-val val) IF success fail)
     #'(begin
         (define convert-committer (name-convert arg-id #f))
         (IF convert-committer success fail))]))

(define-syntax (converter-committer stx)
  (syntax-parse stx
    [(_ arg-id (name-convert convert-committer converted-val val))
     #'(define converted-val (convert-committer))]))

(define-syntax (converter-binder stx)
  (syntax-parse stx
    [(_ arg-id (name-convert convert-committer converted-val val))
     #'(define val converted-val)]))

(define-for-syntax (build-veneer-predicate-or-converter super converter? names)
  (with-syntax ([(name name? name-convert check?
                       ann ann-terms ann-op-name
                       name-instance indirect-static-infos)
                 names])
    (define ann-str (shrubbery-syntax->string #`(#,group-tag . ann-terms)))
    (define all-ann-str
      (if (and super (veneer-desc-predicate-id super))
          (annotation-string-and ann-str
                                 (shrubbery-syntax->string
                                  (veneer-desc-id super)))
          ann-str))
    (syntax-parse #'ann
      [ann::annotation-predicate-form
       #:when (or (not super)
                  (not (veneer-desc-convert-id super)))
       (define super? (and super
                           (veneer-desc-predicate-id super)))
       (list
        (if converter?
            #`(define name-convert
                #,(cond
                    [(syntax-e #'check?)
                     #`(let ([name? ann.predicate])
                         (let ([name? (lambda (v)
                                        #,(if super?
                                              #`(and (#,super? v)
                                                     (name? v))
                                              #`(name? v)))])
                           (lambda (v who)
                             (if (name? v)
                                 (if who
                                     v
                                     (lambda () v))
                                 (if who
                                     (raise-binding-failure who "argument" v '#,all-ann-str)
                                     #f)))))]
                    [else
                     #`(lambda (v who)
                         (if who
                             v
                             (lambda () v)))]))
            #`(define name?
                #,(cond
                    [(syntax-e #'check?)
                     #`(let ([name? ann.predicate])
                         (let ([name? (lambda (v)
                                        #,(if super?
                                              #`(and (#,super? v)
                                                     (name? v))
                                              #`(name? v)))])
                           (case-lambda
                             [(v) (name? v)]
                             [(v who) (unless (name? v)
                                        (raise-binding-failure who "argument" v '#,all-ann-str))])))]
                    [else
                     #`(lambda (v)
                         #t)]))))]
      [ann::annotation-binding-form
       #:with arg-parsed::binding-form #'ann.binding
       #:with arg-impl::binding-impl #'(arg-parsed.infoer-id () arg-parsed.data)
       #:with arg-info::binding-info #'arg-impl.info
       (unless (syntax-e #'check?)
         (raise-unchecked-disallowed #'ann-op-name (respan #'ann-terms)))
       (unless converter?
         (raise-syntax-error #f
                             "converter annotation not allowed without a `converter` clause"
                             #'ann-op-name (respan #'ann-terms)))
       (list
        #`(define (name-convert v who)
            (arg-info.matcher-id v
                                 arg-info.data
                                 if/blocked
                                 #,(cond
                                     [(and super
                                           (veneer-desc-predicate-id super))
                                      #`(let ([cvt1 (let ()
                                                      (arg-info.committer-id v arg-info.data)
                                                      (arg-info.binder-id v arg-info.data)
                                                      (define-static-info-syntax/maybe arg-info.bind-id
                                                        arg-info.bind-static-info ...)
                                                      ...
                                                      ann.body)])
                                          #,(cond
                                              [(veneer-desc-convert-id super)
                                               => (lambda (id)
                                                    #`(#,id cvt1 who))]
                                              [else
                                               #`(if #`(#,(veneer-desc-predicate-id super) cvt1)
                                                     (if who
                                                         cvt1
                                                         (lambda () cvt1))
                                                     (if who
                                                         (raise-binding-failure who "argument" v '#,all-ann-str)
                                                         #f))]))]
                                     [else
                                      #`(let ([commit (lambda ()
                                                        (arg-info.committer-id v arg-info.data)
                                                        (arg-info.binder-id v arg-info.data)
                                                        (define-static-info-syntax/maybe arg-info.bind-id
                                                          arg-info.bind-static-info ...)
                                                        ...
                                                        ann.body)])
                                          (if who
                                              (commit)
                                              commit))])
                                 (if who
                                     (raise-binding-failure who "argument" v '#,all-ann-str)
                                     #f))))])))

(define-for-syntax (extract-representation-static-infos ann-stx)
  (syntax-parse ann-stx
    [ann::annotation-predicate-form
     #'ann.static-infos]
    [ann::annotation-binding-form
     #'ann.static-infos]))

(define-for-syntax (build-veneer-desc super options
                                      parent-name interface-names all-interfaces private-interfaces
                                      method-mindex method-names method-vtable method-results method-private dots
                                      public-indexable?
                                      public-setable?
                                      public-appendable?
                                      public-comparable?
                                      names)
  (with-syntax ([(name name-extends class:name constructor-maker-name name-defaults name-ref
                       name? name-convert check? converter?
                       dot-provider-name prefab-guard-name
                       instance-static-infos dot-providers)
                 names])
    (let ([method-shapes (build-quoted-method-shapes method-vtable method-names method-mindex)]
          [method-map (build-quoted-method-map method-mindex)]
          [method-result-expr (build-method-result-expression method-results)]
          [flags #`(#,@(if public-indexable? '(get) null)
                    #,@(if public-setable? '(set) null)
                    #,@(if public-appendable? '(append) null)
                    #,@(if public-comparable? '(compare) null))]
          [interface-names (interface-names->quoted-list interface-names all-interfaces private-interfaces 'public)])
      (list
       (build-syntax-definition/maybe-extension
        'rhombus/class #'name #'name-extends
        #`(veneer-desc (quote-syntax #,interface-names)
                       '#,method-shapes
                       (quote-syntax #,method-vtable)
                       '#,method-map
                       #,method-result-expr
                       '#,(map car dots)
                       (quote-syntax dot-provider-name)
                       (#,(quote-syntax quasisyntax) instance-static-infos)
                       '#,flags
                       ;; ----------------------------------------
                       (quote-syntax name)
                       (quote-syntax #,parent-name)
                       #,(and (syntax-e #'check?)
                              #'(quote-syntax name?))
                       #,(and (syntax-e #'name-convert)
                              #'(quote-syntax name-convert))
                       (quote-syntax dot-providers)))))))
