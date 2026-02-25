#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "interface-parse.rkt"
                     (submod "interface-meta.rkt" for-class)
                     "expose.rkt"
                     "origin.rkt"
                     (only-in "class-parse.rkt"
                              :options-block
                              class-reflect-name
                              in-class-desc-space
                              objects-desc-dot-provider
                              check-exports-distinct
                              check-fields-methods-dots-distinct
                              constructor-as-expression?
                              check-consistent-construction
                              added-method-body))
         "forwarding-sequence.rkt"
         "definition.rkt"
         (submod "annotation.rkt" for-class)
         (submod "annot-macro.rkt" for-class)
         "interface-clause.rkt"
         "interface-clause-parse.rkt"
         "class-top-level.rkt"
         "class-clause-tag.rkt"
         "class-step.rkt"
         "class-static-info.rkt"
         "dotted-sequence-parse.rkt"
         "class-transformer.rkt"
         (only-meta-in 1
                       "class-method.rkt")
         (only-in "class-annotation.rkt"
                  build-extra-internal-id-aliases
                  no-annotation-transformer)
         "class-dot.rkt"
         (only-in "class-method.rkt"
                  raise-not-an-instance)
         (submod "namespace.rkt" for-exports)
         "class-able.rkt"
         "name-prefix.rkt")

(provide (for-space rhombus/defn
                    interface))

(define-defn-syntax interface
  (definition-transformer
    (lambda (stxes name-prefix effect-id)
      (parse-interface stxes name-prefix effect-id))))

(define-for-syntax (parse-interface stxes name-prefix effect-id)
  (syntax-parse stxes
    #:datum-literals (group)
    [(_ name-seq::dotted-identifier-sequence options::options-block)
     #:with full-name::dotted-identifier #'name-seq
     #:with name #'full-name.name
     #:with name-extends #'full-name.extends
     #:with tail-name #'full-name.tail-name
     #:with orig-stx stxes
     #:with reflect-name (class-reflect-name #'options.name name-prefix #'name)
     (define body #'(options.form ...))
     (define intro (make-syntax-introducer #t))
     ;; The shape of `finish-data` is recognized in `interface-annotation+finish`
     ;; and "interface-meta.rkt"
     (define finish-data #`([orig-stx base-stx #,(intro #'scope-stx)
                                      reflect-name #,effect-id name name-extends tail-name]
                            ;; data accumulated from parsed clauses:
                            ()))
     (annotation-to-be-defined! #'name)
     #`(#,(cond
            [(null? (syntax-e body))
             #`(interface-annotation+finish #,finish-data [#:ctx base base] ())]
            [else
             #`(rhombus-mixed-nested-forwarding-sequence
                (interface-annotation+finish #,finish-data) rhombus-class
                (interface-body-step #,finish-data . #,(intro body)))]))]))

(define-class-body-step interface-body-step
  :interface-clause
  interface-clause?
  interface-expand-data
  class-clause-accum)

(define-syntax interface-annotation+finish
  (lambda (stx)
    (syntax-parse stx
      [(_ ([orig-stx base-stx init-scope-stx
                     reflect-name effect-id name name-extends tail-name]
           . _)
          [#:ctx forward-base-ctx forward-ctx]
          exports
          [option stx-param] ...)
       #:with scope-stx ((make-syntax-delta-introducer #'forward-ctx #'forward-base-ctx) #'init-scope-stx)
       (define stxes #'orig-stx)
       (define options (parse-annotation-options #'orig-stx #'(option ...) #'(stx-param ...)))
       (define supers (interface-names->interfaces stxes (reverse (hash-ref options 'extends '()))))

       (define-values (unexposed-internal-name internal-name extra-internal-names)
         (extract-internal-ids options
                               #'scope-stx #'base-stx
                               #'orig-stx))

       (define annotation-rhs (hash-ref options 'annotation-rhs #f))

       (define intro (make-syntax-introducer))
       (define (temporary template #:name [name #'name])
         (and name
              (intro (datum->syntax #f (string->symbol (format template (syntax-e name)))))))

       (define internal-internal-name (or internal-name
                                          ;; we need an internal accessor if there are any
                                          ;; non-final methods; for non-abstract, we need a way
                                          ;; to access a vtable from `this`, and for non-final
                                          ;; generally, we need a way to get a vtable in case
                                          ;; of subclasses that re-override a private implementation
                                          (and (hash-ref options 'has-non-final-method? #f)
                                               (temporary "internal-internal-~a"))))

       (define name-instance-id (temporary "~a-instance"))

       (define internal-name-instance-id (if internal-name
                                             (temporary "~a-instance" #:name internal-internal-name)
                                             name-instance-id))

       (define dot-providers (add-super-dot-providers name-instance-id #f supers))
       (define internal-dot-providers (add-super-dot-providers internal-name-instance-id #f supers))

       (define implementable-name (hash-ref options 'implementable #f))

       (define-values (call-statinfo-indirect-id
                       index-statinfo-indirect-id
                       index-set-statinfo-indirect-id
                       append-statinfo-indirect-id
                       compare-statinfo-indirect-id
                       contains-statinfo-indirect-id

                       super-call-statinfo-indirect-id

                       static-infos-id
                       static-infos-exprs

                       instance-static-infos-id
                       instance-static-infos-expr
                       instance-static-infos

                       internal-instance-static-infos-id
                       internal-instance-static-infos-expr

                       dot-static-infos-id
                       dot-static-infos-expr

                       internal-dot-static-infos-id
                       internal-dot-static-infos-expr

                       all-static-infos
                       internal-all-static-infos)
         (extract-instance-static-infoss #'name options #f supers
                                         #hasheq() #hasheq()
                                         internal-name
                                         dot-providers internal-dot-providers
                                         intro))

       (with-syntax ([name? (temporary "~a?")]
                     [name-instance name-instance-id]
                     [internal-name? (temporary "~a?" #:name internal-internal-name)]
                     [internal-name-instance internal-name-instance-id]
                     [all-static-infos all-static-infos]
                     [internal-all-static-infos internal-all-static-infos]
                     [instance-static-infos instance-static-infos]
                     [call-statinfo-indirect call-statinfo-indirect-id]
                     [index-statinfo-indirect index-statinfo-indirect-id]
                     [index-set-statinfo-indirect index-set-statinfo-indirect-id]
                     [append-statinfo-indirect append-statinfo-indirect-id]
                     [compare-statinfo-indirect compare-statinfo-indirect-id]
                     [contains-statinfo-indirect contains-statinfo-indirect-id]
                     [super-call-statinfo-indirect super-call-statinfo-indirect-id]
                     [dot-providers dot-providers]
                     [implementable-name (and implementable-name
                                              ((make-expose #'scope-stx #'base-stx) implementable-name))])
         (values
          #`(begin
              #,@(build-instance-static-infos-defs static-infos-id static-infos-exprs
                                                   instance-static-infos-id instance-static-infos-expr
                                                   internal-instance-static-infos-id internal-instance-static-infos-expr
                                                   dot-static-infos-id dot-static-infos-expr
                                                   internal-dot-static-infos-id internal-dot-static-infos-expr)
              #,@(build-interface-annotation internal-name
                                             annotation-rhs
                                             supers
                                             #'(name name-extends tail-name
                                                     name? name-instance
                                                     internal-name? internal-name-instance
                                                     implementable-name
                                                     all-static-infos internal-all-static-infos
                                                     orig-stx))
              #,@(build-extra-internal-id-aliases internal-name extra-internal-names #:interface? #t)
              (interface-finish [orig-stx base-stx scope-stx
                                          reflect-name name name-extends tail-name
                                          name? name-instance
                                          #,internal-name internal-name? internal-name-instance
                                          #,internal-internal-name
                                          all-static-infos internal-all-static-infos
                                          instance-static-infos dot-providers
                                          call-statinfo-indirect index-statinfo-indirect
                                          index-set-statinfo-indirect append-statinfo-indirect
                                          compare-statinfo-indirect contains-statinfo-indirect
                                          super-call-statinfo-indirect]
                                exports
                                [option stx-param] ...))))])))

(define-syntax interface-finish
  (lambda (stx)
    (syntax-parse stx
      [(_ [orig-stx base-stx scope-stx
                    reflect-name name name-extends tail-name
                    name? name-instance
                    maybe-internal-name internal-name? internal-name-instance
                    internal-internal-name-id
                    all-static-infos internal-all-static-infos
                    instance-static-infos dot-providers
                    call-statinfo-indirect index-statinfo-indirect
                    index-set-statinfo-indirect append-statinfo-indirect
                    compare-statinfo-indirect contains-statinfo-indirect
                    super-call-statinfo-indirect]
          exports
          [option stx-param] ...)
       (define stxes #'orig-stx)
       (define options (parse-options #'orig-stx #'(option ...) #'(stx-param ...)))
       (define supers (interface-names->interfaces stxes (reverse (hash-ref options 'extends '()))))
       (define parent-names (map interface-desc-id supers))
       (define primitive-properties (hash-ref options 'primitive-properties '()))
       (define added-methods (reverse (hash-ref options 'methods '())))
       (define-values (method-mindex   ; symbol -> mindex
                       method-names    ; index -> symbol-or-identifier
                       method-vtable   ; index -> function-identifier or '#:abstract
                       method-results  ; symbol -> nonempty list of identifiers; first one implies others
                       method-private  ; symbol -> identifier or (list identifier)
                       method-private-inherit ; symbol -> (vector ref-id index maybe-result-id)
                       method-decls    ; symbol -> identifier, intended for checking distinct
                       abstract-name)  ; #f or identifier
         (extract-method-tables stxes added-methods #f supers
                                #hasheq() #hasheq()
                                #f #f))

       (define dots (hash-ref options 'dots '()))
       (define dot-provider-rhss (map cdr dots))
       (check-fields-methods-dots-distinct stxes #hasheq() method-mindex method-names method-decls dots)

       (define expose (make-expose #'scope-stx #'base-stx))
       (define exs (parse-exports #'(combine-out . exports) expose))
       (define replaced-ht (check-exports-distinct stxes exs '() method-mindex dots))

       (define internal-name (let ([id #'maybe-internal-name])
                               (and (syntax-e id) id)))
       (define internal-internal-name (and (syntax-e #'internal-internal-name-id)
                                           #'internal-internal-name-id))
       (define implementable-name (hash-ref options 'implementable #f))

       (define given-expression-macro-rhs (hash-ref options 'expression-macro-rhs #f))
       (define given-constructor-rhs (hash-ref options 'constructor-rhs #f))
       (define given-constructor-stx-params (hash-ref options 'constructor-stx-params #f))
       (define given-constructor-name (hash-ref options 'constructor-name #f))
       (check-consistent-construction #'orig-stx null null null #'name
                                      given-constructor-rhs given-constructor-name given-expression-macro-rhs)
       (define constructor-rhs
         (and (not (constructor-as-expression? given-constructor-rhs))
              given-constructor-rhs))
       (define expression-macro-rhs
         (or given-expression-macro-rhs
             (and (constructor-as-expression? given-constructor-rhs)
                  given-constructor-rhs)))

       (define parent-dot-providers
         (for/list ([parent (in-list supers)]
                    #:do [(define dp (objects-desc-dot-provider parent))]
                    #:when dp)
           dp))

       (define-values (callable? here-callable? public-callable?)
         (able-method-status 'call #f supers method-mindex method-vtable method-private))
       (define-values (indexable? here-indexable? public-indexable?)
         (able-method-status 'get #f supers method-mindex method-vtable method-private))
       (define-values (setable? here-setable? public-setable?)
         (able-method-status 'set #f supers method-mindex method-vtable method-private))
       (define-values (appendable? here-appendable? public-appendable?)
         (able-method-status 'append #f supers method-mindex method-vtable method-private))
       (define-values (comparable? here-comparable? public-comparable?)
         (able-method-status 'compare #f supers method-mindex method-vtable method-private
                             #:name 'compare_to))
       (define-values (container? here-container? public-container?)
         (able-method-status 'contains #f supers method-mindex method-vtable method-private
                             #:name 'contains))

       (define post-forms (hash-ref options 'post-forms null))

       (define (temporary template #:name [name #'name])
         (and name
              ((make-syntax-introducer) (datum->syntax #f (string->symbol (format template (syntax-e name)))))))

       (with-syntax ([prop:name (temporary "prop:~a")]
                     [name-ref (temporary "~a-ref")]
                     [name-ref-or-error (temporary "~a-ref-or-error")]
                     [prop:internal-name (temporary "prop:~a" #:name internal-internal-name)]
                     [constructor-name (and (or (not expression-macro-rhs)
                                                constructor-rhs)
                                            (or (and given-constructor-name
                                                     (not (bound-identifier=? #'name (expose given-constructor-name)))
                                                     (expose given-constructor-name))
                                                #'name))]
                     [(super-name ...) parent-names]
                     [(export ...) exs]
                     [implementable-name (and implementable-name
                                              (expose implementable-name))]
                     [(dot-id ...) (map car dots)]
                     [dot-provider-name (or (and (or (pair? dot-provider-rhss)
                                                     (and (pair? parent-dot-providers)
                                                          (pair? (cdr parent-dot-providers))))
                                                 (temporary "dot-provider-~a"))
                                            (and (pair? parent-dot-providers)
                                                 (car parent-dot-providers)))])
         (with-syntax ([internal-name-ref (if internal-internal-name
                                              (temporary "~a-ref" #:name internal-internal-name)
                                              #'name-ref)])
           (define defns
             (reorder-for-top-level
              (append
               (if (eq? (syntax-local-context) 'top-level)
                   ;; forward declaration for methods:
                   (list #'(define-syntaxes (name?) (values)))
                   null)
               (build-methods method-results
                              added-methods method-mindex method-names method-private method-private-inherit
                              #f #f #f #f
                              #hasheq() #hasheq()
                              #'(name reflect-name internal-name? #f #f #f
                                      internal-name-ref
                                      ()
                                      []
                                      []
                                      []
                                      []
                                      []
                                      []
                                      []
                                      [super-name ...]
                                      []
                                      #f))
               (build-interface-property internal-internal-name
                                         #'(name prop:name name? name-ref name-ref-or-error
                                                 prop:internal-name internal-name? internal-name-ref))
               (build-interface-dot-handling method-mindex method-names method-vtable method-results replaced-ht
                                             internal-name
                                             expression-macro-rhs constructor-rhs given-constructor-stx-params
                                             dot-provider-rhss parent-dot-providers
                                             #'(name reflect-name name-extends tail-name
                                                     name? name-instance internal-name-ref name-ref-or-error
                                                     internal-name-instance internal-name-ref
                                                     dot-provider-name [dot-id ...]
                                                     dot-providers
                                                     constructor-name
                                                     [export ...]
                                                     base-stx scope-stx))
               (if constructor-rhs
                   (build-interface-constructor-static-info given-constructor-rhs
                                                            #'(constructor-name all-static-infos))
                   null)
               (if (syntax-e #'implementable-name)
                   (list (build-interface-implementable-expression #'implementable-name))
                   null)
               (build-interface-desc supers parent-names options
                                     method-mindex method-names method-vtable method-results method-private
                                     dots
                                     internal-name
                                     callable? indexable? setable? appendable? comparable? container?
                                     primitive-properties
                                     #'(name name-extends prop:name name-ref name-ref-or-error
                                             prop:internal-name internal-name? internal-name-ref
                                             implementable-name
                                             dot-provider-name
                                             instance-static-infos
                                             super-call-statinfo-indirect call-statinfo-indirect))
               (build-method-results added-methods
                                     method-mindex method-vtable method-private
                                     method-results
                                     #f
                                     #'internal-name-ref
                                     #'call-statinfo-indirect callable?
                                     #'index-statinfo-indirect indexable?
                                     #'index-set-statinfo-indirect setable?
                                     #'append-statinfo-indirect appendable?
                                     #'compare-statinfo-indirect comparable?
                                     #'contains-statinfo-indirect container?
                                     #'super-call-statinfo-indirect))))
           (transfer-origins
            (syntax->list #'(option ...))
            #`(begin
                #,@defns
                #,@post-forms))))])))

(define-for-syntax (build-interface-property internal-internal-name names)
  (with-syntax ([(name prop:name name? name-ref name-ref-or-error
                       prop:internal-name internal-name? internal-name-ref)
                 names])
    (append
     (if internal-internal-name
         (list
          #`(define-values (prop:internal-name internal-name? internal-name-ref)
              (make-struct-type-property 'name)))
         null)
     (list
      #`(define-values (prop:name name? name-ref)
          (make-struct-type-property 'name
                                     #,@(if internal-internal-name
                                            #`(#f (list (cons prop:internal-name
                                                              (lambda (vt) vt))))
                                            '())))
      #`(define (name-ref-or-error v)
          (define vtable (name-ref v #f))
          (or vtable
              (raise-not-an-instance 'name v)))))))

(define-for-syntax (build-interface-annotation internal-name annotation-rhs supers names)
  (with-syntax ([(name name-extends tail-name
                       name? name-instance
                       internal-name? internal-name-instance
                       implementable-name
                       all-static-infos internal-all-static-infos
                       orig-stx)
                 names])
    (append
     (if internal-name
         (with-syntax ([internal-name internal-name])
           (list
            #`(define-annotation-syntax internal-name
                (identifier-annotation internal-name?
                                       internal-all-static-infos))))
         null)
     (cond
       [(and annotation-rhs
             (eq? '#:none (syntax-e annotation-rhs)))
        null]
       [else
        (list
         (build-syntax-definition/maybe-extension
          'rhombus/annot #'name #'name-extends
          (cond
            [annotation-rhs
             (if (eq? '#:error (syntax-e annotation-rhs))
                 #'no-annotation-transformer
                 (wrap-class-transformer #'name #'tail-name
                                         ((make-syntax-introducer) annotation-rhs)
                                         #'make-annotation-prefix-operator
                                         #:extra-args (list #'ctx)
                                         "interface"))]
            [else
             #`(identifier-annotation name? all-static-infos)])
          #:form #'orig-stx))])
     (if (syntax-e #'implementable-name)
         (list
          #'(define-annotation-syntax implementable-name
              (identifier-annotation name? all-static-infos)))
         null))))

(define-for-syntax (build-interface-desc supers parent-names options
                                         method-mindex method-names method-vtable method-results method-private
                                         dots
                                         internal-name
                                         callable? indexable? setable? appendable? comparable? container?
                                         primitive-properties
                                         names)
  (with-syntax ([(name name-extends prop:name name-ref name-ref-or-error
                       prop:internal-name internal-name? internal-name-ref
                       implementable-name
                       dot-provider-name
                       instance-static-infos
                       super-call-statinfo-indirect call-statinfo-indirect)
                 names])
    (let ([method-shapes (build-quoted-method-shapes method-vtable method-names method-mindex)]
          [method-map (build-quoted-method-map method-mindex)]
          [method-result-expr (build-method-result-expression method-results)]
          [custom-annotation? (and (hash-ref options 'annotation-rhs #f) #t)])
      (append
       (if internal-name
           (list
            #`(define-syntax #,(in-class-desc-space internal-name)
                ;; could improve by avoiding duplicate information
                (interface-desc-maker
                 (lambda ()
                   (interface-internal-desc (quote-syntax #,parent-names)
                                            '#,method-shapes
                                            (quote-syntax #,method-vtable)
                                            '#,method-map
                                            #,method-result-expr
                                            #f ; dots not used directly
                                            #f
                                            (#,(quote-syntax quasisyntax) instance-static-infos)
                                            '()
                                            ;; --------------------
                                            (quote-syntax name)
                                            #f
                                            (quote-syntax prop:internal-name)
                                            (quote-syntax prop:internal-name)
                                            (quote-syntax internal-name-ref)
                                            (quote-syntax internal-name-ref)
                                            #,custom-annotation?
                                            #f
                                            null
                                            (quote #,(build-quoted-private-method-list 'method method-private))
                                            (quote #,(build-quoted-private-method-list 'property method-private)))))))
           null)
       (let ([maker
              #`(interface-desc-maker
                 (lambda ()
                   (interface-desc (quote-syntax #,parent-names)
                                   '#,method-shapes
                                   (quote-syntax #,method-vtable)
                                   '#,method-map
                                   #,method-result-expr
                                   '#,(map car dots)
                                   #,(and (syntax-e #'dot-provider-name)
                                          #'(quote-syntax dot-provider-name))
                                   (#,(quote-syntax quasisyntax) instance-static-infos)
                                   '#,(append
                                       (if callable? '(call) '())
                                       (if indexable? '(get) '())
                                       (if setable? '(set) '())
                                       (if appendable? '(append) '())
                                       (if comparable? '(compare) '())
                                       (if container? '(contains) '()))
                                   ;; ----------------------------------------
                                   (quote-syntax name)
                                   #,(and internal-name
                                          #`(quote-syntax #,internal-name))
                                   (quote-syntax prop:name)
                                   #,(and (syntax-e #'prop:internal-name)
                                          #'(quote-syntax prop:internal-name))
                                   (quote-syntax name-ref-or-error)
                                   #,(and (syntax-e #'internal-name-ref)
                                          #'(quote-syntax internal-name-ref))
                                   #,custom-annotation?
                                   #,(let ([id (if (syntax-e #'call-statinfo-indirect)
                                                   #'call-statinfo-indirect
                                                   #'super-call-statinfo-indirect)])
                                       (if (and id (syntax-e id))
                                           #`(quote-syntax #,id)
                                           #f))
                                   (list #,@(for/list ([pp (in-list primitive-properties)])
                                              #`(cons (quote-syntax #,(car pp))
                                                      (quote-syntax #,(cdr pp))))))))])
         (cond
           [(syntax-e #'implementable-name)
            (list
             #`(define-syntax #,(in-class-desc-space #'implementable-name) #,maker)
             (build-syntax-definition/maybe-extension
              'rhombus/class #'name #'name-extends
              #`(interface-desc-maker
                (lambda ()
                  (interface-unimplementable-variant (quote-syntax #,(in-class-desc-space #'implementable-name)))))))]
           [else
            (list
             (build-syntax-definition/maybe-extension
              'rhombus/class #'name #'name-extends
              maker))]))))))
