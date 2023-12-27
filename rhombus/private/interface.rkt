#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "interface-parse.rkt"
                     (submod "interface-meta.rkt" for-class)
                     "expose.rkt"
                     (only-in "class-parse.rkt"
                              :options-block
                              in-class-desc-space
                              objects-desc-dot-provider
                              check-exports-distinct
                              check-fields-methods-dots-distinct
                              added-method-body))
         "forwarding-sequence.rkt"
         "definition.rkt"
         (submod "dot.rkt" for-dot-provider)
         (submod "annotation.rkt" for-class)
         (submod "annot-macro.rkt" for-class)
         "interface-clause.rkt"
         "interface-clause-parse.rkt"
         "class-top-level.rkt"
         "class-clause-tag.rkt"
         "class-step.rkt"
         "class-static-info.rkt"
         "dotted-sequence-parse.rkt"
         (for-syntax "class-transformer.rkt")
         (only-meta-in 1
                       "class-method.rkt")
         (only-in "class-annotation.rkt"
                  build-extra-internal-id-aliases)
         "class-dot.rkt"
         (only-in "class-method.rkt"
                  raise-not-an-instance)
         "parse.rkt"
         (submod "namespace.rkt" for-exports)
         "class-able.rkt")

(provide (for-space rhombus/defn
                    interface))

(define-defn-syntax interface
  (definition-transformer
    (lambda (stxes)
      (parse-interface stxes))))

(define-for-syntax (parse-interface stxes)
  (syntax-parse stxes
    #:datum-literals (group)
    [(_ name-seq::dotted-identifier-sequence options::options-block)
     #:with full-name::dotted-identifier #'name-seq
     #:with name #'full-name.name
     #:with name-extends #'full-name.extends
     #:with tail-name #'full-name.tail-name
     #:with orig-stx stxes
     (define body #'(options.form ...))
     (define intro (make-syntax-introducer #t))
     ;; The shape of `finish-data` is recognzied in `interface-annotation+finish`
     ;; and "interface-meta.rkt"
     (define finish-data #`([orig-stx base-stx #,(intro #'scope-stx)
                                      name name-extends tail-name]
                            ;; data accumulated from parsed clauses:
                            ()))
     (define interface-data-stx #f)
     #`(#,(cond
            [(null? (syntax-e body))
             #`(interface-annotation+finish #,finish-data [#:ctx base base] ())]
            [else
             #`(rhombus-mixed-nested-forwarding-sequence
                (interface-annotation+finish #,finish-data) rhombus-class
                (interface-body-step (#,interface-data-stx ()) . #,(intro body)))]))]))

(define-class-body-step interface-body-step
  :interface-clause
  interface-expand-data
  class-clause-accum)

(define-syntax interface-annotation+finish
  (lambda (stx)
    (syntax-parse stx
      [(_ ([orig-stx base-stx init-scope-stx
                     name name-extends tail-name]
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
                                          ;; we need an internal accessor if there are any non-abstract,
                                          ;; non-final methods, since those need a way to access a
                                          ;; vtable from `this`
                                          (and (hash-ref options 'has-non-abstract-method? #f)
                                               (temporary "internal-internal-~a"))))

       (define-values (call-statinfo-indirect-id
                       index-statinfo-indirect-id
                       index-set-statinfo-indirect-id
                       append-statinfo-indirect-id

                       super-call-statinfo-indirect-id

                       static-infos-id
                       static-infos-exprs
                       instance-static-infos

                       indirect-static-infos
                       internal-indirect-static-infos)
         (extract-instance-static-infoss #'name options #f supers #hasheq() intro))

       (with-syntax ([name? (temporary "~a?")]
                     [name-instance (temporary "~a-instance")]
                     [internal-name? (temporary "~a?" #:name internal-internal-name)]
                     [internal-name-instance (if internal-name
                                                 (temporary "~a-instance" #:name internal-internal-name)
                                                 #'name-instance)]
                     [indirect-static-infos indirect-static-infos]
                     [instance-static-infos instance-static-infos]
                     [call-statinfo-indirect call-statinfo-indirect-id]
                     [index-statinfo-indirect index-statinfo-indirect-id]
                     [index-set-statinfo-indirect index-set-statinfo-indirect-id]
                     [append-statinfo-indirect append-statinfo-indirect-id]
                     [super-call-statinfo-indirect super-call-statinfo-indirect-id])
         (values
          #`(begin
              #,@(build-instance-static-infos-defs static-infos-id static-infos-exprs)
              #,@(build-interface-annotation internal-name
                                             annotation-rhs
                                             #'(name name-extends tail-name
                                                     name? name-instance
                                                     internal-name? internal-name-instance
                                                     indirect-static-infos))
              #,@(build-extra-internal-id-aliases internal-name extra-internal-names)
              (interface-finish [orig-stx base-stx scope-stx
                                          name name-extends tail-name
                                          name? name-instance
                                          #,internal-name internal-name? internal-name-instance
                                          #,internal-internal-name
                                          instance-static-infos
                                          call-statinfo-indirect index-statinfo-indirect
                                          index-set-statinfo-indirect append-statinfo-indirect
                                          super-call-statinfo-indirect]
                                exports
                                [option stx-param] ...))))])))

(define-syntax interface-finish
  (lambda (stx)
    (syntax-parse stx
      [(_ [orig-stx base-stx scope-stx
                    name name-extends tail-name
                    name? name-instance
                    maybe-internal-name internal-name? internal-name-instance
                    internal-internal-name-id
                    instance-static-infos
                    call-statinfo-indirect index-statinfo-indirect
                    index-set-statinfo-indirect append-statinfo-indirect
                    super-call-statinfo-indirect]
          exports
          [option stx-param] ...)
       (define stxes #'orig-stx)
       (define options (parse-options #'orig-stx #'(option ...) #'(stx-param ...)))
       (define supers (interface-names->interfaces stxes (reverse (hash-ref options 'extends '()))))
       (define parent-names (map interface-desc-id supers))
       (define added-methods (reverse (hash-ref options 'methods '())))
       (define-values (method-mindex   ; symbol -> mindex
                       method-names    ; index -> symbol-or-identifier
                       method-vtable   ; index -> function-identifier or '#:abstract
                       method-results  ; symbol -> nonempty list of identifiers; first one implies others
                       method-private  ; symbol -> identifier or (list identifier)
                       method-decls    ; symbol -> identifier, intended for checking distinct
                       abstract-name)  ; #f or identifier
         (extract-method-tables stxes added-methods #f supers #hasheq() #f #f))

       (define dots (hash-ref options 'dots '()))
       (define dot-provider-rhss (map cdr dots))
       (check-fields-methods-dots-distinct stxes #hasheq() method-mindex method-names method-decls dots)

       (define exs (parse-exports #'(combine-out . exports)
                                  (make-expose #'scope-stx #'base-stx)))
       (check-exports-distinct stxes exs '() method-mindex dots)

       (define internal-name (let ([id #'maybe-internal-name])
                               (and (syntax-e id) id)))
       (define internal-internal-name (and (syntax-e #'internal-internal-name-id)
                                           #'internal-internal-name-id))

       (define expression-macro-rhs (hash-ref options 'expression-macro-rhs #f))

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

       (define (temporary template #:name [name #'name])
         (and name
              ((make-syntax-introducer) (datum->syntax #f (string->symbol (format template (syntax-e name)))))))

       (with-syntax ([prop:name (temporary "prop:~a")]
                     [name-ref (temporary "~a-ref")]
                     [name-ref-or-error (temporary "~a-ref-or-error")]
                     [prop:internal-name (temporary "prop:~a" #:name internal-internal-name)]
                     [(super-name ...) parent-names]
                     [(export ...) exs]
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
                              added-methods method-mindex method-names method-private
                              #f #f
                              #'(name name-instance internal-name? #f #f
                                      internal-name-ref
                                      ()
                                      []
                                      []
                                      []
                                      []
                                      []
                                      []
                                      [super-name ...]
                                      []))
               (build-interface-property internal-internal-name
                                         #'(name prop:name name? name-ref name-ref-or-error
                                                 prop:internal-name internal-name? internal-name-ref))
               (build-interface-dot-handling method-mindex method-vtable method-results
                                             internal-name
                                             expression-macro-rhs dot-provider-rhss parent-dot-providers
                                             #'(name name-extends tail-name
                                                     name? name-instance internal-name-ref name-ref-or-error
                                                     internal-name-instance internal-name-ref
                                                     dot-provider-name [dot-id ...]
                                                     [export ...]))
               (build-interface-desc supers parent-names options
                                     method-mindex method-names method-vtable method-results method-private dots
                                     internal-name
                                     callable? indexable? setable? appendable?
                                     #'(name name-extends prop:name name-ref name-ref-or-error
                                             prop:internal-name internal-name? internal-name-ref
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
                                     #'super-call-statinfo-indirect))))
           #`(begin . #,defns)))])))

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

(define-for-syntax (build-interface-annotation internal-name annotation-rhs names)
  (with-syntax ([(name name-extends tail-name
                       name? name-instance
                       internal-name? internal-name-instance
                       indirect-static-infos)
                 names])
    (append
     (if internal-name
         (with-syntax ([internal-name internal-name])
           (list
            #`(define-annotation-syntax internal-name (identifier-annotation (quote-syntax internal-name?)
                                                                             (quote-syntax ((#%dot-provider internal-name-instance)))))))
         null)
     (list
      (build-syntax-definition/maybe-extension
       'rhombus/annot #'name #'name-extends
       (if annotation-rhs
           #`(wrap-class-transformer name tail-name
                                     #,((make-syntax-introducer) annotation-rhs)
                                     make-annotation-prefix-operator
                                     "interface")
           #`(identifier-annotation (quote-syntax name?)
                                    (quasisyntax ((#%dot-provider name-instance)
                                                  . indirect-static-infos)))))))))

(define-for-syntax (build-interface-desc supers parent-names options
                                         method-mindex method-names method-vtable method-results method-private dots
                                         internal-name
                                         callable? indexable? setable? appendable?
                                         names)
  (with-syntax ([(name name-extends prop:name name-ref name-ref-or-error
                       prop:internal-name internal-name? internal-name-ref
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
                                         #,custom-annotation?
                                         #f
                                         (quote #,(build-quoted-private-method-list 'method method-private))
                                         (quote #,(build-quoted-private-method-list 'property method-private)))))
           null)
       (list
        (build-syntax-definition/maybe-extension
         'rhombus/class #'name #'name-extends
         #`(interface-desc (quote-syntax #,parent-names)
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
                               (if appendable? '(append) '()))
                           ;; ----------------------------------------
                           (quote-syntax name)
                           #,(and internal-name
                                  #`(quote-syntax #,internal-name))
                           (quote-syntax prop:name)
                           #,(and (syntax-e #'prop:internal-name)
                                  #'(quote-syntax prop:internal-name))
                           (quote-syntax name-ref-or-error)
                           #,custom-annotation?
                           #,(let ([id (if (syntax-e #'call-statinfo-indirect)
                                           #'call-statinfo-indirect
                                           #'super-call-statinfo-indirect)])
                               (if (and id (syntax-e id))
                                   #`(quote-syntax #,id)
                                   #f)))))))))
