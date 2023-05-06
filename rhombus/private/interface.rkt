#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/syntax-local
                     enforest/hier-name-parse
                     "srcloc.rkt"
                     "name-path-op.rkt"
                     "introducer.rkt"
                     "tag.rkt"
                     "interface-parse.rkt"
                     (submod "interface-meta.rkt" for-class)
                     (only-in "class-parse.rkt"
                              :options-block
                              in-class-desc-space
                              check-exports-distinct
                              check-fields-methods-dots-distinct))
         "forwarding-sequence.rkt"
         "definition.rkt"
         "expression.rkt"
         (submod "dot.rkt" for-dot-provider)
         (submod "annotation.rkt" for-class)
         (submod "annot-macro.rkt" for-class)
         "interface-clause.rkt"
         "interface-clause-parse.rkt"
         "class-top-level.rkt"
         "class-together-parse.rkt"
         "class-clause-tag.rkt"
         "dotted-sequence-parse.rkt"
         (for-syntax "class-transformer.rkt")
         (only-meta-in 1
                       "class-method.rkt")
         (only-in "class-annotation.rkt"
                  build-extra-internal-id-aliases)
         "class-dot.rkt"
         "parse.rkt"
         (submod "namespace.rkt" for-exports))

(provide interface)

(module+ for-together
  (provide interface_for_together))

(define-syntax interface
  (definition-transformer
    (lambda (stxes)
      (parse-interface stxes))))

(define-syntax interface_for_together
  (definition-transformer
    (lambda (stxes)
      (parse-interface stxes #t))))

(define-for-syntax (parse-interface stxes [for-together? #f])
  (syntax-parse stxes
    #:datum-literals (group block)
    [(_ name-seq::dotted-identifier-sequence options::options-block)
     #:with full-name::dotted-identifier #'name-seq
     #:with name #'full-name.name
     #:with orig-stx stxes
     (define body #'(options.form ...))
     ;; The shape of `finish-data` is recognzied in `interface-annotation+finish`
     ;; and "interface-meta.rkt"
     (define finish-data #`([orig-stx base-stx #,(syntax-local-introduce #'scope-stx)
                                      #,for-together?
                                      full-name name]
                            ;; data accumulated from parsed clauses:
                            ()))
     (define interface-data-stx #f)
     #`(#,(cond
            [(null? (syntax-e body))
             #`(interface-annotation+finish #,finish-data ())]
            [else
             #`(rhombus-mixed-nested-forwarding-sequence (interface-annotation+finish #,finish-data) rhombus-class
                                                         (interface-body-step (#,interface-data-stx ()) . #,(syntax-local-introduce body)))]))]))

(define-syntax interface-body-step
  (lambda (stx)
    ;; parse the first form as a interface clause, if possible, otherwise assume
    ;; an expression or definition
    (syntax-parse stx
      [(_ (data accum) form . rest)
       #:with (~var clause (:interface-clause (interface-expand-data #'data #'accum))) (syntax-local-introduce #'form)
       (syntax-parse (syntax-local-introduce #'clause.parsed)
         #:datum-literals (group parsed)
         [((group (parsed p)) ...)
          #:with (new-accum ...) (class-clause-accum #'(p ...))
          #`(begin p ... (interface-body-step (data (new-accum ... . accum)) . rest))]
         [(g ...)
          #`(interface-body-step (data accum) g ... . rest)])]
      [(_ data+accum form . rest)
       #`(rhombus-top-step
          interface-body-step
          #f
          (data+accum)
          form . rest)]
      [(_ data+accum) #'(begin)])))

(define-syntax interface-annotation+finish
  (lambda (stx)
    (syntax-parse stx
      [(_ ([orig-stx base-stx scope-stx
                     for-together?
                     full-name name]
           . _)
          exports
          option ...)
       (define stxes #'orig-stx)
       (define options (parse-annotation-options #'orig-stx #'(option ...)))

       (define-values (unexposed-internal-name internal-name extra-internal-names)
         (extract-internal-ids options
                               #'scope-stx #'base-stx
                               #'orig-stx))
                                    
       (define annotation-rhs (hash-ref options 'annotation-rhs #f))

       (define (temporary template #:name [name #'name])
         (and name
              ((make-syntax-introducer) (datum->syntax #f (string->symbol (format template (syntax-e name)))))))
       
       (with-syntax ([name? (temporary "~a?")]
                     [name-instance (temporary "~a-instance")]
                     [internal-name? (temporary "~a?" #:name internal-name)]
                     [internal-name-instance (if internal-name
                                                 (temporary "~a-instance" #:name internal-name)
                                                 #'name-instance)])
         (wrap-for-together
          #'for-together?
          #`(begin
              #,@(build-interface-annotation internal-name
                                             annotation-rhs
                                             #'(name name? name-instance
                                                     internal-name? internal-name-instance))
              #,@(build-extra-internal-id-aliases internal-name extra-internal-names)
              (interface-finish [orig-stx base-stx scope-stx
                                          full-name name
                                          name? name-instance
                                          #,internal-name internal-name? internal-name-instance]
                                exports
                                option ...))))])))

(define-syntax interface-finish
  (lambda (stx)
    (syntax-parse stx
      [(_ [orig-stx base-stx scope-stx
                    full-name name
                    name? name-instance
                    maybe-internal-name internal-name? internal-name-instance]
          exports
          option ...)
       (define stxes #'orig-stx)
       (define options (parse-options #'orig-stx #'(option ...)))
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

       (define exs (parse-exports #'(combine-out . exports)))
       (check-exports-distinct stxes exs '() method-mindex dots)

       (define internal-name (let ([id #'maybe-internal-name])
                               (and (syntax-e id) id)))

       (define expression-macro-rhs (hash-ref options 'expression-macro-rhs #f))

       (define parent-dot-providers
         (for/list ([parent (in-list supers)]
                    #:do [(define dp (interface-desc-dot-provider parent))]
                    #:when dp)
           dp))

       (define (temporary template #:name [name #'name])
         (and name
              ((make-syntax-introducer) (datum->syntax #f (string->symbol (format template (syntax-e name)))))))

       (with-syntax ([prop:name (temporary "prop:~a")]
                     [name-ref (temporary "~a-ref")]
                     [prop-internal:name (temporary "prop:~a" #:name internal-name)]
                     [(super-name ...) parent-names]
                     [(export ...) exs]
                     [(dot-id ...) (map car dots)]
                     [dot-provider-name (or (and (or (pair? dot-provider-rhss)
                                                     ((length parent-dot-providers) . > . 1))
                                                 (temporary "dot-provider-~a"))
                                            (and (pair? parent-dot-providers)
                                                 (car parent-dot-providers)))])
         (with-syntax ([internal-name-ref (if internal-name
                                              (temporary "~a-ref" #:name internal-name)
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
                              #'(name name-instance name?
                                      []
                                      []
                                      []
                                      []
                                      []
                                      []
                                      [super-name ...]))
               (build-interface-property internal-name
                                         #'(name prop:name name? name-ref
                                                 prop:internal-name internal-name? internal-name-ref))
               (build-interface-dot-handling method-mindex method-vtable method-results
                                             internal-name
                                             expression-macro-rhs dot-provider-rhss parent-dot-providers
                                             #'(name name? name-instance name-ref
                                                     internal-name-instance internal-name-ref
                                                     dot-provider-name [dot-id ...]
                                                     [export ...]))
               (build-interface-desc parent-names options
                                     method-mindex method-names method-vtable method-results method-private dots
                                     internal-name
                                     #'(name prop:name name-ref
                                             prop:internal-name internal-name? internal-name-ref
                                             dot-provider-name))
               (build-method-results added-methods
                                     method-mindex method-vtable method-private
                                     method-results
                                     #f))))
           #`(begin . #,defns)))])))

(define-for-syntax (build-interface-property internal-name names)
  (with-syntax ([(name prop:name name? name-ref
                       prop:internal-name internal-name? internal-name-ref)
                 names])
    (append
     (if internal-name
         (list
          #`(define-values (prop:internal-name internal-name? internal-name-ref)
              (make-struct-type-property 'name)))
         null)
     (list
      #`(define-values (prop:name name? name-ref)
          (make-struct-type-property 'name
                                     #,@(if internal-name
                                            #`(#f (list (cons prop:internal-name
                                                              (lambda (vt) vt))))
                                            '())))))))

(define-for-syntax (build-interface-annotation internal-name annotation-rhs names)
  (with-syntax ([(name name? name-instance
                       internal-name? internal-name-instance)
                 names])
    (append
     (if internal-name
         (with-syntax ([internal-name internal-name])
           (list
            #`(define-annotation-syntax internal-name (identifier-annotation (quote-syntax internal-name?)
                                                                             (quote-syntax ((#%dot-provider internal-name-instance)))))))
         null)
     (if annotation-rhs
         (list
          #`(define-annotation-syntax name
              (wrap-class-transformer name
                                      #,((make-syntax-introducer) annotation-rhs)
                                      make-annotation-prefix-operator
                                      "interface")))
         (list
          #`(define-annotation-syntax name (identifier-annotation (quote-syntax name?)
                                                                  (quote-syntax ((#%dot-provider name-instance))))))))))
  
(define-for-syntax (build-interface-desc parent-names options
                                         method-mindex method-names method-vtable method-results method-private dots
                                         internal-name
                                         names)
  (with-syntax ([(name prop:name name-ref
                       prop:internal-name internal-name? internal-name-ref
                       dot-provider-name)
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
                (interface-internal-desc (quote-syntax name)
                                         #f
                                         (quote-syntax #,parent-names)
                                         (quote-syntax prop:internal-name)
                                         (quote-syntax internal-name-ref)
                                         '#,method-shapes
                                         (quote-syntax #,method-vtable)
                                         '#,method-map
                                         #,method-result-expr
                                         #,custom-annotation?
                                         '()
                                         #f
                                         (quote #,(build-quoted-private-method-list 'method method-private))
                                         (quote #,(build-quoted-private-method-list 'property method-private)))))
           null)
       (list
        #`(define-syntax #,(in-class-desc-space #'name)
            (interface-desc (quote-syntax name)
                            #,(and internal-name
                                   #`(quote-syntax #,internal-name))
                            (quote-syntax #,parent-names)
                            (quote-syntax prop:name)
                            (quote-syntax name-ref)
                            '#,method-shapes
                            (quote-syntax #,method-vtable)
                            '#,method-map
                            #,method-result-expr
                            #,custom-annotation?
                            '#,(map car dots)
                            #,(and (syntax-e #'dot-provider-name)
                                   #'(quote-syntax dot-provider-name)))))))))
