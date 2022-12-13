#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest/syntax-local
                     enforest/hier-name-parse
                     "srcloc.rkt"
                     "name-path-op.rkt"
                     "introducer.rkt"
                     "tag.rkt"
                     "interface-parse.rkt"
                     (only-in "class-parse.rkt"
                              :options-block
                              in-class-desc-space
                              check-exports-distinct))
         (only-in "class.rkt"
                  [class rhombus-class])
         "forwarding-sequence.rkt"
         "definition.rkt"
         "expression.rkt"
         (submod "dot.rkt" for-dot-provider)
         (submod "annotation.rkt" for-class)
         "interface-clause.rkt"
         "interface-clause-parse.rkt"
         "dotted-sequence-parse.rkt"
         (only-meta-in 1
                       "class-method.rkt")
         "class-dot.rkt"
         "parse.rkt"
         (submod "namespace.rkt" for-exports))

(provide interface)

(define-syntax interface
  (definition-transformer
   (lambda (stxes)
     (syntax-parse stxes
       #:datum-literals (group block)
       [(_ name-seq::dotted-identifier-sequence options::options-block)
        #:with full-name::dotted-identifier #'name-seq
        #:with name #'full-name.name
        #:with orig-stx stxes
        (define body #'(options.form ...))
        (define finish-data #`[orig-stx base-stx #,(syntax-local-introduce #'scope-stx)
                                        full-name name])
        (cond
          [(null? (syntax-e body))
           #`((interface-finish #,finish-data ()))]
          [else
           #`((rhombus-mixed-nested-forwarding-sequence (interface-finish #,finish-data) rhombus-class
                                                        (interface-body-step . #,(syntax-local-introduce body))))])]))))

(define-syntax interface-body-step
  (lambda (stx)
    ;; parse the first form as a interface clause, if possible, otherwise assume
    ;; an expression or definition
    (syntax-parse stx
      [(_ form . rest)
       #:with clause::interface-clause (syntax-local-introduce #'form)
       (syntax-parse (syntax-local-introduce #'clause.parsed)
         #:datum-literals (group parsed)
         [((group (parsed p)) ...)
          #`(begin p ... (interface-body-step . rest))]
         [(g ...)
          #`(interface-body-step g ... . rest)])]
      [(_ form . rest)
       #`(rhombus-top-step
          interface-body-step
          #f
          form . rest)]
      [(_) #'(begin)])))

(define-syntax interface-finish
  (lambda (stx)
    (syntax-parse stx
      [(_ [orig-stx base-stx scope-stx
                    full-name name]
          exports
          option ...)
       (define stxes #'orig-stx)
       (define options (parse-options #'orig-stx #'(option ...)))
       (define internal-name (let ([id (hash-ref options 'internal #f)])
                               (and id
                                    ((make-syntax-delta-introducer #'scope-stx #'base-stx) id 'remove))))
       (define supers (interface-names->interfaces stxes (reverse (hash-ref options 'extends '()))))
       (define parent-names (map interface-desc-id supers))
       (define added-methods (reverse (hash-ref options 'methods '())))
       (define-values (method-mindex   ; symbol -> mindex
                       method-names    ; index -> symbol-or-identifier
                       method-vtable   ; index -> function-identifier or '#:abstract
                       method-results  ; symbol -> nonempty list of identifiers; first one implies others
                       method-private  ; symbol -> identifier
                       method-decls    ; symbol -> identifier, intended for checking distinct
                       abstract-name)  ; #f or identifier
         (extract-method-tables stxes added-methods #f supers #hasheq() #f))

       (define exs (parse-exports #'(combine-out . exports)))
       (check-exports-distinct stxes exs '() method-mindex)

       (define (temporary template #:name [name #'name])
         (and name
              ((make-syntax-introducer) (datum->syntax #f (string->symbol (format template (syntax-e name)))))))

       (with-syntax ([name? (temporary "~a?")]
                     [prop:name (temporary "prop:~a")]
                     [name-ref (temporary "~a-ref")]
                     [internal-name? (temporary "~a?" #:name internal-name)]
                     [prop-internal:name (temporary "prop:~a" #:name internal-name)]
                     [name-instance (temporary "~a-instance")]
                     [(super-name ...) parent-names]
                     [(export ...) exs])
         (with-syntax ([internal-name-ref (if internal-name
                                              (temporary "~a-ref" #:name internal-name)
                                              #'name-ref)])
           (define defns
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
              (build-interface-annotation internal-name
                                          #'(name name? name-instance
                                                  internal-name?))
              (build-interface-dot-handling method-mindex method-vtable
                                            #'(name name-instance name-ref
                                                    [export ...]))
              (build-interface-desc parent-names
                                    method-mindex method-names method-vtable method-results
                                    internal-name
                                    #'(name prop:name name-ref
                                            prop:internal-name internal-name? internal-name-ref))
               (build-method-results added-methods
                                     method-mindex method-vtable method-private
                                     method-results)))
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

(define-for-syntax (build-interface-annotation internal-name names)
  (with-syntax ([(name name? name-instance
                       internal-name?)
                 names])
    (append
     (if internal-name
         (with-syntax ([internal-name internal-name])
           (list
            #`(define-annotation-syntax internal-name (identifier-annotation (quote-syntax internal-name)
                                                                             (quote-syntax internal-name?)
                                                                             (quote-syntax ((#%dot-provider name-instance)))))))
         null)
     (list
      #`(define-annotation-syntax name (identifier-annotation (quote-syntax name)
                                                              (quote-syntax name?)
                                                              (quote-syntax ((#%dot-provider name-instance)))))))))

(define-for-syntax (build-interface-desc parent-names
                                         method-mindex method-names method-vtable method-results
                                         internal-name
                                         names)
  (with-syntax ([(name prop:name name-ref
                       prop:internal-name internal-name? internal-name-ref)
                 names])
    (let ([method-shapes (build-quoted-method-shapes method-vtable method-names method-mindex)]
          [method-map (build-quoted-method-map method-mindex)]
          [method-result-expr (build-method-result-expression method-results)])
      (append
       (if internal-name
           (list
            #`(define-syntax #,(in-class-desc-space internal-name)
                (interface-desc (quote-syntax name)
                                #f
                                '()
                                (quote-syntax prop:internal-name)
                                (quote-syntax internal-name-ref)
                                '#,method-shapes
                                (quote-syntax #,method-vtable)
                                '#,method-map
                                #,method-result-expr)))
           null)
       (list
        #`(define-syntax #,(in-class-desc-space #'name)
            (interface-desc (quote-syntax name)
                            #,(and internal-name
                                   #`(quote-syntax #,internal-name))
                            (quote-syntax #,parent-names)
                            (quote-syntax prop:name)
                            (quote-syntax internal-name-ref)
                            '#,method-shapes
                            (quote-syntax #,method-vtable)
                            '#,method-map
                            #,method-result-expr)))))))
