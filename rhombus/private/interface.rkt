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
                              in-class-desc-space))
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
         "parse.rkt")

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
           #`((interface-finish #,finish-data))]
          [else
           #`((rhombus-mixed-forwarding-sequence (interface-finish #,finish-data) rhombus-class
                                                 (interface-body-step . #,(syntax-local-introduce body))))])]))))

(define-syntax interface-body-step
  (lambda (stx)
    ;; parse the first form as a interface clause, if possible, otherwise assume
    ;; an expression or definition
    (syntax-parse stx
      [(_ form . rest)
       #:with clause::interface-clause (syntax-local-introduce #'form)
       #:with (parsed ...) (syntax-local-introduce #'clause.parsed)
       #`(begin
           parsed
           ...
           (interface-body-step . rest))]
      [(_ form . rest)
       #`(begin
           (rhombus-definition form)
           (interface-body-step . rest))]
      [(_) #'(begin)])))

(define-syntax interface-finish
  (lambda (stx)
    (syntax-parse stx
      [(_ [orig-stx base-stx scope-stx
                    full-name name]
          option ...)
       (define stxes #'orig-stx)
       (define options (parse-options #'orig-stx #'(option ...)))
       (define supers (interface-names->interfaces stxes (reverse (hash-ref options 'extends '()))))
       (define parent-names (map interface-desc-id supers))
       (define added-methods (reverse (hash-ref options 'methods '())))
       (define-values (method-map      ; symbol -> index (non-final) or box-of-index (final)
                       method-names    ; index -> symbol-or-identifier
                       method-vtable   ; index -> accessor-identifier or '#:unimplemented
                       method-private  ; symbol -> identifier
                       method-decls    ; symbol -> identifier, intended for checking distinct
                       unimplemented-name) ; #f or identifier
         (build-method-map stxes added-methods #f supers))

       (define (temporary template)
         ((make-syntax-introducer) (datum->syntax #f (string->symbol (format template (syntax-e #'name))))))
    
       (with-syntax ([name? (temporary "~a?")]
                     [interface:name (temporary "interface:~a")]
                     [name-ref (temporary "~a-ref")]
                     [name-instance (temporary "~a-instance")]
                     [(super-name ...) parent-names])
         (define defns
           (append
            (build-methods added-methods method-map method-names method-private
                           #'(name name-instance
                                   []
                                   []
                                   []
                                   []
                                   []
                                   [super-name ...]))
            (build-interface #'(name interface:name name? name-ref))
            (build-interface-annotation #'(name name? name-instance))
            (build-interface-dot-handling #'(name name-instance))
            (build-interface-desc parent-names
                                  method-map method-names method-vtable
                                  #'(name interface:name name-ref))))
         #`(begin . #,defns))])))

(define-for-syntax (build-interface names)
  (with-syntax ([(name interface:name name? name-ref)
                 names])
    (list
     #`(define-values (interface:name name? name-ref)
         (make-struct-type-property 'name)))))

(define-for-syntax (build-interface-annotation names)
  (with-syntax ([(name name? name-instance)
                 names])
    (list
     #`(define-annotation-syntax name (identifier-annotation #'name #'name?
                                                             (quote-syntax ((#%dot-provider name-instance))))))))
  
(define-for-syntax (build-interface-desc parent-names
                                         method-map method-names method-vtable
                                         names)
  (with-syntax ([(name interface:name name-ref)
                 names])
    (list
     #`(define-syntax #,(in-class-desc-space #'name)
         (interface-desc (quote-syntax name)
                         (quote-syntax #,parent-names)
                         (quote-syntax interface:name)
                         (quote-syntax name-ref)
                         '#,(for/vector ([i (in-range (vector-length method-vtable))])
                              (define name (hash-ref method-names i))
                              (if (box? (hash-ref method-map (if (syntax? name) (syntax-e name) name)))
                                  (box name)
                                  name))
                         (quote-syntax #,method-vtable)
                         '#,method-map)))))
