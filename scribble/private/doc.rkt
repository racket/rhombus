#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/transformer
                     enforest/property
                     enforest/proc-name
                     rhombus/private/introducer))

(provide define-doc-syntax)

(module+ for-class
  (provide (for-syntax
            in-doc-space

            doc-transformer-extract-desc
            doc-transformer-extract-space-sym
            doc-transformer-extract-defined
            doc-transformer-extract-metavariables
            doc-transformer-extract-typeset)))

(begin-for-syntax
  (provide (property-out doc-transformer)
           make-doc-transformer)

  (property doc-transformer (extract-desc
                             extract-space-sym
                             extract-defined
                             extract-metavariables
                             extract-typeset))

  (define (make-doc-transformer #:extract-desc extract-desc 
                                #:extract-space-sym extract-space-sym
                                #:extract-name extract-name
                                #:extract-metavariables [extract-metavariables
                                                         (lambda (stx space-name vars) vars)]
                                #:extract-typeset extract-typeset)
    (doc-transformer extract-desc
                     extract-space-sym
                     extract-name
                     extract-metavariables
                     extract-typeset))

  (define in-doc-space (make-interned-syntax-introducer/add 'rhombus/doc)))

(define-syntax (define-doc-syntax stx)
  (syntax-parse stx
    [(_ id:identifier rhs)
     #`(define-syntax #,(in-doc-space #'id)
         rhs)]))
