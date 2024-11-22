#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     rhombus/private/enforest
                     rhombus/private/introducer))

(provide define-doc-syntax)

(module+ for-class
  (provide (for-syntax
            in-doc-space

            doc-transformer?
            doc-transformer-extract-desc
            doc-transformer-extract-space-sym
            doc-transformer-extract-sort-order
            doc-transformer-extract-defined
            doc-transformer-extract-metavariables
            doc-transformer-extract-spacer-infos
            doc-transformer-extract-typeset)))

(begin-for-syntax
  (provide (property-out doc-transformer)
           make-doc-transformer)

  ;; For each function in a `doc-transformer`, a `*` means that the
  ;; value can be one of those, or it can be a list. A `+` means that
  ;; the value is always generaled to a list. Values must all be
  ;; either one of those of a list, and lists need to have the same
  ;; length. When a list is returned by `extract-space-sym`, only the
  ;; first element is bounced back sometimes, since bouncing back is
  ;; just a convenience for abstracting over those other handlers.
  (property doc-transformer (extract-desc       ; stx -> (kind-str)*
                             extract-space-sym  ; stx -> (space-sym-or-#f[-list])*  ; only first result bounced back, sometimes
                             extract-sort-order ; stx (space-sym-or-#f-list)+ -> (int)*
                             extract-defined    ; stx space-sym-or-#f[-list] -> (def-name)* ; see below
                             extract-metavariables ; stx space-sym-or-#f sym-set -> sym-set ; adds to given set
                             extract-spacer-infos ; stx (space-sym-or-#f[-list])+ -> (hash)*
                             extract-typeset))  ; stx (space-sym-or-#f[-list])* (stx -> stx)* -> stx
  ;; A `def-name` as returned by `extract-defined` is one of
  ;;  - identifier                 ; normal reference in relevant space
  ;;  - (list root-id typeset-id)  ; `typeset-id` within namespace `root-id`
  ;;  - (list root-id typeset-id key-str) ; ditto, but key for indexing is `key-str`, not raw-string form of `typeset-id`

  (define (make-doc-transformer #:extract-desc extract-desc 
                                #:extract-space-sym extract-space-sym
                                #:extract-sort-order [extract-sort-order (lambda (stx spcs) (map (lambda (spc) 100) spcs))]
                                #:extract-name extract-name
                                #:extract-metavariables [extract-metavariables
                                                         (lambda (stx space-name vars) vars)]
                                #:extract-spacer-infos [extract-spacer-infos (lambda (stx spcs) (map (lambda (spc) #f) spcs))]
                                #:extract-typeset extract-typeset)
    (doc-transformer extract-desc
                     extract-space-sym
                     extract-sort-order
                     extract-name
                     extract-metavariables
                     extract-spacer-infos
                     extract-typeset))

  (define in-doc-space (make-interned-syntax-introducer/add 'rhombus/doc)))

(define-syntax (define-doc-syntax stx)
  (syntax-parse stx
    [(_ id:identifier rhs)
     #`(define-syntax #,(in-doc-space #'id)
         rhs)]))
