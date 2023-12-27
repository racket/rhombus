#lang racket/base
(require (for-syntax racket/base)
         syntax/parse/pre
         enforest/syntax-local
         "define-arity.rkt"
         "class-primitive.rkt"
         "name-root.rkt"
         "veneer-parse.rkt"
         (only-in "class-parse.rkt"
                  in-class-desc-space
                  objects-desc-method-shapes)
         (for-template
          (only-in "class-clause-parse.rkt"
                   class-clause-extract
                   method-shape-extract))
         "call-result-key.rkt"
         "realm.rkt")

(provide (for-space rhombus/namespace
                    veneer_meta))

(module+ for-class
  (provide veneer-expand-data))

(module+ for-static-info
  (provide (for-syntax veneer-data-static-infos)))

(define-name-root veneer_meta
  #:fields
  (Info
   [describe veneer_meta.describe]))

(define/method (veneer_meta.Info.lookup info key)
  (lookup who info key))

(define-primitive-class Info veneer-data
  #:new
  #:opaque
  #:fields
  ()
  #:properties
  ()
  #:methods
  ([lookup veneer_meta.Info.lookup]))

(define/arity (veneer_meta.describe id)
  #:static-infos ((#%call-result #,veneer-data-static-infos))
  (describe who id))

(struct veneer-expand-data veneer-data (stx accum-stx))
(struct veneer-describe-data veneer-data (desc include-private?))

(define (veneer-expand-data-internal-info-name data)
  (syntax-parse (veneer-expand-data-stx data)
    [(_ base-stx scope-stx
        for-together?
        name
        . _)
     #'name]))

(define (lookup who info key)
  (unless (veneer-data? info)
    (raise-argument-error* who rhombus-realm "veneer_meta.Info" info))
  (unless (symbol? key)
    (raise-argument-error* who rhombus-realm "Symbol" key))
  (case key
    [(name)
     (cond
       [(veneer-expand-data? info)
        (veneer-expand-data-internal-info-name info)]
       [else
        (veneer-desc-id (veneer-describe-data-desc info))])]
    [(method_names method_arities method_visibilities
                   property_names property_arities property_visibilities)
     (cond
       [(veneer-expand-data? info)
        (define r (class-clause-extract who (veneer-expand-data-accum-stx info) key))
        (case key
          [(uses_default_annotation) (null? r)]
          [else r])]
       [else
        (define desc (veneer-describe-data-desc info))
        (case key
          [(internal_names) null]
          [(extends) (syntax->list (veneer-desc-super-id desc))]
          [else
           (method-shape-extract (objects-desc-method-shapes desc)
                                 null
                                 null
                                 key)])])]
    [else
     (raise-arguments-error* who rhombus-realm
                             "unrecognized key symbol"
                             "symbol" key)]))

(define (describe who id)
  (unless (identifier? id)
    (raise-argument-error* who rhombus-realm "Identifier" id))
  (define desc (syntax-local-value* (in-class-desc-space id) veneer-desc-ref))
  (unless desc
    (raise-arguments-error* who rhombus-realm
                            "not bound as a veneer name"
                            "identifier" id))
  (veneer-describe-data desc #f))
