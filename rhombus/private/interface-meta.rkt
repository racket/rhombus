#lang racket/base
(require (for-syntax racket/base)
         syntax/parse/pre
         enforest/syntax-local
         "define-arity.rkt"
         "class-primitive.rkt"
         "name-root.rkt"
         "interface-parse.rkt"
         (only-in "class-parse.rkt"
                  in-class-desc-space)
         (for-template
          (only-in "class-clause-parse.rkt"
                   class-clause-extract
                   method-shape-extract))
         "call-result-key.rkt"
         "static-info.rkt"
         "realm.rkt")

(provide (for-space rhombus/namespace
                    interface_meta))

(module+ for-class
  (provide interface-expand-data))

(module+ for-static-info
  (provide (for-syntax interface-data-static-infos)))

(define-name-root interface_meta
  #:fields
  [Info
   describe])

(define/method (interface_meta.Info.lookup info key)
  (lookup who info key))

(define-primitive-class Info interface-data
  #:new
  #:opaque
  #:fields
  ()
  #:properties
  ()
  #:methods
  ([lookup interface_meta.Info.lookup]
   ))

(struct interface-expand-data interface-data (stx accum-stx))
(struct interface-describe-data interface-data (desc include-private?))

(define (interface-expand-data-internal-info-name data)
  (syntax-parse (interface-expand-data-stx data)
    [(_ base-stx scope-stx
        for-together?
        full-name name
        . _)
     #'name]))

(define (lookup who info key)
  (unless (interface-data? info)
    (raise-argument-error* who rhombus-realm "interface_meta.Info" info))
  (unless (symbol? key)
    (raise-argument-error* who rhombus-realm "Symbol" key))
  (case key
    [(name)
     (cond
       [(interface-expand-data? info)
        (interface-expand-data-internal-info-name info)]
       [else
        (interface-desc-id (interface-describe-data-desc info))])]
    [(extends internal_names
              uses_default_annotation
              method_names method_arities method_visibilities
              property_names property_arities property_visibilities)
     (cond
       [(interface-expand-data? info)
        (define r (class-clause-extract who (interface-expand-data-accum-stx info) key))
        (case key
          [(uses_default_annotation) (null? r)]
          [else r])]
       [else
        (define desc (interface-describe-data-desc info))
        (case key
          [(internal_names) null]
          [(extends) (syntax->list (interface-desc-super-ids desc))]
          [(uses_default_annotation) (not (interface-desc-custom-annotation? desc))]
          [else
           (method-shape-extract (interface-desc-method-shapes desc)
                                 (if (interface-internal-desc? desc)
                                     (interface-internal-desc-private-methods desc)
                                     null)
                                 (if (interface-internal-desc? desc)
                                     (interface-internal-desc-private-properties desc)
                                     null)
                                 key)])])]
    [else
     (raise-arguments-error* who rhombus-realm
                             "unrecognized key symbol"
                             "symbol" key)]))

(define (describe id)
  (define who 'interface_meta.describe)
  (unless (identifier? id)
    (raise-argument-error* who rhombus-realm "Identifier" id))
  (define desc (syntax-local-value* (in-class-desc-space id) interface-desc-ref))
  (unless desc
    (raise-arguments-error* who rhombus-realm
                            "not bound as a interface name or internal name"
                            "identifier" id))
  (interface-describe-data desc #f))

(define-static-info-syntax describe (#%call-result #,interface-data-static-infos))
