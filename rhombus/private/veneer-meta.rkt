#lang racket/base
(require (for-syntax racket/base)
         syntax/parse/pre
         enforest/syntax-local
         "define-arity.rkt"
         "class-primitive.rkt"
         (submod "annotation.rkt" for-class)
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
         "realm.rkt"
         "pack.rkt")

(provide (for-space rhombus/namespace
                    veneer_meta))

(module+ for-class
  (provide veneer-expand-data))

(module+ for-static-info
  (provide (for-syntax get-veneer-data-static-infos)))

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

(define-annotation-syntax Info
  (identifier-annotation veneer-data? #,(get-veneer-data-static-infos)))

(define/arity (veneer_meta.describe id)
  #:static-infos ((#%call-result #,(get-veneer-data-static-infos)))
  (describe who id))

(struct veneer-expand-data veneer-data (stx accum-stx))
(struct veneer-describe-data veneer-data (desc include-private?))

(define (veneer-expand-data-internal-info-name data)
  (syntax-parse (veneer-expand-data-stx data)
    [(_ base-stx scope-stx
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
    [(extends internal_names
              method_names method_arities method_visibilities
              property_names property_arities property_visibilities)
     (cond
       [(veneer-expand-data? info)
        (define r (class-clause-extract who (veneer-expand-data-accum-stx info) key))
        r]
       [else
        (define desc (veneer-describe-data-desc info))
        (case key
          [(internal_names) null]
          [(extends)
           (define super (veneer-desc-super-id desc))
           (if super (list super) null)]
          [else
           (method-shape-extract (objects-desc-method-shapes desc)
                                 null
                                 null
                                 key)])])]
    [else
     (raise-arguments-error* who rhombus-realm
                             "unrecognized key symbol"
                             "symbol" key)]))

(define (unpack-identifier who id-in)
  (define id (unpack-term/maybe id-in))
  (unless (identifier? id)
    (raise-argument-error* who rhombus-realm "Identifier" id-in))
  id)

(define (describe who id-in)
  (define id (unpack-identifier who id-in))
  (define desc (syntax-local-value* (in-class-desc-space id) veneer-desc-ref))
  (unless desc
    (raise-arguments-error* who rhombus-realm
                            "not bound as a veneer name"
                            "identifier" id))
  (veneer-describe-data desc #f))
