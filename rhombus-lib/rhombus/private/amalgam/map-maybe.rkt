#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "provide.rkt"
         "class-primitive.rkt"
         (submod "annotation.rkt" for-class)
         (submod "dot.rkt" for-dot-provider)
         "static-info.rkt"
         "index-result-key.rkt"
         "index-key.rkt"
         "index-property.rkt"
         "call-result-key.rkt"
         "maybe-key.rkt"
         "realm.rkt"
         "define-arity.rkt")

(provide (for-spaces (rhombus/namespace
                      rhombus/annot)
                     MapMaybe))

(module+ for-map
  (provide (for-spaces (#f
                        rhombus/statinfo)
                       Map.maybe)
           MapMaybe.get/optimize
           (for-syntax extract-maybe-statinfo)))

(module+ for-print
  (provide map-maybe?
           map-maybe-ht))

(struct map-maybe (ht)
  #:property prop:indexable (lambda (mm k) (MapMaybe.get mm k))
  #:property prop:field-name->accessor (list* '()
                                              (hasheq 'get (lambda (mm) (lambda (k) (MapMaybe.get mm k))))
                                              #hasheq())
  #:reflection-name 'MapMaybe)

(define-primitive-class MapMaybe map-maybe
  #:lift-declaration
  #:no-constructor-static-info
  #:instance-static-info ((#%index-get MapMaybe.get/optimize))
  #:existing
  #:just-annot
  #:fields ()
  #:namespace-fields
  ()
  #:properties
  ()
  #:methods
  ([get MapMaybe.get]))

(define-for-syntax (extract-maybe-statinfo lhs-si)
  (define si (static-info-lookup lhs-si #'#%index-result))
  (define demaybed-si
    (cond
      [(not si) #f]
      [(static-info-lookup si #'#%maybe)
       => (lambda (maybe-si)
            (static-infos-and si
                              maybe-si))]
      [else si]))
  (if demaybed-si
      #`((#%index-result ((#%maybe #,demaybed-si)))
         #,@(get-map-maybe-static-infos))
      (get-map-maybe-static-infos)))

(define (check-readable-map who ht)
  (unless (hash? ht)
    (raise-annotation-failure who ht "ReadableMap")))

(define/arity (Map.maybe ht)
  #:static-infos ((#%call-result #,(get-map-maybe-static-infos)))
  (check-readable-map who ht)
  (map-maybe ht))

(define/method (MapMaybe.get mm k)
  (unless (map-maybe? mm) (raise-annotation-failure who mm "MapMaybe"))
  (hash-ref (map-maybe-ht mm) k #f))

(define (hash-ref/map-maybe ht key)
  (if (hash? ht)
      (hash-ref ht key #f)
      (check-readable-map 'Map.maybe ht)))

(define-syntax (MapMaybe.get/optimize stx)
  (syntax-parse stx
    [(_ mm key)
     (syntax-parse (unwrap-static-infos #'mm)
       #:literals (Map.maybe)
       [(Map.maybe ht) #`(hash-ref/map-maybe
                          #,(discard-static-infos #'ht)
                          #,(discard-static-infos #'key))]
       [_ (datum->syntax stx (list #'MapMaybe.get #'mm #'key) stx stx)])]
    [(_ . args) (datum->syntax stx (cons #'MapMaybe.get #'args) stx stx)]
    [_ (datum->syntax #'here 'MapMaybe.get stx stx)]))
