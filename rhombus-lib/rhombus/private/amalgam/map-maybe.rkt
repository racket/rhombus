#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "annot-context.rkt")
         "provide.rkt"
         "class-primitive.rkt"
         (submod "annotation.rkt" for-class)
         (submod "dot.rkt" for-dot-provider)
         "static-info.rkt"
         "index-result-key.rkt"
         "index-key.rkt"
         "index-property.rkt"
         "call-result-key.rkt"
         "sequence-element-key.rkt"
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
  ([expect_of MapMaybe.expect_of])
  #:properties
  ()
  #:methods
  ([get MapMaybe.get]))

(define-syntax (map-maybe-of-static-infos data static-infoss)
  #`((#%index-result ((#%maybe #,(cadr static-infoss))))))

(define-annotation-constructor (MapMaybe/again MapMaybe.expect_of)
  ()
  #'map-maybe? #,(get-map-maybe-static-infos)
  1
  #f
  (lambda (predicate-stxs)
    #`(let ([pred #,(car predicate-stxs)])
        (lambda (arg)
          (for/and ([e (in-hash (map-maybe-ht arg))])
            (pred e)))))
  #'map-maybe-of-static-infos #f
  #'map-maybe-build-convert #'())

(define-syntax (map-build-convert arg-id build-convert-stxs kws data)
  arg-id)

(define-for-syntax (do-extract-maybe-statinfo lhs-si)
  (define si (static-info-lookup lhs-si #'#%index-result))
  (cond
    [(not si) #f]
    [(static-info-lookup si #'#%maybe)
     => (lambda (maybe-si)
          (static-infos-and si
                            maybe-si))]
    [else si]))

(define-for-syntax (extract-maybe-statinfo lhs-si)
  (define demaybed-si (do-extract-maybe-statinfo lhs-si))
  (if demaybed-si
      #`((#%index-result ((#%maybe #,demaybed-si)))
         #,@(get-map-maybe-static-infos))
      (get-map-maybe-static-infos)))

(define (check-readable-map who ht)
  (unless (hash? ht)
    (raise-annotation-failure who ht "ReadableMap")))

(define-syntax (select-elem-as-maybe data deps)
  (define args (annotation-dependencies-args deps))
  (define map-i 0)
  (define si
    (or (do-extract-maybe-statinfo (or (and (< map-i (length args))
                                            (list-ref args map-i))
                                       #'()))
        #'()))
  (cond
    [(static-infos-empty? si) #'()]
    [else #`((#%index-result ((#%maybe #,si))))]))

(define-syntax (select-elem data deps)
  (define args (annotation-dependencies-args deps))
  (define mm-i 0)
  (or (static-info-lookup (or (and (< mm-i (length args))
                                   (list-ref args mm-i))
                              #'())
                          #'#%index-result)
      #'()))

(define/arity (Map.maybe ht)
  #:static-infos ((#%call-result ((#%dependent-result (select-elem-as-maybe #f))
                                  #,@(get-map-maybe-static-infos))))
  (check-readable-map who ht)
  (map-maybe ht))

(define/method (MapMaybe.get mm k)
  #:static-infos ((#%call-result ((#%dependent-result (select-elem #f)))))
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
