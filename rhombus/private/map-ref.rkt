#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/syntax-local
                     "srcloc.rkt"
                     "statically-str.rkt")
         "provide.rkt"
         "expression.rkt"
         "repetition.rkt"
         "parse.rkt"
         (submod "map.rkt" for-build)
         "map-ref-set-key.rkt"
         "ref-result-key.rkt"
         "ref-indirect-key.rkt"
         "call-result-key.rkt"
         "static-info.rkt"
         (submod "assign.rkt" for-assign)
         "op-literal.rkt"
         (only-in "string.rkt"
                  +&)
         (submod "set.rkt" for-ref)
         (submod "set.rkt" for-build)
         "repetition.rkt"
         "compound-repetition.rkt"
         "realm.rkt"
         "ref-property.rkt")

(provide ++)

(module+ for-ref
  (provide (for-syntax parse-map-ref-or-set)))

(module+ for-dynamic-static
  (provide (for-spaces (#f
                        rhombus/repet)
                       ++
                       static-++)))

(define-for-syntax (parse-map-ref-or-set map-in stxes more-static?
                                         #:repetition? [repetition? #f])
  (define map (if repetition?
                  map-in
                  (rhombus-local-expand map-in)))
  (define who '|[]|)
  (define (not-static) (string-append "specialization not known" statically-str))
  (syntax-parse stxes
    #:datum-literals (brackets op)
    [(_ ((~and head brackets) index) . assign-tail)
     #:when (not repetition?)
     #:with assign::assign-op-seq #'assign-tail
     (define op (attribute assign.op))
     (define map-set!-id (or (syntax-local-static-info/indirect map #'#%map-set! #'#%set-indirect)
                             (if more-static?
                                 (raise-syntax-error who (not-static) map-in)
                                 #'map-set!)))
     (define map-ref-id (or (syntax-local-static-info/indirect map #'#%map-ref #'#%ref-indirect)
                            (if more-static?
                                (raise-syntax-error who (not-static) map-in)
                                #'map-ref)))
     (local-require enforest/operator)
     (define-values (assign-expr tail) (build-assign
                                        op
                                        #'assign.name
                                        #`(lambda () (#,map-ref-id map-v index-v))
                                        #`(lambda (v) (#,map-set!-id map-v index-v v))
                                        #'map
                                        #'assign.tail))
     (values #`(let ([map-v #,map]
                     [index-v (rhombus-expression index)])
                 #,assign-expr)
             tail)]
    [(_ ((~and head brackets) index) . tail)
     (define (build-ref map index map-static-info)
       (define map-ref-id (or (static-info/indirect map-static-info #'#%map-ref #'#%ref-indirect)
                              (if more-static?
                                  (raise-syntax-error who (not-static) map-in)
                                  #'map-ref)))
       (define e (datum->syntax (quote-syntax here)
                                (list map-ref-id map index)
                                (span-srcloc map #'head)
                                #'head))
       (define result-static-infos (or (static-info/indirect map-static-info #'#%ref-result #'#%ref-indirect)
                                       (syntax-local-static-info map-ref-id #'#%call-result)
                                       #'()))
       (values e result-static-infos))
     (cond
       [repetition?
        (syntax-parse #'index
          [rep::repetition
           #:with map-info::repetition-info map
           (values
            (build-compound-repetition #'head (list map #'rep.parsed)
                                       (lambda (map index)
                                         (build-ref map
                                                    index
                                                    (lambda (key)
                                                      (repetition-static-info-lookup #'map-info.element-static-infos key)))))
            #'tail)])]
       [else
        (define-values (e result-static-infos)
          (build-ref map
                     #'(rhombus-expression index)
                     (lambda (key)
                       (syntax-local-static-info map key))))
        (values (wrap-static-info* e result-static-infos)
                #'tail)])]))

(begin-for-syntax
  (define (syntax-local-static-info/indirect e key indirect-key)
    (or (syntax-local-static-info e key)
        (let ([id (syntax-local-static-info e indirect-key)])
          (and id
               (syntax-local-static-info/indirect id key indirect-key)))))
  (define (static-info/indirect map-static-info key indirect-key)
    (or (map-static-info key)
        (let ([id (map-static-info indirect-key)])
          (and id
               (syntax-local-static-info/indirect id key indirect-key))))))

(define (map-ref map index)
  (cond
    [(vector? map) (vector-ref map index)]
    [(list? map) (list-ref map index)]
    [(hash? map) (hash-ref map index)]
    [(set? map) (hash-ref (set-ht map) index #f)]
    [(refable-ref map #f) => (lambda (ref) (ref map index))]
    [(string? map) (string-ref map index)]
    [(bytes? map) (bytes-ref map index)]
    [else
     (raise-argument-error* 'ref rhombus-realm "_Refable" map)]))

(define (map-set! map index val)
  (cond
    [(and (vector? map) (not (immutable? map))) (vector-set! map index val)]
    [(and (hash? map) (not (immutable? map))) (hash-set! map index val)]
    [(and (set? map) (not (immutable? (set-ht map)))) (if val
                                                          (hash-set! (set-ht map) index #t)
                                                          (hash-remove! (set-ht map) index))]
    [(setable-ref map #f) => (lambda (set) (set map index val))]
    [(and (string? map) (not (immutable? map))) (string-set! map index val)]
    [(and (bytes? map) (not (immutable? map))) (bytes-set! map index val)]
    [else
     (raise-argument-error* 'assign rhombus-realm "_MutableRefable" map)]))

(define-for-syntax (make-++-expression name static?)
  (expression-infix-operator
   name
   `((,(expr-quote +&) . same))
   'automatic
   (lambda (form1-in form2 stx)
     (define form1 (rhombus-local-expand form1-in))
     (define append-id (or (syntax-local-static-info form1 #'#%map-append)
                           (if static?
                               (raise-syntax-error '++ (string-append "specialization not known" statically-str) form1-in)
                               #'map-append)))
     (define si (or (syntax-local-static-info append-id #'#%call-result) #'()))
     (wrap-static-info*
      (datum->syntax (quote-syntax here)
                     (list append-id form1 form2)
                     (span-srcloc form1 form2)
                     stx)
      si))
   'left))

(define-syntax ++ (make-++-expression (expr-quote ++) #f))
(define-syntax static-++ (make-++-expression (expr-quote static-++) #t))

(define-for-syntax (make-++-repetition name static?)
  (repetition-infix-operator
   name
   `((,(expr-quote +&) . same))
   'automatic
   (lambda (form1-in form2 stx)
     (raise-syntax-error #f "not yet ready" stx))
   'left))

(define-repetition-syntax ++ (make-++-repetition (expr-quote ++) #f))
(define-repetition-syntax static-++ (make-++-repetition (expr-quote static-++) #t))

(define (map-append map1 map2)
  (cond
    [(vector? map1) (raise-arguments-error* '++ rhombus-realm
                                            "cannot extend a plain array"
                                            "array" map1
                                            "other value" map2)]
    [(list? map1) (cond
                    [(list? map2) (append map1 map2)]
                    [(vector? map2) (append map1 (vector->list map2))]
                    [else (raise-arguments-error* '++ rhombus-realm
                                                  "cannot append a list and other value"
                                                  "list" map1
                                                  "other value" map2)])]
    [(hash? map1) (cond
                    [(hash? map2) (hash-append/proc map1 map2)]
                    [else (raise-arguments-error* '++ rhombus-realm
                                                  "cannot append a hash map and other value"
                                                  "hash map" map1
                                                  "other value" map2)])]
    [(set? map1) (cond
                   [(set? map2) (set-append/proc map1 map2)]
                   [else (raise-arguments-error* '++ rhombus-realm
                                                 "cannot append a set and other value"
                                                 "set" map1
                                                 "other value" map2)])]
    [(string? map1) (cond
                      [(string? map2) (string-append-immutable map1 map2)]
                      [else (raise-arguments-error* '++ rhombus-realm
                                                    "cannot append a string and other value"
                                                    "string" map1
                                                    "other value" map2)])]
    [(bytes? map1) (cond
                     [(bytes? map2) (bytes-append map1 map2)]
                     [else (raise-arguments-error* '++ rhombus-realm
                                                   "cannot append a byte string and other value"
                                                   "byte string" map1
                                                   "other value" map2)])]
    [else (raise-argument-error* '++ rhombus-realm "List || Array || Map || Set || String" map1)]))
