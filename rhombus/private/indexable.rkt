#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/syntax-local
                     "srcloc.rkt"
                     "statically-str.rkt"
                     "interface-parse.rkt")
         "provide.rkt"
         "expression.rkt"
         "repetition.rkt"
         (submod "annotation.rkt" for-class)
         "parse.rkt"
         (submod "map.rkt" for-build)
         "index-key.rkt"
         "index-result-key.rkt"
         "index-indirect-key.rkt"
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
         "index-property.rkt"
         "mutability.rkt"
         (only-in "class-desc.rkt" define-class-desc-syntax)
         (only-in "class-method-result.rkt" method-result))

(provide (for-spaces (rhombus/class
                      rhombus/annot)
                     Indexable
                     MutableIndexable)
         ++)

(module+ for-ref
  (provide (for-syntax parse-indexable-ref-or-set)))

(module+ for-dynamic-static
  (provide (for-spaces (#f
                        rhombus/repet)
                       ++
                       static-++)))

(define-values (prop:Indexable Indexable? Indexable-ref)
  (make-struct-type-property 'Indexable
                             #f
                             ;; could have `prop:index` in this list, but
                             ;; direct dispatch is set up in `class` when
                             ;; the interface is implemented, and that
                             ;; picks up the right static info
                             (list)))

(define-values (prop:MutableIndexable MutableIndexable? MutableIndexable-ref)
  (make-struct-type-property 'Indexable
                             #f
                             ;; similarly, could have `prop:setable` in
                             ;; this list, but direct dispatch is better
                             (list)))

(define-annotation-syntax Indexable
  (identifier-annotation #'indexable? #'((#%index-get indexable-index))))
(define (indexable? v)
  (or (Indexable? v)
      (hash? v)
      (list? v)
      (vector? v)
      (set? v)
      (string? v)
      (bytes? v)))

(define-class-desc-syntax Indexable
  (interface-desc #'Indexable
                  #'Indexable
                  #'()
                  #'prop:Indexable
                  #'prop:Indexable
                  #'Indexable-ref
                  '#(#&get)
                  #'#(#:abstract)
                  (hasheq 'get 0)
                  #hasheq()
                  #t
                  '()
                  #f
                  #'()
                  '(get)))

(define-annotation-syntax MutableIndexable
  (identifier-annotation #'mutable-indexable? #'((#%index-get indexable-index)
                                                 (#%index-set indexable-set!))))
(define (mutable-indexable? v)
  (or (MutableIndexable? v)
      (mutable-hash? v)
      (mutable-vector? v)
      (and (set? v) (mutable-hash? (set-ht v)))
      (mutable-string? v)
      (mutable-bytes? v)))

(define-class-desc-syntax MutableIndexable
  (interface-desc #'MutableIndexable
                  #'MutableIndexable
                  #'(Indexable)
                  #'prop:MutableIndexable
                  #'prop:MutableIndexable
                  #'MutableIndexable-ref
                  '#(#&get #&set)
                  #'#(#:abstract #:abstract)
                  (hasheq 'get 0
                          'set 1)
                  (hasheq 'set #'void-result)
                  #t
                  '()
                  #f
                  #'()
                  '(get set)))

(define-syntax void-result
  (method-result #'void? #t #'() 0))


(define-for-syntax (parse-indexable-ref-or-set indexable-in stxes more-static?
                                               #:repetition? [repetition? #f])
  (define indexable (if repetition?
                        indexable-in
                        (rhombus-local-expand indexable-in)))
  (define who '|[]|)
  (define (not-static) (string-append "specialization not known" statically-str))
  (syntax-parse stxes
    #:datum-literals (brackets op)
    [(_ ((~and head brackets) index) . assign-tail)
     #:when (not repetition?)
     #:with assign::assign-op-seq #'assign-tail
     (define op (attribute assign.op))
     (define indexable-set!-id (or (syntax-local-static-info/indirect indexable #'#%index-set #'#%index-set-indirect)
                                   (if more-static?
                                       (raise-syntax-error who (not-static) indexable-in)
                                       #'indexable-set!)))
     (define indexable-ref-id (or (syntax-local-static-info/indirect indexable #'#%index-get #'#%index-get-indirect)
                                  (if more-static?
                                      (raise-syntax-error who (not-static) indexable-in)
                                      #'indexable-index)))
     (local-require enforest/operator)
     (define-values (assign-expr tail) (build-assign
                                        op
                                        #'assign.name
                                        #`(lambda () (#,indexable-ref-id indexable-v index-v))
                                        #`(lambda (v) (#,indexable-set!-id indexable-v index-v v))
                                        #'indexable
                                        #'assign.tail))
     (values #`(let ([indexable-v #,indexable]
                     [index-v (rhombus-expression index)])
                 #,assign-expr)
             tail)]
    [(_ ((~and head brackets) index) . tail)
     (define (build-ref indexable index indexable-static-info)
       (define indexable-ref-id (or (static-info/indirect indexable-static-info #'#%index-get #'#%index-get-indirect)
                                    (if more-static?
                                        (raise-syntax-error who (not-static) indexable-in)
                                        #'indexable-index)))
       (define e (datum->syntax (quote-syntax here)
                                (list indexable-ref-id indexable index)
                                (span-srcloc indexable #'head)
                                #'head))
       (define result-static-infos (or (static-info/indirect indexable-static-info #'#%index-result #'#%index-get-indirect)
                                       (syntax-local-static-info indexable-ref-id #'#%call-result)
                                       #'()))
       (values e result-static-infos))
     (cond
       [repetition?
        (syntax-parse #'index
          [rep::repetition
           #:with indexable-info::repetition-info indexable
           (values
            (build-compound-repetition #'head (list indexable #'rep.parsed)
                                       (lambda (indexable index)
                                         (build-ref indexable
                                                    index
                                                    (lambda (key)
                                                      (repetition-static-info-lookup #'indexable-info.element-static-infos key)))))
            #'tail)])]
       [else
        (define-values (e result-static-infos)
          (build-ref indexable
                     #'(rhombus-expression index)
                     (lambda (key)
                       (syntax-local-static-info indexable key))))
        (values (wrap-static-info* e result-static-infos)
                #'tail)])]))

(begin-for-syntax
  (define (syntax-local-static-info/indirect e key indirect-key)
    (or (syntax-local-static-info e key)
        (let ([id (syntax-local-static-info e indirect-key)])
          (and id
               (syntax-local-static-info/indirect id key indirect-key)))))
  (define (static-info/indirect indexable-static-info key indirect-key)
    (or (indexable-static-info key)
        (let ([id (indexable-static-info indirect-key)])
          (and id
               (syntax-local-static-info/indirect id key indirect-key))))))

(define (indexable-index indexable index)
  (cond
    [(vector? indexable) (vector-ref indexable index)]
    [(list? indexable) (list-ref indexable index)]
    [(hash? indexable) (hash-ref indexable index)]
    [(set? indexable) (hash-ref (set-ht indexable) index #f)]
    [(indexable-ref indexable #f) => (lambda (ref) (ref indexable index))]
    [(string? indexable) (string-ref indexable index)]
    [(bytes? indexable) (bytes-ref indexable index)]
    [else
     (raise-argument-error* 'ref rhombus-realm "Indexable" indexable)]))

(define (indexable-set! indexable index val)
  (cond
    [(and (vector? indexable) (not (immutable? indexable))) (vector-set! indexable index val)]
    [(and (hash? indexable) (not (immutable? indexable))) (hash-set! indexable index val)]
    [(and (set? indexable) (not (immutable? (set-ht indexable)))) (if val
                                                          (hash-set! (set-ht indexable) index #t)
                                                          (hash-remove! (set-ht indexable) index))]
    [(setable-ref indexable #f) => (lambda (set) (set indexable index val))]
    [(and (string? indexable) (not (immutable? indexable))) (string-set! indexable index val)]
    [(and (bytes? indexable) (not (immutable? indexable))) (bytes-set! indexable index val)]
    [else
     (raise-argument-error* 'assign rhombus-realm "MutableIndexable" indexable)]))

(define-for-syntax (make-++-expression name static?)
  (expression-infix-operator
   name
   `((,(expr-quote +&) . same))
   'automatic
   (lambda (form1-in form2 stx)
     (define form1 (rhombus-local-expand form1-in))
     (define append-id (or (syntax-local-static-info form1 #'#%append)
                           (if static?
                               (raise-syntax-error '++ (string-append "specialization not known" statically-str) form1-in)
                               #'general-append)))
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

(define (general-append map1 map2)
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
